defmodule Maty.Typechecker.Preprocessor do
  alias Maty.Typechecker.TypeSpecParser
  alias Maty.Typechecker.Error
  alias Maty.Utils
  require Logger

  def process_handler_annotation(
        module: module,
        function: {name, arity},
        handler_label: handler_label,
        session_types: session_types,
        store: handler_store,
        kind: handler_kind,
        meta: meta
      ) do
    with {:ok, session_type} <- Map.fetch(session_types, handler_label),
         {:ok, st} <- Maty.Parser.parse(session_type) do
      Utils.Env.add_at_key(
        module,
        handler_store,
        handler_label,
        %{function: {name, arity}, st: st}
      )
    else
      :error ->
        error = Error.missing_handler(handler_label, meta)
        Logger.error(error)
        {:error, error}

      {:error, _} ->
        error = Error.invalid_session_type_annotation(handler_label)
        Logger.error(error)
        {:error, error}
    end

    Module.delete_attribute(module, handler_kind)
  end

  def process_type_annotation(module: module, function: func_id = {name, args}) do
    case Module.get_attribute(module, :spec) do
      [{:spec, {:"::", _, [{spec_name, _, args_asts}, return_ast]}, _module} | _] ->
        arity = length(args)

        with {:info, {^name, ^arity}} <- {:info, {spec_name, length(args_asts)}},
             {:args_ok, parsed_arg_types} <- {:args_ok, parse_spec_args(args_asts, spec_name)},
             {:return_ok, {:ok, parsed_return_type}} <-
               {:return_ok, TypeSpecParser.parse(return_ast)} do
          Utils.Env.prepend_to_key(
            module,
            :psi,
            {name, arity},
            {parsed_arg_types, parsed_return_type}
          )

          Module.delete_attribute(module, :spec)
        else
          {:info, _} ->
            error =
              Error.function_spec_info_mismatch(
                spec_name: spec_name,
                spec_arity: length(args_asts),
                fn_name: name,
                fn_arity: arity
              )

            Logger.error(error)
            Module.put_attribute(module, :spec_errors, {{name, arity}, error})

          {:args_ok, {:error, msg}} ->
            error = Error.spec_args_not_well_typed(spec_name, args_asts, msg)
            Logger.error(error)
            Module.put_attribute(module, :spec_errors, {{name, arity}, error})

          {:return_ok, {:error, msg}} ->
            error = Error.spec_return_not_well_typed(spec_name, return_ast, msg)
            Logger.error(error)
            Module.put_attribute(module, :spec_errors, {{name, arity}, error})

          other ->
            error = Error.internal_error(func_id, other)

            Logger.error(error)
            Module.put_attribute(module, :spec_errors, {{name, arity}, error})
        end

      _ ->
        :ok
    end
  end

  defp parse_spec_args(asts, spec_name) do
    parsed_results = Enum.map(asts, &TypeSpecParser.parse/1)
    failed_index = Enum.find_index(parsed_results, fn res -> match?({:error, _}, res) end)

    if is_nil(failed_index) do
      Enum.map(parsed_results, fn {:ok, type} -> type end)
    else
      {:error, msg} = Enum.at(parsed_results, failed_index)
      {:error, Error.parse_error_at(failed_index, spec_name, msg)}
    end
  end
end

defmodule Maty.Typechecker.TypeSpecParser do
  alias Maty.Typechecker.Error
  alias Maty.Types.T, as: Type

  @doc """
  Parses a restricted type specification AST into an internal type representation.

  Handles:
  - :atom, nil
  - {atom_name, _, []}  (looks up atom_name in type_env or treats as basic type)
  - {ast1, ast2}        (2-tuple)
  - {:{}, _, elements} (n-tuple)
  """
  @spec parse(type_ast :: Macro.t()) :: {:ok, Type.t()} | {:error, String.t()}
  def parse(type_ast) do
    type_env =
      Maty.Types.payload_types()
      |> Enum.map(&{&1, &1})
      |> Enum.into(%{})
      |> Map.merge(Maty.Types.map())
      |> Map.put(:no_return, :no_return)
      |> Map.put(:any, :any)
      |> Map.put(:map, :map)

    parse(type_ast, type_env)
  end

  defp parse(type_ast, type_env) do
    do_parse(type_ast, type_env)
  end

  # Base Case: Literal nil
  defp do_parse(nil, _type_env), do: {:ok, nil}

  # Date Type constructor
  defp do_parse({{:., _, [{:__aliases__, _, [:Date]}, :t]}, _, []}, _type_env), do: {:ok, :date}

  # Base Case: Literal Atom (e.g., :ok, :error, :atom)
  defp do_parse(atom_name, _type_env) when is_atom(atom_name) do
    {:ok, atom_name}
  end

  # Case 1: Zero-arity Type Constructor {name, _, []}
  defp do_parse({atom_name, _meta, []}, type_env) when is_atom(atom_name) do
    case Map.fetch(type_env, atom_name) do
      {:ok, internal_type} -> {:ok, internal_type}
      :error -> {:error, Error.unknown_type_constructor(atom_name)}
    end
  end

  # Case 2: 2-Tuple {ast1, ast2}
  defp do_parse({ast1, ast2}, type_env) when is_tuple(ast1) or is_atom(ast1) do
    parse_tuple_elements([ast1, ast2], type_env)
  end

  # Case 3: N-Tuple {:{}, _, [elements...]}
  defp do_parse({:{}, _meta, elements_list}, type_env) when is_list(elements_list) do
    parse_tuple_elements(elements_list, type_env)
  end

  # Catch-all for unsupported AST formats
  defp do_parse(other_ast, _type_env) do
    {:error, Error.unsupported_type_constructor(other_ast)}
  end

  defp parse_tuple_elements(element_asts, type_env) do
    parsed_elements =
      Enum.reduce_while(
        element_asts,
        [],
        fn elem_ast, acc ->
          case do_parse(elem_ast, type_env) do
            {:ok, parsed_type} -> {:cont, [{:ok, parsed_type} | acc]}
            {:error, _} = err -> {:halt, err}
          end
        end
      )

    case parsed_elements do
      {:error, _} = first_error ->
        first_error

      list_of_oks when is_list(list_of_oks) ->
        final_types =
          list_of_oks
          |> Enum.reverse()
          |> Enum.map(fn {:ok, type} -> type end)

        {:ok, {:tuple, final_types}}

      _ ->
        {:error, Error.internal_error("parsing tuple elements")}
    end
  end
end
