defmodule Maty.Typechecker.Preprocessor do
  require Logger

  alias Maty.Typechecker.TypeSpecParser
  alias Maty.Typechecker.Error
  alias Maty.Utils

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

      {:error, parse_error} ->
        error =
          Error.TypeSpecification.invalid_session_type_annotation(
            module,
            meta,
            handler_label,
            parse_error
          )

        Logger.error(error)
        {:error, error}
    end

    Module.delete_attribute(module, handler_kind)
  end

  def process_type_annotation(module: module, function: func_id = {name, args}) do
    case Module.get_attribute(module, :spec) do
      [{:spec, {:"::", meta, [{spec_name, _, args_asts}, return_ast]}, _module} | _] ->
        arity = length(args)

        with {:info, {^name, ^arity}} <- {:info, {spec_name, length(args_asts)}},
             {:args_ok, parsed_arg_types} <-
               {:args_ok, parse_spec_args(args_asts)},
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
              Error.TypeSpecification.function_spec_info_mismatch(
                module,
                meta,
                spec_id: {spec_name, length(args_asts)},
                func_id: func_id
              )

            Logger.error(error)
            Module.put_attribute(module, :spec_errors, {{name, arity}, error})

          {:args_ok, {:error, {failed_index, internal_error}}} ->
            error =
              Error.TypeSpecification.spec_args_parse_error_at(
                module,
                meta,
                func_id,
                failed_index,
                args_asts,
                internal_error
              )

            Logger.error(error)
            Module.put_attribute(module, :spec_errors, {{name, arity}, error})

          {:return_ok, {:error, parse_error}} ->
            error =
              Error.TypeSpecification.spec_return_not_well_typed(
                module,
                meta,
                spec_name,
                return_ast,
                parse_error
              )

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

  defp parse_spec_args(asts) do
    parsed_results = Enum.map(asts, &TypeSpecParser.parse/1)
    failed_index = Enum.find_index(parsed_results, fn res -> match?({:error, _}, res) end)

    if is_nil(failed_index) do
      Enum.map(parsed_results, fn {:ok, type} -> type end)
    else
      {:error, parsed_error} = Enum.at(parsed_results, failed_index)
      {:error, {failed_index, parsed_error}}
    end
  end
end
