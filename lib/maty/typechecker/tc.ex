defmodule Maty.Typechecker.Tc do
  alias Maty.{ST, Utils}
  alias Maty.Typechecker.Error

  alias Maty.Types.T, as: Type
  import Maty.Types.T, only: [is?: 2]

  require Logger

  @typedoc """
  Represents types that are supported by the Maty typechecker.
  These are primitive types that can be checked directly.
  """
  @type value ::
          :any
          | :atom
          | :binary
          | :boolean
          | :date
          | nil
          | :number
          | :no_return
          | :pid
          | :ref

  @typedoc """
  Represents Elixir AST nodes that can be typechecked.
  """
  @type ast :: Macro.t()

  @typedoc """
  Environment mapping variable names to their types.
  Used to track types of variables during typechecking.
  """
  @type var_env() :: %{atom() => value()}

  # Guard that checks if a value is one of the directly supported types.
  defguardp is_supported_type(val)
            when is_atom(val) or
                   is_binary(val) or
                   is_boolean(val) or
                   is_number(val) or
                   is_pid(val) or
                   (is_map_key(val, :__struct__) and val.__struct__ == Date)

  @doc """
  Typechecks an AST node and returns its type along with potentially updated environment.

  Returns:
    - `{:ok, type, var_env}` when typechecking succeeds
    - `{:error, error_message, var_env}` when typechecking fails
  """
  @deprecated "Use TCV2.tc_expr instead"
  @spec typecheck(var_env(), ast()) :: {:ok, value(), var_env()} | {:error, binary(), var_env()}
  def typecheck(var_env, nil), do: {:ok, nil, var_env}
  def typecheck(var_env, val) when is_boolean(val), do: {:ok, :boolean, var_env}
  def typecheck(var_env, val) when is_atom(val), do: {:ok, :atom, var_env}
  def typecheck(var_env, val) when is_binary(val), do: {:ok, :binary, var_env}
  def typecheck(var_env, val) when is_number(val), do: {:ok, :number, var_env}
  def typecheck(var_env, val) when is_pid(val), do: {:ok, :pid, var_env}
  def typecheck(var_env, val) when is_reference(val), do: {:ok, :ref, var_env}
  def typecheck(var_env, {:no_return, _meta, []}), do: {:ok, :no_return, var_env}
  def typecheck(var_env, {:any, _meta, []}), do: {:ok, :any, var_env}

  # Typecheck a date literal (e.g., ~D[2021-01-01])
  def typecheck(var_env, {:%, _, [Date, {:%{}, _, _}]}), do: {:ok, :date, var_env}

  # Typecheck a date type (e.g., Date.t())
  def typecheck(var_env, {{:., _, [{:__aliases__, _, [:Date]}, :t]}, _, []}),
    do: {:ok, :date, var_env}

  # Typecheck a variable.
  # Looks up the variable in the environment and returns its type.
  def typecheck(var_env, {var, meta, ctx})
      when is_atom(var) and (is_atom(ctx) or is_nil(ctx)) do
    with {:ok, type} <- Map.fetch(var_env, var) do
      {:ok, type, var_env}
    else
      :error -> {:error, Error.variable_not_exist(meta, var), var_env}
    end
  end

  # Typecheck a 2-tuple.
  # Typechecks both elements and returns a tuple type with their types.
  def typecheck(var_env, {lhs, rhs}) do
    with {:lhs, {:ok, lhs_type, _}} <- {:lhs, typecheck(var_env, lhs)},
         {:rhs, {:ok, rhs_type, _}} <- {:rhs, typecheck(var_env, rhs)} do
      {:ok, {:tuple, [lhs_type, rhs_type]}, var_env}
    else
      {:lhs, {:error, msg, _}} -> {:error, Error.lhs_failed(msg), var_env}
      {:rhs, {:error, msg, _}} -> {:error, Error.rhs_failed(msg), var_env}
    end
  end

  # Typecheck an n-tuple (where n > 2).
  # Typechecks all elements and returns a tuple type with their types.
  def typecheck(var_env, {:{}, _, items}) when is_list(items) do
    with {:ok, {:list, types}, updated_env} <- typecheck(var_env, items) do
      {:ok, {:tuple, types}, updated_env}
    else
      {:error, msg, err_env} -> {:error, msg, err_env}
    end
  end

  # Typecheck a list.
  # Typechecks all elements and returns a list type with their types.
  # Provides detailed error information including the index of failing elements.
  def typecheck(var_env, []), do: {:ok, {:list, []}, var_env}

  def typecheck(var_env, vals) when is_list(vals) do
    vals
    |> Enum.with_index()
    |> Enum.reduce_while({[], var_env}, fn {element, index}, {acc, current_env} ->
      case typecheck(current_env, element) do
        {:ok, type, updated_env} ->
          {:cont, {[type | acc], updated_env}}

        {:error, msg, _err_env} ->
          meta =
            case element do
              {_, meta, _} -> meta
              {{_, meta, _}, _} -> meta
            end

          Logger.info(inspect(element), ansi_color: :yellow)

          error = Error.list_index_error(meta, index, msg)

          {:try, _, [[do: {:__block__, _, _block}, catch: _]]} = element

          {:halt, {:error, error, current_env}}
      end
    end)
    |> case do
      {types, final_env} ->
        {:ok, {:list, Enum.reverse(types)}, final_env}

      {:error, error, err_env} ->
        {:error, error, err_env}
    end
  end

  # Typecheck a union type specification (|).
  # For example, in a type like `integer() | string()`.
  # Returns a sorted list of the union's member types.
  def typecheck(var_env, {:|, _, items}) when is_list(items) do
    with {:ok, {:list, types}, updated_env} <- typecheck(var_env, items) do
      {:ok, {:|, Enum.sort(types)}, updated_env}
    else
      {:error, _, _} = error -> error
    end
  end

  # Typecheck arithmetic operations (+, -, *, /).
  # Ensures both operands are numbers and returns a number type.
  def typecheck(var_env, {{:., _, [:erlang, op]}, _, [lhs, rhs]}) when op in [:+, :-, :*, :/] do
    with {:ok, lhs_type, env1} <- typecheck(var_env, lhs),
         {:ok, rhs_type, env2} <- typecheck(env1, rhs) do
      case {lhs_type, rhs_type} do
        {:number, :number} -> {:ok, :number, env2}
        _ -> {:error, Error.binary_operator_requires_numbers(op, lhs_type, rhs_type), env2}
      end
    end
  end

  # Typecheck string concatenation (<>).
  # Ensures both operands are binaries and returns a binary type.
  def typecheck(var_env, {:<<>>, _, [{:"::", _, [lhs, _]}, {:"::", _, [rhs, _]}]}) do
    with {:ok, lhs_type, env1} <- typecheck(var_env, lhs),
         {:ok, rhs_type, env2} <- typecheck(env1, rhs) do
      case {lhs_type, rhs_type} do
        {:binary, :binary} -> {:ok, :binary, env2}
        _ -> {:error, Error.binary_operator_requires_binaries(lhs_type, rhs_type), env2}
      end
    end
  end

  # Typecheck comparison operations (==, !=, <, >, <=, >=).
  # For equality operations, ensures operands have the same type.
  # For ordering operations, ensures operands are numbers.
  # Returns a boolean type.
  def typecheck(var_env, {{:., _, [:erlang, op]}, _, [lhs, rhs]})
      when op in [:==, :!=, :<, :>, :<=, :>=] do
    with {:ok, lhs_type, env1} <- typecheck(var_env, lhs),
         {:ok, rhs_type, env2} <- typecheck(env1, rhs) do
      case op do
        # for equality comparisons, require that both operands have the same type
        op when op in [:==, :!=] ->
          if lhs_type == rhs_type do
            {:ok, :boolean, env2}
          else
            error = Error.comparison_operator_requires_same_type(op, lhs_type, rhs_type)
            {:error, error, env2}
          end

        # for ordering comparisons, we assume numbers are required
        _ ->
          if lhs_type == :number and rhs_type == :number do
            {:ok, :boolean, env2}
          else
            error = Error.comparison_operator_requires_numbers(op, lhs_type, rhs_type)
            {:error, error, env2}
          end
      end
    end
  end

  # Typecheck logical not operation.
  # Ensures the operand is a boolean and returns a boolean type.
  def typecheck(var_env, {{:., _, [:erlang, :not]}, _, [expr]}) do
    with {:ok, expr_type, updated_env} <- typecheck(var_env, expr) do
      if expr_type == :boolean do
        {:ok, :boolean, updated_env}
      else
        error = Error.logical_operator_requires_boolean("not", expr_type)
        {:error, error, updated_env}
      end
    end
  end

  # Typechecks a match expression (=).
  # First typechecks the right side, then validates that the left side pattern
  # can match the resulting type, updating the environment with any new bindings.
  #
  # ## Examples
  #
  #     x = 5              # Adds x -> :number to environment
  #     {a, b} = {1, "2"}  # Adds a -> :number, b -> :binary to environment
  #     [head | tail] = [1, 2, 3]  # Adds head -> :number, tail -> {:list, [:number, :number]}
  def typecheck(var_env, {:=, meta, [lhs, rhs]}) do
    with {:ok, rhs_type, updated_env} <- typecheck(var_env, rhs) do
      typecheck_match_lhs(lhs, rhs_type, updated_env, meta)
    end
  end

  # Typechecks a map literal or construction.
  # Typechecks each key-value pair and constructs a map type with the value types.
  #
  # Note: This implementation assumes that map keys are direct literals or
  # constants and does not typecheck the keys themselves. Computed or variable
  # keys are not currently supported in typechecking.
  #
  # ## Example
  #
  #     %{a: 1, b: "hello"}  # Returns {:map, %{a: :number, b: :binary}}
  def typecheck(var_env, {:%{}, _meta, pairs}) when is_list(pairs) do
    Enum.reduce_while(pairs, {:ok, %{}, var_env}, fn {key, value}, {:ok, acc, env} ->
      case typecheck(env, value) do
        {:ok, v_type, updated_env} ->
          {:cont, {:ok, Map.put(acc, key, v_type), updated_env}}

        {:error, msg, err_env} ->
          {:halt, {:error, msg, err_env}}
      end
    end)
    |> case do
      {:ok, type_map, final_env} -> {:ok, {:map, type_map}, final_env}
      error -> error
    end
  end

  # Special typecheck handler for the register function in Maty.Actor.
  # Validates arguments to ensure proper actor registration:
  # 1. ap_pid must be a pid
  # 2. role must be an atom
  # 3. callback must be a valid function
  # 4. state must be a valid maty_actor_state
  #
  # Returns the expected return type of register: {:ok, maty_actor_state}
  def typecheck(var_env, {:register, meta, args}) when length(args) == 4 do
    [ap_pid, role, callback, state] = args

    with {:arg1, {:ok, :pid, env1}} <- {:arg1, typecheck(var_env, ap_pid)},
         {:arg2, {:ok, :atom, env2}} <- {:arg2, typecheck(env1, role)},
         {:arg3, {:ok, _, env3}} <- {:arg3, typecheck(env2, callback)},
         {:arg4, {:ok, state_shape, env4}} <- {:arg4, typecheck(env3, state)},
         {:m, true, _} <- {:m, is?(state_shape, :maty_actor_state), state_shape} do
      {:ok, {:tuple, [:atom, Type.maty_actor_state()]}, env4}
    else
      {:arg1, {:ok, other_type, env}} ->
        error = Error.invalid_ap_type(meta, expected: :pid, got: other_type)
        {:error, error, env}

      {:arg1, {:error, error, env}} ->
        {:error, error, env}

      {:arg2, {:ok, other_type, env}} ->
        error = Error.role_type_invalid(meta, other_type)
        {:error, error, env}

      {:arg2, {:error, error, env}} ->
        {:error, error, env}

      {:arg3, {:error, error, env}} ->
        {:error, error, env}

      {:arg4, {:error, error, env}} ->
        {:error, error, env}

      {:m, false, other_type} ->
        error = Error.maty_actor_state_type_invalid(meta, other_type)
        {:error, error, var_env}
    end
  end

  # Errors if function tries to perform communication via raw receive
  def typecheck(var_env, {:receive, meta, _}), do: {:error, Error.no_raw_receive(meta), var_env}

  # Errors if function tries to perform communication via raw send
  def typecheck(var_env, {{:., meta, [:erlang, :send]}, _, _}) do
    {:error, Error.no_raw_send(meta), var_env}
  end

  # Typechecks an anonymous function.
  #
  # Note: This is a simplified implementation that assigns all functions the generic
  # :function type without analyzing their parameter or return types. A full
  # constraint-based type system for functions is outside the scope of this project.
  #
  # ## Example
  #
  #     fn x -> x + 1 end  # Returns :function
  def typecheck(var_env, {:fn, _, [{:->, _, [_args, _expr]}]}) do
    {:ok, :function, var_env}
  end

  # Typechecks a function capture with explicit module (e.g., &Module.func/1).
  #
  # Note: As with anonymous functions, this is a simplified implementation that
  # assigns the generic :function type without analyzing parameter or return types.
  def typecheck(var_env, {:&, _, [{:/, _, [{{:., _, [_module, _function]}, _, _}, _arity]}]}) do
    {:ok, :function, var_env}
  end

  # Typechecks a function capture without explicit module (e.g., &func/1).
  #
  # Note: As with other function forms, this is a simplified implementation that
  # assigns the generic :function type without detailed analysis.
  def typecheck(var_env, {:&, _, [{:/, _, [_function, _arity]}]}) do
    {:ok, :function, var_env}
  end

  # Typechecks a block scope
  def typecheck(var_env, {:__block__, _, body}) do
    typecheck(var_env, body)
  end

  # Typechecks a function call or another expression with a context list.
  # Attempts to look up the function name in the environment to determine its type.
  #
  # If the variable exists in the environment, returns its type.
  # Otherwise, reports an error that the function doesn't exist.
  def typecheck(var_env, {var, meta, args}) when is_atom(var) and is_list(args) do
    arity = length(args)
    func = Utils.to_func(name: var, arity: arity)

    case Map.fetch(var_env, var) do
      {:ok, type} ->
        # Variable exists directly in the environment
        {:ok, type, var_env}

      :error ->
        # Try to look up as a function with arity
        case Map.fetch(var_env, {var, arity}) do
          {:ok, clauses} when is_list(clauses) ->
            cond do
              length(clauses) == 1 ->
                [{param_types, return_type}] = clauses

                case typecheck_function_args(var_env, args, param_types, meta) do
                  {:ok, _updated_env} -> {:ok, return_type, var_env}
                  {:error, error} -> {:error, error, var_env}
                end

              true ->
                {:error, Error.ambiguous_function_call(meta, func), var_env}
            end

          :error ->
            {:error, Error.function_not_exist(func), var_env}
        end
    end
  end

  # Typechecks a left-hand side of a match expression against a known right-hand side type.
  # Updates the environment with new variable bindings and validates type compatibility.
  #
  # Returns:
  #  - `{:ok, matched_type, updated_env}` when match is valid
  #  - `{:error, error_message, env}` when match can't succeed
  @spec typecheck_match_lhs(ast(), value(), var_env(), Keyword.t()) ::
          {:ok, value(), var_env()} | {:error, binary(), var_env()}

  # Handles variable binding in pattern matching.
  # Adds or updates the variable in the environment with the right-hand side type.
  defp typecheck_match_lhs({var, _, ctx}, rhs_type, var_env, _meta)
       when is_atom(var) and (is_atom(ctx) or is_nil(ctx)) do
    updated_env = Map.put(var_env, var, rhs_type)
    {:ok, rhs_type, updated_env}
  end

  # Handles atom literals in pattern matching.
  # Ensures the right-hand side is also an atom.
  defp typecheck_match_lhs(lhs, rhs_type, var_env, meta) when is_atom(lhs) do
    if rhs_type == :atom do
      {:ok, :atom, var_env}
    else
      {:error, Error.type_mismatch(meta, expected: :atom, got: rhs_type), var_env}
    end
  end

  # Handles 2-tuple destructuring in pattern matching.
  # Recursively typechecks each element against the corresponding type from the tuple.
  defp typecheck_match_lhs({a, b}, {:tuple, [a_type, b_type]}, var_env, meta) do
    with {:ok, _, var_env} <- typecheck_match_lhs(a, a_type, var_env, meta),
         {:ok, _, var_env} <- typecheck_match_lhs(b, b_type, var_env, meta) do
      {:ok, {:tuple, [a_type, b_type]}, var_env}
    end
  end

  # Handles n-tuple destructuring in pattern matching.
  # Ensures the tuple sizes match and recursively typechecks each element.
  defp typecheck_match_lhs({:{}, _, elements}, {:tuple, types}, var_env, meta) do
    if length(elements) != length(types) do
      {:error, Error.tuple_size_mismatch(), var_env}
    else
      typecheck_match_elements(elements, types, var_env, meta)
    end
  end

  # Handles list destructuring in pattern matching.
  # Ensures the list sizes match and recursively typechecks each element.
  defp typecheck_match_lhs(elements, {:list, types}, var_env, meta) when is_list(elements) do
    if length(elements) != length(types) do
      {:error, Error.list_size_mismatch(), var_env}
    else
      typecheck_match_elements(elements, types, var_env, meta)
    end
  end

  # Fallback for unsupported pattern types in pattern matching.
  # Returns an error with information about the unsupported pattern.
  defp typecheck_match_lhs(lhs, _rhs_type, var_env, meta) do
    {:error, "#{inspect(meta)} Unsupported pattern in match: #{inspect(lhs)}", var_env}
  end

  # Helper function that recursively typechecks elements in a list or tuple pattern match.
  # Processes each element-type pair and accumulates environment changes.
  defp typecheck_match_elements(elements, types, var_env, meta) do
    Enum.zip(elements, types)
    |> Enum.reduce_while({:ok, var_env}, fn {element, type}, {:ok, env} ->
      case typecheck_match_lhs(element, type, env, meta) do
        {:ok, _, updated_env} -> {:cont, {:ok, updated_env}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, updated_env} ->
        collection_type = if is_list(elements), do: {:list, types}, else: {:tuple, types}
        {:ok, collection_type, updated_env}

      error ->
        error
    end
  end

  # Helper to typecheck function arguments against parameter types
  defp typecheck_function_args(var_env, [], [], _meta), do: {:ok, var_env}

  defp typecheck_function_args(var_env, args, param_types, meta) do
    if length(args) != length(param_types) do
      {:error, Error.arity_mismatch(meta, expected: length(param_types), got: length(args))}
    else
      Enum.zip(args, param_types)
      |> Enum.reduce_while({:ok, var_env}, fn {arg, expected_type}, {:ok, env} ->
        case typecheck(env, arg) do
          {:ok, arg_type, updated_env} ->
            if arg_type == expected_type do
              {:cont, {:ok, updated_env}}
            else
              {:halt,
               {:error, Error.arg_type_mismatch(meta, expected: expected_type, got: arg_type)}}
            end

          {:error, error, _} ->
            {:halt, {:error, error}}
        end
      end)
    end
  end

  @doc """
  Typechecks a complete function definition against its @spec annotation.

  This function orchestrates the entire typechecking process for a function:
  1. Retrieves type specifications from module attributes
  2. Ensures the number of parameters matches the function arity
  3. Constructs an initial environment with function parameters
  4. Typechecks the function body
  5. Verifies the return type matches the spec

  Returns:
    - `{:ok, return_type}` when typechecking succeeds
    - `{:error, error_message}` when typechecking fails

  Note: This implementation requires that functions have proper @spec annotations.
  """
  @spec typecheck_function(module(), {atom(), non_neg_integer()}, [Macro.t()]) ::
          [{:ok, value()} | {:error, binary()}]
  def typecheck_function(module, {_name, arity} = fn_info, clauses) do
    type_specs = Module.get_attribute(module, :psi) |> Enum.into(%{})

    with {:spec, {:ok, fn_types}} <- {:spec, Map.fetch(type_specs, fn_info)} do
      defs = fn_types |> Enum.reverse() |> Enum.zip(clauses)

      for {{spec_args, spec_return}, {meta, args, _guards, block}} <- defs do
        with {:spec_args, ^arity} <- {:spec_args, length(spec_args)} do
          # Create initial environment with typed function parameters
          typed_args =
            Enum.zip(spec_args, args)
            |> Enum.reduce_while(
              {:ok, %{}},
              fn {arg_type, arg}, {_, var_env} ->
                case arg do
                  # Simple variable parameter: func(x)
                  {arg_var, _, ctx} when ctx in [nil, Maty.Macros] ->
                    {:cont, {:ok, Map.put(var_env, arg_var, arg_type)}}

                  # Labeled parameter: func(name: value)
                  {label, {arg_var, _, nil}} when is_atom(label) ->
                    {:cont, {:ok, Map.put(var_env, arg_var, arg_type)}}

                  # List pattern - head|tail: func([head|tail])
                  {:|, _, [{head_var, _, nil}, {tail_var, _, nil}]}
                  when is_list(arg_type) or arg_type == :list ->
                    # Extract element type, assume homogeneous list
                    elem_type =
                      case arg_type do
                        {:list, [type | _]} -> type
                        # Empty list case
                        {:list, []} -> :any
                        # General list type
                        :list -> :any
                        # Fallback
                        _ -> :any
                      end

                    {:cont,
                     {:ok,
                      var_env
                      |> Map.put(head_var, elem_type)
                      |> Map.put(tail_var, {:list, [elem_type]})}}

                  # Simple list pattern with fixed elements: func([a, b, c])
                  list when is_list(list) ->
                    # Check if all elements are variables
                    all_vars =
                      Enum.all?(list, fn
                        {var, _, nil} when is_atom(var) -> true
                        _ -> false
                      end)

                    if all_vars do
                      vars = Enum.map(list, fn {var, _, _} -> var end)

                      # Determine element types based on arg_type
                      element_types =
                        case arg_type do
                          {:list, types} when length(types) == length(list) -> types
                          # Fallback
                          _ -> List.duplicate(:any, length(list))
                        end

                      updated_env =
                        Enum.zip(vars, element_types)
                        |> Enum.reduce(var_env, fn {var, type}, env -> Map.put(env, var, type) end)

                      {:cont, {:ok, updated_env}}
                    else
                      {:halt, {:error, Error.unsupported_list_pattern()}}
                    end

                  # Simple tuple pattern: func({a, b})
                  {a, b} ->
                    case arg_type do
                      {:tuple, [a_type, b_type]} ->
                        a_env =
                          case a do
                            {a_var, _, nil} when is_atom(a_var) -> Map.put(var_env, a_var, a_type)
                            # Non-variable patterns aren't bound
                            _ -> var_env
                          end

                        b_env =
                          case b do
                            {b_var, _, nil} when is_atom(b_var) -> Map.put(a_env, b_var, b_type)
                            # Non-variable patterns aren't bound
                            _ -> a_env
                          end

                        {:cont, {:ok, b_env}}

                      _ ->
                        {:halt, {:error, Error.tuple_param_type_mismatch()}}
                    end

                  # N-tuple pattern: func({a, b, c})
                  {:{}, _, elements} ->
                    case arg_type do
                      {:tuple, types} when length(types) == length(elements) ->
                        # Zip elements with their types and update environment
                        updated_env =
                          Enum.zip(elements, types)
                          |> Enum.reduce(var_env, fn
                            {{var, _, nil}, type}, env when is_atom(var) ->
                              Map.put(env, var, type)

                            _, env ->
                              # Non-variable patterns aren't bound
                              env
                          end)

                        {:cont, {:ok, updated_env}}

                      _ ->
                        {:halt, {:error, Error.n_tuple_param_type_mismatch()}}
                    end

                  # Map pattern: func(%{key: value})
                  {:%{}, _, pairs} when is_list(pairs) ->
                    case arg_type do
                      {:map, map_types} ->
                        updated_env =
                          Enum.reduce_while(pairs, {:ok, var_env}, fn
                            {key, {value_var, _, nil}}, {:ok, env} when is_atom(value_var) ->
                              if Map.has_key?(map_types, key) do
                                value_type = Map.get(map_types, key)
                                {:cont, {:ok, Map.put(env, value_var, value_type)}}
                              else
                                {:halt, {:error, Error.missing_map_key(key)}}
                              end

                            _, _ ->
                              {:halt, {:error, Error.unsupported_map_pattern()}}
                          end)

                        case updated_env do
                          {:ok, env} -> {:cont, {:ok, env}}
                          error -> {:halt, error}
                        end

                      _ ->
                        {:halt, {:error, Error.map_param_type_mismatch()}}
                    end

                  # Unsupported pattern
                  other ->
                    {:halt, {:error, Error.unsupported_param_pattern(other)}}
                end
              end
            )

          case typed_args do
            {:error, error_msg} when is_binary(error_msg) ->
              {:error, error_msg}

            {:error, _other} ->
              func = Utils.to_func(fn_info)
              error = Error.at_least_one_arg_not_well_typed(func, spec_args)
              {:error, error}

            {:ok, var_env} ->
              # Extract and typecheck function body
              body = extract_body(block)
              res = typecheck(var_env, body)

              with {:ok, {:list, types}, _var_env} <- res,
                   {:return, ^spec_return} <- {:return, List.last(types)} do
                {:ok, spec_return}
              else
                {:error, error, _var_env} ->
                  {:error, error}

                {:return, other} ->
                  error = Error.return_types_mismatch(meta, expected: spec_return, got: other)
                  {:error, error}
              end
          end
        else
          {:spec_args, _} ->
            func = Utils.to_func(fn_info)
            error = Error.arity_mismatch(meta, func)
            {:error, error}
        end
      end
    else
      {:spec, :error} ->
        error = Error.no_spec_for_function(type_specs)
        Logger.error("[#{module}] Missing function spec #{error}")
        {:error, error}
    end
  end

  # tcExpr :: Env -> ST -> Expr -> Either String (Maybe (Type, ST))
  @spec session_typecheck(module(), var_env(), ST.t(), ast()) ::
          {:error, binary(), var_env()}
          | {:ok, {:just, ST.t()}, var_env()}
          | {:ok, MapSet.t(:done | :suspend), var_env()}

  # Typechecks `maty_send` operations within a session-typed context.
  #
  # This function ensures that message sending operations comply with the current session type.
  # Specifically, it validates:
  # 1. The current session state must be in a sending state (SOut)
  # 2. The message label must match one of the available branches
  # 3. The destination role must match the expected recipient
  # 4. The payload must have the expected type
  #
  # Returns:
  #  - `{:ok, {:just, next_state}, env}` - Type check succeeded, with the next session state
  #  - `{:error, error_message, env}` - Type check failed with specific reason
  def session_typecheck(
        _module,
        var_env,
        st,
        {:maty_send, meta, [_session, role, {label, payload}]}
      )
      when is_atom(role) and is_atom(label) do
    with {:pre, %ST.SOut{to: expected_role, branches: branches}} <- {:pre, st} do
      case find_matching_branch(branches, label) do
        {:ok, branch} ->
          validate_send_operation(
            meta,
            var_env,
            role,
            payload,
            expected_role,
            branch
          )

        :error ->
          # here I could potentially report possible valid continuations
          {:error, Error.no_branch_with_this_label(meta, got: label), var_env}
      end
    else
      {:pre, other} ->
        error =
          Error.pre_condition_cannot_send(meta,
            expected: %ST.SOut{to: :role, branches: []},
            got: other
          )

        {:error, error, var_env}
    end
  end

  # Typechecks a `case` expression in a session-typed context.
  #
  # In session typing, a `case` expression must handle all possible branches of the current session type.
  # This function ensures that:
  # 1. The scrutinee expression is well-typed
  # 2. Each branch of the case statement corresponds to a branch in the session type
  # 3. All possible session type branches are covered by the case branches
  #
  # Returns:
  #  - `{:ok, MapSet.t(:done | :suspend), env}` - All branches of the session type are handled
  #  - `{:error, error_message, env}` - Some branches are not handled or other errors
  def session_typecheck(module, var_env, st, {:case, meta, [expr, [do: branches]]}) do
    case typecheck(var_env, expr) do
      {:ok, _expr_type, updated_env} ->
        case validate_case_branches(module, updated_env, st, branches, meta) do
          {:ok, branch_returns} -> {:ok, branch_returns, updated_env}
          {:error, msg} -> {:error, msg, updated_env}
        end

      {:error, error, env} ->
        {:error, error, env}
    end
  end

  # Typechecks suspending with a handler in a session-typed context.
  #
  # In session typing, a function can only suspend with a handler when:
  # 1. The current session state is an SName type, indicating continuation with a named handler
  # 2. The handler being suspended to matches the expected handler in the session type
  # 3. The role specified in the suspension matches the expected role for the handler
  # 4. The suspended state is a valid maty_actor_state
  #
  # Returns:
  #  - `{:ok, MapSet.t(:suspend), env}` - Suspension is valid according to session types
  #  - `{:error, error_message, env}` - Suspension is invalid with specific reason
  def session_typecheck(
        _module,
        var_env,
        pre,
        {{:., _, [:erlang, :throw]}, _, [{:{}, meta, [:suspend, handler_label, state_ast]}]}
      ) do
    with {:pre, %ST.SName{handler: expected_handler}} <- {:pre, pre} do
      with {:ok, state_shape, var_env} <- typecheck(var_env, state_ast),
           {:state, true, _} <- {:state, is?(state_shape, :maty_actor_state), state_shape},
           {:handler, ^expected_handler} <- {:handler, handler_label} do
        {:ok, MapSet.new([:suspend]), var_env}
      else
        {:error, msg, env} ->
          {:error, msg, env}

        {:state, false, other_shape} ->
          error = Error.maty_actor_state_type_invalid(meta, other_shape)
          {:error, error, var_env}

        {:handler, other_handler} ->
          error = Error.handler_mismatch(meta, expected: expected_handler, got: other_handler)
          {:error, error, var_env}
      end
    else
      {:pre, other} ->
        error = Error.invalid_suspension_state(meta, other)
        {:error, error, var_env}
    end
  end

  # Typechecks the :done tuple which ends communication.
  # Makes sure that this only happens when the session precondition states communication must end
  #
  # returns {:error, binary(), var_env()} | {:ok, MapSet.t(:done), var_env()}

  # {{:., _, [:erlang, :throw]}, _, [{:done, state_ast}]}

  def session_typecheck(_module, var_env, pre, {:{}, meta, [:done, :ok, state_ast]}) do
    with {:pre, %ST.SEnd{}} <- {:pre, pre},
         {:ok, state_shape, var_env} <- typecheck(var_env, state_ast),
         {:m, true, _} <- {:m, is?(state_shape, :maty_actor_state), state_shape} do
      {:ok, MapSet.new([:done]), var_env}
    else
      {:pre, other} ->
        error = Error.invalid_termination_state(meta, other)
        {:error, error, var_env}

      {:error, error, var_env} ->
        {:error, error, var_env}

      {:m, false, other_type} ->
        error = Error.maty_actor_state_type_invalid(meta, other_type)
        {:error, error, var_env}
    end
  end

  # Default fallback for expressions in a session-typed context.
  #
  # Most expressions (assignments, function calls, literals, etc.) don't affect the session type.
  # This function delegates to the standard typechecking function while preserving the
  # current session type.
  #
  # The only expressions that progress session types are:
  # - maty_send operations (sending a message)
  # - suspend operations (continuing with a handler)
  # - done operations (ending communication)
  # - case expressions (branching based on received messages)
  #
  # returns {:error, binary(), var_env()} | {:ok, {:just, ST.t()}, var_env()}
  def session_typecheck(module, var_env, st, expr) do
    type_specs = Module.get_attribute(module, :psi) |> Enum.into(%{})
    updated_var_env = Map.merge(var_env, type_specs)

    case typecheck(updated_var_env, expr) do
      {:ok, _some_type, new_env} ->
        filtered_env = Map.drop(new_env, Map.keys(type_specs))
        {:ok, {:just, st}, filtered_env}

      {:error, msg, env} ->
        {:error, msg, env}
    end
  end

  @spec find_matching_branch([ST.t()], atom()) :: :error | {:ok, ST.t()}
  defp find_matching_branch(branches, label) do
    case Enum.find(branches, &(&1.label == label)) do
      nil -> :error
      branch -> {:ok, branch}
    end
  end

  @spec validate_send_operation(keyword(), var_env(), atom(), term(), atom(), ast()) ::
          {:error, binary(), var_env()}
          | {:ok, {:just, ST.t()}, var_env()}
  defp validate_send_operation(meta, var_env, role, payload, expected_role, branch) do
    with {:ok, payload_type, _} <- typecheck(var_env, payload),
         :ok <- validate_payload_type(meta, payload_type, branch.payload),
         :ok <- validate_role(meta, role, expected_role) do
      {:ok, {:just, branch.continue_as}, var_env}
    else
      {:error, msg} -> {:error, msg, var_env}
      {:error, msg, env} -> {:error, msg, env}
    end
  end

  # Helper validation functions
  @spec validate_role(keyword(), term(), term()) :: {:error, binary()} | :ok
  defp validate_payload_type(meta, actual, expected) do
    if actual == expected do
      :ok
    else
      {:error, Error.message_payload_type_mismatch(meta, expected: expected, got: actual)}
    end
  end

  @spec validate_role(keyword(), atom(), atom()) :: {:error, binary()} | :ok
  defp validate_role(meta, actual, expected) do
    if actual == expected do
      :ok
    else
      {:error, Error.send_role_mismatch(meta, expected: expected, got: actual)}
    end
  end

  @spec validate_case_branches(module(), var_env(), St.t(), ast(), keyword()) ::
          {:error, binary()}
          | {:ok, MapSet.t(:done | :suspend)}
  defp validate_case_branches(module, var_env, st, branches, meta) do
    case get_session_branches(st) do
      {:ok, all_branches, st_branches} ->
        handled_branch_ids = process_case_branches(module, var_env, st_branches, branches)
        check_branch_coverage(all_branches, handled_branch_ids, meta)

      {:error, error} ->
        {:error, error}
    end
  end

  @spec process_case_branches(module(), var_env(), [ST.t()], ast()) :: MapSet.t()
  defp process_case_branches(module, var_env, st_branches, branches) do
    for {:->, _meta, [_, branch_block]} <- branches, reduce: MapSet.new() do
      acc ->
        body = extract_body(branch_block)

        case handle_session_branch(module, var_env, st_branches, body) do
          {:ok, id} -> MapSet.put(acc, id)
          {:error, _} -> acc
        end
    end
  end

  @spec check_branch_coverage(MapSet.t(), MapSet.t(), keyword()) ::
          {:error, binary()}
          | {:ok, MapSet.t(:done | :suspend)}
  defp check_branch_coverage(all_branches, handled_branch_ids, meta) do
    if MapSet.equal?(all_branches, handled_branch_ids) do
      branch_returns =
        all_branches
        |> Enum.map(&get_branch_leaf/1)
        |> MapSet.new()

      {:ok, branch_returns}
    else
      unhandled = MapSet.difference(all_branches, handled_branch_ids) |> MapSet.size()
      {:error, Error.unhandled_session_branches(meta, unhandled)}
    end
  end

  @doc """
  Typechecks an entire handler.
  Checks the arguments passed to the handler to makes sure everything looks good.
  Typechecks the body of the handler
  And ensures there are no session types left over.
  """
  @spec session_typecheck_handler(module, {atom(), integer()}, ast()) ::
          {:error, binary()}
          | {:ok, nil}
  def session_typecheck_handler(module, handler, clauses) do
    delta = Utils.Env.get_map(module, :delta_M)
    type_specs = Utils.Env.get_map(module, :psi)

    with {:ok, %{function: fn_info, st: st}} <- Map.fetch(delta, handler),
         {:ok, fn_types} <- Map.fetch(type_specs, fn_info),
         variants = length(fn_types),
         {:branches, ^variants} <- {:branches, length(st.branches)} do
      func = Utils.to_func(fn_info)
      defs = Enum.zip([Enum.reverse(fn_types), clauses, st.branches])

      for {{spec_args, _spec_return}, {meta, args, _guards, block}, branch} <- defs do
        with {:spec_args, message_t} <- {:spec_args, Enum.at(spec_args, 1)},
             {:payload_type, {:tuple, [:atom, payload_t]}} <- {:payload_type, message_t} do
          [
            role,
            {label, payload},
            {session_ctx_var, _, _},
            {maty_actor_state_var, _, _}
          ] = args

          # construct the type environment
          var_env = %{
            session_ctx_var => Type.session_ctx(),
            maty_actor_state_var => Type.maty_actor_state()
          }

          var_env =
            case payload do
              val when is_supported_type(val) ->
                var_env

              {payload_var, _, nil} when is_atom(payload_var) ->
                Map.put(var_env, payload_var, payload_t)

              other_type ->
                # todo: handle more complicated shapes here
                error = Error.payload_var_has_other_value(meta, other_type)
                Logger.error("[#{module}] Unsupported payload shape #{error}")
                var_env
            end

          cond do
            role != st.from ->
              error =
                Error.handler_role_mismatch(meta,
                  expected: st.from,
                  got: role
                )

              {:error, error}

            branch.label != label ->
              error =
                Error.handler_message_label_mismatch(meta,
                  expected: branch.label,
                  got: label
                )

              {:error, error}

            branch.payload != payload_t ->
              error =
                Error.message_payload_type_mismatch(meta,
                  expected: branch.payload,
                  got: payload_t
                )

              {:error, error}

            true ->
              st = branch.continue_as
              body = extract_handler_body(block)

              case session_typecheck_block(module, var_env, st, body) do
                {:ok, {:just, remaining_st}, _var_env} ->
                  error = Error.remaining_session_type(func, remaining_st)
                  {:error, error}

                {:ok, _exits, _var_env} ->
                  {:ok, nil}

                {:error, error, _var_env} ->
                  {:error, error}
              end
          end
        else
          {:payload_type, shape} ->
            error = Error.message_format_invalid(meta, got: shape)
            {:error, error}
        end
      end
    else
      {:branches, _} ->
        error = Error.insufficient_function_clauses()
        Logger.error("[#{module}] Incomplete Session Type branch coverage #{error}")
        {:error, error}
    end
  end

  # Typechecks the body of a handler by progressing and propagating the updated session type each time an expression is evaluated
  @spec session_typecheck_block(module(), var_env(), ST.t(), ast()) ::
          {:error, binary(), var_env()}
          | {:ok, {:just, ST.t()}, var_env()}
          | {:ok, MapSet.t(:done | :suspend), var_env()}
  def session_typecheck_block(module, var_env, st, expressions) when is_list(expressions) do
    Enum.reduce_while(expressions, {:ok, {:just, st}, var_env}, fn
      expr, {:ok, {:just, current_st}, current_env} ->
        case session_typecheck(module, current_env, current_st, expr) do
          {:ok, {:just, new_st}, new_env} ->
            {:cont, {:ok, {:just, new_st}, new_env}}

          {:ok, exits, new_env} ->
            {:halt, {:ok, exits, new_env}}

          {:error, error, new_env} ->
            {:halt, {:error, error, new_env}}
        end
    end)
  end

  def session_typecheck_init_handler(module, handler, _clauses) do
    delta = Utils.Env.get_map(module, :delta_I)
    type_specs = Utils.Env.get_map(module, :psi)

    with {:ok, %{function: fn_info, st: st}} when not is_nil(st) <- Map.fetch(delta, handler),
         {:ok, _fn_types} <- Map.fetch(type_specs, fn_info) do
      # check that the init_handler only ever sends and suspends
      # ensure sufficient branch coverage
      # todo: needs to be implemented
      :under_construction
    else
      other ->
        Logger.error("hmm: #{inspect(other)}")

        hello = Map.fetch(delta, handler)
        Logger.debug("#{handler}\n\nDELTA: #{inspect(delta)}\n\nhello: #{inspect(hello)}")
        # Map.fetch(type_specs, fn_info)
        {:error, "Under construction"}
    end
  end

  # Special function for ensuring the init_actor callback is typed properly
  @spec session_typecheck_init_actor(module(), {atom(), integer()}, ast()) ::
          {:error, binary()}
          | {:ok, term()}
  def session_typecheck_init_actor(module, fn_info, clauses) do
    type_specs = Module.get_attribute(module, :psi) |> Enum.into(%{})

    with {:spec, {:ok, fn_types}} <- {:spec, Map.fetch(type_specs, fn_info)} do
      for {{spec_args, spec_return}, clause} <- Enum.zip(fn_types, clauses) do
        {meta, args, _guards, block} = clause

        var_env =
          Enum.zip(args, spec_args)
          |> Enum.reduce(%{}, fn
            {{fst, snd}, {:tuple, [fst_type, snd_type]}}, var_env ->
              var_env
              |> update_env(fst, fst_type)
              |> update_env(snd, snd_type)

            {{collection, items}, arg_types}, var_env when collection in [:tuple, :list] ->
              items
              |> Enum.zip(arg_types)
              |> Enum.reduce(var_env, fn {var, type}, acc -> update_env(acc, var, type) end)

            {{_arg_var, _, nil} = arg, arg_type}, var_env ->
              update_env(var_env, arg, arg_type)

            # todo: add more clauses here to allow more arg shapes
            other, acc ->
              error = Error.unsupported_argument_shape(meta, other)
              Logger.error("[#{module}] Unsupported arg shape #{error}")
              acc
          end)

        body = block |> extract_body()
        res = typecheck(var_env, body)

        with {:ok, {:list, types}, _var_env} <- res,
             {:register, true} <- {:register, contains_register_call?(block)},
             {:return, ^spec_return} <- {:return, List.last(types)} do
          {:ok, spec_return}
        else
          {:error, error, _var_env} ->
            {:error, error}

          {:register, false} ->
            {:error, Error.missing_registration()}

          {:return, other} ->
            error = Error.return_types_mismatch(meta, expected: spec_return, got: other)
            {:error, error}
        end
      end
    else
      {:spec, other} ->
        Logger.error("[#{module}] Function spec is broken: #{other}")
        {:error, Error.unexpected()}
    end
  end

  @spec performs_communication?(ast()) :: boolean()
  def performs_communication?({_meta, _args, _guards, block}) do
    {_, found} =
      block
      |> extract_body()
      |> Macro.prewalk(false, fn
        {:maty_send, _meta, _args} = node, _acc -> {node, true}
        node, acc -> {node, acc}
      end)

    found
  end

  # Determines which branch of a session type (if any) is handled by a particular code branch.
  #
  # For each session type branch, it attempts to typecheck the body with that branch as the precondition.
  # If typechecking succeeds, that branch is considered handled.
  #
  # Returns:
  #  - `{:ok, branch_id}` - Successfully identified a single handled branch
  #  - `{:error, reason}` - No branches are handled or multiple branches are handled
  @spec handle_session_branch(module(), var_env(), [ST.t()], ast()) ::
          {:error, ST.SBranch.t(), binary()}
          | {:handled, ST.SBranch.t()}

  defp handle_session_branch(module, var_env, st_branches, body) do
    results =
      for st_branch <- st_branches do
        branch_id = st_branch.branches |> List.first()

        case session_typecheck_block(module, var_env, st_branch, body) do
          {:ok, {:just, _}, _} ->
            error = Error.branch_not_fully_handled(branch_id)
            {:error, branch_id, error}

          {:ok, exits, _} ->
            if MapSet.size(exits) > 1 do
              error = Error.too_many_branch_exits(branch_id)
              {:error, branch_id, error}
            else
              {:handled, branch_id}
            end

          {:error, error, _} ->
            {:error, branch_id, error}
        end
      end

    handled =
      results
      |> Enum.filter(fn r -> match?({:handled, _}, r) end)
      |> Enum.map(fn {:handled, id} -> id end)

    errors = results |> Enum.filter(fn r -> match?({:error, _, _}, r) end)

    cond do
      length(handled) == 1 ->
        {:ok, hd(handled)}

      length(handled) > 1 ->
        {:error, Error.ambiguous_branch_match(handled)}

      length(errors) > 0 && length(handled) == 0 ->
        error_messages = Enum.map(errors, fn {:error, id, msg} -> Error.at_branch(id, msg) end)
        {:error, Error.type_errors_prevented_match(error_messages)}

      true ->
        # No branches matched but no type errors occurred
        {:error, Error.no_compatible_session_branch()}
    end
  end

  # checks the AST of the init_actor callback to check if the function registers in a session
  @spec contains_register_call?(ast()) :: boolean()
  defp contains_register_call?(ast) do
    {_, found} =
      Macro.prewalk(ast, false, fn
        {:register, _meta, _args} = node, _acc -> {node, true}
        node, acc -> {node, acc}
      end)

    found
  end

  @spec update_env(var_env(), ast(), term()) :: var_env()
  defp update_env(var_env, {var, _, _}, type) do
    Map.update(var_env, var, type, fn _ -> type end)
  end

  @spec flatten_branches(ST.t()) :: [ST.t()]
  defp flatten_branches(%ST.SOut{to: role} = st) do
    st.branches |> Enum.map(fn x -> %ST.SOut{to: role, branches: [x]} end)
  end

  defp flatten_branches(%ST.SIn{from: role} = st) do
    st.branches |> Enum.map(fn x -> %ST.SIn{from: role, branches: [x]} end)
  end

  @spec get_session_branches(ST.t()) ::
          {:error, binary()}
          | {:ok, MapSet.t(ST.SBranch.t()), [ST.t()]}
  defp get_session_branches(%ST.SIn{from: from, branches: branches}) do
    {:ok, MapSet.new(branches), flatten_branches(%ST.SIn{from: from, branches: branches})}
  end

  defp get_session_branches(%ST.SOut{to: to, branches: branches}) do
    {:ok, MapSet.new(branches), flatten_branches(%ST.SOut{to: to, branches: branches})}
  end

  defp get_session_branches(%ST.SName{} = st), do: {:error, Error.no_branches_in_suspend(st)}

  defp get_session_branches(%ST.SEnd{} = st), do: {:error, Error.no_branches_in_end(st)}

  defp get_session_branches(other), do: {:error, Error.unsupported_session_type_in_branch(other)}

  @spec get_branch_leaf(ST.t()) :: :done | :suspend
  defp get_branch_leaf(st) do
    case st do
      %ST.SBranch{} -> st.continue_as |> get_branch_leaf()
      %ST.SEnd{} -> :done
      %ST.SName{} -> :suspend
      _ -> st.branches |> hd() |> get_branch_leaf()
    end
  end

  # Extracts a list of expressions from an AST body.
  #
  # This handles both block expressions (`do: begin ... end`) and single expressions.
  #
  # Returns:
  #   - A list of expressions representing the body
  @spec extract_body(Macro.t()) :: [Macro.t()]
  defp extract_body({:__block__, _, block}), do: block
  defp extract_body(expr), do: [expr]

  defp extract_handler_body(block) do
    case block do
      {:try, _, [[do: {:__block__, _, [_, _, {:__block__, _, body}]}, catch: _]]} ->
        body

      {:try, _, [[do: {:__block__, _, [_, _, {:case, _, [_, _]} = case_expr]}, catch: _]]} ->
        [case_expr]

      {:try, _, [[do: {:__block__, _, body}, catch: _]]} ->
        body
    end
  end
end
