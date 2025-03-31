defmodule Maty.Typechecker.Tc do
  alias Maty.{ST, Utils}
  alias Maty.Typechecker.Error

  alias Maty.Types.T, as: Type
  import Maty.Types.T, only: [is?: 2, is_handler_return?: 1]

  require Logger

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
          | :reference
          | :unit

  @type ast :: Macro.t()

  @type var_env() :: %{atom() => value()}

  defguardp is_supported_type(val)
            when is_atom(val) or
                   is_binary(val) or
                   is_boolean(val) or
                   is_number(val) or
                   is_pid(val) or
                   (is_map_key(val, :__struct__) and val.__struct__ == Date)

  # tcVal :: Env -> Val -> Either String Type
  @spec typecheck(var_env(), ast()) :: {:ok, value(), var_env()} | {:error, binary(), var_env()}
  def typecheck(var_env, :unit), do: {:ok, :unit, var_env}
  def typecheck(var_env, nil), do: {:ok, nil, var_env}
  def typecheck(var_env, val) when is_boolean(val), do: {:ok, :boolean, var_env}
  def typecheck(var_env, val) when is_atom(val), do: {:ok, :atom, var_env}
  def typecheck(var_env, val) when is_binary(val), do: {:ok, :binary, var_env}
  def typecheck(var_env, val) when is_number(val), do: {:ok, :number, var_env}
  def typecheck(var_env, val) when is_pid(val), do: {:ok, :pid, var_env}
  def typecheck(var_env, val) when is_reference(val), do: {:ok, :reference, var_env}

  # date literal
  def typecheck(var_env, {:%, _, [Date, {:%{}, _, _}]}), do: {:ok, :date, var_env}

  # date type
  def typecheck(var_env, {{:., _, [{:__aliases__, _, [:Date]}, :t]}, _, []}),
    do: {:ok, :date, var_env}

  # variables
  def typecheck(var_env, {var, meta, ctx})
      when is_atom(var) and (is_atom(ctx) or is_nil(ctx)) do
    case Map.fetch(var_env, var) do
      {:ok, type} -> {:ok, type, var_env}
      :error -> {:error, Error.variable_not_exist(meta, var), var_env}
    end
  end

  # 2-tuples
  def typecheck(var_env, {lhs, rhs}) do
    with {:lhs, {:ok, lhs_type, _}} <- {:lhs, typecheck(var_env, lhs)},
         {:rhs, {:ok, rhs_type, _}} <- {:rhs, typecheck(var_env, rhs)} do
      {:ok, {:tuple, [lhs_type, rhs_type]}, var_env}
    else
      {:lhs, {:error, msg, _}} -> {:error, Error.lhs_failed(msg), var_env}
      {:rhs, {:error, msg, _}} -> {:error, Error.rhs_failed(msg), var_env}
    end
  end

  # 3-tuples or larger
  def typecheck(var_env, {:{}, _, items}) when is_list(items) do
    case typecheck(var_env, items) do
      {:ok, {:list, types}, var_env} -> {:ok, {:tuple, types}, var_env}
      {:error, _, _} = error -> error
    end
  end

  # lists
  def typecheck(var_env, vals) when is_list(vals) do
    res =
      vals
      |> Enum.reduce_while({[], var_env}, fn x, {acc, var_env} ->
        case typecheck(var_env, x) do
          {:ok, type, var_env} -> {:cont, {[type | acc], var_env}}
          # todo: give better error messages
          {:error, msg, _var_env} -> {:halt, {:error, msg, var_env}}
        end
      end)

    case res do
      {types, _} -> {:ok, {:list, Enum.reverse(types)}, var_env}
      {:error, error, var_env} -> {:error, error, var_env}
    end
  end

  # type spec or
  def typecheck(var_env, {:|, _, items}) when is_list(items) do
    case typecheck(var_env, items) do
      {:ok, {:list, types}, var_env} -> {:ok, {:|, Enum.sort(types)}, var_env}
      {:error, _, _} = error -> error
    end
  end

  # arithmetic operators
  def typecheck(var_env, {{:., _, [:erlang, op]}, _, [lhs, rhs]}) when op in [:+, :-, :*, :/] do
    with {:ok, lhs_type, var_env} <- typecheck(var_env, lhs),
         {:ok, rhs_type, var_env} <- typecheck(var_env, rhs) do
      case {lhs_type, rhs_type} do
        {:number, :number} -> {:ok, :number, var_env}
        _ -> {:error, Error.binary_operator_requires_numbers(op, lhs_type, rhs_type), var_env}
      end
    end
  end

  # string concatenation operator
  def typecheck(var_env, {:<<>>, _, [{:"::", _, [lhs, _]}, {:"::", _, [rhs, _]}]}) do
    with {:ok, lhs_type, var_env} <- typecheck(var_env, lhs),
         {:ok, rhs_type, var_env} <- typecheck(var_env, rhs) do
      case {lhs_type, rhs_type} do
        {:binary, :binary} -> {:ok, :binary, var_env}
        _ -> {:error, Error.binary_operator_requires_binaries(lhs_type, rhs_type), var_env}
      end
    end
  end

  # comparison operators
  def typecheck(var_env, {{:., _, [:erlang, op]}, _, [lhs, rhs]})
      when op in [:==, :!=, :<, :>, :<=, :>=] do
    with {:ok, lhs_type, var_env} <- typecheck(var_env, lhs),
         {:ok, rhs_type, var_env} <- typecheck(var_env, rhs) do
      case op do
        # for equality comparisons, require that both operands have the same type
        op when op in [:==, :!=] ->
          if lhs_type == rhs_type do
            {:ok, :boolean, var_env}
          else
            error = Error.comparison_operator_requires_same_type(op, lhs_type, rhs_type)
            {:error, error, var_env}
          end

        # for ordering comparisons, we assume numbers are required
        _ ->
          if lhs_type == :number and rhs_type == :number do
            {:ok, :boolean, var_env}
          else
            error = Error.comparison_operator_requires_numbers(op, lhs_type, rhs_type)
            {:error, error, var_env}
          end
      end
    end
  end

  # logical not
  def typecheck(var_env, {{:., _, [:erlang, :not]}, _, [expr]}) do
    with {:ok, expr_type, var_env} <- typecheck(var_env, expr) do
      if expr_type == :boolean do
        {:ok, :boolean, var_env}
      else
        error = Error.logical_operator_requires_boolean("not", expr_type)
        {:error, error, var_env}
      end
    end
  end

  # match operator
  def typecheck(var_env, {:=, _meta, [lhs, rhs]}) when lhs == rhs, do: typecheck(var_env, rhs)
  # TODO: FIX THIS
  def typecheck(var_env, {:=, _meta, [lhs, rhs]} = _expr) do
    with {:ok, rhs_type, updated_env} <- typecheck(var_env, rhs) do
      case lhs do
        # both are atoms - unlikely but possible I suppose
        {a, b} when is_atom(a) and is_atom(b) ->
          with {:z, {_, [rhs_a, rhs_b]}} <- {:z, rhs_type},
               {:a, ^a} <- {:a, rhs_a},
               {:b, ^b} <- {:b, rhs_b} do
            {:ok, rhs_type, updated_env}
          else
            {:z, other} ->
              error = "`rhs` does not have the correct/expected shape"
              Logger.error(error)
              {:error, error, updated_env}

            {:a, _} ->
              error = "the type of `a` does not match the expected type of `a`"
              Logger.error(error)
              {:error, error, updated_env}

            {:b, _} ->
              error = "the type of `b` does not match the expected type of `b`"
              Logger.error(error)
              {:error, error, updated_env}
          end

        # 2-tuple {atom, some value} - tagged variable - very common
        {a, b} when is_atom(a) ->
          with {:z, {_, [rhs_a, rhs_b]}} <- {:z, rhs_type},
               {:ok, a_type, updated_env} <- typecheck(updated_env, a),
               {:a, ^a_type} <- {:a, rhs_a},
               {:b, {var, _, nil}} when is_atom(var) <- {:b, b} do
            # todo: check that b and rhs_b have the same type
            updated_env = Map.update(updated_env, var, rhs_b, fn _ -> rhs_b end)

            {:ok, rhs_type, updated_env}
          else
            {:z, other} ->
              error = "`rhs` does not have the correct/expected type"
              Logger.error(error)
              {:error, error, updated_env}

            {:error, error, _} ->
              # a could not be typechecked
              {:error, error, updated_env}

            {:a, other} ->
              error = "the type of `a` does not match the expected type of `a`"
              Logger.error(error)
              {:error, error, updated_env}

            {:b, _} ->
              error = "the type of `b` does not match the expected type of `b`"
              Logger.error(error)
              {:error, error, updated_env}
          end

        # 2-tuple where one of the things is not a list?
        {a, b} when not (is_list(a) or is_list(b)) ->
          Logger.debug("pin 2: #{inspect(rhs_type)}")
          # rhs must return a tuple with two types
          {_, [a_type, b_type]} = rhs_type

          res = typecheck(var_env, b)

          Logger.debug(inspect(res))

          checked_a =
            case a do
              {var_a, _, ctx_a} when is_atom(var_a) and is_nil(ctx_a) ->
                # just update the env
                updated_env = Map.update(updated_env, var_a, a_type, fn _ -> a_type end)
                {:ok, a_type, updated_env}

              otherwise ->
                # check its type
                res_a = typecheck(var_env, a)

                Logger.debug("pin 4: #{inspect(otherwise)}\n\n#{inspect(res_a)}")
                {:error, "a types don't agree", updated_env}
            end

          checked_b =
            case b do
              {var_b, _, ctx_b} when is_atom(var_b) and is_nil(ctx_b) ->
                # just update the env
                updated_env = Map.update(updated_env, var_b, b_type, fn _ -> b_type end)
                {:ok, b_type, updated_env}

              otherwise ->
                # check its type
                res_b = typecheck(var_env, b)

                Logger.debug("pin 5: #{inspect(otherwise)}\n\n#{inspect(res_b)}")
                {:error, "b types don't agree", updated_env}
            end

          with {:a, {:ok, _, _}} <- {:a, checked_a},
               {:b, {:ok, _, updated_env}} <- {:b, checked_b} do
            {:ok, rhs, updated_env}
          else
            {:a, other_a} ->
              other_a

            {:b, other_b} ->
              other_b
          end

        # n-tuple or list
        {collection, types} when collection in [:tuple, :list] ->
          {^collection, r_types} = rhs_type

          res =
            Enum.zip(types, r_types)
            |> Enum.with_index()
            |> Enum.reduce([], fn {{x, r_type}, idx}, acc ->
              cond do
                typecheck(updated_env, x) != r_type ->
                  [{:error, "lhs and rhs don't match at position #{idx}"} | acc]

                true ->
                  [{:ok, r_type} | acc]
              end
            end)
            |> Enum.reverse()

          if Utils.deep_contains?(res, :error) do
            {:error, "there are errors", updated_env}
          else
            {:ok, rhs_type, updated_env}
          end

        # single variable
        {var, _, ctx} when is_atom(var) and is_nil(ctx) ->
          updated_env = Map.update(updated_env, var, rhs_type, fn _ -> rhs_type end)
          {:ok, rhs_type, updated_env}

        # some other unsupported situation
        other ->
          Logger.error("unexpected situation: #{inspect(other)}")
          {:error, Error.unexpected(), updated_env}
      end
    else
      {:error, error, _} ->
        Logger.error(error)
        {:error, error, var_env}
    end
  end

  def typecheck(var_env, {:%{}, _meta, pairs}) when is_list(pairs) do
    Enum.reduce_while(pairs, {:ok, %{}, var_env}, fn {key, value}, {:ok, acc, var_env} ->
      case typecheck(var_env, value) do
        {:ok, v_type, var_env} ->
          {:cont, {:ok, Map.put(acc, key, v_type), var_env}}

        {:error, msg, var_env} ->
          {:halt, {:error, msg, var_env}}
      end
    end)
    |> case do
      {:ok, type_map, var_env} -> {:ok, {:map, type_map}, var_env}
      error -> error
    end
  end

  # Special handler for register function
  def typecheck(var_env, {:register, meta, args}) when length(args) == 4 do
    [ap_pid, role, callback, state] = args

    with {:arg1, {:ok, :pid, var_env}} <- {:arg1, typecheck(var_env, ap_pid)},
         {:arg2, {:ok, :atom, var_env}} <- {:arg2, typecheck(var_env, role)},
         {:arg3, {:ok, _, var_env}} <- {:arg3, typecheck(var_env, callback)},
         {:arg4, {:ok, state_shape, var_env}} <- {:arg4, typecheck(var_env, state)},
         {:m, true, _} <- {:m, is?(state_shape, :maty_actor_state), state_shape} do
      {:ok, {:tuple, [:atom, Type.maty_actor_state()]}, var_env}
    else
      {:arg1, {:ok, other_type, _}} ->
        error = Error.invalid_ap_type(meta, expected: :pid, got: other_type)
        {:error, error, var_env}

      {:arg1, {:error, error, _}} ->
        {:error, error, var_env}

      {:arg2, {:ok, other_type, _}} ->
        error = Error.role_type_invalid(meta, other_type)
        {:error, error, var_env}

      {:arg2, {:error, error, _}} ->
        {:error, error, var_env}

      {:arg3, {:error, error, _}} ->
        {:error, error, var_env}

      {:arg4, {:error, error, _}} ->
        {:error, error, var_env}

      {:m, false, other_type} ->
        error = Error.maty_actor_state_type_invalid(meta, other_type)
        {:error, error, var_env}
    end
  end

  def typecheck(var_env, {:fn, _, [{:->, _, [_args, _expr]}]}) do
    {:ok, :function, var_env}
  end

  def typecheck(var_env, {:&, _, [{:/, _, [{{:., _, [_module, _function]}, _, _}, _arity]}]}) do
    {:ok, :function, var_env}
  end

  def typecheck(var_env, {:&, _, [{:/, _, [_function, _arity]}]}) do
    {:ok, :function, var_env}
  end

  # functions - or other stuff with a ctx list
  def typecheck(var_env, {var, _meta, ctx} = expr) when is_atom(var) and is_list(ctx) do
    func = "#{var}/#{length(ctx)}"

    case Map.fetch(var_env, var) do
      {:ok, type} ->
        {:ok, type, var_env}

      :error ->
        Logger.error(inspect(expr))
        {:error, Error.function_not_exist(func), var_env}
    end
  end

  def typecheck_function(module, {name, arity} = fn_info, clauses) do
    type_specs = Module.get_attribute(module, :type_specs) |> Enum.into(%{})

    with {:spec, {:ok, fn_types}} <- {:spec, Map.fetch(type_specs, fn_info)} do
      defs = fn_types |> Enum.reverse() |> Enum.zip(clauses)

      for {{spec_args, spec_return}, {meta, args, _guards, block}} <- defs do
        with {:spec_args, ^arity} <- {:spec_args, length(spec_args)} do
          typed_args =
            Enum.zip(spec_args, args)
            |> Enum.reduce_while(
              {:ok, %{}},
              fn {arg_type, arg}, {_, var_env} ->
                case arg do
                  {arg_var, _, nil} ->
                    {:cont, {:ok, Map.put(var_env, arg_var, arg_type)}}

                  {label, {arg_var, _, nil}} when is_atom(label) ->
                    {:cont, {:ok, Map.put(var_env, arg_var, arg_type)}}

                  # todo: handle non-variable arguments to functions
                  other ->
                    {:halt, {:error, other}}
                end
              end
            )

          case typed_args do
            {:error, other} ->
              Logger.error(inspect(other))
              func = "#{name}/#{arity}"
              {:error, Error.at_least_one_arg_not_well_typed(func)}

            {:ok, var_env} ->
              body = block |> extract_body()
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
            func = "#{name}/#{arity}"
            error = Error.arity_mismatch(meta, func)
            {:error, error}
        end
      end
    else
      {:spec, :error} ->
        error = Error.no_spec_for_function(type_specs)
        Logger.error(error)
        {:error, error}
    end
  end

  # tcExpr :: Env -> ST -> Expr -> Either String (Maybe (Type, ST))
  @spec session_typecheck(module(), var_env(), ST.t(), ast()) ::
          {:error, binary(), var_env()}
          | {:ok, {:just, {value(), ST.t()}}, var_env()}
          | {:ok, :nothing, var_env()}

  @doc """

  Typechecks `maty_send` operations.
  Makes sure they can only happen when the session precondition allows the program to send a message
  """
  def session_typecheck(
        _module,
        var_env,
        st,
        {:maty_send, meta, [session, role, {label, payload}]}
      )
      when is_atom(role) and is_atom(label) do
    with %ST.SOut{to: expected_role, branches: branches} <- st do
      case Enum.find(branches, &(&1.label == label)) do
        branch when not is_nil(branch) ->
          expected_payload = branch.payload

          with {:arg1, {:ok, session_ctx_shape, _}} <- {:arg1, typecheck(var_env, session)},
               {:s, true, _} <- {:s, is?(session_ctx_shape, :session_ctx), session_ctx_shape},
               {:arg2, {:ok, ^expected_payload, _}} <- {:arg2, typecheck(var_env, payload)} do
            cond do
              expected_role != role ->
                error =
                  Error.send_role_mismatch(
                    meta,
                    expected: expected_role,
                    got: role
                  )

                {:error, error, var_env}

              true ->
                {:ok, {:just, {nil, branch.continue_as}}, var_env}
            end
          else
            {:arg1, {:error, error, _var_env}} ->
              {:error, error, var_env}

            {:s, false, other_type} ->
              error = Error.session_ctx_type_invalid(meta, other_type)
              {:error, error, var_env}

            {:arg2, {:ok, other_type, _var_Env}} ->
              error =
                Error.message_payload_type_mismatch(meta,
                  expected: expected_payload,
                  got: other_type
                )

              {:error, error, var_env}
          end

        nil ->
          # todo: report possible valid continuations
          error = Error.no_branch_with_this_label(meta, got: label)
          {:error, error, var_env}
      end
    else
      _pre ->
        {:error, "session precondition does not allow program to send at this point", var_env}
    end
  end

  # ? what about case expressions in regular functions?
  def session_typecheck(module, var_env, st, {:case, meta, [expr, [do: branches]]}) do
    with {:ok, _expr_type, var_env} <- typecheck(var_env, expr) do
      all_branches = MapSet.new(st.branches)
      st_branches = flatten_branches(st)

      handled_branch_ids =
        for {:->, _meta, [_, branch_block]} <- branches, reduce: MapSet.new() do
          acc ->
            body = extract_body(branch_block)

            case handle_session_branch(module, var_env, st_branches, body) do
              {:ok, id} -> MapSet.put(acc, id)
              {:error, _} -> acc
            end
        end

      cond do
        all_branches != handled_branch_ids ->
          unhandled = MapSet.difference(all_branches, handled_branch_ids) |> MapSet.size()
          error = Error.unhandled_session_branches(meta, unhandled)
          {:error, error, var_env}

        true ->
          {:ok, :nothing, var_env}
      end
    else
      error -> error
    end
  end

  @doc """
  Typechecks suspending with some handler.
  Makes sure a function only suspends if the session precondition allows it.
  """
  def session_typecheck(
        module,
        var_env,
        pre,
        {:{}, _meta, [:suspend, {fun_capture, role}, state_ast]}
      )
      when is_atom(role) do
    # ! need to check that this is the right handler that I suspended with
    with %ST.SName{handler: _handler} <- pre do
      st_map = Module.get_attribute(module, :annotated_handlers) |> Enum.into(%{})

      case fun_capture do
        {:&, meta, [{:/, _, [{{:., _, [mod, fn_name]}, _, _}, 4]}]} ->
          # explicit module form: &Module.fun/arity
          with {:module, true, _} <- {:module, mod == module, mod},
               {:ok, state_shape, var_env} <- typecheck(var_env, state_ast),
               {:m, true, _} <- {:m, is?(state_shape, :maty_actor_state), state_shape},
               {:ok, st} <- Map.fetch(st_map, {fn_name, 4}) do
            cond do
              st.from != role ->
                error =
                  Error.provided_handler_role_pair_mismatch(meta,
                    expected: st.from,
                    got: role
                  )

                {:error, error, var_env}

              true ->
                {:ok, :nothing, var_env}
            end
          else
            {:module, false, mod} ->
              {:error, Error.handler_from_unexpected_module(meta, expected: module, got: mod),
               var_env}

            {:error, msg, var_env} ->
              error = Error.session_typecheck_handler_unexpected(msg)
              Logger.error(error)
              {:error, Error.something_went_wrong(), var_env}

            {:m, false, other_type} ->
              {:error, Error.maty_actor_state_type_invalid(meta, other_type), var_env}

            :error ->
              func = "#{fn_name}/4"
              {:error, Error.function_missing_session_type(meta, func)}
          end

        {:&, meta, [{:/, _, [fn_name, 4]}]} ->
          # implicit module form: &fun/arity
          with {:ok, state_shape, var_env} <- typecheck(var_env, state_ast),
               {:m, true, _} <- {:m, is?(state_shape, :maty_actor_state), state_shape},
               {:ok, st} <- Map.fetch(st_map, {fn_name, 4}) do
            cond do
              st.from != role ->
                error =
                  Error.provided_handler_role_pair_mismatch(meta,
                    expected: st.from,
                    got: role
                  )

                {:error, error, var_env}

              true ->
                {:ok, :nothing, var_env}
            end
          else
            {:error, msg, var_env} ->
              error = Error.session_typecheck_handler_unexpected(msg)
              {:error, error, var_env}

            {:m, false, other_type} ->
              {:error, Error.maty_actor_state_type_invalid(meta, other_type), var_env}

            :error ->
              func = "#{fn_name}/4"
              {:error, Error.function_missing_session_type(meta, func), var_env}
          end
      end
    else
      _pre -> {:error, "precondition doesn't allow suspending at this point", var_env}
    end
  end

  @doc """
  Typechecks the :done tuple which ends communication.
  Makes sure that this only happens when the session precondition states communication must end
  """
  def session_typecheck(_module, var_env, pre, {:{}, meta, [:done, :unit, state_ast]}) do
    with {:pre, %ST.SEnd{}} <- {:pre, pre},
         {:ok, state_shape, var_env} <- typecheck(var_env, state_ast),
         {:m, true, _} <- {:m, is?(state_shape, :maty_actor_state), state_shape} do
      {:ok, :nothing, var_env}
    else
      {:pre, _pre} ->
        {:error, "precondition doesn't allow ending communication at this point", var_env}

      {:error, error, _var_env} ->
        {:error, error, var_env}

      {:m, false, other_type} ->
        error = Error.maty_actor_state_type_invalid(meta, other_type)
        {:error, error, var_env}
    end
  end

  # ! could potentially be handled by the regular tc function since this cannot progress the session type
  def session_typecheck(module, var_env, st, {:=, meta, [lhs, rhs]}) do
    with {:ok, {:just, {rhs_type, new_st}}, var_env} <-
           session_typecheck(module, var_env, st, rhs) do
      case lhs do
        {var, _meta, context} when is_atom(var) and (is_atom(context) or is_nil(context)) ->
          var_env = Map.put(var_env, var, rhs_type)
          {:ok, {:just, {rhs_type, new_st}}, var_env}

        shape ->
          error = Error.unsupported_lhs_assignment(meta, shape)
          {:error, error, var_env}
      end
    else
      {:ok, :nothing} ->
        {:ok, :nothing, var_env}

      {:error, error} ->
        {:error, error, var_env}

      other ->
        error = Error.session_typecheck_match_unexpected(meta, other)
        Logger.error(error)
        {error, error, var_env}
    end
  end

  # ? what does this even do?
  # ! could potentially be handled by the regular tc function since this cannot progress the session type
  def session_typecheck(module, var_env, st, {name, meta, args})
      when is_atom(name) and is_list(args) do
    arity = length(args)
    func_types = Module.get_attribute(module, :type_specs) |> Enum.into(%{})

    with {:ok, types} <- Map.fetch(func_types, {name, arity}) do
      clause_results =
        for {param_types, return_type} <- types do
          params_type_errors =
            Enum.zip_with(param_types, args, fn p, a ->
              case typecheck(var_env, a) do
                {:ok, ^p, _var_env} -> true
                _ -> false
              end
            end)
            |> Enum.filter(&(not &1))

          cond do
            length(param_types) != arity ->
              error = Error.too_few_arguments(meta, expected: arity, got: length(param_types))
              {:error, error}

            length(params_type_errors) != 0 ->
              func = "#{name}/#{arity}"
              error = Error.at_least_one_arg_not_well_typed(func)
              {:error, error}

            true ->
              {:ok, return_type}
          end
        end

      return =
        Enum.find(clause_results, {:error, Error.something_went_wrong()}, fn {status, _} ->
          status == :ok
        end)

      case return do
        {:ok, some_type} -> {:ok, {:just, {some_type, st}}, var_env}
        {:error, some_error} -> {:error, some_error, var_env}
        _ -> {:error, Error.unreachable(), var_env}
      end
    else
      :error ->
        Logger.error("var: #{inspect(name)}\n\n ctx: #{inspect(args)}\n\n#{inspect(st)}")
        {:error, Error.function_no_type(), var_env}
    end
  end

  # forwards typechecking to the relevant regular tc function since the expression does not progress the session type
  def session_typecheck(_module, var_env, st, expr) do
    case typecheck(var_env, expr) do
      {:ok, some_type, new_env} -> {:ok, {:just, {some_type, st}}, new_env}
      error -> error
    end
  end

  @doc """
  Typechecks an entire handler.
  Checks the arguments passed to the handler to makes sure everything looks good.
  Typechecks the body of the handler
  And ensures there are no session types left over.
  """
  def session_typecheck_handler(module, {name, arity} = fn_info, clauses) do
    annotated_handlers = Module.get_attribute(module, :annotated_handlers) |> Enum.into(%{})
    type_specs = Module.get_attribute(module, :type_specs) |> Enum.into(%{})

    func = "#{name}/#{arity}"

    with {:handler, {:ok, st}} <- {:handler, Map.fetch(annotated_handlers, fn_info)},
         {:spec, {:ok, fn_types}} <- {:spec, Map.fetch(type_specs, fn_info)},
         variants = length(fn_types),
         {:branches, ^variants} <- {:branches, length(st.branches)} do
      defs = Enum.zip([Enum.reverse(fn_types), clauses, st.branches])

      for {{spec_args, spec_return}, {meta, args, _guards, block}, branch} <- defs do
        with {:spec_args, [message_t, role_t, session_ctx_t, state_t]} <- {:spec_args, spec_args},
             {:arg1, {:tuple, [:atom, payload_t]}} <- {:arg1, message_t},
             {:arg2, true, _} <- {:arg2, is?(role_t, :role), role_t},
             {:arg3, true, _} <- {:arg3, is?(session_ctx_t, :session_ctx), session_ctx_t},
             {:arg4, true, _} <- {:arg4, is?(state_t, :maty_actor_state), state_t},
             {:return, true, _} <- {:return, is_handler_return?(spec_return), spec_return} do
          [
            {label, payload},
            role,
            {session_ctx_var, _, _},
            {maty_actor_state_shape, _, _}
          ] = args

          # construct the type environment
          var_env = %{
            session_ctx_var => Type.session_ctx(),
            maty_actor_state_shape => Type.maty_actor_state()
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
                Logger.error(error)
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

              # typecheck the function body
              body = extract_body(block)
              # todo: typecheck the function return type against the spec return type
              # will need to change the session typecheck function return type for that

              case session_typecheck_block(module, var_env, st, body) do
                {:ok, :nothing, _var_env} -> {:ok, spec_return}
                {:error, error, _var_env} -> {:error, error}
              end
          end
        else
          {:spec_args, _} ->
            error = Error.handler_args_shape_invalid(meta)
            Logger.error(error)
            {:error, error}

          {:arg1, _} ->
            error = Error.message_format_invalid()
            Logger.error(error)
            {:error, error}

          {:arg2, false, type} ->
            error = Error.role_type_invalid(meta, type)
            {:error, error}

          {:arg3, false, type} ->
            error = Error.session_ctx_type_invalid(meta, type)
            {:error, error}

          {:arg4, false, type} ->
            error = Error.maty_actor_state_type_invalid(meta, type)
            {:error, error}

          {:return, false, type} ->
            error = Error.handler_return_type_invalid(meta, type)
            {:error, error}
        end
      end
    else
      {:handler, :error} ->
        error = Error.unannotated_handler(func)
        Logger.error(error)
        {:error, error}

      {:spec, :error} ->
        error = Error.missing_spec_annotation(func)
        {:error, error}

      {:branches, _} ->
        error = "Not enough function clauses to support the annotated session type"
        Logger.error(error)
        {:error, error}
    end
  end

  # Typechecks the body of a handler by progressing and propagating the updated session type each time an expression is evaluated
  def session_typecheck_block(module, var_env, st, expressions) when is_list(expressions) do
    Enum.reduce_while(expressions, {:ok, st, var_env}, fn expr, {:ok, current_st, current_env} ->
      case session_typecheck(module, current_env, current_st, expr) do
        {:ok, {:just, {_, new_st}}, new_env} ->
          {:cont, {:ok, new_st, new_env}}

        {:ok, :nothing, new_env} ->
          {:halt, {:ok, :nothing, new_env}}

        {:error, error, new_env} ->
          {:halt, {:error, error, new_env}}

        other ->
          error = "Unexpected return from session_typecheck: #{inspect(other)}"
          Logger.error(error)
          {:halt, {:error, error, var_env}}
      end
    end)
  end

  # Special function for ensuring the init_actor callback is typed properly
  def session_typecheck_init_actor(module, fn_info, clauses) do
    type_specs = Module.get_attribute(module, :type_specs) |> Enum.into(%{})

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
              Logger.error(error)
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
        Logger.error("spec is busted: #{other}")
        {:error, Error.unexpected()}
    end
  end

  @doc """
  Handles a single branch of a session type.
  It splits up all branches of a session type and tries to progress the function as much as possible until a value or error is returned
  """
  defp handle_session_branch(module, var_env, st_branches, body) do
    result =
      for st_branch <- st_branches, reduce: [] do
        acc ->
          res = session_typecheck_block(module, var_env, st_branch, body)
          branch_id = st_branch.branches |> List.first()

          case res do
            {:ok, :nothing, _} -> [branch_id | acc]
            {:error, _, _} -> acc
          end
      end

    case result do
      [handled_id] -> {:ok, handled_id}
      [] -> {:error, Error.no_matching_session_branch()}
      _ -> {:error, Error.unreachable()}
    end
  end

  # checks the AST of the init_actor callback to check if the function registers in a session
  defp contains_register_call?(ast) do
    {_, found} =
      Macro.prewalk(ast, false, fn
        {:register, _meta, _args} = node, _acc ->
          {node, true}

        node, acc ->
          {node, acc}
      end)

    found
  end

  defp update_env(var_env, {var, _, _}, type) do
    Map.update(var_env, var, type, fn _ -> type end)
  end

  defp flatten_branches(%ST.SOut{to: role} = st) do
    st.branches |> Enum.map(fn x -> %ST.SOut{to: role, branches: [x]} end)
  end

  defp flatten_branches(%ST.SIn{from: role} = st) do
    st.branches |> Enum.map(fn x -> %ST.SIn{from: role, branches: [x]} end)
  end

  defp extract_body({:__block__, _, block}), do: block
  defp extract_body(expr), do: [expr]
end
