defmodule Maty.Typechecker.Tc do
  alias Maty.{ST, Utils}
  alias Maty.Typechecker.Error

  alias Maty.Types.T, as: Type
  import Maty.Types.T, only: [is?: 2]

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

  # tcExpr :: Env -> ST -> Expr -> Either String (Maybe (Type, ST))
  @spec session_typecheck(module(), var_env(), ST.t(), ast()) ::
          {:error, binary(), var_env()}
          | {:ok, {:just, {value(), ST.t()}}, var_env()}
          | {:ok, :nothing, var_env()}

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
  def typecheck(var_env, {var, _meta, ctx} = expr)
      when is_atom(var) and (is_atom(ctx) or is_nil(ctx)) do
    case Map.fetch(var_env, var) do
      {:ok, type} ->
        {:ok, type, var_env}

      :error ->
        Logger.error("var: #{inspect(expr)}")
        {:error, Error.variable_not_exist(), var_env}
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
          {:ok, type, var_env} ->
            {:cont, {[type | acc], var_env}}

          # todo give better error messages
          {:error, msg, _var_env} ->
            {:halt, {:error, msg, var_env}}
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

  def typecheck(var_env, {:=, _meta, [lhs, rhs]} = _expr) do
    with {:ok, rhs_type, updated_env} <- typecheck(var_env, rhs) do
      case lhs do
        {a, b} when is_atom(a) and is_atom(b) ->
          with {:z, {_, [rhs_a, rhs_b]}} <- {:z, rhs_type},
               {:a, ^a} <- {:a, rhs_a},
               {:b, ^b} <- {:b, rhs_b} do
            {:ok, rhs_type, updated_env}
          else
            {:z, other} -> Logger.error("1 something else happened: #{inspect(other)}")
            {:a, _} -> Logger.error("a1 is not the same")
            {:b, _} -> Logger.error("b1 is not the same")
          end

        {a, b} when is_atom(a) ->
          with {:z, {_, [rhs_a, rhs_b]}} <- {:z, rhs_type},
               {:a, ^a} <- {:a, rhs_a},
               {:b, {var, _, nil}} when is_atom(var) <- {:b, b} do
            # todo: check that b and rhs_b have the same type
            updated_env = Map.update(updated_env, var, rhs_b, fn _ -> rhs_b end)

            # Logger.debug(
            #   "expr: #{inspect(expr)}\n\nhas type: #{inspect(rhs_type)}\n\nwith env: #{inspect(updated_env)}"
            # )

            {:ok, rhs_type, updated_env}
          else
            {:z, other} ->
              Logger.debug("lhs: \n#{inspect(rhs)}")

              Logger.error("2 something else happened: #{inspect(other)}")

            {:a, _} ->
              Logger.error("a2 is not the same")

            {:b, _} ->
              Logger.error("b2 is not a variable")
          end

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

        {var, _, ctx} when is_atom(var) and is_nil(ctx) ->
          updated_env = Map.update(updated_env, var, rhs_type, fn _ -> rhs_type end)
          {:ok, rhs_type, updated_env}

        other ->
          Logger.error("unexpected situation: #{inspect(other)}")
          {:error, "unexp", updated_env}
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
  def typecheck(var_env, {:register, _meta, args}) when length(args) == 4 do
    [ap_pid, role, callback, state] = args

    # todo make error messages more descriptive
    with {:a1, {:ok, :pid, var_env}} <- {:a1, typecheck(var_env, ap_pid)},
         {:a2, {:ok, :atom, var_env}} <- {:a2, typecheck(var_env, role)},
         {:a3, {:ok, _, var_env}} <- {:a3, typecheck(var_env, callback)},
         {:a4, {:ok, state_shape, var_env}} <- {:a4, typecheck(var_env, state)},
         {:z, true} <- {:z, is?(state_shape, :maty_actor_state)} do
      {:ok, {:tuple, [:ok, Type.maty_actor_state()]}, var_env}
    else
      {:a1, _} -> {:error, "access point pid type is not pid", var_env}
      {:a2, _} -> {:error, "role type is not atom", var_env}
      {:a3, _} -> {:error, "callback is not a function", var_env}
      {:a4, _} -> {:error, "state variable not of type maty_actor_state", var_env}
      {:z, other} -> {:error, "some other type error: #{inspect(other)}", var_env}
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

  defp extract_body({:__block__, _, block}), do: block
  defp extract_body(expr), do: [expr]

  def typecheck_function(module, {_name, arity} = fn_info, clauses) do
    type_specs = Module.get_attribute(module, :type_specs) |> Enum.into(%{})

    with {:spec, {:ok, fn_types}} <- {:spec, Map.fetch(type_specs, fn_info)} do
      defs = fn_types |> Enum.reverse() |> Enum.zip(clauses)

      for {{spec_args, spec_return}, {_meta, args, _guards, block}} <- defs do
        # todo: typecheck args
        # if they are variables add them to the type environment
        # if they are expressions check that the expression evaluates to the type specified in the spec

        with {:spec_args, ^arity} <- {:spec_args, length(spec_args)} do
          typed_args =
            Enum.zip(spec_args, args)
            |> Enum.reduce_while(
              {:ok, %{}},
              fn {arg_type, arg}, {_, var_env} ->
                case arg do
                  {arg_var, _, nil} -> {:cont, {:ok, Map.put(var_env, arg_var, arg_type)}}
                  _ -> {:halt, :error}
                end
              end
            )

          case typed_args do
            :error ->
              {:error, Error.at_least_one_arg_not_well_typed()}

            {:ok, var_env} ->
              body = block |> extract_body()
              res = typecheck(var_env, body)

              with {:ok, {:list, types}, _var_env} <- res,
                   {:return, ^spec_return} <- {:return, List.last(types)} do
                {:ok, spec_return}
              else
                {:error, error, _var_env} -> {:error, error}
                {:return, _other} -> {:error, Error.return_types_mismatch()}
              end
          end
        else
          {:spec_args, _} ->
            error = Error.arity_mismatch()
            Logger.error(error)
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

  defguardp is_supported_type(val)
            when is_atom(val) or
                   is_binary(val) or
                   is_boolean(val) or
                   is_number(val) or
                   is_pid(val) or
                   (is_map_key(val, :__struct__) and val.__struct__ == Date)

  defp is_handler_return?({:|, [v1, v2]}),
    do: (is?(v1, :done) and is?(v2, :suspend)) or (is?(v1, :suspend) and is?(v2, :done))

  defp is_handler_return?(val), do: is?(val, :done) or is?(val, :suspend)

  def session_typecheck_handler(module, {name, arity} = fn_info, clauses) do
    annotated_handlers = Module.get_attribute(module, :annotated_handlers) |> Enum.into(%{})
    type_specs = Module.get_attribute(module, :type_specs) |> Enum.into(%{})

    func = "#{name}/#{arity}"

    with {:handler, {:ok, st}} <- {:handler, Map.fetch(annotated_handlers, fn_info)},
         {:spec, {:ok, fn_types}} <- {:spec, Map.fetch(type_specs, fn_info)},
         variants = length(fn_types),
         {:branches, ^variants} <- {:branches, length(st.branches)} do
      defs = Enum.zip([Enum.reverse(fn_types), clauses, st.branches])

      # for each clause for a given function do a bunch of checks
      for {{{spec_args, spec_return}, clause, branch}, _idx} <- Enum.with_index(defs, 1) do
        # Logger.debug("clause #{idx}/#{length(clauses)}")

        # do the spec checks here
        with {:spec_args, [message_t, role_t, session_ctx_t, state_t]} <- {:spec_args, spec_args},
             {:a1, {:tuple, [:atom, payload_t]}} <- {:a1, message_t},
             #  todo: double match is redundant
             {:a2, role_shape} <- {:a2, role_t},
             {:r, true} <- {:r, is?(role_shape, :role)},
             {:a3, true} <- {:a3, is?(session_ctx_t, :session_ctx)},
             {:a4, state_shape} <- {:a4, state_t},
             {:m, true} <- {:m, is?(state_shape, :maty_actor_state)},
             {:spec_return, true} <- {:spec_return, is_handler_return?(spec_return)} do
          {_meta, args, _guards, block} = clause

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

              other ->
                error = Error.payload_var_has_other_value(other)
                Logger.error(error)
                var_env
            end

          cond do
            role != st.from ->
              error = Error.handler_role_mismatch()
              Logger.error(error)
              {:error, error}

            branch.label != label ->
              error = Error.message_label_mismatch()
              Logger.error(error)
              {:error, error}

            branch.payload != payload_t ->
              error = Error.message_payload_type_mismatch()
              Logger.error(error)
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
            error = Error.handler_args_shape_invalid()
            Logger.error(error)
            {:error, error}

          {:a1, _} ->
            error = Error.message_format_invalid()
            Logger.error(error)
            {:error, error}

          {:a2, _} ->
            error = Error.role_type_invalid()
            Logger.error(error)
            {:error, error}

          {:r, _} ->
            error = Error.role_type_invalid()
            Logger.error(error)
            {:error, error}

          {:a3, _} ->
            error = Error.session_ctx_type_invalid()
            Logger.error(error)
            {:error, error}

          {:a4, _} ->
            error = Error.maty_actor_state_type_invalid()
            Logger.error(error)
            {:error, error}

          {:m, _} ->
            {:error, "maty state not well typed"}

          {:spec_return, other} ->
            error = Error.handler_return_type_invalid(other)
            Logger.error(error)
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
        Logger.error(error)
        {:error, error}

      {:branches, _} ->
        error = "not enough function clauses to support the annotated session type"
        Logger.error(error)
        {:error, error}
    end
  end

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

  def session_typecheck(
        _module,
        var_env,
        %ST.SOut{to: expected_role, branches: branches},
        {:maty_send, _meta, [session, role, {label, payload}]}
      )
      when is_atom(role) and is_atom(label) do
    case Enum.find(branches, &(&1.label == label)) do
      branch when not is_nil(branch) ->
        expected_payload = branch.payload

        with {:ok, session_ctx_shape, _} <- typecheck(var_env, session),
             {:s, true} <- {:s, is?(session_ctx_shape, :session_ctx)},
             {:ok, ^expected_payload, _} <- typecheck(var_env, payload) do
          cond do
            expected_role != role -> {:error, Error.role_mismatch(), var_env}
            true -> {:ok, {:just, {nil, branch.continue_as}}, var_env}
          end
        else
          other ->
            # todo more granular errors
            Logger.error("Unexpected return from session_typecheck ST.SOut: #{inspect(other)}")
            {:error, "something went wrong", var_env}
        end

      nil ->
        error = Error.message_label_incompatible()
        {:error, error, var_env}
    end
  end

  def session_typecheck(module, var_env, st, {:case, _meta, [expr, [do: branches]]}) do
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
          error = Error.unhandled_session_branches()
          {:error, error, var_env}

        true ->
          {:ok, :nothing, var_env}
      end
    else
      error -> error
    end
  end

  def session_typecheck(
        module,
        var_env,
        %ST.SName{handler: _handler},
        {:{}, _meta, [:suspend, {fun_capture, expected_role}, state_ast]}
      )
      when is_atom(expected_role) do
    st_map = Module.get_attribute(module, :annotated_handlers) |> Enum.into(%{})

    case fun_capture do
      {:&, _, [{:/, _, [{{:., _, [mod, fn_name]}, _, _}, 4]}]} ->
        # explicit module form: &Module.fun/arity
        with {:ok, state_shape, var_env} <- typecheck(var_env, state_ast),
             {:m, true} <- {:m, is?(state_shape, :maty_actor_state)},
             {:ok, st} <- Map.fetch(st_map, {fn_name, 4}) do
          cond do
            mod != module -> {:error, Error.handler_from_unexpected_module(), var_env}
            st.from != expected_role -> {:error, Error.handler_role_mismatch(), var_env}
            true -> {:ok, :nothing, var_env}
          end
        else
          {:error, msg, var_env} ->
            error = Error.session_typecheck_handler_unexpected(msg)
            Logger.error(error)
            {:error, Error.something_went_wrong(), var_env}

          {:m, _} ->
            {:error, "maty state not well typed", var_env}

          :error ->
            {:error, Error.function_missing_session_type()}
        end

      {:&, _, [{:/, _, [fn_name, 4]}]} ->
        # implicit module form: &fun/arity
        with {:ok, state_shape, var_env} <- typecheck(var_env, state_ast),
             {:m, true} <- {:m, is?(state_shape, :maty_actor_state)},
             {:ok, st} <- Map.fetch(st_map, {fn_name, 4}) do
          cond do
            st.from != expected_role -> {:error, Error.handler_role_mismatch(), var_env}
            true -> {:ok, :nothing, var_env}
          end
        else
          {:error, msg, var_env} ->
            error = Error.session_typecheck_handler_unexpected(msg)
            Logger.error(error)
            {:error, Error.something_went_wrong(), var_env}

          {:m, _} ->
            {:error, "maty state not well typed", var_env}

          :error ->
            {:error, Error.function_missing_session_type()}
        end
    end
  end

  def session_typecheck(_module, var_env, %ST.SEnd{}, {:{}, _, [:done, :unit, state_ast]}) do
    with {:ok, state_shape, var_env} <- typecheck(var_env, state_ast),
         {:m, true} <- {:m, is?(state_shape, :maty_actor_state)} do
      {:ok, :nothing, var_env}
    else
      {:error, error, _var_env} -> {:error, error, var_env}
      {:m, _} -> {:error, "maty state not well typed", var_env}
    end
  end

  def session_typecheck(module, var_env, st, {:=, _meta, [lhs, rhs]}) do
    with {:ok, {:just, {rhs_type, new_st}}, var_env} <-
           session_typecheck(module, var_env, st, rhs) do
      case lhs do
        {var, _meta, context} when is_atom(var) and (is_atom(context) or is_nil(context)) ->
          var_env = Map.put(var_env, var, rhs_type)
          {:ok, {:just, {rhs_type, new_st}}, var_env}

        _ ->
          {:error, Error.invalid_lhs_assignment(), var_env}
      end
    else
      {:ok, :nothing} ->
        {:ok, :nothing, var_env}

      {:error, err} ->
        {:error, err, var_env}

      other ->
        Logger.error(Error.session_typecheck_match_unexpected(other))
    end
  end

  def session_typecheck(module, var_env, st, {name, _meta, args})
      when is_atom(name) and is_list(args) do
    arity = length(args)
    func_types = Module.get_attribute(module, :type_specs) |> Enum.into(%{})

    with {:ok, types} <- Map.fetch(func_types, {name, arity}) do
      # lookup the type of the function in the module annotations
      # we get back a list of tuples where the first element is the param types (which is itself a list)
      # and the second element is the return type
      # I will then check to see if the types of all the args match the types of all the params
      # if they do, then we return the corresponding return type and leave the session unchanged
      # if they don't all match then we move on to the next branch
      # if no branches match then we return an error saying these args cannot be passed to this function
      # we can also error early by checking that the args and arity match

      clause_results =
        for {param_types, return_type} <- types do
          # the fact that I'm doing this before the cond-do expression kinda defeats the purpose

          params_type_errors =
            Enum.zip_with(param_types, args, fn p, a ->
              case typecheck(var_env, a) do
                {:ok, ^p, _var_env} -> true
                _ -> false
              end
            end)
            |> Enum.filter(&(not &1))

          cond do
            length(param_types) != arity -> {:error, "not enough params"}
            length(params_type_errors) != 0 -> {:error, "mismatch between param and arg types"}
            true -> {:ok, return_type}
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

  def session_typecheck_init_actor(module, fn_info, clauses) do
    type_specs = Module.get_attribute(module, :type_specs) |> Enum.into(%{})

    with {:spec, {:ok, fn_types}} <- {:spec, Map.fetch(type_specs, fn_info)} do
      for {{spec_args, spec_return}, clause} <- Enum.zip(fn_types, clauses) do
        {_meta, args, _guards, block} = clause

        var_env =
          Enum.zip(args, spec_args)
          |> Enum.reduce(%{}, fn {{arg_var, _, _}, arg_typ}, acc ->
            Map.put(acc, arg_var, arg_typ)
          end)

        body = block |> extract_body()
        res = typecheck(var_env, body)

        # todo: make sure that this function registers in session

        with {:ok, {:list, types}, _var_env} <- res,
             {:return, ^spec_return} <- {:return, List.last(types)} do
          {:ok, spec_return}
        else
          {:error, error, _var_env} -> {:error, error}
          {:return, _other} -> {:error, Error.return_types_mismatch()}
        end
      end
    else
      {:spec, other} -> Logger.error("shits fucked: #{other}")
    end
  end

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
      _ -> {:error, Error.multiple_matching_session_branches()}
    end
  end

  def flatten_branches(%ST.SOut{to: role} = st) do
    st.branches |> Enum.map(fn x -> %ST.SOut{to: role, branches: [x]} end)
  end

  def flatten_branches(%ST.SIn{from: role} = st) do
    st.branches |> Enum.map(fn x -> %ST.SIn{from: role, branches: [x]} end)
  end
end
