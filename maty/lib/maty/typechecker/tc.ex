defmodule Maty.Typechecker.Tc do
  alias Maty.{ST, Utils}
  alias Maty.Typechecker.Error

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
  def typecheck(var_env, {var, _meta, ctx}) when is_atom(var) and (is_atom(ctx) or is_nil(ctx)) do
    case Map.fetch(var_env, var) do
      {:ok, type} -> {:ok, type, var_env}
      :error -> {:error, "variable doesn't exist", var_env}
    end
  end

  # 2-tuples
  def typecheck(var_env, {lhs, rhs}) do
    with {:lhs, {:ok, lhs_type, _}} <- {:lhs, typecheck(var_env, lhs)},
         {:rhs, {:ok, rhs_type, _}} <- {:rhs, typecheck(var_env, rhs)} do
      {:ok, {:tuple, [lhs_type, rhs_type]}, var_env}
    else
      {:lhs, {:error, msg, _}} -> {:error, "lhs failed: #{msg}", var_env}
      {:rhs, {:error, msg, _}} -> {:error, "rhs failed: #{msg}", var_env}
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
    types =
      vals
      |> Enum.reduce_while([], fn x, acc ->
        case typecheck(var_env, x) do
          {:ok, type, _var_env} -> {:cont, [type | acc]}
          # todo give better error messages
          {:error, msg, _var_env} -> {:halt, msg}
        end
      end)
      |> Enum.reverse()

    cond do
      Utils.deep_contains?(types, :error) -> {:error, types, var_env}
      true -> {:ok, {:list, types}, var_env}
    end
  end

  # type spec or
  def typecheck(var_env, {:|, _, items}) when is_list(items) do
    case typecheck(var_env, items) do
      {:ok, {:list, types}, var_env} -> {:ok, {:|, Enum.sort(types)}, var_env}
      {:error, _, _} = error -> error
    end
  end

  # functions - or other stuff with a ctx list
  def typecheck(var_env, {var, _meta, ctx}) when is_atom(var) and is_list(ctx) do
    case Map.fetch(var_env, var) do
      {:ok, type} -> {:ok, type, var_env}
      :error -> {:error, "function doesn't exist", var_env}
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

        var_env = %{}

        with {:spec_args, ^arity} <- {:spec_args, length(spec_args)} do
          # todo convert to reduce or reduce while
          spec_errors =
            for {arg_type, arg} <- Enum.zip(spec_args, args) do
              case arg do
                {arg_var, _, nil} ->
                  var_env = Map.put(var_env, arg_var, arg_type)
                  {:ok, arg_type, var_env}

                other ->
                  Logger.error("arg looks like this: #{inspect(other)}")
                  {:error, "not yet handled", var_env}
              end
            end
            |> Enum.any?(fn {result, _, _} -> result == :error end)

          cond do
            spec_errors ->
              {:error, "at least one argument is not well typed"}

            true ->
              body = block |> extract_body()
              res = typecheck(var_env, body)

              with {:ok, {:list, types}, _var_env} <- res,
                   {:return, ^spec_return} <- {:return, List.last(types)} do
                {:ok, spec_return, var_env}
              else
                {:error, error, _var_env} -> {:error, error, var_env}
                {:return, _other} -> {:error, "return types don't match", var_env}
              end
          end
        else
          {:spec_args, _} ->
            error = "arity mismatch"
            Logger.error(error)
            {:error, error}
        end
      end
    else
      {:spec, :error} ->
        error = "no spec for this function"
        Logger.error(error)
        {:error, error}
    end
  end

  defguardp is_handler_return(type) when is_tuple(type) and type == {:|, [:done, :suspend]}
  defguardp is_handler_return(type) when is_atom(type) and type in [:done, :suspend]

  defguardp is_supported_type(val)
            when is_atom(val) or
                   is_binary(val) or
                   is_boolean(val) or
                   is_number(val) or
                   is_pid(val) or
                   (is_map_key(val, :__struct__) and val.__struct__ == Date)

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
             {:a2, :role} <- {:a2, role_t},
             {:a3, :session_ctx} <- {:a3, session_ctx_t},
             {:a4, :maty_actor_state} <- {:a4, state_t},
             {:spec_return, ret_t} when is_handler_return(ret_t) <- {:spec_return, spec_return} do
          {_meta, args, _guards, block} = clause

          [
            {label, payload},
            role,
            {session_ctx_var, _, _},
            {maty_actor_state_var, _, _}
          ] = args

          # construct the type environment
          var_env = %{
            session_ctx_var => :session_ctx,
            maty_actor_state_var => :maty_actor_state
          }

          var_env =
            case payload do
              val when is_supported_type(val) ->
                var_env

              {payload_var, _, nil} when is_atom(payload_var) ->
                Map.put(var_env, payload_var, payload_t)

              other ->
                Logger.error("Payload has some other value: #{inspect(other)}")
                var_env
            end

          cond do
            role != st.from ->
              Logger.error("handler role mismatch")

            branch.label != label ->
              Logger.error("message label mismatch")

            branch.payload != payload_t ->
              Logger.error("message payload type mismatch")

            true ->
              st = branch.continue_as

              # typecheck the function body
              body = block |> extract_body()
              res = session_typecheck_block(module, var_env, st, body)

              # todo: typecheck the function return type against the spec return type
              # will need to change the session typecheck function return type for that

              res
          end
        else
          {:spec_args, _} ->
            error = "handler args shape not looking good"
            Logger.error(error)
            {:error, error}

          {:a1, _} ->
            error = "message not formatted properly"
            Logger.error(error)
            {:error, error}

          {:a2, _} ->
            error = "role not typed properly"
            Logger.error(error)
            {:error, error}

          {:a3, _} ->
            error = "session_ctx not typed properly"
            Logger.error(error)
            {:error, error}

          {:a4, _} ->
            error = "maty_actor_state not typed properly"
            Logger.error(error)
            {:error, error}

          {:spec_return, other} ->
            error = "invalid return type for handler"

            Logger.error("#{error}: #{inspect(other)} is #{inspect(other)}")

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
        error =
          "not enough function clauses to support the annotated session type"

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
        %ST.SOut{
          to: expected_role,
          branches: branches
        },
        {:maty_send, _meta, [session, role, {label, payload}]}
      )
      when is_atom(role) and is_atom(label) do
    case Enum.find(branches, &(&1.label == label)) do
      branch when not is_nil(branch) ->
        expected_payload = branch.payload

        with {:ok, :session_ctx, _} <- typecheck(var_env, session),
             {:ok, ^expected_payload, _} <- typecheck(var_env, payload) do
          cond do
            expected_role != role -> {:error, "role mismatch", var_env}
            true -> {:ok, {:just, {nil, branch.continue_as}}, var_env}
          end
        else
          other ->
            Logger.error("Unexpected return from session_typecheck ST.SOut: #{inspect(other)}")
            {:error, "something went wrong", var_env}
        end

      nil ->
        {:error, "message label incompatible with session precondition", var_env}
    end
  end

  def session_typecheck(module, var_env, st, {:case, _meta, [expr, [do: branches]]}) do
    with {:ok, _expr_type, var_env} <- typecheck(var_env, expr) do
      all_branches = MapSet.new(st.branches)
      st_branches = flatten_branches(st)

      handled_branch_ids =
        for {:->, _meta, [_, branch_block]} <- branches do
          body = branch_block |> extract_body()

          [handled_id] =
            for st_branch <- st_branches, reduce: [] do
              acc ->
                res = session_typecheck_block(module, var_env, st_branch, body)
                branch_id = st_branch.branches |> List.first()

                case res do
                  {:ok, :nothing, _} -> [branch_id | acc]
                  {:error, _, _} -> acc
                end
            end

          handled_id
        end
        |> MapSet.new()

      cond do
        all_branches != handled_branch_ids -> {:error, "unhandled session type branches", var_env}
        true -> {:ok, :nothing, var_env}
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
      {:&, _meta1, [{:/, _meta2, [{{:., _meta3, [mod, fun_name]}, _meta4, _}, 4]}]} ->
        # explicit module form: &Module.fun/arity
        with {:ok, :maty_actor_state, var_env} <- typecheck(var_env, state_ast),
             {:ok, st} <- Map.fetch(st_map, {fun_name, 4}) do
          cond do
            mod != module -> {:error, "Handler function from unexpected module", var_env}
            st.from != expected_role -> {:error, "handler role mismatch", var_env}
            true -> {:ok, :nothing, var_env}
          end
        else
          {:error, msg, var_env} ->
            error = "Unexpected return from session_typecheck ST.SHandler: #{inspect(msg)}"
            Logger.error(error)
            {:error, "something went wrong and I'm not sure what yet", var_env}

          :error ->
            {:error, "this function doesn't seem to have a session type stored"}
        end

      {:&, _meta1, [{:/, _meta2, [fun_name, 4]}]} ->
        # implicit module form: &fun/arity
        with {:ok, :maty_actor_state, var_env} <- typecheck(var_env, state_ast),
             {:ok, st} <- Map.fetch(st_map, {fun_name, 4}) do
          cond do
            st.from != expected_role -> {:error, "handler role mismatch", var_env}
            true -> {:ok, :nothing, var_env}
          end
        else
          {:error, msg, var_env} ->
            error = "Unexpected return from session_typecheck ST.SHandler: #{inspect(msg)}"
            Logger.error(error)

            {:error, "something went not well", var_env}

          :error ->
            {:error, "this function doesn't seem to have a session type stored"}
        end
    end
  end

  def session_typecheck(_module, var_env, %ST.SEnd{}, {:{}, _, [:done, :unit, state_ast]}) do
    with {:ok, :maty_actor_state, var_env} <- typecheck(var_env, state_ast) do
      {:ok, :nothing, var_env}
    else
      {:error, error, _var_env} -> {:error, error, var_env}
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
          {:error, "Invalid left-hand side in assignment", var_env}
      end
    else
      {:ok, :nothing} ->
        {:ok, :nothing, var_env}

      {:error, err} ->
        {:error, err, var_env}

      other ->
        Logger.error("Unexpected return from session_typecheck match operator: #{inspect(other)}")
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
            length(param_types) != arity ->
              {:error, "not enough params"}

            length(params_type_errors) != 0 ->
              {:error, "mismatch between param and arg types"}

            true ->
              {:ok, return_type}
          end
        end

      return =
        Enum.find(clause_results, {:error, "something went wrong"}, fn {status, _} ->
          status == :ok
        end)

      case return do
        {:ok, some_type} -> {:ok, {:just, {some_type, st}}, var_env}
        {:error, some_error} -> {:error, some_error, var_env}
        _ -> {:error, "This should be unreachable", var_env}
      end
    else
      :error ->
        Logger.error("var: #{inspect(name)}\n\n ctx: #{inspect(args)}\n\n#{inspect(st)}")
        {:error, "function doesn't seem to have a type", var_env}
    end
  end

  def flatten_branches(%ST.SOut{to: role} = st) do
    st.branches |> Enum.map(fn x -> %ST.SOut{to: role, branches: [x]} end)
  end

  def flatten_branches(%ST.SIn{from: role} = st) do
    st.branches |> Enum.map(fn x -> %ST.SIn{from: role, branches: [x]} end)
  end

  # def merge_branches(%ST.SOut{to: role} = st, branches) do
  #   %ST.SOut{to: role, branches: branches}
  # end
end
