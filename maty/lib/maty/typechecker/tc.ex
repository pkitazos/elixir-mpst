defmodule Maty.Typechecker.Tc do
  alias Maty.ST

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
  def typecheck(var_env, %Date{}), do: {:ok, :date, var_env}

  def typecheck(var_env, {var, _meta, ctx}) when is_atom(var) and (is_atom(ctx) or is_nil(ctx)) do
    case Map.fetch(var_env, var) do
      {:ok, type} -> {:ok, type, var_env}
      :error -> {:error, "variable doesn't exist", var_env}
    end
  end

  # def session_typecheck_handler(module, var_env, {{name, 4}, :def, _meta, clauses}) do
  #   st_pairs = Module.get_attribute(module, :pairs) |> Enum.into(%{})

  #   for {_meta1, [{expected_label, payload}, expected_role, session_ctx, maty_actor_state], _,
  #        block} <-
  #         clauses do
  #     case Map.fetch(st_pairs, {name, 4, expected_label}) do
  #       {:ok, {handler, st_key}} ->
  #         st = ST.Lookup.get(st_key)

  #         # %Maty.ST.SIn{
  #         #   from: :buyer1,
  #         #   message: {:title, :string},
  #         #   continue_as: [
  #         #     %Maty.ST.SOut{
  #         #       to: :buyer1,
  #         #       message: {:quote, :number},
  #         #       continue_as: [%Maty.ST.SHandler{handler: :decision_handler}]
  #         #     }
  #         #   ]
  #         # }

  #         cond do
  #           st.from != expected_role -> {:error, "role mismatch"}
  #           elem(st.msg, 0) != expected_label -> {:error, "message label mismatch"}
  #         end

  #         {:ok, "something something"}

  #       other ->
  #         Logger.error("Unexpected return from session_typecheck: #{inspect(other)}")
  #     end
  #   end
  # end

  def session_typecheck_block(module, var_env, st, expressions) when is_list(expressions) do
    Enum.reduce_while(expressions, {:ok, st, var_env}, fn expr, {:ok, current_st, current_env} ->
      case session_typecheck(module, current_env, current_st, expr) do
        {:ok, {:just, {_, new_st}}, new_env} ->
          {:cont, {:ok, new_st, new_env}}

        {:ok, :nothing, new_env} ->
          {:halt, {:ok, :nothing, new_env}}

        {:error, error_msg, new_env} ->
          {:halt, {:error, error_msg, new_env}}

        other ->
          Logger.error("Unexpected return from session_typecheck: #{inspect(other)}")

          {:halt, other}
      end
    end)
  end

  def session_typecheck(
        module,
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
        {:error, "message label incompatible with session precondition"}
    end
  end

  def session_typecheck(
        module,
        var_env,
        %ST.SName{handler: handler},
        {:{}, _meta, [:suspend, {fun_capture, expected_role}, state_ast]}
      )
      when is_atom(expected_role) do
    st_map = Module.get_attribute(module, :annotated_handlers) |> Enum.into(%{})

    case fun_capture do
      {:&, _meta1, [{:/, _meta2, [{{:., _meta3, [mod, fun_name]}, _meta4, _}, 4]}]} ->
        # explicit module form: &Module.fun/arity
        with {:ok, :maty_actor_state, var_env} <- typecheck(var_env, state_ast),
             {:ok, st_key} <- Map.fetch(st_map, {fun_name, 4}) do
          st = ST.Lookup.get(st_key)

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
             {:ok, st_key} <- Map.fetch(st_map, {fun_name, 4}) do
          st = ST.Lookup.get(st_key)

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
        other -> {:error, "This should be unreachable", var_env}
      end
    else
      :error -> {:error, "function doesn't seem to have a type", var_env}
    end
  end
end
