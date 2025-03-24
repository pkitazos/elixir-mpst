defmodule Maty.Typechecker.Tc do
  alias Maty.ST
  alias Maty.Typechecker.Error
  alias Maty.Utils

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
  def typecheck(var_env, %Date{}), do: {:ok, :date, var_env}

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

  # functions - or other stuff with a ctx list
  def typecheck(var_env, {var, _meta, ctx}) when is_atom(var) and is_list(ctx) do
    case Map.fetch(var_env, var) do
      {:ok, type} -> {:ok, type, var_env}
      :error -> {:error, "function doesn't exist", var_env}
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

  [
    {
      [
        {:tuple, [:title, :binary]},
        :atom,
        :session_ctx,
        :maty_actor_state
      ],
      {:tuple, [:suspend, {:tuple, [:function, :atom]}, :maty_actor_state]}
    }
  ]

  {[line: 67, column: 7],
   [
     {:title, {:title, [version: 0, line: 67, column: 30], nil}},
     :buyer1,
     {:session, [version: 1, line: 67, column: 47], nil},
     {:state, [version: 2, line: 67, column: 56], nil}
   ], [],
   {:__block__, [],
    [
      {:=, [line: 68, column: 12],
       [
         {:amount, [version: 3, line: 68, column: 5], nil},
         {:lookup_price, [line: 68, column: 14],
          [{:title, [version: 0, line: 68, column: 27], nil}]}
       ]},
      {:maty_send, [line: 70, column: 5],
       [
         {:session, [version: 1, line: 70, column: 15], nil},
         :buyer1,
         {:quote, {:amount, [version: 3, line: 70, column: 42], nil}}
       ]},
      {:{}, [line: 71, column: 5],
       [
         :suspend,
         {{:&, [line: 71, column: 17],
           [
             {:/, [],
              [
                {{:., [line: 71, column: 28], [TwoBuyer.Participants.Seller, :decision_handler]},
                 [no_parens: true, line: 71, column: 29], []},
                4
              ]}
           ]}, :buyer2},
         {:state, [version: 2, line: 71, column: 59], nil}
       ]}
    ]}}

  %Maty.ST.SIn{
    from: :buyer1,
    branches: [
      %Maty.ST.SBranch{
        label: :title,
        payload: :string,
        continue_as: %Maty.ST.SOut{
          to: :buyer1,
          branches: [
            %Maty.ST.SBranch{
              label: :quote,
              payload: :number,
              continue_as: %Maty.ST.SName{handler: :decision_handler}
            }
          ]
        }
      }
    ]
  }

  def session_typecheck_handler(module, {name, arity} = fn_info, clauses) do
    annotated_handlers = Module.get_attribute(module, :annotated_handlers) |> Enum.into(%{})
    type_specs = Module.get_attribute(module, :type_specs) |> Enum.into(%{})

    var_env = %{}
    func = "#{name}/#{arity}"

    with {:handler, {:ok, _st}} <- {:handler, Map.fetch(annotated_handlers, fn_info)},
         {:spec, {:ok, fn_types}} <- {:spec, Map.fetch(type_specs, fn_info)} do
      definitions = fn_types |> Enum.reverse() |> Enum.zip(clauses)

      # for each clause for a given function do a bunch of checks
      for {{spec, clause}, idx} <- Enum.with_index(definitions, 1) do
        "clause #{idx}/#{length(clauses)} \nspec: #{inspect(spec)}\n\nclause: #{inspect(clause)}"
        :ok
        # |> Logger.debug()

        # do the spec checks here
        # construct the type environment
        # typecheck the function body
        # typecheck the function return type against the spec return type
      end
    else
      {:handler, :error} ->
        error = Error.unannotated_handler(func)
        Logger.error(error)
        {:error, error, var_env}

      {:spec, :error} ->
        error = Error.missing_spec_annotation(func)
        Logger.error(error)
        {:error, error, var_env}
    end
  end

  def session_typecheck_header(module, {name, arity} = fn_info, meta, args) do
    annotated_handlers = Module.get_attribute(module, :annotated_handlers) |> Enum.into(%{})
    type_specs = Module.get_attribute(module, :type_specs) |> Enum.into(%{})

    [
      {_label, {payload_var, _, _}},
      role,
      {session_ctx_var, _, _},
      {maty_actor_state_var, _, _}
    ] = args

    # this function currently assumes that I'm going to nicely be given a single branch to typecheck
    # in reality the function should be able to work given various clauses and various typespecs

    func = "#{name}/#{arity}"

    [line: line, column: _column] = meta

    var_env = %{}

    with {:handler, {:ok, st}} <- {:handler, Map.fetch(annotated_handlers, fn_info)},
         {:spec, {:ok, fn_types}} <- {:spec, Map.fetch(type_specs, fn_info)} do
      [
        {
          [
            {:tuple, [:atom, :binary]},
            :atom,
            :session_ctx,
            :maty_actor_state
          ],
          {:tuple, [:atom, {:tuple, [:function, :atom]}, :maty_actor_state]}
        }
      ]

      [
        {
          [
            {:tuple, [:title, :binary]},
            :atom,
            :session_ctx,
            :maty_actor_state
          ],
          {:tuple, [:suspend, {:tuple, [:function, :atom]}, :maty_actor_state]}
        }
      ]

      # {{name, arity}, kind, meta_1,
      #  [
      #    {meta_1, args_1, [], {:__block__, [], block_1}},
      #    {meta_2, args_2, [], {:__block__, [], block_1}}
      #  ]}

      cond do
        st.from != role ->
          error = Error.participant_mismatch(func, line, expected: st.from, got: role)
          Logger.error(error)
          {:error, error, var_env}

        true ->
          # all I know at this point is that there is a @spec associated with this function,
          # but not what that spec is and whether it's a valid handler type
          # need to check that it actually matches that shape I so eagerly destructured above
          # need to make sure that:
          # - first variable is a message = {:atom, var} or something equivalent
          # - second variable is a `role` (and later on that the session type :from role matches)
          # - third variable is a `session_ctx` (if the argument passed is just a variable then that's fine, if it is destructured it should match the correct structure, if it's anything else it should probably error)
          # - fourth variable is a `maty_actor_state`

          Enum.find(fn_types, fn _ -> nil end)

          var_env = %{
            payload_var => :binary,
            session_ctx_var => :session_ctx,
            maty_actor_state_var => :maty_actor_state
          }

          {:ok, {:just, {nil, st}}, var_env}
      end
    else
      {:handler, :error} ->
        error = Error.unannotated_handler(func)
        Logger.error(error)
        {:error, error, var_env}

      {:spec, :error} ->
        error = Error.missing_spec_annotation(func)
        Logger.error(error)
        {:error, error, var_env}
    end

    :in_progress
  end

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
        {:error, "message label incompatible with session precondition"}
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
      :error -> {:error, "function doesn't seem to have a type", var_env}
    end
  end
end
