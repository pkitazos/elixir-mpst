defmodule Maty.Typechecker.TCV2 do
  require Logger

  alias Maty.{ST, Utils}
  alias Maty.Types.T, as: Type
  alias Maty.Typechecker.{Error, Helpers}

  @typedoc """
  Represents Elixir AST nodes that can be typechecked.
  """
  @type ast :: Macro.t()

  @typedoc """
  Environment mapping variable names to their types.
  Used to track types of variables during typechecking.
  """
  @type var_env() :: %{atom() => Type.t()}
  @doc """
  Typechecks an expression AST node within a given variable environment and
  session state pre-condition.

  Corresponds to the formal judgement: Ψ; Δ; Γ ∣ Q₁ ⊳ e : T ⊲ Q₂

  Returns:
    - `{:ok, {elixir_type, next_session_state}, var_env}` on success
    - `{:error, error_message, var_env}` on failure
  """
  @spec tc_expr(module :: module(), var_env :: var_env(), st_pre :: ST.t(), ast :: ast()) ::
          {:ok, {Type.t(), ST.t()}, var_env()} | {:error, binary(), var_env()}

  # --- Revised Value Typing Clauses ---

  # Base Literals (Val-BaseLit adaptation)
  # These are pure values; they preserve the current session state.
  def tc_expr(_module, var_env, st_pre, value) when is_boolean(value) do
    myDEBUG(0)
    {:ok, {:boolean, st_pre}, var_env}
  end

  def tc_expr(_module, var_env, st_pre, nil) do
    myDEBUG(0)
    {:ok, {nil, st_pre}, var_env}
  end

  def tc_expr(_module, var_env, st_pre, value) when is_binary(value) do
    myDEBUG(0)
    {:ok, {:binary, st_pre}, var_env}
  end

  def tc_expr(_module, var_env, st_pre, value) when is_number(value) do
    myDEBUG(0)
    {:ok, {:number, st_pre}, var_env}
  end

  def tc_expr(_module, var_env, st_pre, value) when is_pid(value) do
    myDEBUG(0)
    {:ok, {:pid, st_pre}, var_env}
  end

  def tc_expr(_module, var_env, st_pre, value) when is_reference(value) do
    myDEBUG(0)
    {:ok, {:ref, st_pre}, var_env}
  end

  # Date Literal check (example, assuming Date struct exists)
  def tc_expr(_module, var_env, st_pre, {:%, _, [Date, {:%{}, _, _}]}) do
    myDEBUG(0)
    {:ok, {:date, st_pre}, var_env}
  end

  def tc_expr(_module, var_env, st_pre, {{:., _, [{:__aliases__, _, [:Date]}, :t]}, _, []}) do
    myDEBUG(0)
    {:ok, {:date, st_pre}, var_env}
  end

  def tc_expr(_module, var_env, st_pre, {:no_return, _meta, []}) do
    myDEBUG(0)
    {:ok, {:no_return, st_pre}, var_env}
  end

  def tc_expr(_module, var_env, st_pre, {:any, _meta, []}) do
    myDEBUG(0)
    {:ok, {:any, st_pre}, var_env}
  end

  # List Construction [v1, v2, ...] (Val-Cons / Val-EmptyList adaptation)
  # Enforces homogeneity. Preserves session state.
  def tc_expr(_module, var_env, st_pre, []) do
    myDEBUG(1)
    {:ok, {{:list, :any}, st_pre}, var_env}
  end

  def tc_expr(module, var_env, st_pre, items) when is_list(items) do
    myDEBUG(1)
    # Process list elements sequentially
    Enum.reduce_while(
      items,
      {:ok, {[], st_pre}, var_env},
      fn item, {:ok, {acc_types, current_st}, current_env} ->
        case tc_expr(module, current_env, current_st, item) do
          {:ok, {item_type, next_st}, next_env} ->
            {:cont, {:ok, {[item_type | acc_types], next_st}, next_env}}

          {:error, error, err_env} ->
            {:halt, {:error, error, err_env}}
        end
      end
    )
    |> case do
      {:ok, {types_rev, final_st}, final_env} ->
        element_type = types_rev |> Enum.reverse() |> Helpers.unify_list_types()

        if element_type == :error_incompatible do
          # todo: add meta information to the error if possible
          meta = []
          {:error, Error.list_elements_incompatible(meta, Enum.reverse(types_rev)), final_env}
        else
          {:ok, {{:list, element_type}, final_st}, final_env}
        end

      {:error, error, err_env} ->
        {:error, error, err_env}
    end
  end

  # Empty Tuple (Val-EmptyTuple adaptation)
  # Preserves session state.
  def tc_expr(_module, var_env, st_pre, {:{}, _, []}) do
    myDEBUG(2)
    # Formal: Tuple[]
    {:ok, {{:tuple, []}, st_pre}, var_env}
  end

  # --- 2-Tuple Value Construction ---
  # Handles literal 2-tuples {v1, v2} explicitly, distinct from n-tuples {:{}, _, [...]} in the AST
  def tc_expr(module, var_env, st_pre, {v1_ast, v2_ast}) do
    myDEBUG(2)

    with {:v1, {:ok, {v1_type, v1_st}, v1_env}} <-
           {:v1, tc_expr(module, var_env, st_pre, v1_ast)},
         {:v2, {:ok, {v2_type, v2_st}, v2_env}} <- {:v2, tc_expr(module, v1_env, v1_st, v2_ast)} do
      {:ok, {{:tuple, [v1_type, v2_type]}, v2_st}, v2_env}
    else
      {:v1, {:error, error, err_env}} -> {:error, error, err_env}
      {:v2, {:error, error, err_env}} -> {:error, error, err_env}
    end
  end

  # n-Tuple Construction {v1, v2, ...} (Val-Tuple adaptation)
  # Preserves session state.
  def tc_expr(module, var_env, st_pre, {:{}, _, items}) when is_list(items) do
    myDEBUG(2)
    # Process tuple elements sequentially, threading env and state
    Enum.reduce_while(
      items,
      {:ok, {[], st_pre}, var_env},
      fn item, {:ok, {acc_types, current_st}, current_env} ->
        case tc_expr(module, current_env, current_st, item) do
          {:ok, {item_type, next_st}, next_env} ->
            {:cont, {:ok, {[item_type | acc_types], next_st}, next_env}}

          {:error, error, err_env} ->
            {:halt, {:error, error, err_env}}
        end
      end
    )
    |> case do
      {:ok, {types_rev, final_st}, final_env} ->
        # Formal: Tuple[A1, ..., An]
        {:ok, {{:tuple, Enum.reverse(types_rev)}, final_st}, final_env}

      {:error, error, err_env} ->
        {:error, error, err_env}
    end
  end

  # Map Construction %{k1 => v1, ...} (Val-Map / Val-EmptyMap adaptation)
  # Allows heterogeneous value types, deviates slightly from our formalisation
  def tc_expr(_module, var_env, st_pre, {:%{}, _, []}) do
    myDEBUG(3)
    {:ok, {{:map, %{}}, st_pre}, var_env}
  end

  # Clause for Non-Empty Map %{k1 => v1, ...}
  def tc_expr(module, var_env, st_pre, {:%{}, meta, pairs}) when is_list(pairs) do
    myDEBUG(3)

    Enum.reduce_while(
      pairs,
      {:ok, {%{}, st_pre}, var_env},
      fn {key_ast, value_ast}, {:ok, {acc_type_map, current_st}, current_env} ->
        case tc_expr(module, current_env, current_st, key_ast) do
          {:ok, {key_type, key_st}, key_env} ->
            if key_type == :atom do
              literal_key = Helpers.ast_to_literal(key_ast)

              # Check if the key AST could be converted to a literal atom
              if is_atom(literal_key) do
                # 4. Typecheck the value AST (using state and env from key check)
                case tc_expr(module, key_env, key_st, value_ast) do
                  {:ok, {value_type, value_st}, value_env} ->
                    # Key is valid atom, value typechecked successfully.
                    # Store value_type using the actual literal key in the result type map.
                    updated_map = Map.put(acc_type_map, literal_key, value_type)
                    # Continue reduction with updated map, latest state, and env.
                    {:cont, {:ok, {updated_map, value_st}, value_env}}

                  {:error, error, err_env} ->
                    # Value typechecking failed, halt the reduction.
                    {:halt, {:error, error, err_env}}
                end
              else
                # Key AST was type :atom but couldn't be converted to a literal atom (e.g., complex expression resulting in an atom)
                # Or Helpers.ast_to_literal returned an error indicator.
                # We require literal atom keys.
                # Error for non-literal atom key
                error = Error.complex_map_key(meta, key_ast)
                {:halt, {:error, error, key_env}}
              end
            else
              # Key type check failed (key_type was not :atom).
              error = Error.invalid_map_key_type(meta, expected: :atom, got: key_type)
              # Halt the reduction.
              {:halt, {:error, error, key_env}}
            end

          {:error, error, err_env} ->
            # Key typechecking failed (error occurred during tc_expr on key_ast).
            # Halt the reduction.
            {:halt, {:error, error, err_env}}
        end
      end
    )
    |> case do
      {:ok, {type_map, final_st}, final_env} ->
        # Resulting type is {:map, %{key_literal => value_type, ...}}
        {:ok, {{:map, type_map}, final_st}, final_env}

      # Reduction was halted due to an error
      {:error, error, err_env} ->
        {:error, error, err_env}
    end
  end

  # --- Logical Not Operator (T-Not) ---

  def tc_expr(module, var_env, st_pre, {{:., meta, [:erlang, :not]}, _, [operand_ast]}) do
    myDEBUG(6)

    case tc_expr(module, var_env, st_pre, operand_ast) do
      {:ok, {:boolean, operand_st}, operand_env} ->
        {:ok, {:boolean, operand_st}, operand_env}

      {:ok, {other_type, _operand_st}, operand_env} ->
        error = Error.logical_operator_requires_boolean(meta, "not", other_type)
        {:error, error, operand_env}

      {:error, error, err_env} ->
        {:error, error, err_env}
    end
  end

  # --- Binary Operators (T-Op) ---

  # Handles Arithmetic and Comparison Ops: +, -, *, /, <, >, <=, >=, ==, !=
  def tc_expr(module, var_env, st_pre, {{:., meta, [:erlang, op]}, _, [lhs_ast, rhs_ast]})
      when op in [:+, :-, :*, :/, :<, :>, :<=, :>=, :==, :!=] do
    myDEBUG(7)

    with {:lhs, {:ok, {lhs_type, lhs_st}, lhs_env}} <-
           {:lhs, tc_expr(module, var_env, st_pre, lhs_ast)},
         {:rhs, {:ok, {rhs_type, rhs_st}, rhs_env}} <-
           {:rhs, tc_expr(module, lhs_env, lhs_st, rhs_ast)},
         {:op_check, {:ok, result_type}, _} <-
           {:op_check, Helpers.op_type_rel(op, lhs_type, rhs_type), [lhs_type, rhs_type, rhs_env]} do
      {:op_check, :error,
       [
         :atom,
         :number,
         %{
           state:
             {:map,
              %{
                callbacks: {:map, %{ref: {:tuple, [:atom, :function]}}},
                sessions:
                  {:map,
                   %{
                     ref:
                       {:map,
                        %{
                          id: :ref,
                          handlers: {:map, %{atom: {:tuple, [:function, :atom]}}},
                          participants: {:map, %{atom: :pid}},
                          local_state: :any
                        }}
                   }}
              }},
           amount: :number
         }
       ]}

      {:ok, {result_type, rhs_st}, rhs_env}
    else
      {:lhs, {:error, msg, env}} ->
        {:error, msg, env}

      {:rhs, {:error, msg, env}} ->
        {:error, msg, env}

      {:op_check, :error, [lhs_type, rhs_type, rhs_env]} ->
        error = Error.binary_operator_type_mismatch(meta, op, lhs_type, rhs_type)
        {:error, error, rhs_env}
    end
  end

  # Handles String Concatenation: <>

  def tc_expr(
        module,
        var_env,
        st_pre,
        {:<<>>, meta, [{:"::", _, [lhs_ast, _]}, {:"::", _, [rhs_ast, _]}]}
      ) do
    myDEBUG(8)

    with {:lhs, {:ok, {lhs_type, lhs_st}, lhs_env}} <-
           {:lhs, tc_expr(module, var_env, st_pre, lhs_ast)},
         {:rhs, {:ok, {rhs_type, rhs_st}, rhs_env}} <-
           {:rhs, tc_expr(module, lhs_env, lhs_st, rhs_ast)},
         {:op_check, {:ok, result_type}, _} <-
           {:op_check, Helpers.op_type_rel(:<>, lhs_type, rhs_type),
            [lhs_type, rhs_type, rhs_env]} do
      {:ok, {result_type, rhs_st}, rhs_env}
    else
      {:lhs, {:error, msg, env}} ->
        {:error, msg, env}

      {:rhs, {:error, msg, env}} ->
        {:error, msg, env}

      {:op_check, :error, [lhs_type, rhs_type, rhs_env]} ->
        error = Error.binary_operator_type_mismatch(meta, :<>, lhs_type, rhs_type)
        {:error, error, rhs_env}
    end
  end

  # Handles Boolean Ops: and, or
  def tc_expr(module, var_env, st_pre, {op, meta, [lhs_ast, rhs_ast]})
      when op in [:and, :or] do
    myDEBUG(9)

    with {:lhs, {:ok, {lhs_type, lhs_st}, lhs_env}} <-
           {:lhs, tc_expr(module, var_env, st_pre, lhs_ast)},
         {:rhs, {:ok, {rhs_type, rhs_st}, rhs_env}} <-
           {:rhs, tc_expr(module, lhs_env, lhs_st, rhs_ast)},
         {:op_check, {:ok, result_type}, _} <-
           {:op_check, Helpers.op_type_rel(op, lhs_type, rhs_type), [lhs_type, rhs_type, rhs_env]} do
      {:ok, {result_type, rhs_st}, rhs_env}
    else
      {:lhs, {:error, msg, env}} ->
        {:error, msg, env}

      {:rhs, {:error, msg, env}} ->
        {:error, msg, env}

      {:op_check, :error, [lhs_type, rhs_type, rhs_env]} ->
        error = Error.logical_operator_type_mismatch(meta, op, lhs_type, rhs_type)
        {:error, error, rhs_env}
    end
  end

  # --- Function Application (T-App) ---
  # Handles local function calls: f(arg1, arg2, ...)
  def tc_expr(module, var_env, st_pre, {func_name, meta, arg_asts})
      when is_atom(func_name) and is_list(arg_asts) and meta != [] and
             func_name not in [:=, :%, :{}, :|, :<<>>] do
    myDEBUG(10)

    arity = length(arg_asts)
    func_id = {func_name, arity}

    psi = Utils.Env.get_map(module, :psi)

    case Map.fetch(psi, func_id) do
      {:ok, signatures} when is_list(signatures) and signatures != [] ->
        Enum.reduce_while(
          arg_asts,
          {:ok, [], st_pre, var_env},
          fn arg_ast, {:ok, {acc_arg_types, current_st}, current_env} ->
            case tc_expr(module, current_env, current_st, arg_ast) do
              {:ok, {actual_type, next_st}, next_env} ->
                # Accumulate actual types and pass state/env forward
                {:cont, {:ok, {[actual_type | acc_arg_types], next_st}, next_env}}

              {:error, error, err_env} ->
                # Halt on first argument error
                {:halt, {:error, error, err_env}}
            end
          end
        )
        |> case do
          {:ok, actual_arg_types_rev, final_st, final_env} ->
            actual_arg_types = Enum.reverse(actual_arg_types_rev)

            Enum.find(signatures, fn {param_types, _return_type} ->
              # Check arity and type compatibility for this signature
              length(actual_arg_types) == length(param_types) and
                Enum.zip(actual_arg_types, param_types)
                |> Enum.all?(fn {actual, expected} -> actual == expected end)
            end)
            |> case do
              {_param_types, return_type} ->
                {:ok, {return_type, final_st}, final_env}

              nil ->
                error = Error.no_matching_function_clause(meta, func_id, actual_arg_types)
                {:error, error, final_env}
            end

          {:error, error, final_env} ->
            # An error occurred during argument typechecking
            {:error, error, final_env}
        end

      # should not happen if specs are added correctly, but we handle defensively
      {:ok, []} ->
        # todo: more specific error
        Logger.error("spec might be broken?")
        error = Error.function_not_exist(meta, func_id)
        {:error, error, var_env}

      :error ->
        # function definition not found in Psi
        func = Utils.to_func(func_id)
        Logger.error("can't find function #{func} in Psi")
        error = Error.function_not_exist(meta, func)
        {:error, error, var_env}
    end
  end

  # --- Raw Communication Checks ---

  # Disallow raw 'receive'
  def tc_expr(_module, var_env, _st_pre, {:receive, meta, _}) do
    myDEBUG(11)
    {:error, Error.no_raw_receive(meta), var_env}
  end

  # Disallow raw 'send' (Kernel.send/2 or :erlang.send/2)
  def tc_expr(_module, var_env, _st_pre, {{:., meta, [:erlang, :send]}, _, args})
      when length(args) in [2, 3] do
    myDEBUG(12)
    {:error, Error.no_raw_send(meta), var_env}
  end

  # Check for Kernel.send/2
  def tc_expr(_module, var_env, _st_pre, {{:., meta, [:Kernel, :send]}, _, args})
      when length(args) == 2 do
    myDEBUG(13)
    {:error, Error.no_raw_send(meta), var_env}
  end

  # --- Anonymous Functions and Captures ---
  # todo: these are a bit rudimentary

  # Anonymous function: fn args -> ... end
  def tc_expr(_module, var_env, st_pre, {:fn, _meta, [{:->, _, [args_ast, _body_ast]}]}) do
    myDEBUG(14)
    arity = length(args_ast)
    {:ok, {{:fun, arity}, st_pre}, var_env}
  end

  # Remote function capture: &Mod.fun/arity
  def tc_expr(
        _module,
        var_env,
        st_pre,
        {:&, _meta, [{:/, _, [{{:., _, [_mod, _fun]}, _, _}, arity]}]}
      )
      when is_integer(arity) do
    myDEBUG(15)
    {:ok, {{:fun, arity}, st_pre}, var_env}
  end

  # Local function capture: &fun/arity
  def tc_expr(_module, var_env, st_pre, {:&, _meta, [{:/, _, [fun_atom, arity]}]})
      when is_atom(fun_atom) and is_integer(arity) do
    myDEBUG(16)
    {:ok, {{:fun, arity}, st_pre}, var_env}
  end

  # --- Block Scope ---
  def tc_expr(module, var_env, st_pre, {:__block__, _, body_asts}) when is_list(body_asts) do
    myDEBUG(17)
    tc_expr_list(module, var_env, st_pre, body_asts)
  end

  # Computation Typing

  # --- Let Expression / Match Operator (T-Let) ---
  # Represents `pattern = expr1`
  def tc_expr(module, var_env, st_pre, {:=, _meta, [pattern_ast, expr1_ast]}) do
    myDEBUG(18)

    with {:e1, {:ok, {type_A, st_post_e1}, env_post_e1}} <-
           {:e1, tc_expr(module, var_env, st_pre, expr1_ast)},
         {:p_match, {:ok, _new_bindings, updated_env_with_bindings}} <-
           {:p_match, tc_pattern(pattern_ast, type_A, env_post_e1)} do
      {:ok, {type_A, st_post_e1}, updated_env_with_bindings}
    else
      {:e1, {:error, msg, env}} -> {:error, msg, env}
      {:p_match, {:error, msg, env}} -> {:error, msg, env}
    end
  end

  # --- Case Expression (T-Case) ---
  # AST: {:case, meta, [scrutinee_ast, [do: clauses_list]]}
  def tc_expr(module, var_env, st_pre, {:case, meta, [scrutinee_ast, [do: clauses_list]]}) do
    myDEBUG(19)

    with {:scrutinee, {:ok, {type_A, st_after_scrutinee, env_after_scrutinee}}} <-
           {:scrutinee, tc_expr(module, var_env, st_pre, scrutinee_ast)},
         :ok <- Helpers.check_st_unchanged(st_pre, st_after_scrutinee, meta) do
      branch_results =
        Enum.map(clauses_list, fn {:->, _clause_meta, [p_ast, e_ast]} ->
          case tc_pattern(p_ast, type_A, env_after_scrutinee) do
            {:ok, _bindings, env_for_e} ->
              tc_expr(module, env_for_e, st_pre, e_ast)
              |> case do
                # Success for this branch, return the result pair
                {:ok, {type_Ti, st_Q_i}, _env_final_i} -> {:ok, {type_Ti, st_Q_i}}
                # Error in branch body
                {:error, msg, _env} -> {:error, msg}
              end

            {:error, msg, _env} ->
              # Pattern didn't match type A
              {:error, msg}
          end
        end)

      # check if any branch failed
      if Enum.any?(branch_results, fn r -> match?({:error, _}, r) end) do
        {:error, first_error_msg} = Enum.find(branch_results, fn r -> match?({:error, _}, r) end)
        {:error, first_error_msg, env_after_scrutinee}
      else
        successful_results = Enum.map(branch_results, fn {:ok, res} -> res end)

        case Helpers.join_branch_results(successful_results) do
          {:ok, {final_T, final_Q2}} -> {:ok, {final_T, final_Q2}, env_after_scrutinee}
          {:error, error_msg} -> {:error, error_msg, env_after_scrutinee}
        end
      end
    else
      {:scrutinee, {:error, msg, env}} -> {:error, msg, env}
      {:error, msg} -> {:error, msg, var_env}
    end
  end

  # --- Maty Send Operation (T-Send) ---
  # Matches call to Maty.DSL.internal_send/3 which the send/2 macro expands to
  def tc_expr(
        module,
        var_env,
        st_pre,
        {{:., meta, [Maty.DSL, :internal_send]}, _, [_session_ctx, recipient_ast, message_ast]}
      ) do
    myDEBUG(20)

    case st_pre do
      %ST.SOut{to: expected_role, branches: branches} ->
        with {:recipient, {:ok, {:atom, recipient_st}, recipient_env}} <-
               {:recipient, tc_expr(module, var_env, st_pre, recipient_ast)},
             # Ensure recipient check pure
             :ok <- Helpers.check_st_unchanged(st_pre, recipient_st, meta),

             # Check recipient matches expected role
             :ok <- Helpers.check_recipient_role(recipient_ast, expected_role, meta),

             # 4. Check message structure {label, payload_expr}
             {:message, {:ok, literal_label, payload_expr_ast}} <-
               {:message, Helpers.check_message_structure(message_ast, meta)},

             # Typecheck payload expression
             {:payload, {:ok, {actual_payload_type, payload_st}, payload_env}} <-
               {:payload, tc_expr(module, recipient_env, recipient_st, payload_expr_ast)},

             # 5. Find matching branch for the label
             {:branch, {:ok, matched_branch}} <-
               {:branch,
                Helpers.find_matching_branch(branches, {literal_label, actual_payload_type})},
             %{payload: expected_payload_type, continue_as: st_Sj} = matched_branch,

             # Ensure payload check pure
             :ok <- Helpers.check_st_unchanged(recipient_st, payload_st, meta),
             # Check payload type matches expected
             :ok <- Helpers.check_payload_type(actual_payload_type, expected_payload_type, meta) do
          # All checks passed!
          # Result type is :atom, next session state is st_Sj
          # Use env after payload check
          {:ok, {:atom, st_Sj}, payload_env}
        else
          {:error, msg, env} -> {:error, msg, env}
          {:error, msg} -> {:error, msg, var_env}
          {:branch, :error} -> {:error, ""}
          {label, e} when label in [:recipient, :message, :branch, :payload] -> e
        end

      other_state ->
        error = Error.send_invalid_state(meta, expected: "SOut", got: other_state)
        {:error, error, var_env}
    end
  end

  # --- Maty Suspend Operation (T-Suspend) ---
  # Matches throw({:suspend, handler, state}) from Maty.DSL.suspend/2
  def tc_expr(
        module,
        var_env,
        st_pre,
        {{:., _, [:erlang, :throw]}, _, [{:{}, meta, [:suspend, handler_ast, state_ast]}]}
      ) do
    myDEBUG(21)

    with {:h, {:ok, {handler_type, h_st}, h_env}} <-
           {:h, tc_expr(module, var_env, st_pre, handler_ast)},
         :ok <- Helpers.check_st_unchanged(st_pre, h_st, meta),
         :ok <- Helpers.check_handler_type(handler_type, meta),
         #
         {:v, {:ok, {state_type, v_st}, v_env}} <- {:v, tc_expr(module, h_env, h_st, state_ast)},
         :ok <- Helpers.check_st_unchanged(h_st, v_st, meta),
         :ok <- Helpers.check_maty_state_type(state_type, meta) do
      {:ok, {:no_return, {:st_bottom}}, v_env}
    else
      {:error, msg, env} -> {:error, msg, env}
      {:error, msg} -> {:error, msg, var_env}
    end
  end

  # --- Maty Done Operation (T-Done) ---
  # Matches throw({:done, state}) from Maty.DSL.done/1
  # AST: {:throw, meta, [{:done, state_ast}]}

  def tc_expr(module, var_env, st_pre, {{:., _, [:erlang, :throw]}, meta, [done: state_ast]}) do
    myDEBUG(22)

    with {:v, {:ok, {state_type, v_st}, v_env}} <-
           {:v, tc_expr(module, var_env, st_pre, state_ast)},
         :ok <- Helpers.check_st_unchanged(st_pre, v_st, meta),
         :ok <- Helpers.check_maty_state_type(state_type, meta) do
      {:ok, {:no_return, {:st_bottom}}, v_env}
    else
      {:error, msg, env} -> {:error, msg, env}
      {:error, msg} -> {:error, msg, var_env}
    end
  end

  # --- Maty Register Operation (T-Register) ---
  def tc_expr(
        module,
        var_env,
        st_pre,
        {{:., _m1, [{:., _m2, [:Maty, :DSL]}, :register]}, meta,
         [ap_pid_ast, role_ast, reg_info_ast, state_ast]}
      ) do
    myDEBUG(22)

    with {:ap, {:ok, {:pid, pid_st}, pid_env}} <-
           {:ap, tc_expr(module, var_env, st_pre, ap_pid_ast)},
         :ok <- Helpers.check_st_unchanged(st_pre, pid_st, meta),
         #
         {:role, {:ok, {:atom, role_st}, role_env}} <-
           {:role, tc_expr(module, pid_env, pid_st, role_ast)},
         :ok <- Helpers.check_st_unchanged(pid_st, role_st, meta),

         #
         {:reg_info, {:ok, {_reg_info_type, reg_info_st}, reg_info_env}} <-
           {:reg_info, tc_expr(module, role_env, role_st, reg_info_ast)},
         :ok <- Helpers.check_st_unchanged(role_st, reg_info_st, meta),

         #
         {:handler_check, {:ok, args_env}} <-
           {:handler_check,
            check_registration_info(module, reg_info_ast, reg_info_env, reg_info_st, meta)},
         {:state, {:ok, {state_type, state_st}, state_env}} <-
           {:state, tc_expr(module, args_env, reg_info_st, state_ast)},
         :ok <- Helpers.check_st_unchanged(reg_info_st, state_st, meta),
         :ok <- Helpers.check_maty_state_type(state_type, meta) do
      return_type = {:tuple, [:ok, Type.maty_actor_state()]}
      {:ok, {return_type, state_st}, state_env}
    else
      {:ap, {:error, msg, env}} ->
        {:error, msg, env}

      {:ap, {:ok, {other, _, _}, env}} ->
        {:error, Error.invalid_ap_type(meta, expected: :pid, got: other), env}

      {:error, msg} ->
        {:error, msg, var_env}

      {:role, {:error, msg, env}} ->
        {:error, msg, env}

      {:role, {:ok, {other, _, _}, env}} ->
        {:error, Error.role_type_invalid(meta, other), env}

      {:reg_info, {:error, msg, env}} ->
        {:error, msg, env}

      {:handler_check, {:error, msg, err_env}} ->
        {:error, msg, err_env}

      {:state, {:error, msg, env}} ->
        {:error, msg, env}

      {:state, {:ok, {other, _, _}, env}} ->
        {:error, Error.invalid_maty_state_type(meta, got: other), env}

      other ->
        {:error, Error.internal_error("Unexpected mismatch in register check: #{inspect(other)}"),
         var_env}
    end
  end

  # Variable Lookup (TV-Var adaptation)
  # Looking up a variable is pure; preserves session state.
  def tc_expr(_module, var_env, st_pre, {var_name, meta, context})
      when is_atom(var_name) and (is_nil(context) or is_list(context)) do
    myDEBUG(25)

    case Map.fetch(var_env, var_name) do
      {:ok, elixir_type} -> {:ok, {elixir_type, st_pre}, var_env}
      :error -> {:error, Error.variable_not_exist(meta, var_name), var_env}
    end
  end

  # Clause for TV-MsgHandler, TV-InitHandler (Handler names as values)
  # These need access to the Delta environment. Preserves session state.
  # --- Simplified Handler Name / Atom Logic ---
  # Assumes variable shadowing doesn't occur for handler names.

  # This clause handles atoms that might be handler names.
  def tc_expr(module, var_env, st_pre, value)
      when is_atom(value) and not is_nil(value) do
    myDEBUG(23)
    # Check Delta environments directly
    delta_M = Utils.Env.get_map(module, :delta_M)
    delta_I = Utils.Env.get_map(module, :delta_I)

    cond do
      Map.has_key?(delta_M, value) ->
        {:ok, {:maty_handler_msg, st_pre}, var_env}

      Map.has_key?(delta_I, value) ->
        {:ok, {:maty_handler_init, st_pre}, var_env}

      true ->
        # Not a handler name, treat as a standard atom literal.
        # Fall through by calling the more general atom clause.
        myDEBUG(203)

        # Logger.debug(
        #   "Already checked if #{value} is a variable or handler so it could only be an atom?",
        #   ansi_color: :magenta
        # )

        {:ok, {:atom, st_pre}, var_env}
    end
  end

  # General Atom Literal Clause (catches atoms not matched above)
  def tc_expr(_module, var_env, st_pre, value) when is_atom(value) and not is_nil(value) do
    myDEBUG(24)
    # Logger.debug("This: #{inspect(value)} is an atom", ansi_color: :magenta)

    {:ok, {:atom, st_pre}, var_env}
  end

  @doc """
  Checks if a function definition is well-formed according to WF-Func.
  Verifies argument patterns, body type, return type, and session state purity.

  Returns `{:ok, return_type}` or `{:error, message}` for each function clause checked.
  """
  @spec check_wf_function(
          module :: module(),
          func_id :: {atom(), non_neg_integer()},
          clauses :: [Macro.t()]
        ) ::
          [{:ok, Type.t()} | {:error, binary()}]
  def check_wf_function(module, {_name, arity} = func_id, clauses) do
    psi = Utils.Env.get_map(module, :psi)

    case Map.fetch(psi, func_id) do
      {:ok, signatures} when is_list(signatures) ->
        defs = Enum.zip(signatures, clauses)

        for {{spec_args_types, spec_return_type},
             {clause_meta, arg_pattern_asts, _guards, body_block}} <- defs do
          with :arity_ok <-
                 check_clause_arity(
                   clause_meta,
                   func_id,
                   arity,
                   arg_pattern_asts,
                   spec_args_types
                 ),
               #
               {:args_ok, body_var_env} <-
                 check_argument_patterns(clause_meta, arg_pattern_asts, spec_args_types),
               #
               {:body_ok, {actual_return_type, final_st}, _final_env} <-
                 check_function_body(module, body_var_env, body_block),
               :state_ok <- check_final_state(clause_meta, func_id, final_st),
               :type_ok <- check_return_type(clause_meta, actual_return_type, spec_return_type) do
            {:ok, actual_return_type}
          else
            {:error, error_msg} -> {:error, error_msg}
          end
        end

      :error ->
        func_str = Utils.to_func(func_id)
        error = Error.no_spec_for_function(func_str)
        List.duplicate({:error, error}, length(clauses))
    end
  end

  @doc """
  Checks a single message handler clause based on WF-MsgHandler.

  Verifies the declared role/label/payload pattern against the session type
  associated with the handler_label, and checks if the body correctly
  implements the session continuation, ending in done/suspend.

  Returns `{:ok, {label, role}}` if well-formed, identifying the session
  branch handled. Otherwise returns `{:error, message}`.
  """
  @spec check_wf_message_handler_clause(
          module :: module(),
          handler_label :: atom(),
          handler_ast_clause :: Macro.t(),
          st_pre :: ST.t(),
          type_signature :: tuple()
        ) ::
          {:ok, {atom(), atom()}} | {:error, binary()}
  def check_wf_message_handler_clause(
        module,
        handler_label,
        {clause_meta, args, _guards, body_block},
        st_pre,
        type_signature
      ) do
    with [received_role, message_pattern_ast, state_var_ast, _session_ctx_var_ast] <- args,
         {[declared_role, message_type, _state, _session_ctx], _return_type} <- type_signature,
         # get name of state variable
         {state_var, _, _} <- state_var_ast,
         # role should already just be an atom
         {:ok, {:atom, ^st_pre}, _} <- tc_expr(module, %{}, st_pre, received_role),
         # get shape of message
         # bind whatever from the message payload
         {:ok, _message_bindings, var_env} <- tc_pattern(message_pattern_ast, message_type, %{}),
         # create all the necessary bindings (this thing Γ_args, x : ActorState(B))
         # need to also add psi here
         var_env = Map.put(var_env, state_var, Type.maty_actor_state()),
         #  var_env = Map.merge(gamma_args_env, psi),
         # check the session type is SIn
         {:st, %ST.SIn{from: expected_role, branches: branches}} <- {:st, st_pre},
         # check handler and session type roles align
         {:role, ^expected_role} <- {:role, ^received_role = declared_role},
         {label, _} <- message_pattern_ast,
         {:tuple, [_, payload_type]} <- message_type,
         # check there is a matching branch
         # take note of this branch
         {:branch, {:ok, handler_branch}} <-
           {:branch, Helpers.find_matching_branch(branches, {label, payload_type})},
         # extract only the relevant bits (leave out the try catch)
         body = extract_body(body_block) do
      case tc_expr_list(module, var_env, handler_branch.continue_as, body) do
        {:ok, {:no_return, {:st_bottom}}, _var_env} -> {:ok, handler_branch}
        {:error, msg, _var_env} -> {:error, msg}
        other -> {:error, "Unreachable? #{inspect(other)}"}
      end

      # -------------------------
      #  {:ok, state_var_name} <- extract_state_var(state_var_ast, clause_meta),
      #  {:ok, declared_role} <- extract_declared_role(role_ast, clause_meta),
      #  {:ok, {declared_label, payload_pattern_ast}} <- extract_message_pattern(message_pattern_ast,clause_meta),
      #  {:session_type_ok, _expected_role, branches} <- check_session_type_is_sin(st_pre,declared_role,handler_label,clause_meta),
      #  {:branch_found, matched_branch} <- Helpers.find_matching_branch(branches, declared_label, clause_meta),
      #  %{payload: expected_payload_type_A, continue_as: continuation_state_Sj} = matched_branch,
      #  {:pattern_ok, _bindings_gamma_prime, env_gamma_prime} <- tc_pattern(payload_pattern_ast, expected_payload_type_A, %{}),
      #  body_env = Map.put(env_gamma_prime, state_var_name, Type.maty_actor_state()),
      #  body_asts = extract_body(body_block),
      #  {:body_ok, {:no_return, {:st_bottom}}, _final_env} <- tc_expr_list(module, body_env, continuation_state_Sj, body_asts)
      #  return the session type branch we covered
      # {:ok, {declared_label, declared_role}}
    else
      {:error, msg, _env} ->
        {:error, msg}

      {:error, msg} ->
        Logger.error("here1")
        {:error, msg}

      {:st, other_st} ->
        %{
          function: {:title_handler, 4},
          st: %Maty.ST.SIn{
            from: :buyer1,
            branches: [
              %Maty.ST.SBranch{
                label: :title,
                payload: :binary,
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
        }

        {:error, "st broken: #{inspect(other_st)}"}

      {:role, other_role} ->
        {:error, "role broken: #{inspect(other_role)}"}

      {:branch, :error} ->
        {:error, "branch broken"}

      {:body_ok, {final_type, final_state}, _env} ->
        error =
          Error.handler_body_wrong_termination(
            clause_meta,
            handler_label,
            final_type,
            final_state
          )

        {:error, error}

      other ->
        Logger.error("here2")
        {:error, Error.internal_error("Unexpected mismatch in handler check: #{inspect(other)}")}
    end
  end

  @doc """
  Checks a single initialisation handler clause based on WF-InitHandler.

  Verifies the declared argument pattern against the spec type, and checks
  if the body correctly implements the initial session state, ending in
  done/suspend.

  Returns `{:ok, handler_label}` if well-formed. Otherwise returns `{:error, message}`.
  """

  # [
  #   {_meta, [args_ast, state_var_ast, session_ctx], [],
  #    {:try, _, [[do: {:__block__, _, [_, _, {:__block__, _, block}]}, catch: _]]}}
  # ]

  @spec check_wf_init_handler_clause(
          module :: module(),
          handler_label :: atom(),
          handler_ast_clause :: Macro.t(),
          # %{function: {name, arity}, st: session_type}
          delta_i_entry :: map(),
          # List of {[arg_types], return_type} for the function
          psi_entry :: list()
        ) ::
          {:ok, atom()} | {:error, binary()}
  def check_wf_init_handler_clause(
        _module,
        handler_label,
        {clause_meta, args, _guards, _body_block},
        _delta_i_entry,
        _psi_entry
      ) do
    with [_arg_pattern_ast, _state_var_ast, _session_ctx_var_ast] <- args do
      #  get name of state variable
      #  bind whatever from the args
      #  check the session type is NOT SIn (should be SOut or Suspend)
      #  check there is a matching branch
      #  take note of this branch
      #  create all the necessary bindings (this thing Γ_args, x : ActorState(B))
      #  extract only the relevant bits (leave out the try catch)
      #  typecheck that shit and make sure we don't get a return value and our return session is st_bottom
      # ------------------------------
      #  {:ok, state_var_name} <- extract_state_var(state_var_ast, clause_meta),
      #  initial_session_state_S <- delta_i_entry.st,
      #  {:ok, expected_arg_type_A} <- extract_init_handler_arg_type(psi_entry,delta_i_entry.function,handler_label),
      #  {:pattern_ok, _bindings_gamma_p, env_gamma_p} <- tc_pattern(arg_pattern_ast, expected_arg_type_A, %{}),
      # Γ = Γₚ, x:ActorState
      #  body_env = Map.put(env_gamma_p, state_var_name, Type.maty_actor_state()),
      #  body_asts = extract_body(body_block),
      #  {:body_ok, {:no_return, {:st_bottom}}, _final_env} <-  tc_expr_list(module, body_env, initial_session_state_S, body_asts)
      # all checks passed!
      {:ok, handler_label}
    else
      {:error, msg} ->
        {:error, msg}

      {:body_ok, {final_type, final_state}, _env} ->
        error =
          Error.handler_body_wrong_termination(
            clause_meta,
            handler_label,
            final_type,
            final_state
          )

        {:error, error}

      other ->
        {:error,
         Error.internal_error("Unexpected mismatch in init handler check: #{inspect(other)}")}
    end
  end

  def check_wf_on_link_callback(
        _module,
        _func_id,
        _clause,
        _delta_i,
        _psi_entry
      ) do
    # bind variables from the function signature
    # create the var env
    # typecheck the function (calls to register etc.)
    # make sure there is at least one call to register in this function
    # make sure the function returns {:ok, some_actor_state}
  end

  # -------------- HELPERS sorta --------------------

  defp extract_reg_info_details([{:callback, {handler_name, _, nil}}, {:args, args_ast}], _meta)
       when is_atom(handler_name) do
    {:ok, handler_name, args_ast}
  end

  defp extract_reg_info_details(other_ast, _meta) do
    {:error, :invalid_structure, other_ast}
  end

  @spec check_registration_info(
          module :: module(),
          reg_info_ast :: ast(),
          var_env :: var_env(),
          st_pre :: ST.t(),
          meta :: keyword()
        ) :: :ok | {:error, String.t(), var_env()}
  defp check_registration_info(module, reg_info_ast, var_env, st_pre, meta) do
    with {:ok, handler_name, args_ast} <- extract_reg_info_details(reg_info_ast, meta),
         #
         delta_I <- Utils.Env.get_map(module, :delta_I),
         {:handler, {:ok, delta_entry}, _} <-
           {:handler, Map.fetch(delta_I, handler_name), handler_name},
         func_id = delta_entry.function,
         #
         {:ok, {actual_args_type, args_st}, args_env} <-
           tc_expr(module, var_env, st_pre, args_ast),
         :ok <- Helpers.check_st_unchanged(st_pre, args_st, meta),
         #
         psi <- Utils.Env.get_map(module, :psi),
         {:func, {:ok, [{[expected_arg_type], _return_type} | _]}, {_, handler_name}} <-
           {:func, Map.fetch(psi, func_id), {func_id, handler_name}},
         #
         {true, _, args_env} <-
           {Type.is?(actual_args_type, expected_arg_type) or actual_args_type == :any or
              expected_arg_type == :any, {handler_name, actual_args_type, expected_arg_type},
            args_env} do
      {:ok, args_env}
    else
      {:error, :invalid_structure, details} ->
        {:error, Error.invalid_registration_info_structure(meta, details), var_env}

      {:handler, :error, handler_name} when not is_nil(handler_name) ->
        {:error, Error.register_unknown_handler(meta, handler_name), var_env}

      {:error, msg, err_env} ->
        {:error, "Error typechecking registration arguments: #{msg}", err_env}

      {:error, msg} ->
        {:error, msg, var_env}

      {:func, :error, {func_id, handler_name}} when not is_nil(func_id) ->
        error =
          Error.internal_error(
            "Missing or invalid Psi entry for init handler #{handler_name} / #{inspect(func_id)}"
          )

        {:error, error, var_env}

      {false, {handler_name, actual_args_type, expected_arg_type}, args_env} ->
        error =
          Error.register_arg_type_mismatch(meta,
            handler: handler_name,
            expected: expected_arg_type,
            got: actual_args_type
          )

        {:error, error, args_env}

      other ->
        {:error,
         Error.internal_error("Unexpected error in check_registration_info: #{inspect(other)}"),
         var_env}
    end
  end

  # defp extract_init_handler_arg_type(psi_entry, func_id, handler_label) do
  #   case psi_entry do
  #     [{[type_A], _return_type}] ->
  #       {:ok, type_A}

  #     _ ->
  #       {:error,
  #        Error.internal_error(
  #          "Mismatch between Delta_I and Psi structure for init handler #{handler_label} / #{inspect(func_id)}"
  #        )}
  #   end
  # end

  # defp extract_state_var({state_var_name, _, _}, _meta) when is_atom(state_var_name) do
  #   {:ok, state_var_name}
  # end

  # defp extract_state_var(state_var_ast, meta) do
  #   {:error, Error.handler_state_var_not_atom(meta, state_var_ast)}
  # end

  # defp extract_declared_role(role_ast, meta) do
  #   declared_role = Helpers.ast_to_literal(role_ast)

  #   if is_atom(declared_role) do
  #     {:ok, declared_role}
  #   else
  #     {:error, Error.handler_role_not_atom(meta, role_ast)}
  #   end
  # end

  # defp extract_message_pattern({label, payload}, _meta)
  #      when is_atom(label) do
  #   {:ok, {label, payload}}
  # end

  # defp extract_message_pattern(message_pattern_ast, meta) do
  #   {:error, Error.handler_msg_pattern_invalid(meta, message_pattern_ast)}
  # end

  # defp check_session_type_is_sin(
  #        %ST.SIn{from: expected_role, branches: branches},
  #        declared_role,
  #        _handler_label,
  #        clause_meta
  #      ) do
  #   if declared_role == expected_role do
  #     {:session_type_ok, expected_role, branches}
  #   else
  #     error =
  #       Error.handler_role_mismatch(
  #         clause_meta,
  #         expected: expected_role,
  #         got: declared_role
  #       )

  #     {:error, error}
  #   end
  # end

  # defp check_session_type_is_sin(other_st, _declared_role, handler_label, clause_meta) do
  #   error = Error.handler_session_type_not_sin(clause_meta, handler_label, got: other_st)
  #   {:error, error}
  # end

  defp check_clause_arity(meta, func_id, arity, arg_pattern_asts, spec_args_types) do
    if length(arg_pattern_asts) == length(spec_args_types) do
      :arity_ok
    else
      func_str = Utils.to_func(func_id)
      error = Error.arity_mismatch(meta, func_str, expected: length(spec_args_types), got: arity)
      {:error, error}
    end
  end

  defp check_argument_patterns(_meta, arg_pattern_asts, spec_args_types) do
    initial_arg_env = %{}

    args_check_result =
      Enum.zip(arg_pattern_asts, spec_args_types)
      |> Enum.reduce_while(
        {:ok, %{}, initial_arg_env},
        fn {p_ast, expected_type}, {:ok, acc_bindings, current_env} ->
          case tc_pattern(p_ast, expected_type, current_env) do
            {:ok, new_bindings, updated_env} ->
              case Helpers.check_and_merge_bindings(acc_bindings, new_bindings, current_env) do
                {:ok, merged_bindings, _env_ignored} ->
                  {:cont, {:ok, merged_bindings, updated_env}}

                {:error, msg, _env} ->
                  {:halt, {:error, msg}}
              end

            {:error, msg, _env} ->
              {:halt, {:error, msg}}
          end
        end
      )

    case args_check_result do
      {:ok, _final_bindings, body_var_env} ->
        {:args_ok, body_var_env}

      {:error, msg} ->
        {:error, msg}
    end
  end

  defp check_function_body(module, body_var_env, body_block) do
    body_asts = extract_body(body_block)
    initial_st = %ST.SEnd{}

    case tc_expr_list(module, body_var_env, initial_st, body_asts) do
      {:ok, {return_type, final_st}, final_env} ->
        {:body_ok, {return_type, final_st}, final_env}

      {:error, msg, _env} ->
        {:error, msg}
    end
  end

  defp check_final_state(meta, func_id, final_st) do
    expected_st = %ST.SEnd{}

    if final_st == expected_st do
      :state_ok
    else
      error = Error.function_altered_state(meta, func_id, final_st)
      {:error, error}
    end
  end

  defp check_return_type(meta, actual_return_type, spec_return_type) do
    if actual_return_type == spec_return_type do
      :type_ok
    else
      error =
        Error.return_type_mismatch(meta,
          expected: spec_return_type,
          got: actual_return_type
        )

      {:error, error}
    end
  end

  # Processes a list of expressions sequentially using tc_expr.
  # Returns the result of the *last* expression in the list.
  @spec tc_expr_list(
          module :: module(),
          var_env :: var_env(),
          st_pre :: ST.t(),
          ast_list :: [ast()]
        ) ::
          {:ok, {Type.t(), ST.t()}, var_env()} | {:error, binary(), var_env()}
  def tc_expr_list(_module, var_env, st_pre, []) do
    # Result of an empty block is nil, state preserved.
    {:ok, {nil, st_pre}, var_env}
  end

  def tc_expr_list(module, var_env, st_pre, ast_list) do
    # Reduce over expressions, threading state and env. Keep track of the *last* result.
    Enum.reduce_while(
      ast_list,
      {:ok, {nil, st_pre}, var_env},
      fn expr_ast, {:ok, {_prev_result, current_st}, current_env} ->
        # Logger.info("AST: #{inspect(expr_ast)}")

        case tc_expr(module, current_env, current_st, expr_ast) do
          {:ok, last_result, next_env} ->
            # Store the latest successful result and continue
            {:cont, {:ok, last_result, next_env}}

          {:error, error, err_env} ->
            # Halt on first error
            {:halt, {:error, error, err_env}}
        end
      end
    )
  end

  @doc """
  Checks if a pattern AST is compatible with an expected type and calculates
  the variable bindings introduced by the pattern. Corresponds to `⊢ p : A ⟹ Γ'`.

  Returns `{:ok, new_bindings, updated_env}` or `{:error, message, original_env}`.
  `new_bindings` contains only the variables bound in this pattern.
  `updated_env` is the original_env merged with new_bindings.
  """
  @spec tc_pattern(pattern_ast :: ast(), expected_type :: Type.t(), var_env :: var_env()) ::
          {:ok, map(), var_env()} | {:error, binary(), var_env()}

  # Pat-Var: Pattern is a variable 'x'
  def tc_pattern({var_name, _meta, context}, expected_type, var_env)
      when is_atom(var_name) and (is_atom(context) or is_nil(context)) do
    myDEBUG(300)
    new_bindings = %{var_name => expected_type}
    updated_env = Map.merge(var_env, new_bindings)
    {:ok, new_bindings, updated_env}
  end

  # Pat-Wild: Pattern is '_'
  def tc_pattern(:_, _expected_type, var_env) do
    myDEBUG(301)
    {:ok, %{}, var_env}
  end

  def tc_pattern({:_, _meta, _context}, _expected_type, var_env) do
    myDEBUG(302)

    {:ok, %{}, var_env}
  end

  # Pat-Value: Pattern is a literal value 'v'
  def tc_pattern(literal_pattern, expected_type, var_env)
      when is_number(literal_pattern) or
             is_binary(literal_pattern) or
             is_boolean(literal_pattern) or
             is_atom(literal_pattern) do
    myDEBUG(303)

    case Helpers.get_literal_type(literal_pattern) do
      {:ok, literal_type} ->
        if literal_type == expected_type or literal_type == :atom do
          {:ok, %{}, var_env}
        else
          # todo: Extract meta if possible
          meta = []

          error =
            Error.pattern_type_mismatch(meta,
              pattern: literal_pattern,
              expected: expected_type,
              got: literal_type
            )

          {:error, error, var_env}
        end

      :error ->
        # Should not happen for basic literals
        meta = []

        error =
          Error.internal_error(
            meta,
            "Could not get type for literal pattern #{inspect(literal_pattern)}"
          )

        {:error, error, var_env}
    end
  end

  # Pat-EmptyList: Pattern is '[]'
  def tc_pattern([], expected_type, var_env) do
    myDEBUG(304)

    case expected_type do
      {:list, _} ->
        {:ok, %{}, var_env}

      :any ->
        {:ok, %{}, var_env}

      _ ->
        meta = []

        # todo: fix the `got` value
        error =
          Error.pattern_type_mismatch(meta,
            pattern: [],
            expected: expected_type,
            got: "{:list, :any}"
          )

        {:error, error, var_env}
    end
  end

  # Pat-EmptyTuple: Pattern is '{}'
  def tc_pattern({:{}, _, []}, expected_type, var_env) do
    myDEBUG(305)

    case expected_type do
      {:tuple, []} ->
        {:ok, %{}, var_env}

      :any ->
        {:ok, %{}, var_env}

      _ ->
        meta = []

        error =
          Error.pattern_type_mismatch(meta,
            pattern: {},
            expected: expected_type,
            got: "{:tuple, []}"
          )

        {:error, error, var_env}
    end
  end

  # Pat-EmptyMap: Pattern is '%{}'
  def tc_pattern({:%{}, _, []}, expected_type, var_env) do
    myDEBUG(306)

    case expected_type do
      {:map, _} ->
        {:ok, %{}, var_env}

      :any ->
        {:ok, %{}, var_env}

      _ ->
        meta = []

        error =
          Error.pattern_type_mismatch(meta,
            pattern: %{},
            expected: expected_type,
            got: "{:map, %{}}"
          )

        {:error, error, var_env}
    end
  end

  # --- Recursive Pattern Clauses ---

  # Pat-Cons
  def tc_pattern({:|, meta, [p1_ast, p2_ast]}, expected_type, var_env) do
    myDEBUG(307)

    case expected_type do
      {:list, element_type} ->
        with {:p1, {:ok, bindings1, env1}} <- {:p1, tc_pattern(p1_ast, element_type, var_env)},
             {:p2, {:ok, bindings2, _env2}} <- {:p2, tc_pattern(p2_ast, expected_type, env1)},
             # pass env1 because env2 already contains bindings1
             # we only need to check bindings2 against bindings1
             {:merge, {:ok, merged_bindings, final_env}} <-
               {:merge, Helpers.check_and_merge_bindings(bindings1, bindings2, env1)} do
          {:ok, merged_bindings, final_env}
        else
          {:p1, {:error, msg, env}} -> {:error, msg, env}
          {:p2, {:error, msg, env}} -> {:error, msg, env}
          {:merge, {:error, msg, env}} -> {:error, msg, env}
        end

      other_type ->
        error =
          Error.pattern_type_mismatch(meta, pattern: "[h|t]", expected: "List", got: other_type)

        {:error, error, var_env}
    end
  end

  # Pat-Tuple
  def tc_pattern({p1_ast, p2_ast} = pattern_ast, expected_type, var_env) do
    myDEBUG(308)

    case expected_type do
      {:tuple, [type_a, type_b]} ->
        with {:p1, {:ok, bindings1, env1}} <- {:p1, tc_pattern(p1_ast, type_a, var_env)},
             {:p2, {:ok, bindings2, _env2}} <- {:p2, tc_pattern(p2_ast, type_b, env1)},
             {:merge, {:ok, merged_bindings, final_env}} <-
               {:merge, Helpers.check_and_merge_bindings(bindings1, bindings2, env1)} do
          {:ok, merged_bindings, final_env}
        else
          {:p1, {:error, msg, env}} -> {:error, msg, env}
          {:p2, {:error, msg, env}} -> {:error, msg, env}
          {:merge, {:error, msg, env}} -> {:error, msg, env}
        end

      {:tuple, other_types} ->
        meta = []
        error = Error.pattern_arity_mismatch(meta, "Tuple", expected: 2, got: length(other_types))
        {:error, error, var_env}

      other_type ->
        meta = []

        error =
          Error.pattern_type_mismatch(meta,
            pattern: pattern_ast,
            expected: "Tuple",
            got: other_type
          )

        {:error, error, var_env}
    end
  end

  def tc_pattern({:{}, meta, elements_asts}, expected_type, var_env) do
    myDEBUG(309)

    case expected_type do
      {:tuple, expected_types} when length(elements_asts) == length(expected_types) ->
        # process elements sequentially, checking disjointness at each step
        initial_acc = {:ok, %{}, var_env}

        Enum.zip(elements_asts, expected_types)
        |> Enum.reduce_while(
          initial_acc,
          fn {p_ast, p_expected_type}, {:ok, acc_bindings, current_env} ->
            case tc_pattern(p_ast, p_expected_type, current_env) do
              {:ok, new_bindings, updated_env} ->
                case Helpers.check_and_merge_bindings(acc_bindings, new_bindings, current_env) do
                  {:ok, merged_bindings, _env_ignored} ->
                    # use updated_env from the recursive call for the next iteration
                    {:cont, {:ok, merged_bindings, updated_env}}

                  {:error, msg, _env} ->
                    # halt on conflict
                    {:halt, {:error, msg, current_env}}
                end

              {:error, msg, _env} ->
                # halt if recursive call fails
                {:halt, {:error, msg, current_env}}
            end
          end
        )
        |> case do
          {:ok, final_bindings, final_env} -> {:ok, final_bindings, final_env}
          {:error, msg, env} -> {:error, msg, env}
        end

      {:tuple, expected_types} ->
        error =
          Error.pattern_arity_mismatch(meta, "Tuple",
            expected: length(expected_types),
            got: length(elements_asts)
          )

        {:error, error, var_env}

      other_type ->
        error =
          Error.pattern_type_mismatch(meta, pattern: "{...}", expected: "Tuple", got: other_type)

        {:error, error, var_env}
    end
  end

  # Pat-Map
  # Assuming keys k_i are literal atoms.
  def tc_pattern({:%{}, meta, pairs}, expected_type, var_env) do
    myDEBUG(310)

    case expected_type do
      {:map, expected_type_map} ->
        initial_acc = {:ok, %{}, var_env}

        Enum.reduce_while(
          pairs,
          initial_acc,
          fn {key_ast, p_ast}, {:ok, acc_bindings, current_env} ->
            literal_key = Helpers.ast_to_literal(key_ast)

            if not is_atom(literal_key) do
              {:halt, {:error, Error.pattern_map_key_not_atom(meta, key_ast), current_env}}
            else
              # check if key exists in expected type map and get expected value type
              case Map.fetch(expected_type_map, literal_key) do
                {:ok, p_expected_type} ->
                  case tc_pattern(p_ast, p_expected_type, current_env) do
                    {:ok, new_bindings, updated_env} ->
                      # check disjointness and merge
                      case Helpers.check_and_merge_bindings(
                             acc_bindings,
                             new_bindings,
                             current_env
                           ) do
                        {:ok, merged_bindings, _env_ignored} ->
                          {:cont, {:ok, merged_bindings, updated_env}}

                        {:error, msg, _env} ->
                          {:halt, {:error, msg, current_env}}
                      end

                    {:error, msg, _env} ->
                      {:halt, {:error, msg, current_env}}
                  end

                :error ->
                  # key from pattern not found in expected map type
                  error = Error.pattern_map_key_not_found(meta, literal_key)
                  {:halt, {:error, error, current_env}}
              end
            end
          end
        )
        |> case do
          {:ok, final_bindings, final_env} -> {:ok, final_bindings, final_env}
          {:error, msg, env} -> {:error, msg, env}
        end

      other_type ->
        error =
          Error.pattern_type_mismatch(meta, pattern: "%{...}", expected: "Map", got: other_type)

        {:error, error, var_env}
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

  defp extract_body({:try, _, [[do: {:__block__, _, [_, _, body_block]}, catch: _]]}),
    do: extract_body(body_block)

  defp extract_body(expr) when not is_list(expr), do: [expr]

  # def myDEBUG(_num), do: :ok
  def myDEBUG(num), do: Logger.debug("[#{num}]", ansi_color: :light_green)
end
