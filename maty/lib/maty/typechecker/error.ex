defmodule Maty.Typechecker.Error do
  def spec_args_not_well_typed(spec_name, args_types) do
    "Problem with @spec for #{spec_name}. Function args: \n\t#{inspect(args_types)} are not well typed"
  end

  def spec_return_not_well_typed(spec_name, return_type) do
    "Problem with @spec for #{spec_name}. Function return: \n\t#{inspect(return_type)} are not well typed"
  end

  def version_mismatch(expected, got) do
    "Found version #{got} but expected #{expected}."
  end

  def handler_already_taken(handler, prev, curr) do
    "Can't apply handler \'#{handler}\' to function #{curr}, function #{prev} was already annotated with this handler"
  end

  # pin
  def missing_handler(handler) do
    "No handler in this module named #{handler}"
  end

  def no_private_handlers(meta) do
    with_meta(meta, "Handlers can't be private functions")
  end

  # pin
  def unannotated_handler(func) do
    "Function: #{func} has not been annotated with a handler"
  end

  def missing_spec_annotation(func) do
    "Function: #{func} is missing a @spec annotation"
  end

  def variable_not_exist(meta, var) do
    with_meta(meta, "variable #{var} doesn't exist")
  end

  def lhs_failed(msg) do
    "typechecking lhs failed: #{msg}"
  end

  def rhs_failed(msg) do
    "typechecking rhs failed: #{msg}"
  end

  def binary_operator_requires_numbers(op, lhs_type, rhs_type) do
    "Binary operator #{op} requires numbers, got #{inspect(lhs_type)} and #{inspect(rhs_type)}"
  end

  def binary_operator_requires_binaries(lhs_type, rhs_type) do
    "Binary operator <> requires binaries, got #{inspect(lhs_type)} and #{inspect(rhs_type)}"
  end

  def comparison_operator_requires_same_type(op, lhs_type, rhs_type) do
    "Comparison operator #{op} requires both operands to be of the same type, got #{inspect(lhs_type)} and #{inspect(rhs_type)}"
  end

  def comparison_operator_requires_numbers(op, lhs_type, rhs_type) do
    "Comparison operator #{op} requires both operands to be numbers, got #{inspect(lhs_type)} and #{inspect(rhs_type)}"
  end

  def logical_operator_requires_boolean(op, expr_type) do
    "Logical operator #{op} requires a boolean operand, got #{inspect(expr_type)}"
  end

  # pin
  def function_not_exist(func) do
    "function #{func} doesn't exist"
  end

  # pin
  def at_least_one_arg_not_well_typed(func, spec_args) do
    "#{func} with types #{inspect(spec_args)} arguments not well typed. At least one argument is not well typed."
  end

  def too_few_arguments(meta, opts) do
    with_meta(meta, "Too few params were given to function.\n#{display_opts(opts)}")
  end

  def arity_mismatch(meta, func) do
    with_meta(meta, "Arity mismatch between #{func} spec and function definition")
  end

  # pin
  def no_spec_for_function(type_specs) do
    "no spec for this function: #{inspect(type_specs)}"
  end

  def return_types_mismatch(meta, opts) do
    with_meta(meta, "Return types don't match.\n#{display_opts(opts)}")
  end

  def handler_args_shape_invalid(meta) do
    with_meta(
      meta,
      "Invalid number or shape of arguments given to handler. Handlers must have 4 arguments."
    )
  end

  # pin
  def message_format_invalid do
    "message not formatted properly"
  end

  def no_matching_function_clause(meta, func) do
    with_meta(meta, "No function clause matches provided function info: #{func}")
  end

  def provided_handler_role_pair_mismatch(meta, opts) do
    with_meta(
      meta,
      "the provided handler and role don't match. The provided handler handles messages from a different participant.\n#{display_opts(opts)}"
    )
  end

  def role_type_invalid(meta, type) do
    with_meta(meta, "@spec defines role type as #{inspect(type)}, must use :role or :atom type")
  end

  def session_ctx_type_invalid(meta, type) do
    with_meta(
      meta,
      "@spec defines session ctx type as #{inspect(type)}, must use :session_ctx type"
    )
  end

  def maty_actor_state_type_invalid(meta, type) do
    with_meta(
      meta,
      "@spec defines maty actor state type as #{inspect(type)}, must use :maty_actor_state type"
    )
  end

  def handler_return_type_invalid(meta, other) do
    with_meta(
      meta,
      "invalid return type for handler: #{inspect(other)} is not in [:suspend, :done]"
    )
  end

  def payload_var_has_other_value(meta, type) do
    with_meta(
      meta,
      "Payload has unsupported shape, must be variable or tuple of variables: #{inspect(type)} won't do"
    )
  end

  def handler_role_mismatch(meta, opts) do
    with_meta(meta, "handler role mismatch.\n#{display_opts(opts)}")
  end

  def handler_message_label_mismatch(meta, opts) do
    with_meta(meta, "handler message label mismatch.\n#{display_opts(opts)}")
  end

  def message_payload_type_mismatch(meta, opts) do
    with_meta(meta, "message payload type mismatch.\n#{display_opts(opts)}")
  end

  def send_role_mismatch(meta, opts) do
    with_meta(
      meta,
      "recipient role mismatch. Sending to incorrect participant in session.\n#{display_opts(opts)}"
    )
  end

  def no_branch_with_this_label(meta, got: got) do
    with_meta(
      meta,
      "message label incompatible with session precondition. No continuation sends message with label: #{inspect(got)}"
    )
  end

  def session_typecheck_handler_unexpected(msg) do
    "Unexpected return from session_typecheck ST.SName: #{inspect(msg)}"
  end

  def something_went_wrong do
    "Something went wrong"
  end

  def unexpected do
    "an unexpected error occurred"
  end

  def arg_type_mismatch(meta, opts) do
    with_meta(meta, "Argument type mismatch.\n#{display_opts(opts)}")
  end

  def ambiguous_function_call(meta, func) do
    with_meta(meta, "Too many function specs defined for function: #{func}")
  end

  def function_missing_session_type(meta, func) do
    with_meta(meta, "function #{func} doesn't seem to have a session type stored")
  end

  def handler_from_unexpected_module(meta, opts) do
    with_meta(meta, "Handler function from unexpected module.\n#{display_opts(opts)}")
  end

  def unhandled_session_branches(meta, unhandled) do
    branch_msg =
      if unhandled == 1 do
        "1 branch remains unhandled"
      else
        "#{unhandled} branches remain unhandled"
      end

    with_meta(meta, "Handler exits without handling all session type branches. #{branch_msg}")
  end

  def unsupported_lhs_assignment(meta, shape) do
    with_meta(meta, "Unsupported lhs in assignment, found: #{inspect(shape)}")
  end

  def session_typecheck_match_unexpected(meta, other) do
    with_meta(meta, "Unexpected return from session_typecheck match operator: #{inspect(other)}")
  end

  def forbidden_receive(meta, func) do
    with_meta(
      meta,
      "Maty.Actor illegal communication. Function #{func} perform receive operation using `Elixir.receive` construct instead of relying on annotated handlers"
    )
  end

  def forbidden_send(meta, func) do
    with_meta(
      meta,
      "Maty.Actor illegal communication. Function #{func} performs send operation using `Elixir.send/2` instead of `Maty.Actor.maty_send/3`"
    )
  end

  def unsupported_argument_shape(meta, shape) do
    with_meta(meta, "Some other shape was provided: #{inspect(shape)}")
  end

  def missing_registration do
    "init_actor function does not register actor in session"
  end

  def invalid_ap_type(meta, opts) do
    with_meta(meta, "Invalid type given to AP.\n#{display_opts(opts)}")
  end

  def function_no_type do
    "Can't typecheck this function"
  end

  def invalid_function_capture(meta, fun_capture) do
    with_meta(meta, "Unsupported function reference syntax: #{inspect(fun_capture)}")
  end

  def no_compatible_session_branch do
    "No matching session branch found"
  end

  def ambiguous_branch_match(ids) do
    "Multiple session branches match this case branch.\nExpected 1 branch to match, instead got #{length(ids)} matches"
  end

  def type_errors_prevented_match(branch_errors) do
    "There were type errors preventing matching.\n#{branch_errors}"
  end

  def pre_condition_cannot_send(meta, opts) do
    with_meta(
      meta,
      "Session precondition does not allow sending at this point.\n#{display_opts(opts)}"
    )
  end

  def invalid_suspension_state(meta, other) do
    with_meta(
      meta,
      "Session precondition does not allow suspending at this point. Session state: \n#{other}"
    )
  end

  def invalid_termination_state(meta, other) do
    with_meta(
      meta,
      "Session precondition does not allow terminating at this point. Session state: \n#{other}"
    )
  end

  def handler_mismatch(meta, opts) do
    with_meta(meta, "Handler suspends with incorrect handler.\n#{display_opts(opts)}")
  end

  def unreachable do
    "This should be unreachable"
  end

  def invalid_session_type(error) do
    "Invalid session type: #{inspect(error)}"
  end

  # -----------------------------------------------------------------

  defp with_meta(meta, str) do
    meta = Keyword.take(meta, [:line, :column])
    "#{inspect(meta)} #{str}"
  end

  defp display_opts([expected: expected, got: got] = _opts) do
    "Expected: #{inspect(expected)} \nGot: #{inspect(got)}"
  end
end
