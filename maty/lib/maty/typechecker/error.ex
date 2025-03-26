defmodule Maty.Typechecker.Error do
  def participant_mismatch(handler, line, [expected: expected, got: got] = _opts) do
    "Error in #{handler} message came from wrong participant (line: #{line})\n- Expected:\t#{expected}\n- Got:\t\t#{got}\n"
  end

  def malformed_message(handler, line, [tuple_size: tuple_size] = _opts) do
    "Error in #{handler} malformed message (line: #{line})\n" <>
      "Messages should be structured as: 2-tuples {:label, message}\nReceived #{tuple_size}-tuple\n"
  end

  def label_mismatch(handler, line, [expected: expected, got: got] = _opts) do
    "Error in #{handler} message label incorrect (line: #{line})\n- Expected:\t#{expected}\n- Got:\t\t#{got}\n"
  end

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

  def missing_handler(handler) do
    "No handler in this module named #{handler}"
  end

  def no_private_handlers() do
    "Handlers can't be private functions"
  end

  def unsupported_spec_type() do
    "Unsupported type in @spec annotation"
  end

  def unannotated_handler(func) do
    "Function: #{func} has not been annotated with a handler"
  end

  def missing_spec_annotation(func) do
    "Function: #{func} is missing a @spec annotation"
  end

  def variable_not_exist do
    "variable doesn't exist"
  end

  def lhs_failed(msg) do
    "lhs failed: #{msg}"
  end

  def rhs_failed(msg) do
    "rhs failed: #{msg}"
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

  def function_not_exist(func) do
    "function #{func} doesn't exist"
  end

  def at_least_one_arg_not_well_typed do
    "at least one argument is not well typed"
  end

  def arity_mismatch do
    "arity mismatch"
  end

  def no_spec_for_function(type_specs) do
    "no spec for this function: #{inspect(type_specs)}"
  end

  def return_types_mismatch do
    "return types don't match"
  end

  def handler_args_shape_invalid do
    "handler args shape not looking good"
  end

  def message_format_invalid do
    "message not formatted properly"
  end

  def role_type_invalid do
    "role not typed properly"
  end

  def session_ctx_type_invalid do
    "session_ctx not typed properly"
  end

  def maty_actor_state_type_invalid do
    "maty_actor_state not typed properly"
  end

  def handler_return_type_invalid(other) do
    "invalid return type for handler: #{inspect(other)} is #{inspect(other)}"
  end

  def payload_var_has_other_value(other) do
    "Payload has some other value: #{inspect(other)}"
  end

  def handler_role_mismatch do
    "handler role mismatch"
  end

  def message_label_mismatch do
    "message label mismatch"
  end

  def message_payload_type_mismatch do
    "message payload type mismatch"
  end

  def session_typecheck_handler_unexpected(msg) do
    "Unexpected return from session_typecheck ST.SHandler: #{inspect(msg)}"
  end

  def something_went_wrong do
    "something went wrong and I'm not sure what yet"
  end

  def function_missing_session_type do
    "this function doesn't seem to have a session type stored"
  end

  def handler_from_unexpected_module do
    "Handler function from unexpected module"
  end

  def session_type_branches_mismatch do
    "not enough function clauses to support the annotated session type"
  end

  def role_mismatch do
    "role mismatch"
  end

  def message_label_incompatible do
    "message label incompatible with session precondition"
  end

  def unhandled_session_branches do
    "unhandled session type branches"
  end

  def invalid_lhs_assignment do
    "Invalid lhs-hand side in assignment"
  end

  def session_typecheck_match_unexpected(other) do
    "Unexpected return from session_typecheck match operator: #{inspect(other)}"
  end

  def function_no_type do
    "function doesn't seem to have a type"
  end

  def no_matching_session_branch do
    "No matching session branch found"
  end

  def multiple_matching_session_branches do
    "Multiple matching session branches found"
  end

  def unreachable do
    "This should be unreachable"
  end
end
