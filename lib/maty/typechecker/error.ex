defmodule Maty.Typechecker.Error do
  alias Maty.Utils

  def invalid_session_type_annotation(handler) do
    "Problem with @st annotation for #{inspect(handler)}. Can't parse Session Type string"
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

  def missing_handler(handler, meta) do
    with_meta(
      meta,
      "Handler #{handler} uses label not available in this module's @st annotations"
    )
  end

  def function_spec_info_mismatch(
        spec_name: spec_name,
        spec_arity: spec_arity,
        fn_name: fn_name,
        fn_arity: fn_arity
      ) do
    "Spec info: {#{spec_name}, #{spec_arity}} doesn't match function info: {#{fn_name}, #{fn_arity}}"
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

  def type_mismatch(meta, opts) do
    with_meta(meta, "Type mismatch:\n#{display_opts(opts)}")
  end

  def tuple_size_mismatch do
    "Tuple size mismatch in pattern match"
  end

  def list_size_mismatch do
    "List size mismatch in pattern match"
  end

  def list_index_error(meta, index, msg) do
    with_meta(meta, "Expression at index #{index} returned error: #{msg}")
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

  def unsupported_list_pattern do
    "Unsupported list pattern in function parameter"
  end

  def tuple_param_type_mismatch do
    "Type mismatch for tuple pattern in function parameter"
  end

  def n_tuple_param_type_mismatch do
    "Type mismatch for n-tuple pattern in function parameter"
  end

  def missing_map_key(key) do
    "Map key #{inspect(key)} not found in type spec"
  end

  def unsupported_map_pattern do
    "Unsupported map pattern in function parameter"
  end

  def map_param_type_mismatch do
    "Type mismatch for map pattern in function parameter"
  end

  def unsupported_param_pattern(other) do
    "Unsupported parameter pattern: #{inspect(other)}"
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

  def no_raw_receive(meta) do
    with_meta(meta, "Raw receive is not allowed in a Maty.Actor.")
  end

  def no_raw_send(meta) do
    with_meta(meta, "Raw send(...) is not allowed in a Maty.Actor.")
  end

  def non_handler_communication(fn_info) do
    "Non-handler function: #{Utils.to_func(fn_info)} attempts communication"
  end

  def message_format_invalid(meta, got: shape) do
    with_meta(
      meta,
      "Handler messages can only be tagged 2-tuples. Instead got: #{inspect(shape)}"
    )
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

  def session_typecheck_unexpected(other) do
    "Unexpected return from session_typecheck: #{inspect(other)}"
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

  def remaining_session_type(func, st) do
    "Handler #{func} does not properly progress to the end of its annotated session type. Remaining session type: #{inspect(st)}"
  end

  def handler_return_type_mismatch(func, expected: expected, got: got) do
    opts = [expected: MapSet.to_list(expected), got: MapSet.to_list(got)]
    "Handler #{func} return does not match @spec return type.\n#{display_opts(opts)}"
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

  def at_branch(id, msg) do
    "Branch #{inspect(id)}: #{msg}"
  end

  def branch_not_fully_handled(id) do
    "Did not fully handle branch: #{inspect(id)}"
  end

  def too_many_branch_exits(id) do
    "Too many exits for a single branch: #{inspect(id)}"
  end

  def insufficient_function_clauses do
    "Not enough function clauses to support the annotated session type"
  end

  def no_branches_in_suspend(st) do
    "SName types not directly supported in case expressions: #{inspect(st)}"
  end

  def no_branches_in_end(st) do
    "SEnd types have no branches to handle: #{inspect(st)}"
  end

  def unsupported_session_type_in_branch(st) do
    "Unsupported session type in case expression: #{inspect(st)}"
  end

  def unsupported_argument_shape(meta, shape) do
    with_meta(meta, "Some other shape was provided: #{inspect(shape)}")
  end

  def missing_registration do
    "on_link function does not register actor in session"
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
  def conflicting_pattern_bindings(_1), do: "conflicting_pattern_bindings"
  def internal_error(a), do: "Internal Error: #{a}"

  def unknown_type_constructor(a) do
    "Unknown type constructor or type atom not found in type_env: #{a}"
  end

  def spec_return_not_well_typed(spec_name, return_ast, msg) do
    "@spec return #{inspect(return_ast)} for #{inspect(spec_name)} is not well typed: #{inspect(msg)}"
  end

  def spec_args_not_well_typed(spec_name, args_asts_list, msg) do
    "@spec args #{inspect(args_asts_list)} for #{inspect(spec_name)} are not well typed: #{inspect(msg)}"
  end

  def unsupported_type_constructor(a) do
    "Unsupported type specification AST structure: #{inspect(a)}"
  end

  def parse_error_at(failed_index, spec_name, msg) do
    "Error parsing argument ##{failed_index + 1} in spec #{spec_name}: #{msg}"
  end

  def unexpected_error(func_id, other) do
    "Unexpected error during spec processing for #{func_id}: #{inspect(other)}"
  end

  def internal_error(a, b), do: "Internal Error: #{inspect(a)} - #{inspect(b)}"
  def list_elements_incompatible(_a, _b), do: "list_elements_incompatible"
  def complex_map_key(_a, _b), do: "complex_map_key"
  def invalid_map_key_type(_a, _b), do: "invalid_map_key_type"
  def pattern_map_key_not_found(_a, _b), do: "pattern_map_key_not_found"
  def pattern_map_key_not_atom(_a, _b), do: "pattern_map_key_not_atom"

  def pattern_type_mismatch(_a, opts),
    do: "pattern type mismatch: #{inspect(opts)}"

  # "pattern_type_mismatch:\n\tpattern: #{inspect(pattern)}\n\texpected: #{inspect(expected)}\n\tgot: #{inspect(got)}"

  def return_type_mismatch(_a, _b), do: "return_type_mismatch"
  def handler_msg_pattern_invalid(_a, _b), do: "handler_msg_pattern_invalid"
  def handler_role_not_atom(_a, _b), do: "handler_role_not_atom"
  def handler_state_var_not_atom(_a, _b), do: "handler_state_var_not_atom"
  def register_arg_type_mismatch(_a, _b), do: "register_arg_type_mismatch"
  def register_unknown_handler(_a, _b), do: "register_unknown_handler"
  def invalid_registration_info_structure(_a, _b), do: "invalid_registration_info_structure"
  def invalid_maty_state_type(_a, _b), do: "invalid_maty_state_type"

  def function_not_exist(_a, func), do: "function_not_exist: #{func}"
  def case_scrutinee_altered_state(_a, _b), do: "case_scrutinee_altered_state"
  def case_branches_incompatible_types(_a, _b), do: "case_branches_incompatible_types"
  def case_branches_incompatible_states(_a, _b), do: "case_branches_incompatible_states"
  def send_message_not_tuple(_a, _b), do: "send_message_not_tuple"
  def send_invalid_label(_a, _b), do: "send_invalid_label"
  def send_payload_mismatch(_a, _b), do: "send_payload_mismatch"

  def pattern_arity_mismatch(_a, _b, _c), do: "pattern_arity_mismatch"
  def function_altered_state(_a, _b, _c), do: "function_altered_state"
  def arity_mismatch(_a, _b, _c), do: "arity_mismatch"
  def handler_session_type_not_sin(_a, _b, _c), do: "handler_session_type_not_sin"
  def no_matching_function_clause(_a, _b, _c), do: "no_matching_function_clause"
  def logical_operator_requires_boolean(_a, _b, _c), do: "logical_operator_requires_boolean"
  def invalid_maty_state_type(_a, _b, _c), do: "invalid_maty_state_type"

  def handler_body_wrong_termination(_a, _b, _c, _d), do: "handler_body_wrong_termination"
  def logical_operator_type_mismatch(_a, _b, _c, _d), do: "logical_operator_type_mismatch"
  def binary_operator_type_mismatch(_a, _b, _c, _d), do: "binary_operator_type_mismatch"

  def wrong_number_of_clauses do
    "Incompatible number of clauses defined for on_link/2"
  end

  def wrong_number_of_specs do
    "Incompatible number of @spec's defined for on_link/2"
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
