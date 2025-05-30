defmodule Maty.Typechecker.Error do
  alias Maty.Utils

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

  def variable_not_exist(meta, var) do
    with_meta(meta, "variable #{var} doesn't exist")
  end

  def lhs_failed(msg) do
    "typechecking lhs failed: #{msg}"
  end

  def rhs_failed(msg) do
    "typechecking rhs failed: #{msg}"
  end

  def list_index_error(meta, index, msg) do
    with_meta(meta, "Expression at index #{index} returned error: #{msg}")
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

  def provided_handler_role_pair_mismatch(meta, opts) do
    with_meta(
      meta,
      "the provided handler and role don't match. The provided handler handles messages from a different participant.\n#{display_opts(opts)}"
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

  def unhandled_session_branches(meta, unhandled) do
    branch_msg =
      if unhandled == 1 do
        "1 branch remains unhandled"
      else
        "#{unhandled} branches remain unhandled"
      end

    with_meta(meta, "Handler exits without handling all session type branches. #{branch_msg}")
  end

  def session_typecheck_match_unexpected(meta, other) do
    with_meta(meta, "Unexpected return from session_typecheck match operator: #{inspect(other)}")
  end

  def remaining_session_type(func, st) do
    "Handler #{func} does not properly progress to the end of its annotated session type. Remaining session type: #{inspect(st)}"
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

  def no_branches_in_suspend(st) do
    "SName types not directly supported in case expressions: #{inspect(st)}"
  end

  def no_branches_in_end(st) do
    "SEnd types have no branches to handle: #{inspect(st)}"
  end

  def unsupported_session_type_in_branch(st) do
    "Unsupported session type in case expression: #{inspect(st)}"
  end

  def missing_registration do
    "on_link function does not register actor in session"
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

  def register_unknown_handler(_a, _b), do: "register_unknown_handler"

  def invalid_registration_info_structure(_a, _b), do: "invalid_registration_info_structure"

  def case_scrutinee_altered_state(_a, _b), do: "case_scrutinee_altered_state"

  def handler_role_not_atom(_a, _b), do: "handler_role_not_atom"

  def handler_state_var_not_atom(_a, _b), do: "handler_state_var_not_atom"

  def handler_session_type_not_sin(_a, _b, _c), do: "handler_session_type_not_sin"

  def handler_body_wrong_termination(_a, _b, _c, _d), do: "handler_body_wrong_termination"

  # -----------------------------------------------------------------
  def internal_error(a), do: "Internal Error: #{a}"

  def internal_error(a, b), do: "Internal Error: #{inspect(a)} - #{inspect(b)}"

  # -----------------------------------------------------------------

  defp with_meta(meta, str) do
    meta = Keyword.take(meta, [:line, :column])
    "#{inspect(meta)} #{str}"
  end

  defp display_opts([expected: expected, got: got] = _opts) do
    "Expected: #{inspect(expected)} \nGot: #{inspect(got)}"
  end
end
