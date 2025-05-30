defmodule Maty.Typechecker.Error.TypeMismatch do
  defp with_meta(meta, str) do
    meta = Keyword.take(meta, [:line, :column])
    "#{inspect(meta)} #{str}"
  end

  defp display_opts([expected: expected, got: got] = _opts) do
    "Expected: #{inspect(expected)} \nGot: #{inspect(got)}"
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

  def type_mismatch(meta, opts) do
    with_meta(meta, "Type mismatch:\n#{display_opts(opts)}")
  end

  def return_types_mismatch(meta, opts) do
    with_meta(meta, "Return types don't match.\n#{display_opts(opts)}")
  end

  def arg_type_mismatch(meta, opts) do
    with_meta(meta, "Argument type mismatch.\n#{display_opts(opts)}")
  end

  def return_type_mismatch(_a, _b), do: "return_type_mismatch"

  def binary_operator_type_mismatch(_a, _b, _c, _d), do: "binary_operator_type_mismatch"

  def logical_operator_type_mismatch(_a, _b, _c, _d), do: "logical_operator_type_mismatch"

  def logical_operator_requires_boolean(_a, _b, _c), do: "logical_operator_requires_boolean"

  def list_elements_incompatible(_a, _b), do: "list_elements_incompatible"

  def case_branches_incompatible_types(_a, _b), do: "case_branches_incompatible_types"

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

  def invalid_maty_state_type(_a, _b), do: "invalid_maty_state_type"

  def invalid_maty_state_type(_a, _b, _c), do: "invalid_maty_state_type"

  def register_arg_type_mismatch(_a, _b), do: "register_arg_type_mismatch"

  def send_message_not_tuple(_a, _b), do: "send_message_not_tuple"

  def send_invalid_label(_a, _b), do: "send_invalid_label"

  def send_payload_mismatch(_a, _b), do: "send_payload_mismatch"

  def invalid_ap_type(meta, opts) do
    with_meta(meta, "Invalid type given to AP.\n#{display_opts(opts)}")
  end

  def handler_return_type_mismatch(func, expected: expected, got: got) do
    opts = [expected: MapSet.to_list(expected), got: MapSet.to_list(got)]
    "Handler #{func} return does not match @spec return type.\n#{display_opts(opts)}"
  end

  def case_branches_incompatible_states(_a, _b), do: "case_branches_incompatible_states"
end