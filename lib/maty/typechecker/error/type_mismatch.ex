defmodule Maty.Typechecker.Error.TypeMismatch do
  defp with_meta(meta, str) do
    meta = Keyword.take(meta, [:line, :column])
    "#{inspect(meta)} #{str}"
  end

  defp display_opts([expected: expected, got: got] = _opts) do
    "Expected: #{inspect(expected)} \nGot: #{inspect(got)}"
  end

  def logical_operator_requires_boolean(op, expr_type) do
    "Logical operator #{op} requires a boolean operand, got #{inspect(expr_type)}"
  end

  def return_type_mismatch(_a, _b), do: "return_type_mismatch"

  def binary_operator_type_mismatch(_a, _b, _c, _d), do: "binary_operator_type_mismatch"

  def logical_operator_type_mismatch(_a, _b, _c, _d), do: "logical_operator_type_mismatch"

  def logical_operator_requires_boolean(_a, _b, _c), do: "logical_operator_requires_boolean"

  def list_elements_incompatible(_a, _b), do: "list_elements_incompatible"

  def case_branches_incompatible_types(_a, _b), do: "case_branches_incompatible_types"

  def send_role_mismatch(meta, opts) do
    with_meta(
      meta,
      "recipient role mismatch. Sending to incorrect participant in session.\n#{display_opts(opts)}"
    )
  end

  def invalid_maty_state_type(_a, _b), do: "invalid_maty_state_type"

  def invalid_maty_state_type(_a, _b, _c), do: "invalid_maty_state_type"

  def send_message_not_tuple(_a, _b), do: "send_message_not_tuple"

  def case_branches_incompatible_states(_a, _b), do: "case_branches_incompatible_states"
end
