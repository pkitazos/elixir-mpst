defmodule Maty.Typechecker.Error do
  def version_mismatch(expected, got) do
    "Found version #{got} but expected #{expected}."
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

  def no_raw_receive(meta) do
    with_meta(meta, "Raw receive is not allowed in a Maty.Actor.")
  end

  def no_raw_send(meta) do
    with_meta(meta, "Raw send(...) is not allowed in a Maty.Actor.")
  end

  def unexpected do
    "an unexpected error occurred"
  end

  def case_scrutinee_altered_state(_a, _b), do: "case_scrutinee_altered_state"

  def handler_body_wrong_termination(_a, _b, _c, _d), do: "handler_body_wrong_termination"

  # -----------------------------------------------------------------
  def internal_error(a), do: "Internal Error: #{a}"

  def internal_error(a, b), do: "Internal Error: #{inspect(a)} - #{inspect(b)}"

  # -----------------------------------------------------------------

  defp with_meta(meta, str) do
    meta = Keyword.take(meta, [:line, :column])
    "#{inspect(meta)} #{str}"
  end
end
