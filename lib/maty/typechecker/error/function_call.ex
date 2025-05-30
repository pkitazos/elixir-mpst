defmodule Maty.Typechecker.Error.FunctionCall do
  defp with_meta(meta, str) do
    meta = Keyword.take(meta, [:line, :column])
    "#{inspect(meta)} #{str}"
  end

  defp display_opts([expected: expected, got: got] = _opts) do
    "Expected: #{inspect(expected)} \nGot: #{inspect(got)}"
  end

  def function_not_exist(func) do
    "function #{func} doesn't exist"
  end

  def function_not_exist(_a, func), do: "function_not_exist: #{func}"

  def too_few_arguments(meta, opts) do
    with_meta(meta, "Too few params were given to function.\n#{display_opts(opts)}")
  end

  def arity_mismatch(meta, func) do
    with_meta(meta, "Arity mismatch between #{func} spec and function definition")
  end

  def arity_mismatch(_a, _b, _c), do: "arity_mismatch"

  def no_matching_function_clause(meta, func) do
    with_meta(meta, "No function clause matches provided function info: #{func}")
  end

  def no_matching_function_clause(_a, _b, _c), do: "no_matching_function_clause"

  def ambiguous_function_call(meta, func) do
    with_meta(meta, "Too many function specs defined for function: #{func}")
  end

  def function_missing_session_type(meta, func) do
    with_meta(meta, "function #{func} doesn't seem to have a session type stored")
  end

  def handler_from_unexpected_module(meta, opts) do
    with_meta(meta, "Handler function from unexpected module.\n#{display_opts(opts)}")
  end

  def function_no_type do
    "Can't typecheck this function"
  end

  def invalid_function_capture(meta, fun_capture) do
    with_meta(meta, "Unsupported function reference syntax: #{inspect(fun_capture)}")
  end

  def function_altered_state(_a, _b, _c), do: "function_altered_state"

  def insufficient_function_clauses do
    "Not enough function clauses to support the annotated session type"
  end

  def wrong_number_of_clauses do
    "Incompatible number of clauses defined for on_link/2"
  end

  def wrong_number_of_specs do
    "Incompatible number of @spec's defined for on_link/2"
  end
end