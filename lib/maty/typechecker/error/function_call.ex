defmodule Maty.Typechecker.Error.FunctionCall do
  alias Maty.Utils

  defp render_type(type) when is_atom(type), do: ":#{type}"
  defp render_type(type), do: "#{inspect(type)}"

  # ok
  def function_not_exist(module, meta, func_id) do
    line = Keyword.fetch!(meta, :line)
    func_str = Utils.to_func(func_id)

    """
    \n\n** (ElixirMatyTypeError) Function Call Error: Function Does Not Exist
      Module: #{module}
      Line: #{line}
      --
      Function: #{func_str}
      --
      The function #{func_str} is not defined in this module.
    """
  end

  # ok
  def arity_mismatch(module, meta, func_id, expected: expected, got: got) do
    line = Keyword.fetch!(meta, :line)
    func_str = Utils.to_func(func_id)

    """
    \n\n** (ElixirMatyTypeError) Function Call Error: Arity Mismatch
      Module: #{module}
      Line: #{line}
      --
      Function: #{func_str}
      Expected arity: #{expected}
      Got arity: #{got}
      --
      Arity mismatch between function spec and function definition.
    """
  end

  # ok
  def no_matching_function_clause(module, meta, func_id, actual_arg_types) do
    line = Keyword.fetch!(meta, :line)
    func_str = Utils.to_func(func_id)

    formatted_args =
      actual_arg_types
      |> Enum.map(&render_type/1)
      |> Enum.join(", ")

    """
    \n\n** (ElixirMatyTypeError) Function Call Error: No Matching Function Clause
      Module: #{module}
      Line: #{line}
      --
      Function: #{func_str}
      Called with argument types: (#{formatted_args})
      --
      No function clause matches the provided argument types.
    """
  end

  # ok
  def function_altered_state(module, meta, func_id, final_state) do
    line = Keyword.fetch!(meta, :line)
    func_str = Utils.to_func(func_id)

    """
    \n\n** (ElixirMatyTypeError) Function Call Error: Function Altered Session State
      Module: #{module}
      Line: #{line}
      --
      Function: #{func_str}
      Final state: #{inspect(final_state)}
      --
      Function altered the session state when it should remain unchanged.
    """
  end

  # ok
  def wrong_number_of_clauses(module, func_id, expected: expected, got: got) do
    func_str = Utils.to_func(func_id)

    """
    \n\n** (ElixirMatyTypeError) Function Call Error: Wrong Number of Clauses
      Module: #{module}
      Function: #{func_str}
      --
      Expected clauses: #{expected}
      Got clauses: #{got}
      --
      Incompatible number of clauses defined for function.
    """
  end

  # ok
  def wrong_number_of_specs(module, func_id, expected: expected, got: got) do
    func_str = Utils.to_func(func_id)

    """
    \n\n** (ElixirMatyTypeError) Function Call Error: Wrong Number of Specs
      Module: #{module}
      Function: #{func_str}
      --
      Expected specs: #{expected}
      Got specs: #{got}
      --
      Incompatible number of @spec annotations defined for function.
    """
  end
end
