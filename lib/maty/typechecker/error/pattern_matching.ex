defmodule Maty.Typechecker.Error.PatternMatching do
  def conflicting_pattern_bindings(module, meta, conflicting_vars) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Pattern Matching Error: Conflicting Pattern Bindings
      Module: #{module}
      Line: #{line}
      --
      Conflicting variables: #{conflicting_vars}
      --
      The same variable is bound multiple times in this pattern, which is not allowed.
    """
  end

  def pattern_type_mismatch(module, meta, pattern: pattern, expected: expected, got: got) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Pattern Matching Error: Pattern Type Mismatch
      Module: #{module}
      Line: #{line}
      --
      Pattern: #{render_pattern(pattern)}
      Expected type: #{render_type(expected)}
      Got type: #{render_type(got)}
      --
      The pattern does not match the expected type.
    """
  end

  def pattern_arity_mismatch(module, meta, pattern: pattern_type, expected: expected, got: got) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Pattern Matching Error: Pattern Arity Mismatch
      Module: #{module}
      Line: #{line}
      --
      Pattern type: #{pattern_type}
      Expected arity: #{expected}
      Got arity: #{got}
      --
      The pattern has a different number of elements than expected.
    """
  end

  def tuple_arity_mismatch(module, meta, pattern_arity: pattern_arity, expected: expected) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Pattern Matching Error: Tuple Arity Mismatch
      Module: #{module}
      Line: #{line}
      --
      Pattern: #{pattern_arity}-tuple
      Expected: #{expected}-tuple
      --
      The tuple pattern has #{pattern_arity} elements but expected #{expected} elements.
    """
  end

  def pattern_not_tuple(module, meta, got: got_type) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Pattern Matching Error: Pattern Type Mismatch
      Module: #{module}
      Line: #{line}
      --
      Pattern: 2-tuple
      Expected type: tuple
      Got type: #{render_type(got_type)}
      --
      Expected a tuple but got a different type.
    """
  end

  def complex_map_key(module, meta, key_ast) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Pattern Matching Error: Complex Map Key
      Module: #{module}
      Line: #{line}
      --
      Key expression: #{inspect(key_ast)}
      --
      Map patterns require literal atom keys. Complex expressions are not allowed as map keys.
    """
  end

  def invalid_map_key_type(module, meta, expected: expected, got: got) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Pattern Matching Error: Invalid Map Key Type
      Module: #{module}
      Line: #{line}
      --
      Expected key type: #{render_type(expected)}
      Got key type: #{render_type(got)}
      --
      Map pattern keys must be atoms.
    """
  end

  def pattern_map_key_not_found(module, meta, missing_key) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Pattern Matching Error: Map Key Not Found
      Module: #{module}
      Line: #{line}
      --
      Missing key: #{render_pattern(missing_key)}
      --
      The pattern references a map key that is not present in the expected map type.
    """
  end

  def pattern_map_key_not_atom(module, meta, key_ast) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Pattern Matching Error: Map Key Not Atom
      Module: #{module}
      Line: #{line}
      --
      Key expression: #{inspect(key_ast)}
      --
      Map pattern keys must be literal atoms.
    """
  end

  defp render_pattern(pattern) when is_atom(pattern), do: ":#{pattern}"
  defp render_pattern(pattern) when is_binary(pattern), do: "\"#{pattern}\""
  defp render_pattern(pattern) when is_number(pattern), do: "#{pattern}"
  defp render_pattern(pattern) when is_boolean(pattern), do: "#{pattern}"
  defp render_pattern(nil), do: "nil"
  defp render_pattern(pattern), do: "#{inspect(pattern)}"

  defp render_type(type) when is_atom(type), do: ":#{type}"
  defp render_type(type), do: "#{inspect(type)}"
end
