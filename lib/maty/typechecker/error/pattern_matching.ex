defmodule Maty.Typechecker.Error.PatternMatching do
  def conflicting_pattern_bindings(_1), do: "conflicting_pattern_bindings"

  def pattern_type_mismatch(_a, opts),
    do: "pattern type mismatch: #{inspect(opts)}"

  def pattern_arity_mismatch(_a, _b, _c), do: "pattern_arity_mismatch"

  def complex_map_key(_a, _b), do: "complex_map_key"

  def invalid_map_key_type(_a, _b), do: "invalid_map_key_type"

  def pattern_map_key_not_found(_a, _b), do: "pattern_map_key_not_found"

  def pattern_map_key_not_atom(_a, _b), do: "pattern_map_key_not_atom"
end
