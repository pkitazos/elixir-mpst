defmodule Maty.Typechecker.Error.PatternMatching do
  defp with_meta(meta, str) do
    meta = Keyword.take(meta, [:line, :column])
    "#{inspect(meta)} #{str}"
  end

  def tuple_size_mismatch do
    "Tuple size mismatch in pattern match"
  end

  def list_size_mismatch do
    "List size mismatch in pattern match"
  end

  def conflicting_pattern_bindings(_1), do: "conflicting_pattern_bindings"

  def pattern_type_mismatch(_a, opts),
    do: "pattern type mismatch: #{inspect(opts)}"

  def pattern_arity_mismatch(_a, _b, _c), do: "pattern_arity_mismatch"

  def complex_map_key(_a, _b), do: "complex_map_key"

  def invalid_map_key_type(_a, _b), do: "invalid_map_key_type"

  def pattern_map_key_not_found(_a, _b), do: "pattern_map_key_not_found"

  def pattern_map_key_not_atom(_a, _b), do: "pattern_map_key_not_atom"

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

  def handler_msg_pattern_invalid(_a, _b), do: "handler_msg_pattern_invalid"

  def unsupported_argument_shape(meta, shape) do
    with_meta(meta, "Some other shape was provided: #{inspect(shape)}")
  end

  def unsupported_lhs_assignment(meta, shape) do
    with_meta(meta, "Unsupported lhs in assignment, found: #{inspect(shape)}")
  end

  def payload_var_has_other_value(meta, type) do
    with_meta(
      meta,
      "Payload has unsupported shape, must be variable or tuple of variables: #{inspect(type)} won't do"
    )
  end
end
