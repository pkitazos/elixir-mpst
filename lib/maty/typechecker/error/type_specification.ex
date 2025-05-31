defmodule Maty.Typechecker.Error.TypeSpecification do
  def invalid_session_type_annotation(handler) do
    "Problem with @st annotation for #{inspect(handler)}. Can't parse Session Type string"
  end

  def spec_args_not_well_typed(spec_name, args_types) do
    "Problem with @spec for #{spec_name}. Function args: \n\t#{inspect(args_types)} are not well typed"
  end

  def spec_return_not_well_typed(spec_name, return_type) do
    "Problem with @spec for #{spec_name}. Function return: \n\t#{inspect(return_type)} are not well typed"
  end

  def function_spec_info_mismatch(
        spec_name: spec_name,
        spec_arity: spec_arity,
        fn_name: fn_name,
        fn_arity: fn_arity
      ) do
    "Spec info: {#{spec_name}, #{spec_arity}} doesn't match function info: {#{fn_name}, #{fn_arity}}"
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

  def unknown_type_constructor(a) do
    "Unknown type constructor or type atom not found in type_env: #{a}"
  end

  def no_spec_for_function(type_specs) do
    "no spec for this function: #{inspect(type_specs)}"
  end
end
