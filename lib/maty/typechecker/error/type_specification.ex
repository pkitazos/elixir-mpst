defmodule Maty.Typechecker.Error.TypeSpecification do
  alias Maty.Utils
  alias Maty.Typechecker.Error

  defp render_type_list(types) when is_list(types) do
    types
    |> Enum.map(&inspect/1)
    |> Enum.join(", ")
  end

  defp render_type_list(type), do: inspect(type)

  # external functions
  def invalid_session_type_annotation(module, meta, handler_label, %Error.Internal{
        title: title,
        opts: opts,
        message: message
      }) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Type Specification Error: Invalid Session Type Annotation
      Module: #{module}
      Line: #{line}
      --
      Handler: #{handler_label}
      Parse error: #{title}
      #{opts}
      --
      The @st annotation contains an invalid session type string that cannot be parsed.

      Details: #{message}
    """
  end

  def spec_return_not_well_typed(module, meta, spec_name, return_ast, %Error.Internal{
        title: title,
        opts: opts,
        message: message
      }) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Type Specification Error: Invalid Spec Return Type
      Module: #{module}
      Line: #{line}
      --
      Function: #{spec_name}
      Return type: #{inspect(return_ast)}
      Parse error: #{title}
      #{opts}
      --
      The return type specification is invalid.

      Details: #{message}
    """
  end

  def spec_args_parse_error_at(module, meta, func_id, failed_index, args_asts, %Error.Internal{
        title: title,
        opts: opts,
        message: message
      }) do
    line = Keyword.fetch!(meta, :line)
    func_str = Utils.to_func(func_id)

    """
    \n\n** (ElixirMatyTypeError) Type Specification Error: Invalid Spec Argument
      Module: #{module}
      Line: #{line}
      --
      Function: #{func_str}
      Argument types: #{render_type_list(args_asts)}
      Error at argument: ##{failed_index + 1}
      Parse error: #{title}
      #{opts}
      --
      Failed to parse type specification for function argument.

      Details: #{message}
    """
  end

  def function_spec_info_mismatch(module, meta, spec_id: spec_id, func_id: func_id) do
    line = Keyword.fetch!(meta, :line)

    spec_str = Utils.to_func(spec_id)
    func_str = Utils.to_func(func_id)

    """
    \n\n** (ElixirMatyTypeError) Type Specification Error: Function Spec Mismatch
      Module: #{module}
      Line: #{line}
      --
      @spec signature: #{spec_str}
      Function signature: #{func_str}
      --
      The @spec annotation does not match the function definition.
    """
  end

  def no_spec_for_function(module, func_id) do
    func_str = Utils.to_func(func_id)

    """
    \n\n** (ElixirMatyTypeError) Type Specification Error: Missing Function Spec
      Module: #{module}
      --
      Function: #{func_str}
      --
      No @spec annotation found for this function. All functions require type specifications.
    """
  end

  # internal functions

  def unsupported_type_constructor(type_ast) do
    %Error.Internal{
      title: "Unsupported Type Constructor",
      opts: "Type AST: #{inspect(type_ast)}",
      message: "This type specification AST structure is not supported by the typechecker."
    }
  end

  def unknown_type_constructor(type_name) do
    %Error.Internal{
      title: "Unknown Type Constructor",
      opts: "Type: #{type_name}",
      message: "Unknown type constructor or type atom not found in the type environment."
    }
  end

  def heterogeneous_list_error(conflicting_types) do
    formatted_types =
      conflicting_types
      |> Enum.map(&inspect/1)
      |> Enum.join(", ")

    %Error.Internal{
      title: "Heterogeneous List Type",
      opts: "Conflicting types found: #{formatted_types}",
      message: "List type specifications must contain elements of the same type."
    }
  end
end
