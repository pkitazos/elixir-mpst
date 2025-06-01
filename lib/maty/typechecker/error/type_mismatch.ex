defmodule Maty.Typechecker.Error.TypeMismatch do
  alias Maty.Typechecker.Error

  defp render_type(type) when is_atom(type), do: ":#{type}"
  defp render_type(type), do: "#{inspect(type)}"

  defp render_operator(op) when is_atom(op), do: "#{op}"
  defp render_operator(op), do: "#{inspect(op)}"

  def logical_operator_requires_boolean(module, meta, operator, operand_type) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Type Mismatch Error: Logical Operator Type Error
      Module: #{module}
      Line: #{line}
      --
      Operator: #{render_operator(operator)}
      Expected operand type: :boolean
      Got operand type: #{render_type(operand_type)}
      --
      Logical operators require boolean operands.
    """
  end

  def return_type_mismatch(module, meta, expected: expected, got: got) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Type Mismatch Error: Return Type Mismatch
      Module: #{module}
      Line: #{line}
      --
      Expected return type: #{render_type(expected)}
      Got return type: #{render_type(got)}
      --
      The function's actual return type does not match the declared return type.
    """
  end

  def binary_operator_type_mismatch(module, meta, operator, lhs_type, rhs_type) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Type Mismatch Error: Binary Operator Type Error
      Module: #{module}
      Line: #{line}
      --
      Operator: #{render_operator(operator)}
      Left operand type: #{render_type(lhs_type)}
      Right operand type: #{render_type(rhs_type)}
      --
      The operand types are not compatible with this binary operator.
    """
  end

  def logical_operator_type_mismatch(module, meta, operator, lhs_type, rhs_type) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Type Mismatch Error: Logical Operator Type Error
      Module: #{module}
      Line: #{line}
      --
      Operator: #{render_operator(operator)}
      Left operand type: #{render_type(lhs_type)}
      Right operand type: #{render_type(rhs_type)}
      Expected operand types: :boolean and :boolean
      --
      Logical operators require both operands to be boolean.
    """
  end

  def list_elements_incompatible(module, meta, element_types) do
    line = Keyword.fetch!(meta, :line)

    formatted_types =
      element_types
      |> Enum.map(&render_type/1)
      |> Enum.join(", ")

    """
    \n\n** (ElixirMatyTypeError) Type Mismatch Error: Incompatible List Elements
      Module: #{module}
      Line: #{line}
      --
      Element types found: #{formatted_types}
      --
      All elements in a list must have the same type.
    """
  end

  def case_branches_incompatible_types(module, meta, t1: type1, t2: type2) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Type Mismatch Error: Incompatible Case Branch Types
      Module: #{module}
      Line: #{line}
      --
      Branch type 1: #{render_type(type1)}
      Branch type 2: #{render_type(type2)}
      --
      All case branches must return the same type.
    """
  end

  def invalid_maty_state_type(module, meta, %Error.Internal{
        title: title,
        opts: opts,
        message: message
      }) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Type Mismatch Error: #{title}
      Module: #{module}
      Line: #{line}
      --
      #{opts}
      --
      #{message}
    """
  end

  def invalid_maty_state_type(got_type) do
    %Error.Internal{
      title: "Invalid Maty State Type",
      opts: "Expected type: :maty_actor_state\nGot type: #{render_type(got_type)}",
      message: "Maty operations require a valid actor state type."
    }
  end

  def send_message_not_tuple(module, meta, got: message_ast) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Type Mismatch Error: Send Message Not Tuple
      Module: #{module}
      Line: #{line}
      --
      Expected: Tagged tuple message {label, payload}
      Got: #{inspect(message_ast)}
      --
      Messages must be 2-tuples with an atom label and payload.
    """
  end

  def case_branches_incompatible_session_states(module, meta, q1: state1, q2: state2) do
    line = Keyword.fetch!(meta, :line)

    """
    \n\n** (ElixirMatyTypeError) Type Mismatch Error: Incompatible Case Branch Session States
      Module: #{module}
      Line: #{line}
      --
      Branch state 1: #{inspect(state1)}
      Branch state 2: #{inspect(state2)}
      --
      All case branches must result in compatible session states.
    """
  end
end
