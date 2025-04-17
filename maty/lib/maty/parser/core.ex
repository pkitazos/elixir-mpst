defmodule Maty.Parser.Core do
  @moduledoc """
  Low-level implementation of the Session Type Parser.

  This module contains the NimbleParsec-based parser definitions for session types.
  Most users should use the higher-level API in the `ST.Parser` module rather
  than directly using the functions in this module.

  The parser handles:
  - Basic payload types (string, number, boolean, unit)
  - Complex payload types (lists, tuples)
  - Session type constructs (input, output, end)
  - Branch definitions with continuations
  """
  import NimbleParsec
  alias Maty.ST

  @doc false
  # Identifiers - converts CamelCase to snake_case atoms
  identifier =
    ascii_char([?A..?Z, ?a..?z, ?_])
    |> repeat(ascii_char([?A..?Z, ?a..?z, ?0..?9, ?_]))
    |> reduce({List, :to_string, []})
    |> map({Macro, :underscore, []})
    |> map({String, :to_atom, []})

  # Basic types with their Elixir representations
  binary_type = string("binary") |> replace(:binary)
  string_type = string("string") |> replace(:binary)
  number_type = string("number") |> replace(:number)
  date_type = string("date") |> replace(:date)
  unit_type = string("unit") |> replace(:unit)
  boolean_type = string("boolean") |> replace(:boolean)

  # Basic payload type - matches one of the basic types
  basic_payload_type =
    choice([
      binary_type,
      string_type,
      number_type,
      date_type,
      unit_type,
      boolean_type
    ])

  # List type (e.g., string[], number[])
  list_type =
    basic_payload_type
    |> ignore(string("[]"))
    |> map({__MODULE__, :wrap_list_type, []})

  # Whitespace parser - matches space, tab, newline
  whitespace =
    ascii_char([?\s, ?\t, ?\n, ?\r])
    |> repeat()
    |> ignore()

  # Name terminal - matches identifiers as handler names
  name_type =
    identifier
    |> post_traverse({__MODULE__, :wrap_name, []})

  # End terminal
  end_type =
    string("end")
    |> replace(%ST.SEnd{})

  defcombinatorp(
    :payload_type_inner,
    choice([
      parsec(:tuple_inner),
      list_type,
      basic_payload_type
    ])
  )

  defcombinatorp(
    :continuation,
    choice([
      parsec(:session_type),
      name_type
    ])
  )

  defcombinatorp(
    :tuple_inner,
    ignore(string("("))
    |> concat(whitespace)
    |> concat(parsec(:payload_type_inner))
    |> repeat(
      ignore(string(","))
      |> concat(whitespace)
      |> concat(parsec(:payload_type_inner))
    )
    |> concat(whitespace)
    |> ignore(string(")"))
    |> post_traverse({__MODULE__, :wrap_tuple_type, []})
  )

  defcombinatorp(
    :branch_without_cont,
    # Label
    identifier
    |> ignore(string("("))
    |> concat(whitespace)
    # PayloadType
    |> concat(parsec(:payload_type_inner))
    |> concat(whitespace)
    |> ignore(string(")"))
    # Period separator
    |> ignore(string("."))
  )

  # Branch list (comma-separated branches)
  defcombinatorp(
    :branch_list_inner,
    parsec(:branch)
    |> repeat(
      ignore(string(","))
      |> concat(whitespace)
      |> concat(parsec(:branch))
    )
  )

  # Input type without finishing the branch parsing yet
  defcombinatorp(
    :input_inner,
    ignore(string("&"))
    # Role
    |> concat(identifier)
    |> ignore(string(":"))
    |> ignore(string("{"))
    |> concat(whitespace)
    # Branches
    |> concat(parsec(:branch_list))
    |> concat(whitespace)
    |> ignore(string("}"))
    |> post_traverse({__MODULE__, :wrap_input, []})
  )

  # Output type without finishing the branch parsing yet
  defcombinatorp(
    :output_inner,
    ignore(string("+"))
    # Role
    |> concat(identifier)
    |> ignore(string(":"))
    |> ignore(string("{"))
    |> concat(whitespace)
    # Branches
    |> concat(parsec(:branch_list))
    |> concat(whitespace)
    |> ignore(string("}"))
    |> post_traverse({__MODULE__, :wrap_output, []})
  )

  defcombinatorp(
    :session_type_inner,
    choice([
      end_type,
      name_type,
      parsec(:input),
      parsec(:output)
    ])
  )

  defcombinatorp(
    :branch,
    parsec(:branch_without_cont)
    |> concat(parsec(:continuation))
    |> post_traverse({__MODULE__, :wrap_branch, []})
  )

  defparsecp(:branch_list, parsec(:branch_list_inner))
  defparsecp(:input, parsec(:input_inner))
  defparsecp(:output, parsec(:output_inner))
  defparsecp(:session_type, parsec(:session_type_inner))

  @doc """
  Helper function to wrap a type as a list type.

  ## Parameters
  - `type`: The element type of the list

  ## Returns
  - A tuple of the form `{:list, [type]}`
  """
  @spec wrap_list_type(atom()) :: {:list, [atom()]}
  def wrap_list_type(type) do
    {:list, [type]}
  end

  @doc """
  Helper function for parser post-processing to wrap tuple types.

  ## Parameters
  Standard NimbleParsec post-traverse callback parameters.

  ## Returns
  A tuple containing the rest of the input, a list with the
  tuple type, and the context.
  """
  def wrap_tuple_type(rest, args, context, _line, _offset) do
    # Fix the order of elements (they come in reverse)
    types = Enum.reverse(args)
    {rest, [{:tuple, types}], context}
  end

  @doc """
  Helper function for parser post-processing to wrap branch structures.

  ## Parameters
  Standard NimbleParsec post-traverse callback parameters.

  ## Returns
  A tuple containing the rest of the input, a list with the
  branch structure, and the context.
  """
  def wrap_branch(rest, [session_type, payload_type, label], context, _line, _offset) do
    branch = %ST.SBranch{
      label: label,
      payload: payload_type,
      continue_as: session_type
    }

    {rest, [branch], context}
  end

  @doc """
  Helper function for parser post-processing to wrap input structures.

  ## Parameters
  Standard NimbleParsec post-traverse callback parameters.

  ## Returns
  A tuple containing the rest of the input, a list with the
  input structure, and the context.
  """
  def wrap_input(rest, branches_and_role, context, _line, _offset) do
    # Last element is the role, everything else are branches
    {branches, [role]} = Enum.split(branches_and_role, length(branches_and_role) - 1)

    input = %ST.SIn{
      from: role,
      branches: branches
    }

    {rest, [input], context}
  end

  @doc """
  Helper function for parser post-processing to wrap output structures.

  ## Parameters
  Standard NimbleParsec post-traverse callback parameters.

  ## Returns
  A tuple containing the rest of the input, a list with the
  output structure, and the context.
  """
  def wrap_output(rest, branches_and_role, context, _line, _offset) do
    # Last element is the role, everything else are branches
    {branches, [role]} = Enum.split(branches_and_role, length(branches_and_role) - 1)

    output = %ST.SOut{
      to: role,
      branches: branches
    }

    {rest, [output], context}
  end

  @doc """
  Helper function for parser post-processing to wrap name structures.

  ## Parameters
  Standard NimbleParsec post-traverse callback parameters.

  ## Returns
  A tuple containing the rest of the input, a list with the
  name structure, and the context.
  """
  def wrap_name(rest, [handler], context, _line, _offset) do
    name = %ST.SName{handler: handler}
    {rest, [name], context}
  end

  # Export the parsers with documentation

  @doc """
  Parses a complete session type expression.

  ## Parameters
  - `input`: String containing the session type expression

  ## Returns
  - A NimbleParsec result tuple
  """
  defparsec(:parse_session_type, parsec(:session_type))

  @doc """
  Parses a branch in a session type.

  ## Parameters
  - `input`: String containing the branch expression

  ## Returns
  - A NimbleParsec result tuple
  """
  defparsec(:parse_branch, parsec(:branch))

  @doc """
  Parses an input session type.

  ## Parameters
  - `input`: String containing the input expression

  ## Returns
  - A NimbleParsec result tuple
  """
  defparsec(:parse_input, parsec(:input))

  @doc """
  Parses an output session type.

  ## Parameters
  - `input`: String containing the output expression

  ## Returns
  - A NimbleParsec result tuple
  """
  defparsec(:parse_output, parsec(:output))

  @doc """
  Parses a named handler reference.

  ## Parameters
  - `input`: String containing the handler name

  ## Returns
  - A NimbleParsec result tuple
  """
  defparsec(:parse_name, name_type)

  @doc """
  Parses a payload type.

  ## Parameters
  - `input`: String containing the payload type expression

  ## Returns
  - A NimbleParsec result tuple
  """
  defparsec(:payload_type, parsec(:payload_type_inner))

  @doc """
  Parses a tuple type.

  ## Parameters
  - `input`: String containing the tuple expression

  ## Returns
  - A NimbleParsec result tuple
  """
  defparsec(:parse_tuple, parsec(:tuple_inner))

  @doc """
  Parses an identifier.

  ## Parameters
  - `input`: String containing the identifier

  ## Returns
  - A NimbleParsec result tuple
  """
  defparsec(:parse_identifier, identifier)

  @doc """
  Parses a basic type.

  ## Parameters
  - `input`: String containing the basic type expression

  ## Returns
  - A NimbleParsec result tuple
  """
  defparsec(:parse_basic_type, basic_payload_type)

  @doc """
  Parses an end session type.

  ## Parameters
  - `input`: String containing the end expression

  ## Returns
  - A NimbleParsec result tuple
  """
  defparsec(:parse_end, end_type)
end
