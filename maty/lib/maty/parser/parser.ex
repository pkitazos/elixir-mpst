defmodule Maty.Parser do
  alias Maty.ST

  @moduledoc """
  User-friendly API for the Session Type Parser.

  This module provides convenient functions for parsing session type expressions
  into their corresponding ST data structures. It serves as the main entry point
  for using the parser in application code.

  ## Session Type Syntax

  The syntax supported by this parser includes:

  - Input (external choice): `&Role:{ Label1(PayloadType).Continuation, Label2(PayloadType).Continuation }`
  - Output (internal choice): `+Role:{ Label1(PayloadType).Continuation, Label2(PayloadType).Continuation }`
  - End (termination): `end`
  - Named handler reference: `handler_name`

  ### Payload Types

  The following payload types are supported:
  - Basic types: `string`, `number`, `boolean`, `nil`
  - List types: `type[]` (e.g., `string[]`, `number[]`)
  - Tuple types: `(type1, type2, ...)` (e.g., `(string, number)`, `(boolean[], nil)`)

  ### Example Session Type

  ```
  &Server:{
  GetData(string).+Server:{
    Data((string, number[])).end
  }
  }
  ```

  This session type describes a protocol where:
  1. The user sends a `GetData` message with a string payload to the server
  2. The server responds with a `Data` message containing a tuple of a string and a number array
  3. The session then terminates

  Or with named handlers:

  ```
  &Server:{
    GetData(string).+Server:{
      Data((string, number[])).handle_data
    }
  }
  ```
  """

  @typedoc """
  Result of a successful parse operation.
  """
  @type parse_success :: {:ok, ST.t()}

  @typedoc """
  Result of a failed parse operation containing an error message.
  """
  @type parse_error :: {:error, String.t()}

  @typedoc """
  Result of a parse operation, either success or failure.
  """
  @type parse_result :: parse_success() | parse_error()

  @typedoc """
  Result of a successful parse_type operation.
  """
  @type type_success :: {:ok, ST.payload_type()}

  @typedoc """
  Result of a parse_type operation, either success or failure.
  """
  @type type_result :: type_success() | parse_error()

  @doc """
  Parses a complete session type expression.

  Returns `{:ok, parsed_type}` on success or `{:error, reason}` on failure.

  ## Parameters
  - `input`: String containing the session type expression to parse

  ## Examples

      iex> ST.Parser.parse("&Server:{ Ack(nil).end }")
      {:ok, %ST.SIn{
        from: :server,
        branches: [
          %ST.SBranch{
            label: :ack,
            payload: :nil,
            continue_as: %ST.SEnd{}
          }
        ]
      }}

      iex> ST.Parser.parse("+Client:{ Request(string).end }")
      {:ok, %ST.SOut{
        to: :client,
        branches: [
          %ST.SBranch{
            label: :request,
            payload: :binary,
            continue_as: %ST.SEnd{}
          }
        ]
      }}

      iex> ST.Parser.parse("end")
      {:ok, %ST.SEnd{}}
  """
  @spec parse(String.t()) :: parse_result()
  def parse(input) when is_binary(input) do
    normalised_input = String.replace(input, ~r/\s+/, "")

    case Maty.Parser.Core.parse_session_type(normalised_input) do
      {:ok, [result], "", _, _, _} ->
        {:ok, result}

      {:ok, _, rest, _, _, _} ->
        {:error, "Could not parse entire input. Stopped at: #{inspect(rest)}"}

      {:error, reason, rest, _, _, _} ->
        {:error, "Parser error: #{inspect(reason)}. At: #{inspect(rest)}"}
    end
  end

  @doc """
  Parses a session type expression, raising an exception on failure.

  Returns the parsed type directly on success.

  ## Parameters
  - `input`: String containing the session type expression to parse

  ## Examples

      iex> ST.Parser.parse!("end")
      %ST.SEnd{}

      iex> ST.Parser.parse!("invalid")
      ** (RuntimeError) Parser error: ...

  ## Raises
  - `RuntimeError`: When parsing fails for any reason
  """
  @spec parse!(String.t()) :: ST.t() | no_return()
  def parse!(input) when is_binary(input) do
    case parse(input) do
      {:ok, result} -> result
      {:error, reason} -> raise "Parse error: #{reason}"
    end
  end

  @doc """
  Parses a payload type expression.

  Returns `{:ok, parsed_type}` on success or `{:error, reason}` on failure.

  ## Parameters
  - `input`: String containing the payload type expression to parse

  ## Examples

      iex> ST.Parser.parse_type("string")
      {:ok, :binary}

      iex> ST.Parser.parse_type("boolean[]")
      {:ok, {:list, [:boolean]}}

      iex> ST.Parser.parse_type("(string, number)")
      {:ok, {:tuple, [:binary, :number]}}
  """
  @spec parse_type(String.t()) :: type_result()
  def parse_type(input) when is_binary(input) do
    normalized_input = String.replace(input, ~r/\s+/, "")

    case Maty.Parser.Core.payload_type(normalized_input) do
      {:ok, [result], "", _, _, _} ->
        {:ok, result}

      {:ok, _, rest, _, _, _} ->
        {:error, "Could not parse entire type. Stopped at: #{inspect(rest)}"}

      {:error, reason, rest, _, _, _} ->
        {:error, "Type parser error: #{inspect(reason)}. At: #{inspect(rest)}"}
    end
  end

  @doc """
  Parses a payload type expression, raising an exception on failure.

  Returns the parsed type directly on success.

  ## Parameters
  - `input`: String containing the payload type expression to parse

  ## Examples

      iex> ST.Parser.parse_type!("string")
      :binary

      iex> ST.Parser.parse_type!("invalid_type")
      ** (RuntimeError) Type parser error: ...

  ## Raises
  - `RuntimeError`: When parsing fails for any reason
  """
  @spec parse_type!(String.t()) :: ST.payload_type() | no_return()
  def parse_type!(input) when is_binary(input) do
    case parse_type(input) do
      {:ok, result} -> result
      {:error, reason} -> raise "Type parse error: #{reason}"
    end
  end
end
