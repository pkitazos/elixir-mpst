defmodule Maty.Typechecker.CoreChecker do
  alias Maty.Typechecker.Error
  require Logger

  @moduledoc """
  Parts of this module were taken from ElixirST

  Operations related to expression typing.
  """
  # List of accepted types in session types
  @supported_payload_types [
    :any,
    :atom,
    :binary,
    :boolean,
    :date,
    :number,
    :pid,
    :string,
    :no_return,
    nil
  ]

  @doc """
  Returns a list of all accepted types, including :number, :atom, ...
  """
  @spec accepted_types :: [atom]
  def accepted_types() do
    @supported_payload_types
  end

  # Extended list of types accepted by Elixir
  @all_elixir_types [
    :any,
    :atom,
    :binary,
    :bitstring,
    :boolean,
    :exception,
    :float,
    :function,
    :integer,
    :list,
    :map,
    nil,
    :number,
    :pid,
    :port,
    :reference,
    :struct,
    :tuple,
    :string
  ]

  @doc false
  @spec all_types :: [atom]
  def all_types() do
    @all_elixir_types
  end

  @doc """
    Process types in @spec format and returns usable types.

    Accepts: any, atom, binary, boolean, nil, number, pid, string, no_return, list and tuple
    The type of variables is returned if the environment contains the corresponding type of variable.
  """
  @spec get_type(any) :: atom | {:list, any} | {:tuple, list} | list
  def get_type(type) do
    get_type(type, %{})
  end

  @spec get_type(any, %{}) :: atom | {:list, any} | {:tuple, list} | list
  def get_type(types, env) when is_list(types) do
    Enum.map(types, &get_type_internal(&1, env))
  end

  def get_type(types, env) do
    get_type_internal(types, env)
  end

  defp get_type_internal({type, _, _}, _env) when type in @supported_payload_types do
    type
  end

  defp get_type_internal({type, _, _}, _env) when type in [:integer, :float] do
    :number
  end

  # tuple with more than 3 elements
  defp get_type_internal({:{}, _, types}, env) do
    {:tuple, Enum.map(types, &get_type(&1, env))}
  end

  # allow Date.t() type
  defp get_type_internal({{:., _, [{:__aliases__, _, [:Date]}, :t]}, _, []}, _env), do: :date

  # Maty specific types
  # ! this is wrong I'm essentially saying that any variable named one of those 5 words
  # ! automatically gets that type
  # ! need to instead properly inspect the structure of each of these
  defp get_type_internal({:role, _, _}, _env), do: :atom
  defp get_type_internal({:function, _, _}, _env), do: :function
  defp get_type_internal({:session, _, _}, _env), do: :session
  defp get_type_internal({:session_ctx, _, _}, _env), do: :session_ctx
  defp get_type_internal({:maty_actor_state, _, _}, _env), do: :maty_actor_state

  # this case is for types that may be valid in Elixir but which we do not support
  defp get_type_internal({type, _, _}, _env) when type not in @supported_payload_types do
    error = Error.unsupported_spec_type()
    Logger.error(error)
    :error
  end

  defp get_type_internal([], _env), do: {:list, nil}
  defp get_type_internal([type], env), do: {:list, get_type(type, env)}

  defp get_type_internal(type, env) when is_tuple(type) do
    {:tuple, Enum.map(Tuple.to_list(type), &get_type(&1, env))}
  end

  defp get_type_internal(type, _env) when is_binary(type), do: :binary
  defp get_type_internal(type, _env) when is_boolean(type), do: :boolean
  defp get_type_internal(type, _env) when is_nil(type), do: nil
  defp get_type_internal(type, _env) when is_number(type), do: :number
  defp get_type_internal(type, _env) when is_pid(type), do: :pid

  # atoms are literal types
  defp get_type_internal(type, _env) when is_atom(type), do: type

  defp get_type_internal(_, _), do: :error

  # todo: possibly split/reserve this module for @spec typing only
end
