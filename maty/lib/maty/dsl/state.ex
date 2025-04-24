defmodule Maty.DSL.State do
  alias Maty.Types

  defstruct [:sessions, :callbacks]

  def new do
    %Maty.DSL.State{sessions: %{}, callbacks: %{}}
  end

  def set(state, local_state, {session, _}) do
    put_in(state, [:sessions, session.id, :local_state], local_state)
  end

  defmacro set(state, local_state) do
    quote do
      Maty.DSL.State.set(unquote(state), unquote(local_state), var!(session_ctx))
    end
  end

  @spec get(Types.maty_actor_state()) :: map()
  def get(state, {session, _}) do
    get_in(state, [:sessions, session.id, :local_state])
  end

  defmacro get(state) do
    quote do
      Maty.DSL.State.get(unquote(state), var!(session_ctx))
    end
  end
end
