defmodule Maty.DSL.State do
  defstruct [:sessions, :callbacks]

  def new do
    %Maty.DSL.State{sessions: %{}, callbacks: %{}}
  end

  def set(state, _something) do
    # todo: add something to session state
    state
  end

  def get(_state) do
    # todo: get this session state
    nil
  end
end
