defmodule Maty.MatyDSL do
  alias Maty.MatyDSL
  alias Maty.Types

  @spec register(pid(), Types.role(), function(), Types.maty_actor_state()) ::
          {atom(), Types.maty_actor_state()}
  def register(ap_pid, role, callback, state) do
    init_token = make_ref()
    Kernel.send(ap_pid, {:register, role, self(), init_token})

    updated_state = put_in(state, [:callbacks, init_token], {role, callback})
    {:ok, updated_state}
  end

  # todo: could possibly make this redundant
  @spec internal_send({Types.session(), Types.role()}, Types.role(), {atom(), any()}) :: atom()
  def internal_send({session, from}, to, msg) do
    Kernel.send(session.participants[to], {:maty_message, session.id, to, from, msg})
    :ok
  end

  defmacro send(recipient, message) do
    quote do
      MatyDSL.internal_send(var!(session_ctx), unquote(recipient), unquote(message))
    end
  end

  defmacro suspend(next_handler, state) do
    quote do
      throw({:suspend, unquote(next_handler), unquote(state)})
    end
  end

  defmacro done(state) do
    quote do
      throw({:done, unquote(state)})
    end
  end

  # todo: consider renaming or changing api for consistency
  defmacro init_callback(handler_name, args) do
    quote do
      fn session_ctx, state ->
        unquote(handler_name)(unquote(args), session_ctx, state)
      end
    end
  end

  defmodule Maty.MatyDSL.State do
    defstruct [:sessions, :callbacks]

    def new do
      %State{sessions: %{}, callbacks: %{}}
    end

    def set(state, _something) do
      # todo: add something to session state
      state
    end

    def get(state) do
      # todo: get this session state
      nil
    end
  end
end
