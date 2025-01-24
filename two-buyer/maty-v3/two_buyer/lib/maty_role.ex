defmodule TwoBuyerMaty3.MatyRole do
  @moduledoc """
  A custom behavior for Maty-compliant roles.

  The implementing module must define:
    - `init_role(session) :: {:ok, state}`
    - `handle_message(msg, state) :: {:suspend, atom, new_state} | {:stop, reason, new_state} | {:noreply, new_state}`
  """

  @callback init_role(session :: any()) :: {:ok, any()}

  @callback setup(data :: any(), state :: any()) ::
              {:stop, reason :: term(), any()}
              | {:noreply, any()}

  @callback handle_message(msg :: any(), state :: any()) ::
              {:suspend, atom(), any()}
              | {:stop, reason :: term(), any()}
              | {:noreply, any()}

  defmacro __using__(_opts) do
    quote do
      @behaviour MatyRole

      # default no-op for setup
      @impl true
      def setup(_data, state), do: {:noreply, state}

      defoverridable setup: 2
    end
  end

  @doc """
  Spawns a new process that runs the MatyRole's message loop.
  """
  def start_link(module, session) do
    {:ok, pid} = Task.start_link(fn -> init_and_run(module, session) end)
    {:ok, pid}
  end

  defp init_and_run(module, session) do
    {:ok, state} = module.init_role(session)

    case module.setup(nil, state) do
      {:stop, _reason, _new_state} ->
        :ok

      {:noreply, _new_state} ->
        loop(module, state)
    end
  end

  defp loop(module, state) do
    receive do
      msg ->
        case module.handle_message(msg, state) do
          {:suspend, _handler_name, new_state} ->
            loop(module, new_state)

          {:stop, _reason, _new_state} ->
            :ok

          {:noreply, new_state} ->
            loop(module, new_state)
        end
    end
  end
end
