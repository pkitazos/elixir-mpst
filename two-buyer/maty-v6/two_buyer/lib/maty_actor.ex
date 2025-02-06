defmodule MV6.MatyActor do
  alias MV6.AccessPoint

  @type session_id :: String.t()
  @type session_context :: %{any() => pid()}
  @type session_info :: %{
          id: session_id(),
          next_handler: function(),
          participants: session_context(),
          local_state: any()
        }
  @type actor_state :: %{
          sessions: %{session_id() => session_info :: session_info()},
          ap_pid: pid(),
          role: atom()
        }

  @callback init_actor(args :: any()) :: {:ok, actor_state :: any()}

  @callback init_session(session_id :: String.t(), actor_state :: any()) ::
              {session_info :: any(), new_actor_state :: any()}

  defmacro __using__(_opts) do
    quote do
      @behaviour MV6.MatyActor

      def start_link(args) do
        MV6.MatyActor.start_link(__MODULE__, args)
      end

      def maty_send(from, to, session_id, msg),
        do: send(to, {:maty_message, session_id, msg, from})

      def register(participant_pid, session_id) do
        send(participant_pid, {:register, session_id, participant_pid, self()})

        receive do
          {:ok, ^session_id} -> :ok
        end
      end

      def fetch_session(participant_pid, session_id) do
        send(participant_pid, {:fetch_session, session_id, participant_pid, self()})

        receive do
          {:ok, ^session_id} -> :ok
          {:error, reason} -> {:error, reason}
        end
      end
    end
  end

  # ------------------------------------------------------------------

  @doc """
  Starts a new MatyActor process using the given module.
  Calls `module.init_actor(args)` to get initial state,
  then enters a receive loop.
  """
  def start_link(module, args) do
    pid = spawn_link(fn -> init_and_run(module, args) end)
    {:ok, pid}
  end

  defp init_and_run(module, args) do
    {:ok, actor_state} = module.init_actor(args)

    loop(module, actor_state)
  end

  defp loop(module, %{ap_pid: ap, role: role} = actor_state) do
    receive do
      {:maty_message, session_id, msg, from_pid} ->
        with {:ok, session_info} <- get_session(session_id, actor_state) do
          # IO.puts("[DEBUG] ---- msg: #{inspect(msg)} \n\n")
          # IO.puts("[DEBUG] ---- from_pid: #{inspect(from_pid)} \n\n")
          # IO.puts("[DEBUG] ---- session_info: #{inspect(session_info)} \n\n")
          # IO.puts("[DEBUG] ---- actor_state: #{inspect(actor_state)} \n\n")
          # IO.puts("[DEBUG] ---- buyer1: #{inspect(session_info.participants.buyer1)} \n\n")

          {action, next_handler_fun, new_actor_state} =
            session_info.next_handler
            |> apply([msg, from_pid, session_info, actor_state])

          # IO.puts("[MatyActor] applied #{inspect(session_info.next_handler)}")
          # IO.puts("[MatyActor] action: #{inspect(action)}")
          # IO.puts("[MatyActor] next_handler_fun: #{inspect(next_handler_fun)}")

          updated_actor_state =
            handle_action(action, next_handler_fun, session_id, new_actor_state)

          loop(module, updated_actor_state)
        else
          _ ->
            # IO.puts("[MatyActor] Session #{session_id} not found")
            loop(module, actor_state)
        end

      {:register, session_id, caller, main} ->
        AccessPoint.register(ap, session_id, role, caller, main)
        loop(module, actor_state)

      {:fetch_session, session_id, caller, main} ->
        AccessPoint.fetch_session(ap, session_id, caller, main)
        loop(module, actor_state)

      # assumes that no duplication of session_id will occur
      # this message arrives from the access point only
      {:session_participants, session_id, participants, ^ap} ->
        session_info = %{
          id: session_id,
          participants: participants,
          next_handler: nil,
          local_state: %{}
        }

        updated_actor_state = actor_state |> put_in([:sessions], %{session_id => session_info})

        # need to initialise the session_info here
        {_, new_actor_state} = module.init_session(session_id, updated_actor_state)

        loop(module, new_actor_state)
    end
  end

  defp handle_action(:suspend, next_fun, session_id, actor_state) do
    put_in(actor_state, [:sessions, session_id, :next_handler], next_fun)
  end

  defp handle_action(:done, :unit, session_id, actor_state) do
    update_in(actor_state, [:sessions], &Map.delete(&1, session_id))
  end

  defp handle_action(:continue, nil, _session_id, actor_state) do
    actor_state
  end

  defp get_session(session_id, actor_state), do: Map.fetch(actor_state.sessions, session_id)
end
