defmodule Maty.Actor do
  alias Maty.{Types, Utils}

  @callback init_actor(args :: any()) :: {:ok, Types.maty_actor_state()}

  defmacro __using__(_opts) do
    quote do
      use Maty.Hook
      use Maty.Macros

      @behaviour Maty.Actor

      @type role :: Maty.Types.role()
      @type session_id :: Maty.Types.session_id()
      @type init_token :: Maty.Types.init_token()
      @type session :: Maty.Types.session()
      @type session_ctx :: Maty.Types.session_ctx()
      @type maty_actor_state :: Maty.Types.maty_actor_state()
      @type suspend :: Maty.Types.suspend()
      @type done :: Maty.Types.done()

      def start_link(args) do
        Maty.Actor.start_link(__MODULE__, args)
      end

      @spec maty_send({session(), role()}, role(), {atom(), any()}) :: no_return()
      def maty_send({session, from}, to, msg) do
        send(session.participants[to], {:maty_message, session.id, to, from, msg})
      end

      @spec register(pid(), role(), function(), maty_actor_state()) :: {:ok, maty_actor_state()}
      def register(ap_pid, role, callback, state) do
        init_token = make_ref()
        send(ap_pid, {:register, role, self(), init_token})

        updated_state = put_in(state, [:callbacks, init_token], {role, callback})
        {:ok, updated_state}
      end
    end
  end

  # ------------------------------------------------------------------

  @spec start_link(module(), any()) :: {:ok, pid()}
  def start_link(module, args) do
    pid = spawn_link(fn -> init_and_run(module, args) end)

    {:ok, pid}
  end

  @spec init_and_run(module(), any()) :: no_return()
  defp init_and_run(module, args) do
    {:ok, actor_state} = module.init_actor(args)

    # could also pass in the module environments into the loop function here
    loop(module, actor_state)
  end

  @spec loop(module(), Types.maty_actor_state()) :: no_return()
  defp loop(module, actor_state) do
    receive do
      {:maty_message, session_id, to, from, msg} ->
        # to    -> my role
        # from  -> recipient role
        session = actor_state.sessions[session_id]

        handler_label = session.handlers[to]

        # allow handlers to suspend with a label
        # then we grab the correct function reference from the module handler environment
        handler_info = Utils.Env.get_map(module, :delta) |> Map.fetch!(handler_label)
        {handler, expected_role} = handler_info.function

        if from == expected_role do
          {action, next, intermediate_state} = handler.(msg, from, {session, to}, actor_state)

          updated_actor_state =
            case action do
              :suspend ->
                put_in(intermediate_state, [:sessions, session.id, :handlers, to], next)

              :done ->
                update_in(intermediate_state, [:sessions], &Map.delete(&1, session.id))
            end

          loop(module, updated_actor_state)
        else
          send(self(), {:maty_message, session.id, to, msg})
          loop(module, actor_state)
        end

      {:init_session, session_id, participants, init_token} ->
        partial_session = %{
          id: session_id,
          participants: participants,
          handlers: %{},
          local_state: %{}
        }

        initial_actor_state = put_in(actor_state, [:sessions, session_id], partial_session)

        {role, callback} = initial_actor_state.callbacks[init_token]

        {:suspend, handler_info, intermediate_state} =
          callback.({partial_session, role}, initial_actor_state)

        updated_actor_state =
          put_in(intermediate_state, [:sessions, session_id, :handlers, role], handler_info)

        loop(module, updated_actor_state)
    end
  end
end
