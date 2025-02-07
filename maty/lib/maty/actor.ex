defmodule Maty.Actor do
  @callback init_actor(args :: any()) :: {:ok, actor_state :: any()}

  defmacro __using__(_opts) do
    quote do
      @behaviour Maty.Actor

      def start_link(args) do
        Maty.Actor.start_link(__MODULE__, args)
      end

      def maty_send(from, to, session_id, msg) do
        send(to, {:maty_message, session_id, msg, from})
      end

      def register(ap_pid, role, callback, state) do
        init_token = make_ref()
        send(ap_pid, {:register, role, self(), init_token})

        updated_state = put_in(state, [:callbacks, init_token], {role, callback})
        {:ok, updated_state}
      end
    end
  end

  # ------------------------------------------------------------------

  def start_link(module, args) do
    pid = spawn_link(fn -> init_and_run(module, args) end)

    {:ok, pid}
  end

  defp init_and_run(module, args) do
    {:ok, actor_state} = module.init_actor(args)

    loop(module, actor_state)
  end

  defp loop(module, actor_state) do
    receive do
      {:maty_message, session_id, msg, from_pid} ->
        session_info = get_in(actor_state, [:sessions, session_id])

        {action, next_handler_fun, new_actor_state} =
          session_info.next_handler.(msg, from_pid, session_info, actor_state)

        updated_actor_state =
          handle_action(action, next_handler_fun, session_id, new_actor_state)

        loop(module, updated_actor_state)

      {:init_session, session_id, session_participants, init_token} ->
        partial_session_info = %{
          id: session_id,
          participants: session_participants,
          next_handler: nil,
          local_state: %{}
        }

        initial_actor_state = put_in(actor_state, [:sessions, session_id], partial_session_info)

        {_role, callback} = get_in(initial_actor_state, [:callbacks, init_token])

        {:suspend, initial_handler, intermediate_actor_state} =
          callback.(session_id, initial_actor_state)

        updated_actor_state =
          put_in(
            intermediate_actor_state,
            [:sessions, session_id, :next_handler],
            initial_handler
          )

        loop(module, updated_actor_state)
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
end

# TwoBuyer.Participants.Seller.install("#Reference<0.2589905085.2172387332.148561>", %{
#   callbacks: %{
#     "#Reference<0.2589905085.2172387332.148551>" =>
#       {:seller, &TwoBuyer.Participants.Seller.install/2}
#   },
#   sessions: %{
#     "#Reference<0.2589905085.2172387332.148561>" => %{
#       id: "#Reference<0.2589905085.2172387332.148561>",
#       next_handler: nil,
#       participants: %{seller: "#PID<0.169.0>", buyer1: "#PID<0.170.0>", buyer2: "#PID<0.171.0>"},
#       local_state: %{}
#     }
#   }
# })
