defmodule Maty.Types do
  @moduledoc """
  Custom types used in Maty.
  """

  @type session_id :: reference()
  @type init_token :: reference()
  @type role :: atom()

  @type session :: %{
          id: session_id(),
          handlers: %{role() => {function(), role()}},
          participants: %{role() => pid()},
          local_state: any()
        }

  @type session_ctx :: {session(), role()}

  @type maty_actor_state :: %{
          sessions: %{session_id() => session()},
          callbacks: %{init_token() => {role(), function()}}
        }

  @type access_point_state :: %{
          participants: %{role() => :queue.queue({pid(), init_token()})}
        }

  @type suspend :: {:suspend, atom(), maty_actor_state()}
  @type done :: {:done, :unit, maty_actor_state()}

  @maty_types [
    :session_id,
    :init_token,
    :role,
    :session,
    :session_ctx,
    :maty_actor_state,
    :suspend,
    :done
  ]

  def get do
    @maty_types
  end

  def map do
    session_id = :ref
    init_token = :ref
    role = :atom

    session =
      {:map,
       %{
         id: session_id,
         handlers: {:map, %{role => {:tuple, [:function, role]}}},
         participants: {:map, %{role => :pid}},
         local_state: :any
       }}

    maty_actor_state =
      {:map,
       %{
         sessions: {:map, %{session_id => session}},
         callbacks: {:map, %{init_token => {:tuple, [role, :function]}}}
       }}

    %{
      session_id: session_id,
      init_token: init_token,
      role: role,
      session: session,
      session_ctx: {session, role},
      maty_actor_state: maty_actor_state,
      suspend: {:tuple, [:atom, {:tuple, [:function, role]}, maty_actor_state]},
      done: {:tuple, [:atom, :atom, maty_actor_state]}
    }
  end

  # List of accepted types in session types
  @supported_payload_types [
    :any,
    :atom,
    :binary,
    :boolean,
    :date,
    :function,
    :number,
    :pid,
    :ref,
    :string,
    :no_return,
    nil
  ]

  @doc """
  Returns a list of all accepted types, including :number, :atom, ...
  """
  @spec payload_types :: [atom]
  def payload_types() do
    @supported_payload_types
  end

  defmodule T do
    def session_id, do: :ref
    def init_token, do: :ref
    def role, do: :atom

    def session,
      do:
        {:map,
         %{
           id: T.session_id(),
           handlers: {:map, %{T.role() => {:tuple, [:function, T.role()]}}},
           participants: {:map, %{T.role() => :pid}},
           local_state: :any
         }}

    def session_ctx, do: {:tuple, [T.session(), T.role()]}

    def maty_actor_state,
      do:
        {:map,
         %{
           sessions: {:map, %{T.session_id() => T.session()}},
           callbacks: {:map, %{T.init_token() => {:tuple, [T.role(), :function]}}}
         }}

    def suspend, do: {:tuple, [:atom, :atom, T.maty_actor_state()]}

    def done, do: {:tuple, [:atom, :atom, T.maty_actor_state()]}

    # ------------------------------------------------------------------

    def is?(:ref, :session_id), do: true
    def is?(:ref, :init_token), do: true

    def is?(:atom, :role), do: true

    def is?({:map, map}, :session) do
      has_all_keys? =
        Map.has_key?(map, :id) and Map.has_key?(map, :handlers) and
          Map.has_key?(map, :participants) and Map.has_key?(map, :local_state)

      cond do
        not has_all_keys? ->
          false

        true ->
          %{
            id: session_id,
            handlers: {:map, handler_map},
            participants: {:map, participant_map},
            local_state: :any
          } = map

          s_valid? = is?(session_id, :session_id)

          h_valid? =
            handler_map
            |> Map.to_list()
            |> Enum.all?(fn {k, v} -> is?(k, :role) and v == {:tuple, [:function, T.role()]} end)

          p_valid? =
            participant_map
            |> Map.to_list()
            |> Enum.all?(fn {k, v} -> is?(k, :role) and v == :pid end)

          s_valid? and h_valid? and p_valid?
      end
    end

    # standardise what 2-tuple types look like
    def is?({session, role}, :session_ctx), do: is?(session, :session) and is?(role, :role)

    def is?({:tuple, [session, role]}, :session_ctx),
      do: is?(session, :session) and is?(role, :role)

    def is?({:map, map}, :maty_actor_state) do
      has_all_keys? = Map.has_key?(map, :sessions) and Map.has_key?(map, :callbacks)

      cond do
        not has_all_keys? ->
          false

        true ->
          %{sessions: {:map, session_map}, callbacks: {:map, callback_map}} = map

          s_valid? =
            session_map
            |> Map.to_list()
            |> Enum.all?(fn {k, v} -> is?(k, :session_id) and is?(v, :session) end)

          c_valid? =
            callback_map
            |> Map.to_list()
            |> Enum.all?(fn {k, v} ->
              is?(k, :init_token) and v == {:tuple, [T.role(), :function]}
            end)

          s_valid? and c_valid?
      end
    end

    def is?({:tuple, [:atom, :atom, state]}, :suspend),
      do: is?(state, :maty_actor_state)

    def is?({:tuple, [:atom, :atom, state]}, :done), do: is?(state, :maty_actor_state)

    def is?(_, _), do: false

    def is_handler_return?({:|, [v1, v2]}),
      do: (is?(v1, :done) and is?(v2, :suspend)) or (is?(v1, :suspend) and is?(v2, :done))

    def is_handler_return?(val), do: is?(val, :done) or is?(val, :suspend)
  end
end
