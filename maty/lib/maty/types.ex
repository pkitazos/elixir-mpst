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

  @type suspend :: {:suspend, {function(), role()}, maty_actor_state()}
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
    get() |> Enum.map(&{&1, &1}) |> Enum.into(%{})
  end

  defmodule T do
    def session_id, do: :reference
    def init_token, do: :reference
    def role, do: :atom

    def session,
      do:
        {:map,
         %{
           id: T.session_id(),
           handlers: {:map, %{role: {:function, :role}}},
           participants: {:map, %{role: :pid}},
           local_state: :any
         }}

    def session_ctx, do: {T.session(), T.role()}

    def maty_actor_state,
      do:
        {:map,
         %{
           sessions: {:map, %{T.session_id() => T.session()}},
           callbacks: {:map, %{T.init_token() => {T.role(), :function}}}
         }}

    def to_maty_actor_state(val) do
      _session_id = T.session_id()
      _init_token = T.init_token()
      _role = T.role()
      _session = T.session()

      case val do
        {:map,
         %{
           sessions: {:map, %{:session_id => :session}},
           callbacks: {:map, %{:init_token => {:role, :function}}}
         }} ->
          :maty_actor_state

        # {:map,
        #  %{
        #    sessions: {:map, %{^session_id => session}},
        #    callbacks: {:map, %{^init_token => {:function, role}}}
        #  }} ->
        #   :maty_actor_state

        _ ->
          val
      end
    end

    def suspend, do: {:tuple, [:atom, {:tuple, [:function, T.role()]}, T.maty_actor_state()]}

    def to_suspend(val) do
      case val do
        {:tuple, [:atom, {:tuple, [:function, :atom]}, :maty_actor_state]} -> :suspend
        _ -> val
      end
    end

    def done, do: {:tuple, [:atom, :atom, T.maty_actor_state()]}
  end

  def map(:v2) do
    session_id = :reference
    init_token = :reference
    role = :atom

    session =
      {:map,
       %{
         id: session_id,
         handlers: {:map, %{role => {:function, role}}},
         participants: {:map, %{role => :pid}},
         local_state: :any
       }}

    maty_actor_state =
      {:map,
       %{
         sessions: {:map, %{session_id => session}},
         callbacks: {:map, %{init_token => {role, :function}}}
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
    :reference,
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
end
