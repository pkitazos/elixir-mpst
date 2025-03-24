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
  @spec payload_types :: [atom]
  def payload_types() do
    @supported_payload_types
  end
end
