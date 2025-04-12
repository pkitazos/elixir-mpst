defmodule Maty.MatyDSL do
  @doc """
  Sends a message to another participant in the session.

  ## Parameters

  * `recipient` - The role to send the message to
  * `message` - The message to send (typically a tagged tuple)

  ## Examples

      Maty.send(:buyer1, {:quote, amount})
  """
  defmacro send(recipient, message) do
    quote do
      # pin - normally I would have to generate this variable name on the fly to avoid namespace collisions
      maty_send(session_ctx, unquote(recipient), unquote(message))
    end
  end

  @doc """
  Suspends the current handler and transfers control to another handler.

  ## Parameters

  * `next_handler` - The handler function to transfer control to
  * `state` - The current state

  ## Examples

      Maty.suspend(:decision_handler, state)
  """
  defmacro suspend(next_handler, state) do
    quote do
      throw({:suspend, unquote(next_handler), unquote(state)})
    end
  end

  @doc """
  Ends the current session.

  ## Parameters

  * `state` - The current state

  ## Examples

      Maty.end(state)
  """
  defmacro done(state) do
    quote do
      {:done, :ok, unquote(state)}
    end
  end
end
