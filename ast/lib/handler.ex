defmodule Handler do
  @moduledoc """
  Provides macros for defining message handlers with less boilerplate.

  This module introduces a `handler` macro that automatically generates
  a pair of function clauses: one for the specific message pattern and
  another catch-all that ignores invalid messages.
  """

  defmacro __using__(_opts) do
    quote do
      import Handler
    end
  end

  @doc """
  Defines a handler function with automatic handling of unmatched messages.

  ## Parameters

  - `name`: The name of the handler function (atom)
  - `sender_role`: The expected role of the sender (atom or pattern)
  - `pattern`: The message pattern to match
  - `session_var`: Variable name to bind the session to
  - `state_var`: Variable name to bind the state to
  - `body`: The function body to execute when the pattern matches

  ## Examples

      handler :quote_handler, :seller, {:quote, amount}, session, state do
        # Handler implementation
        {:done, :unit, state}
      end

  This expands to:

      def quote_handler({:quote, amount}, :seller, session, state) do
        # Handler implementation
        {:done, :unit, state}
      end

      def quote_handler(_, _, _, state), do: {:continue, nil, state}
  """
  defmacro handler(name, sender_role, pattern, session_var, state_var, do: body) do
    # These lines ensure the variables are correctly captured in the macro expansion
    state_var_name = Macro.var(state_var, __CALLER__.module)
    session_var_name = Macro.var(session_var, __CALLER__.module)

    quote do
      # Handler for the specific message pattern
      def unquote(name)(
            unquote(pattern),
            unquote(sender_role),
            unquote(session_var_name),
            unquote(state_var_name)
          ) do
        unquote(body)
      end

      # Catch-all handler for unmatched messages
      def unquote(name)(_, _, _, unquote(state_var_name)),
        do: {:continue, nil, unquote(state_var_name)}
    end
  end
end
