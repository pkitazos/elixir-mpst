defmodule Maty.HandlerMacros do
  defmacro handler(name, pattern, session_var, state_var, opts, do: body_ast) do
    role = Keyword.get(opts, :from)

    quote do
      # primary clause
      def unquote(name)(
            unquote(pattern),
            from_pid,
            %{participants: participants} = unquote(session_var),
            unquote(state_var)
          )
          when from_pid === participants.unquote(role) do
        unquote(body_ast)
      end

      # fallback clause
      def unquote(name)(_, _, _, unquote(state_var)) do
        {:continue, nil, unquote(state_var)}
      end
    end
  end
end
