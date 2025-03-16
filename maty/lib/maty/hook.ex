defmodule Maty.Hook do
  alias Maty.Typechecker.Core, as: TC_Core
  alias Maty.Typechecker.Message, as: TC_Msg

  @debug_handler :decision_handler

  defmacro __using__(_) do
    quote do
      import Maty.Hook

      Module.register_attribute(__MODULE__, :st, accumulate: true)
      Module.register_attribute(__MODULE__, :handler, accumulate: false)
      Module.register_attribute(__MODULE__, :pairs, accumulate: true, persist: true)

      @on_definition Maty.Hook
      @before_compile Maty.Hook
    end
  end

  def __on_definition__(env, _kind, name, args, _guards, body) do
    sts = Module.get_attribute(env.module, :st) |> Enum.into(%{})

    handler = Module.get_attribute(env.module, :handler)
    Module.delete_attribute(env.module, :handler)

    if handler != nil do
      case Map.fetch(sts, handler) do
        {:ok, _pre} ->
          if handler == @debug_handler do
            IO.puts("\n----------------- #{@debug_handler} ----------------\n")
            # TC.Msg.handler_annotation(name, handler, pre)

            TC_Core.typecheck_header(handler, args, body)

            # not quite this simple unfortunately
            # so if the header matches something we should return the branch label that matches
            # and then update the sessions accordingly
            # what do I mean by this: if we check the header it matches on the branch with {:address, :string}
            # then this specific handler should be tagged with the message, name and arity against its specific branch
            # and then that specific branch should be removed from this handler's possible branches
            # because we want to error if we find another handler that attempts to handle the same message
            arity = length(args)
            Module.put_attribute(env.module, :pairs, {{name, arity}, handler})

            IO.puts("\n-------------------------------------------------\n")
          end

        :error ->
          TC_Msg.handler_annotation(name, handler)
      end
    end
  end

  defmacro __before_compile__(env) do
    Module.get_attribute(env.module, :pairs)
  end
end
