defmodule Maty.Hook do
  alias Maty.Typechecker.Core, as: TC_Core
  alias Maty.Typechecker.Message, as: TC_Msg

  @debug_handler :title_handler

  defmacro __using__(_) do
    quote do
      import Maty.Hook

      Module.register_attribute(__MODULE__, :st, accumulate: true, persist: true)
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
          case TC_Core.typecheck_header(env.module, handler, args, body) do
            {:error, errFn} ->
              errFn.()

            {:ok, {label, key}} ->
              # todo find better name for key
              Module.put_attribute(
                env.module,
                :pairs,
                {{name, length(args), label}, {handler, key}}
              )

              remove_branch(env.module, handler, key)
          end

        :error ->
          TC_Msg.handler_annotation(name, handler)
      end
    end
  end

  def __before_compile__(env) do
    pairs = Module.get_attribute(env.module, :pairs)

    for {k, v} <- pairs do
      IO.puts("#{inspect(k)} --> \n#{inspect(v)}\n\n")
    end
  end

  defp remove_branch(module, handler, key) do
    updated_sts =
      Module.get_attribute(module, :st)
      |> Enum.into(%{})
      |> Map.update(handler, [], &Enum.filter(&1, fn x -> x != key end))
      |> Map.to_list()

    Module.delete_attribute(module, :st)

    for {k, v} <- updated_sts do
      Module.put_attribute(module, :st, {k, v})
    end
  end
end
