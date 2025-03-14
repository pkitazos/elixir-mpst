defmodule Checker do
  defmacro __using__(_) do
    quote do
      import Checker

      Module.register_attribute(__MODULE__, :st, accumulate: true)
      Module.register_attribute(__MODULE__, :handler, accumulate: false)
      Module.register_attribute(__MODULE__, :pairs, accumulate: true, persist: true)

      @on_definition Checker
      @before_compile Checker
    end
  end

  def __on_definition__(env, _kind, name, _args, _guards, body) do
    _pairs = Module.get_attribute(env.module, :pairs)
    sts = Module.get_attribute(env.module, :st) |> Enum.into(%{})

    handler = Module.get_attribute(env.module, :handler)
    Module.delete_attribute(env.module, :handler)

    if handler != nil do
      case Map.fetch(sts, handler) do
        {:ok, pre} ->
          IO.puts("function=#{name} was annotated with handler=#{handler}")
          IO.puts("handler expects session pre-condition=#{inspect(pre)}\n")

          IO.inspect(body)
          Checker.typecheck(body)

        :error ->
          IO.puts("function=#{name} was annotated with an invalid handler name\n")
      end
    else
      # IO.puts("no handler for function=#{name}\n")
    end
  end

  defmacro __before_compile__(env) do
    pairs = Module.get_attribute(env.module, :pairs)

    IO.inspect(pairs)
  end

  def typecheck(ast) do
    :ok
  end
end
