defmodule Maty.Typechecker.Message do
  def handler_annotation(name, handler, pre) do
    IO.puts(
      "function=#{name}\n- annotated with handler=#{handler}\n- expects session pre-condition=#{inspect(pre)}\n"
    )
  end

  def handler_annotation(name, handler) do
    IO.puts("function=#{name}\n- annotated with an invalid handler=#{handler}\n")
  end
end
