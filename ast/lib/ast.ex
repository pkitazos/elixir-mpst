defmodule Ast do
  @moduledoc """
  Documentation for `Ast`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Ast.hello()
      :world

  """
  defmacro debug_ast(do: block) do
    IO.puts("AST: #{inspect(Macro.to_string(quote do: unquote(block)))}")
    quote do: unquote(block)
  end

  def test() do
    quote do
      def quote_handler({:quote, amount}, :seller, session, state) do
        share_amount = amount / 2
        {:done, :unit, state}
      end
    end
  end
end
