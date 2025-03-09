defmodule Maty.HandlerMacroTest do
  use ExUnit.Case

  defmodule TestHandler do
    use Maty.HandlerMacros

    # Define a handler using our macro
    handler :test_handler, :test_role, {:message, data}, session, state do
      {:done, data, state}
    end
  end

  test "handler macro generates correct function clauses" do
    # Test the specific pattern clause
    assert {:done, "test data", %{}} ==
             TestHandler.test_handler({:message, "test data"}, :test_role, %{}, %{})

    # Test the catch-all clause
    assert {:continue, nil, %{}} == TestHandler.test_handler(:unknown, :wrong_role, %{}, %{})
  end
end
