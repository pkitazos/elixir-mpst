defmodule Maty.HandlerMacroTest do
  use ExUnit.Case, async: true
  import Maty.HandlerMacros

  defmodule HandlerUnderTest do
    import Maty.HandlerMacros

    handler :my_handler, {:test_msg, value}, session, state, from: :some_role do
      {:done, {value, session, state}, state}
    end
  end

  test "handler calls the main clause" do
    session = %{participants: %{some_role: :the_pid}}
    state = :some_state

    # matching message
    result = HandlerUnderTest.my_handler({:test_msg, 123}, session, state, :the_pid)
    assert result == {:done, {123, session, state}, state}
  end

  test "handler calls the fallback clause" do
    session = %{participants: %{some_role: :the_pid}}
    state = :some_state

    # non-matching message
    result = HandlerUnderTest.my_handler({:other, 123}, session, state, :another_pid)
    assert result == {:continue, nil, state}
  end
end
