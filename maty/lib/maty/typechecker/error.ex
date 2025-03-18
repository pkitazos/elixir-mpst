defmodule Maty.Typechecker.Error do
  def participant_mismatch(handler, line, [expected: expected, got: got] = _opts) do
    "Error in #{handler} message came from wrong participant (line: #{line})\n- Expected:\t#{expected}\n- Got:\t\t#{got}\n"
  end

  def malformed_message(handler, line, [tuple_size: tuple_size] = _opts) do
    "Error in #{handler} malformed message (line: #{line})\n" <>
      "Messages should be structured as: 2-tuples {:label, message}\nReceived #{tuple_size}-tuple\n"
  end

  def label_mismatch(handler, line, [expected: expected, got: got] = _opts) do
    "Error in #{handler} message label incorrect (line: #{line})\n- Expected:\t#{expected}\n- Got:\t\t#{got}\n"
  end
end
