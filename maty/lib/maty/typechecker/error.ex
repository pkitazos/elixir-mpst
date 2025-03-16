defmodule Maty.Typechecker.Error do
  def participant_mismatch do
    IO.puts("message came from wrong participant")
  end

  def malformed_message do
    IO.puts("malformed message")
  end

  def label_mismatch do
    IO.puts("message label incorrect")
  end
end
