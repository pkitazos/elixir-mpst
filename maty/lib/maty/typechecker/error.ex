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

  def spec_args_not_well_typed(spec_name, args_types) do
    "Problem with @spec for #{spec_name}. Function args: \n\t#{inspect(args_types)} are not well typed"
  end

  def spec_return_not_well_typed(spec_name, return_type) do
    "Problem with @spec for #{spec_name}. Function return: \n\t#{inspect(return_type)} are not well typed"
  end

  def version_mismatch(expected, got) do
    "Found version #{got} but expected #{expected}."
  end

  def handler_already_taken(handler, prev, curr) do
    "Can't apply handler \'#{handler}\' to function #{curr}, function #{prev} was already annotated with this handler"
  end

  def missing_handler(handler) do
    "No handler in this module named #{handler}"
  end

  def no_private_handlers() do
    "Handlers can't be private functions"
  end

  def unsupported_spec_type() do
    "Unsupported type in @spec annotation"
  end

  def unannotated_handler(func) do
    "Function: #{func} has not been annotated with a handler"
  end

  def missing_spec_annotation(func) do
    "Function: #{func} is missing a @spec annotation"
  end
end
