# defmodule Maty.DialyzerTest do
#   @moduledoc """
#   This module exists purely to test Dialyzer's functionality.
#   """

#   @type test_type :: atom()

#   # Test 1: Return type mismatch
#   @spec return_type_test() :: integer()
#   def return_type_test() do
#     "string instead of integer"
#   end

#   # Test 2: Map key type mismatch
#   @spec map_key_test() :: %{test_type() => integer()}
#   def map_key_test() do
#     # Using integer keys instead of atoms
#     %{1 => 10, 2 => 20}
#   end

#   # Test 3: Call the functions to ensure they're not dead code
#   def run_all_tests() do
#     IO.puts("Running dialyzer test functions")

#     return_type_test()
#     map_key_test()

#     :ok
#   end
# end
