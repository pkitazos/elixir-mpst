# test/macros/handler_expansion_test.exs
defmodule Maty.HandlerExpansionTest do
  use ExUnit.Case

  use Maty.Macros

  # For type definitions in the specs
  @type session_ctx :: any()
  @type maty_actor_state :: any()
  @type suspend :: any()
  @type done :: any()

  test "shows macro expansions for different handler patterns" do
    test_cases = [
      {"Simple handler",
       quote do
         handler :simple_handler, :sender, {:message, content :: binary()}, state do
           IO.puts("Received message: #{content}")
           MatyDSL.done(state)
         end
       end,
       quote do
         @spec simple_handler(:sender, {:message, binary()}, session_ctx(), maty_actor_state()) ::
                 suspend() | done()
         def simple_handler(:sender, {:message, content}, session_ctx, state) do
           try do
             IO.puts("Received message: #{content}")
             MatyDSL.done(state)
           catch
             {:suspend, next_handler, new_state} -> {:suspend, {next_handler, :sender}, new_state}
           end
         end
       end}
      # {"Tuple payload",
      #  quote do
      #    handler :tuple_handler,
      #            :sender,
      #            {:pair, {first :: number(), second :: binary()}},
      #            state do
      #      IO.puts("Received pair: #{first}, #{second}")
      #      MatyDSL.send(:receiver, {:response, "Processed"})
      #      MatyDSL.suspend(:next_handler, state)
      #    end
      #  end},
      # {"N-tuple payload",
      #  quote do
      #    handler :ntuple_handler,
      #            :sender,
      #            {:data, {a :: number(), b :: binary(), c :: boolean()}},
      #            state do
      #      IO.puts("Received data: #{a}, #{b}, #{c}")
      #      MatyDSL.done(state)
      #    end
      #  end},
      # {"List payload",
      #  quote do
      #    handler :list_handler, :sender, {:items, items :: list(binary())}, state do
      #      IO.puts("Received items: #{Enum.join(items, ", ")}")
      #      MatyDSL.done(state)
      #    end
      #  end},
      # {"Atom literal",
      #  quote do
      #    handler :atom_handler, :sender, {:command, :start}, state do
      #      IO.puts("Received start command")
      #      MatyDSL.done(state)
      #    end
      #  end},
      # {"Number literal",
      #  quote do
      #    handler :number_handler, :sender, {:value, 42}, state do
      #      IO.puts("Received fixed value")
      #      MatyDSL.done(state)
      #    end
      #  end}
    ]

    for {name, macro, func} <- test_cases do
      expanded = Macro.expand_once(macro, __ENV__)

      IO.puts("\n=== #{name} ===")
      IO.puts("\nORIGINAL:\n```elixir")
      IO.puts(Macro.to_string(macro))
      IO.puts("```\n")
      IO.puts("\nEXPANDED:\n```elixir")
      IO.puts(Macro.to_string(expanded))
      IO.puts("```\n")
      IO.puts("\nEXPECTED:\n```elixir")
      IO.puts(Macro.to_string(func))
      IO.puts("```\n")
      IO.puts(String.duplicate("-", 80))

      assert true
    end
  end
end
