# ctx1 = [context: Elixir, imports: [{1, Kernel}, {2, Kernel}]]
# ctx2 = [context: Elixir]
# ctx3 = []

# incr = {:def, ctx1, [{:hello, ctx2, [{:name, ctx3, Elixir}]}, [do: "hello world"]]}

# full =
#   {:def, [context: Elixir, imports: [{1, Kernel}, {2, Kernel}]],
#    [{:hello, [context: Elixir], [{:name, [], Elixir}]}, [do: "hello world"]]}

# ---
# hello =
#   quote do
#     def date_handler({:date, date}, :seller, _session, state) do
#       log(:date_handler, "Received date=#{date}, finishing.")
#       {:done, :unit, state}
#     end

#     def date_handler(_, _, _, state), do: {:continue, nil, state}
#   end
# ---

# IO.inspect(hello)

# {:__block__, [],
#  [
#    {:def, [context: Elixir, imports: [{1, Kernel}, {2, Kernel}]],
#     [
#       {:date_handler, [context: Elixir],
#        [
#          {:date, {:date, [], Elixir}},
#          :seller,
#          {:_session, [], Elixir},
#          {:state, [], Elixir}
#        ]},
#       [
#         do:
#           {:__block__, [],
#            [
#              {:log, [],
#               [
#                 :date_handler,
#                 {:<<>>, [],
#                  [
#                    "Received date=",
#                    {:"::", [],
#                     [
#                       {{:., [], [Kernel, :to_string]}, [from_interpolation: true],
#                        [{:date, [], Elixir}]},
#                       {:binary, [], Elixir}
#                     ]},
#                    ", finishing."
#                  ]}
#               ]},
#              {:{}, [], [:done, :unit, {:state, [], Elixir}]}
#            ]}
#       ]
#     ]},
#    {:def, [context: Elixir, imports: [{1, Kernel}, {2, Kernel}]],
#     [
#       {:date_handler, [context: Elixir],
#        [
#          {:_, [], Elixir},
#          {:_, [], Elixir},
#          {:_, [], Elixir},
#          {:state, [], Elixir}
#        ]},
#       [do: {:{}, [], [:continue, nil, {:state, [], Elixir}]}]
#     ]}
#  ]}
# ---

# defmacro debug_ast(do: block) do
#   IO.puts("AST: #{inspect(Macro.to_string(quote do: unquote(block)))}")
#   quote do: unquote(block)
# end

# def date_handler(_, _, _, state), do: {:continue, nil, state}


# {:def, [context: Elixir, imports: [{1, Kernel}, {2, Kernel}]],
#  [
#    {:date_handler, [context: Elixir],
#     [
#       {:_, [], Elixir},
#       {:_, [], Elixir},
#       {:_, [], Elixir},
#       {:state, [], Elixir}
#     ]},
#    [do: {:{}, [], [:continue, nil, {:state, [], Elixir}]}]
#  ]}
