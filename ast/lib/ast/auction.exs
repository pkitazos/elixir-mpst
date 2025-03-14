%{
  attributes: [
    session_type_collection:
      {:auction,
       {{:buyer, 2},
        "auction = !bid(number).&{?sold().end,?higher(number).+{!quit().end,!continue().auction}}"}},
    dual_unprocessed_collection: {{:auctioneer, 2}, :auction},
    type_specs: {{:buyer, 2}, {[:pid, :number], :atom}},
    type_specs: {{:decide, 3}, {[:pid, :number, :number], :atom}},
    type_specs: {{:auctioneer, 2}, {[:pid, :number], :atom}},
    type_specs: {{:problematic_buyer, 2}, {[:pid, :number], :atom}}
  ],
  module: Examples.Auction,
  file: "/Users/petroskitazos/dev/Elixir/ElixirST/lib/elixirst/examples/auction.ex",
  deprecated: [],
  unreachable: [],
  anno: {1, 1},
  struct: nil,
  after_verify: [],
  definitions: [
    {{:problematic_buyer, 2}, :def, [line: 56, column: 7],
     [
       {[line: 56, column: 7],
        [
          {:auctioneer, [version: 0, line: 56, column: 25], nil},
          {:_amount, [version: 1, line: 56, column: 37], nil}
        ], [],
        {:__block__, [],
         [
           {{:., [line: 58, column: 5], [:erlang, :send]}, [line: 58, column: 5],
            [{:auctioneer, [version: 0, line: 58, column: 10], nil}, {:bid, true}]},
           {:receive, [line: 60, column: 5],
            [
              [
                do: [
                  {:->, [line: 61, column: 15], [[{:{}, [line: 61, column: 7], [:sold]}], :ok]}
                ]
              ]
            ]}
         ]}}
     ]},
    {{:main, 0}, :def, [line: 67, column: 7],
     [
       {[line: 67, column: 7], [], [],
        {{:., [line: 68, column: 13], [ElixirST, :spawn]}, [line: 68, column: 14],
         [
           {:&, [line: 68, column: 20], [{:/, [], [{:buyer, [line: 68, column: 21], nil}, 2]}]},
           ~c"2",
           {:&, [line: 68, column: 36],
            [{:/, [], [{:auctioneer, [line: 68, column: 37], nil}, 2]}]},
           [200]
         ]}}
     ]},
    {{:decide, 3}, :defp, [line: 20, column: 8],
     [
       {[line: 20, column: 8],
        [
          {:auctioneer_pid, [version: 0, line: 20, column: 15], nil},
          {:amount, [version: 1, line: 20, column: 31], nil},
          {:value, [version: 2, line: 20, column: 39], nil}
        ], [],
        {:case, [line: 21, optimize_boolean: true, type_check: :expr],
         [
           {{:., [line: 21, column: 14], [:erlang, :<]}, [line: 21, column: 14],
            [{:value, [version: 2, line: 21, column: 8], nil}, 100]},
           [
             do: [
               {:->, [line: 21],
                [
                  [false],
                  {:__block__, [line: 21],
                   [
                     {{:., [line: 25, column: 7], [:erlang, :send]}, [line: 25, column: 7],
                      [
                        {:auctioneer_pid, [version: 0, line: 25, column: 12], nil},
                        {:{}, [line: 25, column: 28], [:quit]}
                      ]},
                     :ok
                   ]}
                ]},
               {:->, [line: 21],
                [
                  [true],
                  {:__block__, [line: 21],
                   [
                     {{:., [line: 22, column: 7], [:erlang, :send]}, [line: 22, column: 7],
                      [
                        {:auctioneer_pid, [version: 0, line: 22, column: 12], nil},
                        {:{}, [line: 22, column: 28], [:continue]}
                      ]},
                     {:buyer, [line: 23, column: 7],
                      [
                        {:auctioneer_pid, [version: 0, line: 23, column: 13], nil},
                        {{:., [line: 23, column: 36], [:erlang, :+]}, [line: 23, column: 36],
                         [{:amount, [...], ...}, 10]}
                      ]}
                   ]}
                ]}
             ]
           ]
         ]}}
     ]},
    {{:buyer, 2}, :def, [line: 10, column: 7],
     [
       {[line: 10, column: 7],
        [
          {:auctioneer_pid, [version: 0, line: 10, column: 13], nil},
          {:amount, [version: 1, line: 10, column: 29], nil}
        ], [],
        {:__block__, [],
         [
           {{:., [line: 11, column: 5], [:erlang, :send]}, [line: 11, column: 5],
            [
              {:auctioneer_pid, [version: 0, line: 11, column: 10], nil},
              {:bid, {:amount, [version: 1, line: 11, column: 33], nil}}
            ]},
           {:receive, [line: 13, column: 5],
            [
              [
                do: [
                  {:->, [line: 14, column: 15], [[{:{}, [line: 14, column: 7], [:sold]}], :ok]},
                  {:->, [line: 15, column: 24],
                   [
                     [higher: {:value, [version: 2, line: 15, column: 17], nil}],
                     {:decide, [line: 15, column: 27],
                      [
                        {:auctioneer_pid, [version: 0, line: 15, column: 34], nil},
                        {:amount, [version: 1, line: 15, column: 50], nil},
                        {:value, [version: 2, line: 15, column: 58], nil}
                      ]}
                   ]}
                ]
              ]
            ]}
         ]}}
     ]},
    {{:auctioneer, 2}, :def, [line: 32, column: 7],
     [
       {[line: 32, column: 7],
        [
          {:buyer_pid, [version: 0, line: 32, column: 18], nil},
          {:minimum, [version: 1, line: 32, column: 29], nil}
        ], [],
        {:__block__, [],
         [
           {:=, [line: 33, column: 12],
            [
              {:amount, [version: 3, line: 33, column: 5], nil},
              {:receive, [line: 34, column: 7],
               [
                 [
                   do: [
                     {:->, [line: 35, column: 24],
                      [
                        [bid: {:amount, [version: 2, line: 35, column: 16], nil}],
                        {:amount, [version: 2, line: 36, column: 11], nil}
                      ]}
                   ]
                 ]
               ]}
            ]},
           {:case, [line: 39, optimize_boolean: true, type_check: :expr],
            [
              {{:., [line: 39, column: 15], [:erlang, :>]}, [line: 39, column: 15],
               [
                 {:amount, [version: 3, line: 39, column: 8], nil},
                 {:minimum, [version: 1, line: 39, column: 17], nil}
               ]},
              [
                do: [
                  {:->, [line: 39],
                   [
                     [false],
                     {:__block__, [line: 39],
                      [
                        {{:., [line: 43, column: 7], [:erlang, :send]}, [line: 43, column: 7],
                         [{:buyer_pid, [...], ...}, {:higher, ...}]},
                        {:receive, [line: 45, column: 7], [[do: [...]]]}
                      ]}
                   ]},
                  {:->, [line: 39],
                   [
                     [true],
                     {:__block__, [line: 39],
                      [
                        {{:., [line: 40, column: 7], [:erlang, ...]}, [line: 40, column: 7],
                         [{:buyer_pid, ...}, {...}]},
                        :ok
                      ]}
                   ]}
                ]
              ]
            ]}
         ]}}
     ]}
  ],
  compile_opts: [:debug_info],
  defines_behaviour: false,
  signatures: %{
    {:auctioneer, 2} => {:infer, [{[%{dynamic: :term}, %{dynamic: :term}], %{dynamic: :term}}]},
    {:buyer, 2} => {:infer, [{[%{dynamic: :term}, %{dynamic: :term}], %{dynamic: :term}}]},
    {:main, 0} => {:infer, [{[], %{dynamic: :term}}]},
    {:problematic_buyer, 2} =>
      {:infer,
       [{[%{dynamic: :term}, %{dynamic: :term}], %{dynamic: %{atom: {:union, %{ok: []}}}}}]}
  },
  impls: [],
  relative_file: "lib/elixirst/examples/auction.ex"
}
