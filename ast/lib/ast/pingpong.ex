%{
  attributes: [
    session_type_collection: {:X, {{:pinger, 1}, "X = !ping().?pong().X"}},
    dual_unprocessed_collection: {{:ponger, 1}, :X},
    type_specs: {{:pinger, 1}, {[:pid], :no_return}},
    type_specs: {{:ponger, 1}, {[:pid], :no_return}}
  ],
  module: Examples.PingPong,
  file: "/Users/petroskitazos/dev/Elixir/ElixirST/lib/elixirst/examples/pingpong.ex",
  deprecated: [],
  unreachable: [],
  anno: {1, 1},
  struct: nil,
  after_verify: [],
  definitions: [
    {{:ponger, 1}, :def, [line: 20, column: 7],
     [
       {[line: 20, column: 7], [{:pid, [version: 0, line: 20, column: 14], nil}], [],
        {:__block__, [],
         [
           {:receive, [line: 21, column: 5],
            [
              [
                do: [
                  {:->, [line: 22, column: 15],
                   [
                     [{:{}, [line: 22, column: 7], [:ping]}],
                     {{:., [line: 23, column: 11], [IO, :puts]}, [line: 23, column: 12],
                      [
                        {:<<>>, [alignment: 0, line: 24],
                         [
                           {:"::", [inferred_bitstring_spec: true, line: 24, column: 11],
                            ["Received ping from ", {:binary, [line: 24], nil}]},
                           {:"::", [line: 24, column: 31],
                            [{{:., [...], ...}, [line: 24], [...]}, {:binary, [], ...}]},
                           {:"::", [inferred_bitstring_spec: true, line: 24, column: 11],
                            [". Replying pong from ", {:binary, ...}]},
                           {:"::", [line: 24, column: 67], [{{...}, ...}, {...}]},
                           {:"::", [inferred_bitstring_spec: true, line: 24], [" ", ...]},
                           {:"::", [inferred_bitstring_spec: true], [...]},
                           {:"::", [...], ...}
                         ]}
                      ]}
                   ]}
                ]
              ]
            ]},
           {{:., [line: 29, column: 5], [:erlang, :send]}, [line: 29, column: 5],
            [
              {:pid, [version: 0, line: 29, column: 10], nil},
              {:{}, [line: 29, column: 15], [:pong]}
            ]},
           {:ponger, [line: 31, column: 5], [{:pid, [version: 0, line: 31, column: 12], nil}]}
         ]}}
     ]},
    {{:pinger, 1}, :def, [line: 8, column: 7],
     [
       {[line: 8, column: 7], [{:pid, [version: 0, line: 8, column: 14], nil}], [],
        {:__block__, [],
         [
           {{:., [line: 9, column: 5], [:erlang, :send]}, [line: 9, column: 5],
            [
              {:pid, [version: 0, line: 9, column: 10], nil},
              {:{}, [line: 9, column: 15], [:ping]}
            ]},
           {:receive, [line: 11, column: 5],
            [
              [
                do: [
                  {:->, [line: 12, column: 15],
                   [
                     [{:{}, [line: 12, column: 7], [:pong]}],
                     {{:., [line: 12, column: 20], [IO, :puts]}, [line: 12, column: 21],
                      ["Received pong."]}
                   ]}
                ]
              ]
            ]},
           {:pinger, [line: 15, column: 5], [{:pid, [version: 0, line: 15, column: 12], nil}]}
         ]}}
     ]},
    {{:main, 0}, :def, [line: 34, column: 7],
     [
       {[line: 34, column: 7], [], [],
        {{:., [line: 35, column: 13], [ElixirST, :spawn]}, [line: 35, column: 14],
         [
           {:&, [line: 35, column: 20], [{:/, [], [{:pinger, [line: 35, column: 21], nil}, 1]}]},
           [],
           {:&, [line: 35, column: 35], [{:/, [], [{:ponger, [line: 35, column: 36], nil}, 1]}]},
           []
         ]}}
     ]}
  ],
  compile_opts: [:debug_info],
  defines_behaviour: false,
  signatures: %{
    {:main, 0} => {:infer, [{[], %{dynamic: :term}}]},
    {:pinger, 1} => {:infer, [{[%{dynamic: :term}], %{dynamic: :term}}]},
    {:ponger, 1} => {:infer, [{[%{dynamic: :term}], %{dynamic: :term}}]}
  },
  impls: [],
  relative_file: "lib/elixirst/examples/pingpong.ex"
}
