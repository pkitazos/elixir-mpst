{:__block__, [],
 [
   {:def, [context: Elixir, imports: [{1, Kernel}, {2, Kernel}]],
    [
      {:date_handler, [context: Elixir],
       [
         {:date, {:date, [], Elixir}},
         :seller,
         {:_session, [], Elixir},
         {:state, [], Elixir}
       ]},
      [
        do:
          {:__block__, [],
           [
             {:log, [],
              [
                :date_handler,
                {:<<>>, [],
                 [
                   "Received date=",
                   {:"::", [],
                    [
                      {{:., [], [Kernel, :to_string]}, [from_interpolation: true],
                       [{:date, [], Elixir}]},
                      {:binary, [], Elixir}
                    ]},
                   ", finishing."
                 ]}
              ]},
             {:{}, [], [:done, :unit, {:state, [], Elixir}]}
           ]}
      ]
    ]},
   {:def, [context: Elixir, imports: [{1, Kernel}, {2, Kernel}]],
    [
      {:date_handler, [context: Elixir],
       [
         {:_, [], Elixir},
         {:_, [], Elixir},
         {:_, [], Elixir},
         {:state, [], Elixir}
       ]},
      [do: {:{}, [], [:continue, nil, {:state, [], Elixir}]}]
    ]}
 ]}
