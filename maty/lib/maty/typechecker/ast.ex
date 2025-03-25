defmodule Maty.Typechecker.Ast do
  def throwaway() do
    title_handler()
    decision_handler()
    share_handler()
    date_handler()
  end

  def title_handler() do
    {{:title_handler, 4}, :def, [line: 67, column: 7],
     [
       {[line: 67, column: 7],
        [
          {:title, {:title, [version: 0, line: 67, column: 30], nil}},
          :buyer1,
          {:session, [version: 1, line: 67, column: 47], nil},
          {:state, [version: 2, line: 67, column: 56], nil}
        ], [],
        {:__block__, [],
         [
           {:=, [line: 68, column: 12],
            [
              {:amount, [version: 3, line: 68, column: 5], nil},
              {:lookup_price, [line: 68, column: 14],
               [{:title, [version: 0, line: 68, column: 27], nil}]}
            ]},
           {:maty_send, [line: 70, column: 5],
            [
              {:session, [version: 1, line: 70, column: 15], nil},
              :buyer1,
              {:quote, {:amount, [version: 3, line: 70, column: 42], nil}}
            ]},
           {:{}, [line: 71, column: 5],
            [
              :suspend,
              {{:&, [line: 71, column: 17],
                [
                  {:/, [],
                   [
                     {{:., [line: 71, column: 28],
                       [TwoBuyer.Participants.Seller, :decision_handler]},
                      [no_parens: true, line: 71, column: 29], []},
                     4
                   ]}
                ]}, :buyer2},
              {:state, [version: 2, line: 71, column: 59], nil}
            ]}
         ]}}
     ]}
  end

  def decision_handler() do
    {{:decision_handler, 4}, :def, [line: 57, column: 7],
     [
       {[line: 57, column: 7],
        [
          {:address, {:addr, [version: 0, line: 57, column: 35], nil}},
          :buyer2,
          {:session, [version: 1, line: 57, column: 51], nil},
          {:state, [version: 2, line: 57, column: 60], nil}
        ], [],
        {:__block__, [],
         [
           {:=, [line: 58, column: 10],
            [
              {:date, [version: 3, line: 58, column: 5], nil},
              {:shipping_date, [line: 58, column: 12],
               [{:addr, [version: 0, line: 58, column: 26], nil}]}
            ]},
           {:maty_send, [line: 60, column: 5],
            [
              {:session, [version: 1, line: 60, column: 15], nil},
              :buyer2,
              {:date, {:date, [version: 3, line: 60, column: 41], nil}}
            ]},
           {:{}, [line: 61, column: 5],
            [:done, :unit, {:state, [version: 2, line: 61, column: 20], nil}]}
         ]}},
       {[line: 66, column: 7],
        [
          {:quit, :unit},
          :buyer2,
          {:_session, [version: 0, line: 66, column: 49], nil},
          {:state, [version: 1, line: 66, column: 59], nil}
        ], [],
        {:{}, [line: 66, column: 71],
         [:done, :unit, {:state, [version: 1, line: 66, column: 86], nil}]}}
     ]}
  end

  def share_handler() do
    {{:share_handler, 4}, :def, [line: 31, column: 7],
     [
       {[line: 31, column: 7],
        [
          {:share, {:amount, [version: 0, line: 31, column: 30], nil}},
          :buyer1,
          {:session, [version: 1, line: 31, column: 48], nil},
          {:state, [version: 2, line: 31, column: 57], nil}
        ], [],
        {:case, [line: 32, optimize_boolean: true, type_check: :expr],
         [
           {{:., [line: 32, column: 15], [:erlang, :>]}, [line: 32, column: 15],
            [{:amount, [version: 0, line: 32, column: 8], nil}, 100]},
           [
             do: [
               {:->, [line: 32],
                [
                  [false],
                  {:__block__, [line: 32],
                   [
                     {:=, [line: 36, column: 15],
                      [
                        {:address, [version: 3, line: 36, column: 7], nil},
                        {:get_address, [line: 36, column: 17], []}
                      ]},
                     {:maty_send, [line: 38, column: 7],
                      [
                        {:session, [version: 1, line: 38, column: 17], nil},
                        :seller,
                        {:address, {:address, [version: 3, line: 38, column: 46], nil}}
                      ]},
                     {:{}, [line: 39, column: 7],
                      [
                        :suspend,
                        {{:&, [line: 39, column: 19],
                          [
                            {:/, [],
                             [
                               {{:., [line: 39, column: 30],
                                 [TwoBuyer.Participants.Buyer2, :date_handler]},
                                [no_parens: true, line: 39, column: 31], []},
                               4
                             ]}
                          ]}, :seller},
                        {:state, [version: 2, line: 39, column: 57], nil}
                      ]}
                   ]}
                ]},
               {:->, [line: 32],
                [
                  [true],
                  {:__block__, [line: 32],
                   [
                     {:maty_send, [line: 33, column: 7],
                      [
                        {:session, [version: 1, line: 33, column: 17], nil},
                        :seller,
                        {:quit, :unit}
                      ]},
                     {:{}, [line: 34, column: 7],
                      [
                        :done,
                        :unit,
                        {:state, [version: 2, line: 34, column: 22], nil}
                      ]}
                   ]}
                ]}
             ]
           ]
         ]}}
     ]}
  end

  def date_handler() do
    {{:date_handler, 4}, :def, [line: 45, column: 7],
     [
       {[line: 45, column: 7],
        [
          {:date, {:_date, [version: 0, line: 45, column: 28], nil}},
          :seller,
          {:_session, [version: 1, line: 45, column: 45], nil},
          {:state, [version: 2, line: 45, column: 55], nil}
        ], [],
        {:{}, [line: 46, column: 5],
         [:done, :unit, {:state, [version: 2, line: 46, column: 20], nil}]}}
     ]}
  end
end
