defmodule Maty.Typechecker.Ast do
  def throwaway() do
    init_actor()
    install()
    title_handler()
    decision_handler()
    lookup_price()
    shipping_date()

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

  def lookup_price() do
    {{:lookup_price, 1}, :defp, [line: 71, column: 8],
     [
       {[line: 71, column: 8], [{:_title_str, [version: 0, line: 71, column: 21], nil}], [], 150}
     ]}
  end

  def shipping_date() do
    {{:shipping_date, 1}, :defp, [line: 74, column: 8],
     [
       {[line: 74, column: 8], [{:_addr_str, [version: 0, line: 74, column: 22], nil}], [],
        {:%, [line: 74],
         [Date, {:%{}, [line: 74], [calendar: Calendar.ISO, year: 2021, month: 12, day: 31]}]}}
     ]}
  end

  def install() do
    {{:install, 2}, :def, [line: 33, column: 7],
     [
       {[line: 33, column: 7],
        [
          {:_session, [version: 0, line: 33, column: 15], nil},
          {:state, [version: 1, line: 33, column: 25], nil}
        ], [],
        {:__block__, [],
         [
           {:=, [line: 34, column: 26],
            [
              {:ok, {:updated_state, [version: 2, line: 34, column: 11], nil}},
              {:register, [line: 35, column: 7],
               [
                 {{:., [line: 36, column: 21],
                   [
                     {{:., [line: 36, column: 14],
                       [{:state, [version: 1, line: 36, column: 9], nil}, :global]},
                      [no_parens: true, line: 36, column: 15], []},
                     :ap_pid
                   ]}, [no_parens: true, line: 36, column: 22], []},
                 :seller,
                 {:&, [line: 38, column: 9],
                  [
                    {:/, [],
                     [
                       {{:., [line: 38, column: 20], [TwoBuyer.Participants.Seller, :install]},
                        [no_parens: true, line: 38, column: 21], []},
                       2
                     ]}
                  ]},
                 {:state, [version: 1, line: 39, column: 9], nil}
               ]}
            ]},
           {:{}, [line: 42, column: 5],
            [
              :suspend,
              {{:&, [line: 42, column: 17],
                [
                  {:/, [],
                   [
                     {{:., [line: 42, column: 28],
                       [TwoBuyer.Participants.Seller, :title_handler]},
                      [no_parens: true, line: 42, column: 29], []},
                     4
                   ]}
                ]}, :buyer1},
              {:updated_state, [version: 2, line: 42, column: 56], nil}
            ]}
         ]}}
     ]}
  end

  def init_actor do
    {{:init_actor, 1}, :def, [line: 16, column: 7],
     [
       {[line: 16, column: 7], [{:ap_pid, [version: 0, line: 16, column: 18], nil}], [],
        {:__block__, [],
         [
           {:=, [line: 17, column: 19],
            [
              {:initial_state, [version: 1, line: 17, column: 5], nil},
              {:%{}, [line: 17, column: 21],
               [
                 sessions: {:%{}, [line: 17, column: 33], []},
                 callbacks: {:%{}, [line: 17, column: 49], []},
                 global:
                   {:%{}, [line: 17, column: 62],
                    [ap_pid: {:ap_pid, [version: 0, line: 17, column: 72], nil}]}
               ]}
            ]},
           {:=, [line: 19, column: 26],
            [
              {:ok, {:updated_state, [version: 2, line: 19, column: 11], nil}},
              {:register, [line: 20, column: 7],
               [
                 {:ap_pid, [version: 0, line: 21, column: 9], nil},
                 :seller,
                 {:&, [line: 23, column: 9],
                  [
                    {:/, [],
                     [
                       {{:., [line: 23, column: 20], [TwoBuyer.Participants.Seller, :install]},
                        [no_parens: true, line: 23, column: 21], []},
                       2
                     ]}
                  ]},
                 {:initial_state, [version: 1, line: 24, column: 9], nil}
               ]}
            ]},
           {:ok, {:updated_state, [version: 2, line: 27, column: 11], nil}}
         ]}}
     ]}
  end
end
