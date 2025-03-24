[
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
   ]},
  {{:shipping_date, 1}, :defp, [line: 95, column: 8],
   [
     {[line: 95, column: 8], [{:_addr_str, [version: 0, line: 95, column: 22], nil}], [],
      {:%, [line: 95],
       [
         Date,
         {:%{}, [line: 95], [calendar: Calendar.ISO, year: 2021, month: 12, day: 31]}
       ]}}
   ]},
  {{:lookup_price, 1}, :defp, [line: 92, column: 8],
   [
     {[line: 92, column: 8], [{:_title_str, [version: 0, line: 92, column: 21], nil}], [], 150}
   ]},
  {{:install, 2}, :def, [line: 52, column: 7],
   [
     {[line: 52, column: 7],
      [
        {:_session, [version: 0, line: 52, column: 15], nil},
        {:state, [version: 1, line: 52, column: 25], nil}
      ], [],
      {:__block__, [],
       [
         {:=, [line: 53, column: 26],
          [
            {:ok, {:updated_state, [version: 2, line: 53, column: 11], nil}},
            {:register, [line: 54, column: 7],
             [
               {{:., [line: 55, column: 21],
                 [
                   {{:., [line: 55, column: 14],
                     [{:state, [version: 1, line: 55, column: 9], nil}, :global]},
                    [no_parens: true, line: 55, column: 15], []},
                   :ap_pid
                 ]}, [no_parens: true, line: 55, column: 22], []},
               :seller,
               {:&, [line: 57, column: 9],
                [
                  {:/, [],
                   [
                     {{:., [line: 57, column: 20], [TwoBuyer.Participants.Seller, :install]},
                      [no_parens: true, line: 57, column: 21], []},
                     2
                   ]}
                ]},
               {:state, [version: 1, line: 58, column: 9], nil}
             ]}
          ]},
         {:{}, [line: 61, column: 5],
          [
            :suspend,
            {{:&, [line: 61, column: 17],
              [
                {:/, [],
                 [
                   {{:., [line: 61, column: 28], [TwoBuyer.Participants.Seller, :title_handler]},
                    [no_parens: true, line: 61, column: 29], []},
                   4
                 ]}
              ]}, :buyer1},
            {:updated_state, [version: 2, line: 61, column: 56], nil}
          ]}
       ]}}
   ]},
  {{:init_actor, 1}, :def, [line: 34, column: 7],
   [
     {[line: 34, column: 7], [{:ap_pid, [version: 0, line: 34, column: 18], nil}], [],
      {:__block__, [],
       [
         {:=, [line: 35, column: 19],
          [
            {:initial_state, [version: 1, line: 35, column: 5], nil},
            {:%{}, [line: 35, column: 21],
             [
               sessions: {:%{}, [line: 35, column: 33], []},
               callbacks: {:%{}, [line: 35, column: 49], []},
               global:
                 {:%{}, [line: 35, column: 62],
                  [ap_pid: {:ap_pid, [version: 0, line: 35, column: 72], nil}]}
             ]}
          ]},
         {:=, [line: 37, column: 26],
          [
            {:ok, {:updated_state, [version: 2, line: 37, column: 11], nil}},
            {:register, [line: 38, column: 7],
             [
               {:ap_pid, [version: 0, line: 39, column: 9], nil},
               :seller,
               {:&, [line: 41, column: 9],
                [
                  {:/, [],
                   [
                     {{:., [line: 41, column: 20], [TwoBuyer.Participants.Seller, :install]},
                      [no_parens: true, line: 41, column: 21], []},
                     2
                   ]}
                ]},
               {:initial_state, [version: 1, line: 42, column: 9], nil}
             ]}
          ]},
         {:ok, {:updated_state, [version: 2, line: 45, column: 11], nil}}
       ]}}
   ]},
  {{:decision_handler, 4}, :def, [line: 77, column: 7],
   [
     {[line: 77, column: 7],
      [
        {:address, {:addr, [version: 0, line: 77, column: 35], nil}},
        :buyer2,
        {:session, [version: 1, line: 77, column: 51], nil},
        {:state, [version: 2, line: 77, column: 60], nil}
      ], [],
      {:__block__, [],
       [
         {:=, [line: 78, column: 10],
          [
            {:date, [version: 3, line: 78, column: 5], nil},
            {:shipping_date, [line: 78, column: 12],
             [{:addr, [version: 0, line: 78, column: 26], nil}]}
          ]},
         {:maty_send, [line: 80, column: 5],
          [
            {:session, [version: 1, line: 80, column: 15], nil},
            :buyer2,
            {:date, {:date, [version: 3, line: 80, column: 41], nil}}
          ]},
         {:{}, [line: 81, column: 5],
          [:done, :unit, {:state, [version: 2, line: 81, column: 20], nil}]}
       ]}},
     {[line: 87, column: 7],
      [
        {:quit, :unit},
        :buyer2,
        {:_session, [version: 0, line: 87, column: 49], nil},
        {:state, [version: 1, line: 87, column: 59], nil}
      ], [],
      {:{}, [line: 87, column: 71],
       [:done, :unit, {:state, [version: 1, line: 87, column: 86], nil}]}}
   ]}
]
