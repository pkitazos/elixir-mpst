defmodule Maty.ST.Lookup do
  alias Maty.ST, as: ST

  def get(key) do
    case key do
      "buyer1&title(string).quote_handler" ->
        %ST.SIn{
          from: :buyer1,
          message: {:title, :string},
          continue_as: [%ST.SHandler{handler: :quote_handler}]
        }

      "seller&quote(float).buyer2!share(float)" ->
        %ST.SIn{
          from: :seller,
          message: {:quote, :number},
          continue_as: [
            %ST.SOut{
              to: :buyer2,
              message: {:share, :number},
              continue_as: [%ST.SEnd{}]
            }
          ]
        }

      "buyer1&share(float).{ seller!address(string).date_handler, seller!quit(unit).end }" ->
        %ST.SIn{
          from: :buyer1,
          message: {:share, :number},
          continue_as: [
            %ST.SOut{
              to: :seller,
              message: {:address, :string},
              continue_as: [%ST.SHandler{handler: :date_handler}]
            },
            %ST.SOut{
              to: :seller,
              message: {:quit, :unit},
              continue_as: [%ST.SEnd{}]
            }
          ]
        }

      "seller&date(date).end" ->
        %ST.SIn{
          from: :seller,
          message: {:date, :date},
          continue_as: [%ST.SEnd{}]
        }

      "buyer1&title(string).buyer1!quote(float).decision_handler" ->
        %ST.SIn{
          from: :buyer1,
          message: {:title, :string},
          continue_as: [
            %ST.SOut{
              to: :buyer1,
              message: {:quote, :number},
              continue_as: [%ST.SHandler{handler: :decision_handler}]
            }
          ]
        }

      "buyer2&address(string).buyer2!date(date).end" ->
        %ST.SIn{
          from: :buyer2,
          message: {:address, :string},
          continue_as: [
            %ST.SOut{
              to: :buyer2,
              message: {:date, :date},
              continue_as: [%ST.SEnd{}]
            }
          ]
        }

      "buyer2&quit(unit).end" ->
        %ST.SIn{
          from: :buyer2,
          message: {:quit, :unit},
          continue_as: [%ST.SEnd{}]
        }
    end
  end
end
