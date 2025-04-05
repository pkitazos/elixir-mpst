defmodule Maty.ST.Lookup do
  alias Maty.ST, as: ST

  def get(key) do
    case key do
      # install
      "&buyer1:{title(binary).+seller:{title(binary).quote_handler}}" ->
        %ST.SIn{
          from: :buyer1,
          branches: [
            %ST.SBranch{
              label: :title,
              payload: :binary,
              continue_as: %ST.SOut{
                to: :seller,
                branches: [
                  %ST.SBranch{
                    label: :title,
                    payload: :binary,
                    continue_as: %ST.SName{handler: :quote_handler}
                  }
                ]
              }
            }
          ]
        }

      # quote_handler
      "&seller:{quote(number).+buyer2:{share(number).end}}" ->
        %ST.SIn{
          from: :seller,
          branches: [
            %ST.SBranch{
              label: :quote,
              payload: :number,
              continue_as: %ST.SOut{
                to: :buyer2,
                branches: [
                  %ST.SBranch{
                    label: :share,
                    payload: :number,
                    continue_as: %ST.SEnd{}
                  }
                ]
              }
            }
          ]
        }

      # share_handler
      "&buyer1:{share(number).+seller:{address(binary).date_handler, quit(unit).end}}" ->
        %ST.SIn{
          from: :buyer1,
          branches: [
            %ST.SBranch{
              label: :share,
              payload: :number,
              continue_as: %ST.SOut{
                to: :seller,
                branches: [
                  %ST.SBranch{
                    label: :address,
                    payload: :binary,
                    continue_as: %ST.SName{handler: :date_handler}
                  },
                  %ST.SBranch{
                    label: :quit,
                    payload: :unit,
                    continue_as: %ST.SEnd{}
                  }
                ]
              }
            }
          ]
        }

      # date_handler
      "&seller:{date(date).end}" ->
        %ST.SIn{
          from: :seller,
          branches: [
            %ST.SBranch{
              label: :date,
              payload: :date,
              continue_as: %ST.SEnd{}
            }
          ]
        }

      # title_handler
      "&buyer1:{title(binary).+buyer1:{quote(number).decision_handler}}" ->
        %ST.SIn{
          from: :buyer1,
          branches: [
            %ST.SBranch{
              label: :title,
              payload: :binary,
              continue_as: %ST.SOut{
                to: :buyer1,
                branches: [
                  %ST.SBranch{
                    label: :quote,
                    payload: :number,
                    continue_as: %ST.SName{handler: :decision_handler}
                  }
                ]
              }
            }
          ]
        }

      # decision_handler
      "&buyer2:{address(binary).+buyer2:{date(date).end, quit(unit).end}}" ->
        %ST.SIn{
          from: :buyer2,
          branches: [
            %ST.SBranch{
              label: :address,
              payload: :binary,
              continue_as: %ST.SOut{
                to: :buyer2,
                branches: [
                  %ST.SBranch{
                    label: :date,
                    payload: :date,
                    continue_as: %ST.SEnd{}
                  }
                ]
              }
            },
            %ST.SBranch{
              label: :quit,
              payload: :unit,
              continue_as: %ST.SEnd{}
            }
          ]
        }
    end
  end
end
