defmodule ChatServer.Lookup do
  alias Maty.ST

  def lookup(key) do
    case key do
      "client:chat_server" ->
        %ST.SRec{
          id: :chat,
          continue_as: %ST.SOut{
            to: :server,
            branches: [
              # Branch 1: LookupRoom
              %ST.SBranch{
                label: :lookup_room,
                payload: :number,
                continue_as: %ST.SIn{
                  from: :server,
                  branches: [
                    %ST.SBranch{
                      label: :room_port,
                      payload: {:tuple, [:number, :number]},
                      continue_as: %ST.SRecRef{id: :chat}
                    },
                    %ST.SBranch{
                      label: :room_not_found,
                      payload: :number,
                      continue_as: %ST.SRecRef{id: :chat}
                    }
                  ]
                }
              },
              # Branch 2: CreateRoom
              %ST.SBranch{
                label: :create_room,
                payload: :number,
                continue_as: %ST.SIn{
                  from: :server,
                  branches: [
                    %ST.SBranch{
                      label: :create_room_success,
                      payload: :number,
                      continue_as: %ST.SRecRef{id: :chat}
                    },
                    %ST.SBranch{
                      label: :room_exists,
                      payload: :number,
                      continue_as: %ST.SRecRef{id: :chat}
                    }
                  ]
                }
              },
              # Branch 3: ListRooms
              %ST.SBranch{
                label: :list_rooms,
                payload: :unit,
                continue_as: %ST.SIn{
                  from: :server,
                  branches: [
                    %ST.SBranch{
                      label: :room_list,
                      payload: {:list, [:binary]},
                      continue_as: %ST.SRecRef{id: :chat}
                    }
                  ]
                }
              },
              # Branch 4: Bye
              %ST.SBranch{
                label: :bye,
                payload: :binary,
                continue_as: %ST.SEnd{}
              }
            ]
          }
        }

      "client:c_to_r" ->
        %ST.SRec{
          id: :loop_c,
          continue_as: %ST.SOut{
            to: :room,
            branches: [
              %ST.SBranch{
                label: :outgoing_chat_message,
                payload: :binary,
                continue_as: %ST.SRecRef{id: :loop_c}
              },
              %ST.SBranch{
                label: :leave_room,
                payload: :unit,
                continue_as: %ST.SEnd{}
              }
            ]
          }
        }

      "client:r_to_c" ->
        %ST.SRec{
          id: :loop_r,
          continue_as: %ST.SIn{
            from: :room,
            branches: [
              %ST.SBranch{
                label: :incoming_chat_message,
                payload: :binary,
                continue_as: %ST.SRecRef{id: :loop_r}
              },
              %ST.SBranch{
                label: :bye,
                payload: :unit,
                continue_as: %ST.SEnd{}
              }
            ]
          }
        }

      "server:chat_server" ->
        %ST.SRec{
          id: :chat,
          continue_as: %ST.SIn{
            from: :client,
            branches: [
              %ST.SBRanch{
                label: :lookup_room,
                payload: :number,
                continue_as: %ST.SOut{
                  to: :client,
                  branches: [
                    %ST.SBranch{
                      label: :room_port,
                      payload: {:tuple, [:number, :number]},
                      continue_as: %ST.SRecRef{id: :chat}
                    },
                    %ST.SBranch{
                      label: :room_not_found,
                      payload: :number,
                      continue_as: %ST.SRecRef{id: :chat}
                    }
                  ]
                }
              },
              %ST.SBRanch{
                label: :create_room,
                payload: :number,
                continue_as: %ST.SOut{
                  to: :client,
                  branches: [
                    %ST.SBranch{
                      label: :create_room_success,
                      payload: :number,
                      continue_as: %ST.SRecRef{id: :chat}
                    },
                    %ST.SBranch{
                      label: :room_exists,
                      payload: :number,
                      continue_as: %ST.SRecRef{id: :chat}
                    }
                  ]
                }
              },
              %ST.SBranch{
                label: :list_rooms,
                payload: :unit,
                continue_as: %ST.SOut{
                  to: :client,
                  branches: [
                    %ST.SBranch{
                      label: :room_list,
                      payload: {:list, [:binary]},
                      continue_as: %ST.SRecRef{id: :chat}
                    }
                  ]
                }
              },
              %ST.SBranch{
                label: :bye,
                payload: :binary,
                continue_as: %ST.SEnd{}
              }
            ]
          }
        }

      "room:c_to_r" ->
        %ST.SRec{
          id: :look_r_recv,
          continue_as: %ST.SIn{
            from: :client,
            branches: [
              %ST.SBranch{
                label: :outgoing_chat_message,
                payload: :binary,
                continue_as: %ST.SRecRef{id: :look_r_recv}
              },
              %ST.SBranch{
                label: :leave_room,
                payload: :unit,
                continue_as: %ST.SEnd{}
              }
            ]
          }
        }

      "room:r_to_c" ->
        %ST.SRec{
          id: :loop_r_send,
          continue_as: %ST.SOut{
            to: :client,
            branches: [
              %ST.SBRanch{
                label: :incoming_chat_message,
                payload: :binary,
                continue_as: %ST.SRecRef{id: :loop_r_send}
              },
              %ST.SBranch{
                label: :bye,
                payload: :unit,
                continue_as: %ST.SEnd{}
              }
            ]
          }
        }
    end
  end
end
