defmodule Maty.Typechecker.Core do
  alias Maty.Ast.ST, as: ST
  alias Maty.Typechecker.Error, as: Err

  defp lookup_table(key) do
    case key do
      :install ->
        [
          %ST.SIn{
            from: :buyer1,
            message: {:title, :string},
            continue_as: [%ST.SHandler{handler: :quote_handler}]
          }
        ]

      :quote_handler ->
        [
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
        ]

      :share_handler ->
        [
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
        ]

      :date_handler ->
        [
          %ST.SIn{
            from: :seller,
            message: {:date, :date},
            continue_as: [%ST.SEnd{}]
          }
        ]

      :title_handler ->
        [
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
        ]

      :decision_handler ->
        [
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
          },
          %ST.SIn{
            from: :buyer2,
            message: {:quit, :unit},
            continue_as: [%ST.SEnd{}]
          }
        ]
    end
  end

  def typecheck_header(handler, args, body) do
    cond do
      length(args) > 4 -> IO.puts("too many args passed to handler")
      length(args) < 4 -> IO.puts("too few args passed to handler")
      true -> :ok
    end

    [msg, from | _] = args

    [st | _] = lookup_table(handler)
    # do these checks for all branches

    # for branch <- lookup_table(handler) do
    #   IO.inspect(branch)
    # end

    cond do
      # for this first check any branch should yield the same result
      from != st.from ->
        Err.participant_mismatch()

      # from here on is where there's a difference
      !is_tuple(msg) ->
        Err.malformed_message()

      is_var(msg) ->
        # todo make distinct from non-tuple message
        Err.malformed_message()

      elem(msg, 0) != elem(st.message, 0) ->
        Err.label_mismatch()

      !is_some_val(elem(msg, 1)) ->
        # todo somehow use dialyzer types
        IO.puts("Type mismatch")

      true ->
        IO.puts("function definition ok")
    end
  end

  @type ast_var :: {atom(), list(), nil}

  defp is_var({tag, ctx, children}), do: is_atom(tag) and is_list(ctx) and is_nil(children)
  defp is_var(_), do: false

  defp is_some_val(var) do
    # todo make this list exhaustive
    is_var(var) or
      is_atom(var) or
      is_binary(var) or
      is_number(var) or
      is_bitstring(var) or
      is_boolean(var)
  end

  defp extract_body(do: {:__block__, [], block}), do: block
  defp extract_body(do: expr), do: expr
end
