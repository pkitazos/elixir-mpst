defmodule MyASTHook do
  def __on_definition__(env, kind, name, args, guards, body) do
    IO.puts("---- Original (unexpanded) body:")
    IO.inspect(body)

    # Option A: Single-step expansion
    one_step = Macro.expand(body, env)
    IO.puts("---- Single-step expanded body:")
    IO.inspect(one_step)

    # Option B: Fully recursive expansion
    fully_expanded = deep_expand(body, env)
    IO.puts("---- Fully recursively expanded body:")
    IO.inspect(fully_expanded)
  end

  # Recursively call expand_once on all sub-nodes until stable
  defp deep_expand(ast, env) do
    Macro.prewalk(ast, fn node ->
      expanded = Macro.expand_once(node, env)
      if expanded == node, do: node, else: expanded
    end)
  end
end

defmodule TestMod do
  # This attaches our on_definition hook
  @on_definition MyASTHook

  def title_handler({:title, title}, :buyer1, session, state) do
    amount = lookup_price(title)

    maty_send(session, :buyer1, {:quote, amount})
    {:suspend, {&__MODULE__.decision_handler/4, :buyer2}, state}
  end

  defp lookup_price(_title), do: 150

  defp maty_send({session, from}, to, msg) do
    send(session.participants[to], {:maty_message, session.id, to, from, msg})
  end
end
