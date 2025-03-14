defmodule Maty.Typechecker.Typechecker do
  @spec tc_expr(env, st, expr) ::
          {:ok, nil}
          | {:ok, {type, st}}
          | {:error, String.t()}

  def tc_expr(env, st, expr) do
    with {:ok, maybe_result} <- some_subcheck(...),
         {:ok, some_other} <- some_subcheck_2(...),
         ... do
      # inside here, maybe_result might be nil or {ty, st}
      # so you can handle that explicitly
      ...
      {:ok, final_value}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  def tc_expr(env, st, {:e_return, val}) do
    case tc_val(env, val) do
      {:ok, val_ty} ->
        {:ok, {val_ty, st}}

      {:error, reason} ->
        {:error, reason}
    end
  end
end
