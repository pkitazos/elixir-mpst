defmodule Maty.ST.Parser do
  @moduledoc """

  ## ST annotation grammar

  SIn  ::= ROLE & LABEL(TYPE) . next
  SOut ::= ROLE ! LABEL(TYPE) . next
  next ::= SIn
         | SOut
         | HANDLER_NAME
         | end
         | { branchlist }
  branchlist ::= SOut ( ',' SOut )*

  """
end
