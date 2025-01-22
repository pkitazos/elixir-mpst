defmodule Seller do
  require MatyDSL

  MatyDSL.role :Seller do
    register do
      # something that associates the current process with "Seller" in a session context
    end

    rec :install do
      # "install" loop
      suspend(:title_handler)
    end

    handler :title_handler, from: :Buyer1 do
      :title, title ->
        # handle the message
        MatyDSL.send(:Buyer1, :quote, lookup_price(title))
        suspend(:decision_handler)
    end

    handler :decision_handler, from: :Buyer2 do
      :address, addr ->
        MatyDSL.send(:Buyer2, :date, shipping_date(addr))

      # maybe loop or end

      :quit, _ ->
        MatyDSL.return()
    end
  end
end
