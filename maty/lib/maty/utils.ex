defmodule Maty.Utils do
  def deep_contains?(data, key) when is_list(data) do
    Enum.any?(data, fn item -> deep_contains?(item, key) end)
  end

  def deep_contains?(data, key) when is_tuple(data) do
    data
    |> Tuple.to_list()
    |> deep_contains?(key)
  end

  def deep_contains?(data, key) do
    data == key
  end

  def to_func({name, arity}), do: "#{name}/#{arity}"
  def to_func(name: name, arity: arity), do: "#{name}/#{arity}"

  defmodule Env do
    def setup(module, attribute) do
      Module.register_attribute(module, attribute, accumulate: true, persist: true)
    end

    def get(module, attribute) do
      Module.get_attribute(module, attribute)
    end

    def get_map(module, attribute) do
      Module.get_attribute(module, attribute) |> Enum.into(%{})
    end

    def add_at_key(module, attribute, key, val) do
      updated_entries =
        get_map(module, attribute)
        |> Map.update(key, val, fn _ -> val end)
        |> Map.to_list()

      rewrite(module, attribute, updated_entries)
    end

    def prepend_to_key(module, attribute, key, val) do
      updated_entries =
        get_map(module, attribute)
        |> Map.update(key, [val], fn vals -> [val | vals] end)
        |> Map.to_list()

      rewrite(module, attribute, updated_entries)
    end

    def remove_from_key(module, attribute, key, val) do
      updated_entries =
        get_map(module, attribute)
        |> Map.update(key, [], &Enum.filter(&1, fn x -> x != val end))
        |> Map.to_list()

      rewrite(module, attribute, updated_entries)
    end

    defp rewrite(module, attribute, entries) do
      Module.delete_attribute(module, attribute)

      for entry <- entries do
        Module.put_attribute(module, attribute, entry)
      end
    end
  end
end
