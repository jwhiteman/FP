# mapcat in Elixir using CPS

defmodule My do
  defp map_with_cps(_, [], acc_f), do: acc_f
  defp map_with_cps(map_f, [h|t], acc_f) do
    map_with_cps(
    map_f,
    t,
    fn (acc) -> acc_f.([map_f.(h) | acc]) end
    )
  end

  # mapcat without concat/merge, using CPS
  def mapcat(map_f, lol) do
    List.foldl(
    lol,
    &(&1),
    fn (l, acc) -> map_with_cps(map_f, l, acc) end
    ).([])
  end
end

[2, 3, 4, 5, 6, 7] = My.mapcat &(&1 + 1), [[1, 2, 3], [4, 5, 6]]
