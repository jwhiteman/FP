defmodule MyList do

  # version 1: continuation passing style
  def flatten(l), do: flatten(l, fn (result) -> result end)
  def flatten([], col), do: col.([])
  def flatten([h|t], col) when not is_list(h) do
    flatten(t, fn (acc) -> col.([h | acc]) end)
  end
  def flatten([h|t], col) do
    col.(
    flatten(h)
    ++
    flatten(t)
    )
  end

  # version 2: w/ accumulator
  def flatten2(l), do: flatten2(l, [])
  def flatten2([], acc), do: :lists.reverse(acc)
  def flatten2([h|t], acc) when not is_list(h) do
    flatten2(t, [h|acc])
  end
  def flatten2([h|t], acc) do
    # is there a non-body recursive way to do this?
    :lists.reverse(acc) ++ flatten2(h) ++ flatten2(t)
  end

  def test do
    [1, 2, :two, :too, "tue", 3, 4, 5, 6, :six, "sixx"] = MyList.flatten(
    [ 1, [ 2, :two, :too, "tue", [[[3]]], [4] ], 5, [[[6, :six, "sixx"]]]]
    )
  end

  def test2 do
    [1, 2, :two, :too, "tue", 3, 4, 5, 6, :six, "sixx"] = MyList.flatten2(
    [ 1, [ 2, :two, :too, "tue", [[[3]]], [4] ], 5, [[[6, :six, "sixx"]]]]
    )
  end
end
