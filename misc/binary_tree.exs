# http://is.gd/NzZxYT

defmodule BinaryTree do
  def invert([n, list]) when is_integer(n) and is_list(list) do
    [n, invert(list)]
  end
  def invert([n, nc, m, mc])
  when is_integer(n) and is_integer(m)
  and is_list(nc) and is_list(mc) do

    [m, invert(mc), n, invert(nc)]
  end
  def invert([n, m]) when is_integer(n) and is_integer(m) do
    [m, n]
  end
  def invert([n]) when is_integer(n) do
    [n]
  end
  def invert([]) do
    []
  end
end

BinaryTree.invert([4, [2, [1, 3], 7, [6, 9]]])
# => [4, [7, [9, 6], 2, [3, 1]]]
