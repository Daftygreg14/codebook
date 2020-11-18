defmodule AlgorithmsSpecialization.Week2.ClosestPair do
  @moduledoc false

  @type point :: %{x: integer, y: integer}
  @type pair :: {point, point}

  @spec find([point]) :: pair
  def find(points) when length(points) <= 1, do: {}
  def find([p1, p2]), do: {p1, p2}

  def find(points) do
    x_sorted = Enum.sort_by(points, & &1.x)
    y_sorted = Enum.sort_by(points, & &1.y)

    {_, pair} = closes_pair(x_sorted, y_sorted)
    pair
  end

  defp closes_pair([p1, p2], _), do: {distance({p1, p2}), {p1, p2}}

  defp closes_pair([p1, p2, p3], _) do
    [
      {distance({p1, p2}), {p1, p2}},
      {distance({p1, p3}), {p1, p3}},
      {distance({p2, p3}), {p2, p3}}
    ]
    |> min_by_distance()
  end

  defp min_by_distance(points_with_distance) do
    Enum.min_by(points_with_distance, fn {dist, _} ->
      dist
    end)
  end

  defp closes_pair(px, py) do
    n2 = length(px) |> div(2) |> round()

    {lx, rx} = Enum.split(px, n2)
    {ly, ry} = Enum.split(py, n2)

    {dl, pl} = closes_pair(lx, ly)
    {dr, pr} = closes_pair(rx, ry)

    {d, p} = min_by_distance([{dl, pl}, {dr, pr}])
    sy = split_by_dist(px, py, d)

    merge(sy, {d, p})
  end

  defp distance({%{x: p0x, y: p0y}, %{x: p1x, y: p1y}}) do
    :math.sqrt((p1x - p0x) * (p1x - p0x) + (p1y - p0y) * (p1y - p0y))
  end

  defp split_by_dist(px, py, s) do
    [%{x: x_bar}] = Enum.take(px, -1)
    Enum.filter(py, fn %{x: x} -> abs(x_bar - x) < s end)
  end

  defp merge(points, acc) when length(points) < 1, do: acc
  defp merge([h | t], acc), do: merge(t, merge_loop(h, t, acc))

  defp merge_loop(_, [], acc), do: acc

  defp merge_loop({p0dist, _}, [{p1_dist, _} | _], {dmin, _} = acc) when dmin <= p1_dist - p0dist,
    do: acc

  defp merge_loop(p0, [p1 | t], {dmin, pair}) do
    dist = distance({p0, p1})

    if dist < dmin do
      merge_loop(p0, t, {dist, {p0, p1}})
    else
      merge_loop(p0, t, {dmin, pair})
    end
  end
end
