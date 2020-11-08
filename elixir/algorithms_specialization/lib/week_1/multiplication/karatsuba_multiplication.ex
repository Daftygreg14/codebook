defmodule AlgorithmsSpecialization.Week1.Multiplication.KaratsubaMultiplication do
  @moduledoc false

  @spec multi(integer, integer) :: integer
  def multi(0, _), do: 0
  def multi(_, 0), do: 0
  def multi(x, y) when x < 10 or y < 10, do: x * y

  def multi(x, y) do
    m = Enum.max([x, y])
    n = m |> Integer.digits(m) |> length()
    n2 = div(n, 2)

    a = split_first(x, n2)
    b = split_second(x, n2)
    c = split_first(y, n2)
    d = split_second(y, n2)

    a1 = multi(a, c)
    d1 = multi(b, d)
    e1 = multi(a + b, c + d) - a1 - d1

    a1 * :math.pow(10, n) + e1 * :math.pow(10, div(n, 2)) + d1
  end

  defp split_first(x, n2), do: div(x, calculate_pow(n2))
  defp split_second(x, n2), do: rem(x, calculate_pow(n2))

  defp calculate_pow(n2), do: round(:math.pow(10, n2))
end
