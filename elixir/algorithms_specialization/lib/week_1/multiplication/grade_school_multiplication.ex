defmodule AlgorithmsSpecialization.Week1.Multiplication.GradeSchoolMultiplication do
  @moduledoc false

  require IEx
  @spec multi(integer, integer) :: integer
  def multi(0, _), do: 0
  def multi(_, 0), do: 0

  def multi(int1, int2) do
    digits1 = Integer.digits(int1) |> Enum.reverse()
    digits2 = Integer.digits(int2) |> Enum.reverse()

    digits1
    |> Enum.with_index()
    |> Enum.reduce(0, fn n1, acc1 ->
      digits2
      |> Enum.with_index()
      |> Enum.reduce(acc1, fn n2, acc2 ->
        result = do_calculation(n1, n2)
        acc2 + result
      end)
    end)
  end

  defp do_calculation({digit1, index1}, {digit2, index2}) do
    prod = digit1 * digit2
    power = :math.pow(10, index1) * :math.pow(10, index2)
    prod * power
  end
end
