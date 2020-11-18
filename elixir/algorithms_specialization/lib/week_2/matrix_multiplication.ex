defmodule AlgorithmsSpecialization.Week2.MatrixMultiplication do
  @moduledoc false

  @type matrix :: list(list(integer))

  @spec multi(matrix, matrix) :: matrix
  def multi(m1, m2) do
    {rows_1, columns_1} = size(m1)
    {rows_2, columns_2} = size(m2)

    cond do
      rows_1 != columns_1 || rows_2 != columns_2 ->
        raise "Can not multiply matrices; matrices must be square"

      rows_1 != rows_2 ->
        raise "Can not multiply matrices; matrices must be equal of size"

      rows_1 == 0 || !is_power_of_two(rows_1) ->
        raise "Can not multiply matrices; matrices size must be pow of 2"

      true ->
        do_multi(m1, m2)
    end
  end

  defp is_power_of_two(num) do
    Float.ceil(:math.log2(num)) == Float.floor(:math.log2(num))
  end

  defp size(matrix) do
    num_of_rows = length(matrix)
    [num_of_columns] = matrix |> Enum.map(&length(&1)) |> Enum.uniq()

    {num_of_rows, num_of_columns}
  end

  defp do_multi([[m1]], [[m2]]), do: [[m1 * m2]]

  defp do_multi(m1, m2) do
    # Splitting the matrices into quadrants
    {a11, a12, a21, a22} = to_quadrants(m1)
    {b11, b12, b21, b22} = to_quadrants(m2)

    # Computing the 7 products, recursively (p1, p2...p7)
    p1 = do_multi(a11, subtract(b12, b22))
    p2 = do_multi(add(a11, a12), b22)
    p3 = do_multi(add(a21, a22), b11)
    p4 = do_multi(a22, subtract(b21, b11))
    p5 = do_multi(add(a11, a22), add(b11, b22))
    p6 = do_multi(subtract(a12, a22), add(b21, b22))
    p7 = do_multi(subtract(a11, a21), add(b11, b12))

    # Computing the values of the 4 quadrants of the final matrix c
    c11 = add(add(p5, p6), subtract(p4, p2))
    c12 = add(p1, p2)
    c21 = add(p3, p4)
    c22 = add(subtract(p5, p3), subtract(p1, p7))

    from_quadrants([c11, c12, c21, c22])
  end

  defp to_quadrants(m) do
    r = length(m)
    r_half = div(r, 2) |> round()

    h1 = Enum.take(m, r_half)
    h2 = Enum.take(m, -r_half)

    q1 = h1 |> Enum.map(&Enum.take(&1, r_half))
    q2 = h1 |> Enum.map(&Enum.take(&1, -r_half))
    q3 = h2 |> Enum.map(&Enum.take(&1, r_half))
    q4 = h2 |> Enum.map(&Enum.take(&1, -r_half))

    {q1, q2, q3, q4}
  end

  defp add(m1, m2), do: :lists.zipwith(&add_row/2, m1, m2)
  defp add_row(x, y), do: :lists.zipwith(&:erlang.+/2, x, y)

  defp subtract(m1, m2), do: :lists.zipwith(&subtract_row/2, m1, m2)
  defp subtract_row(x, y), do: :lists.zipwith(&:erlang.-/2, x, y)

  defp from_quadrants(m_list) do
    m_list = List.flatten(m_list)
    rows_or_columns = :math.pow(length(m_list), 0.5) |> round()
    range = 0..(rows_or_columns - 1)

    Enum.reduce(range, [], fn row, row_acc ->
      row_value =
        Enum.reduce(range, [], fn column, col_acc2 ->
          offset = calculate_value_offset(row, column)
          value = Enum.at(m_list, offset)
          [value | col_acc2]
        end)
        |> Enum.reverse()

      [row_value | row_acc]
    end)
    |> Enum.reverse()
  end

  defp calculate_value_offset(row, column) do
    div(row, 2) * 8 + rem(row, 2) * 2 + div(column, 2) * 2 + column
  end
end
