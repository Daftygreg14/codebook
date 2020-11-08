defmodule AlgorithmsSpecialization.Week1.Sorting.InsertionSortTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias AlgorithmsSpecialization.Week1.Sorting.InsertionSort

  describe "sort/1" do
    test "returns [] when arg is empty" do
      assert [] == InsertionSort.sort([])
    end

    test "returns arg when arg is one elem long" do
      assert [1] == InsertionSort.sort([1])
    end

    test "sorts list of integers from exercise" do
      assert [1, 2, 3, 4] == InsertionSort.sort([1, 3, 4, 2])
    end

    test "sorts list of positive integer" do
      check all(list <- list_of(positive_integer()), max_runs: 100) do
        assert Enum.sort(list) == InsertionSort.sort(list)
      end
    end

    test "sorts list of integer" do
      check all(list <- list_of(integer()), max_runs: 100) do
        assert Enum.sort(list) == InsertionSort.sort(list)
      end
    end

    test "sorts list of strings" do
      check all(list <- list_of(binary()), max_runs: 100) do
        assert Enum.sort(list) == InsertionSort.sort(list)
      end
    end
  end
end
