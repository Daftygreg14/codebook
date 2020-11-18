defmodule AlgorithmsSpecialization.Week2.ClosestPairTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias AlgorithmsSpecialization.Week2.ClosestPair

  describe "find/1" do
    test "returns empty tuple when input empty" do
      assert {} == ClosestPair.find([])
    end

    test "returns empty tuple when one point" do
      assert {} == ClosestPair.find([%{x: 1, y: 1}])
    end

    test "returns two points when only two in input" do
      point_1 = %{x: 1, y: 1}
      point_2 = %{x: 2, y: 2}

      assert {point_1, point_2} == ClosestPair.find([point_1, point_2])
    end

    test "returns two points for list length 3" do
      point_1 = %{x: 1, y: 1}
      point_2 = %{x: 2, y: 2}
      point_3 = %{x: 3, y: 3}

      assert {point_1, point_2} == ClosestPair.find([point_1, point_2, point_3])
    end

    test "returns two points for list with negatives" do
      point_1 = %{x: 1, y: 1}
      point_2 = %{x: -2, y: -2}
      point_3 = %{x: 3, y: 3}

      assert {point_1, point_3} == ClosestPair.find([point_1, point_2, point_3])
    end


    test "returns two points for list with doubles" do
      point_1 = %{x: 1, y: 1}
      point_2 = %{x: 2, y: 2}
      point_3 = %{x: 3, y: 3}
      point_4 = %{x: 4, y: 4}
      point_5 = %{x: 1, y: 1}

      assert {point_1, point_5} == ClosestPair.find([point_1, point_2, point_3, point_4, point_5])
    end

    test "returns two points for list" do
      point_1 = %{x: 1, y: 1}
      point_2 = %{x: 2, y: 2}
      point_3 = %{x: 3, y: 3}
      point_4 = %{x: 4, y: 4}
      point_5 = %{x: 1, y: 4}
      point_6 = %{x: 2, y: 4}

      assert {point_1, point_2} == ClosestPair.find([point_1, point_2, point_3, point_4, point_5, point_6])
    end
  end
end
