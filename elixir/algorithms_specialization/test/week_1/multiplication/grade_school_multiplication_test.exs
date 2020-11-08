defmodule AlgorithmsSpecialization.Week1.Multiplication.GradeSchoolMultiplicationTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias AlgorithmsSpecialization.Week1.Multiplication.GradeSchoolMultiplication

  describe "multi/2" do
    test "returns 0 when first arg is 0" do
      assert 0 == GradeSchoolMultiplication.multi(0, 1)
    end

    test "returns 0 when second arg is 0" do
      assert 0 == GradeSchoolMultiplication.multi(1, 0)
    end

    test "multiplicities two integer" do
      check all(int1 <- positive_integer(), int2 <- positive_integer(), max_runs: 100) do
        assert GradeSchoolMultiplication.multi(int1, int2) == int1 * int2
      end
    end
  end
end
