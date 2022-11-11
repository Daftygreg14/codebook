# frozen_string_literal: true

require "spec_helper"
require_relative "../../lib/exercise_2/solution"

RSpec.describe "Exercise2::Solution" do
  # Always returns 495 or 0
  # Returns 0 only for integers with same digits
  # Num of iterations is < 6
  # Num of possible combinations is limited

  (1..999).to_a.each do |n|
    it "returns 495 or 0 - number #{n}" do
      solution = Exercise2::Solution.new(n)
      result = solution.run

      expect(solution.iterations).to be <= 6
      expect([495, 0]).to include(result)
    end
  end

  it "debug" do
    number = rand(1..999)
    solution = Exercise2::Solution.new(number, true)
    result = solution.run

    expect(result).to eq(495)
  end

  it "for number with same digits it will return 0" do
    expect(Exercise2::Solution.new(333).run).to eq(0)
  end

  it "for number with different digits it will return 495" do
    expect(Exercise2::Solution.new(123).run).to eq(495)
  end
end
