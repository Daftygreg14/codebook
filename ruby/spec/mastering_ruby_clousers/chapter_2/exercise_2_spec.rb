# frozen_string_literal: true

require_relative "../../../mastering_ruby_closures/chapter_2/exercise_2"

RSpec.describe "Exercise 2" do
  it "works like map with strings" do
    string = "some other world"
    arr = []
    string.each_word do |x|
      arr << x.capitalize
    end

    expect(arr).to eq(["Some", "Other", "World"])
  end
end
