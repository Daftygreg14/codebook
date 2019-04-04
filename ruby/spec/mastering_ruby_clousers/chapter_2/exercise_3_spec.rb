# frozen_string_literal: true

require_relative "../../../mastering_ruby_closures/chapter_2/exercise_3"

RSpec.describe "my file" do
  it "works like map with strings" do
    path = File.expand_path("spec/fixtures/test.txt")
    letters = []

    MyFile.open(path, "r") do |file|
      file.each_line do |line|
        letters << line.chomp
      end
    end

    expect(letters).to eq(%w[a b c d])
  end
end
