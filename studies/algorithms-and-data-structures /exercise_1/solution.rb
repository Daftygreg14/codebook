#!/usr/bin/env ruby

def prepare_input(n)
  whites = [:white] * n
  blacks = n.odd? ? [:black] * n : [:black] * (n + 1)
  whites + blacks
end

def fetch_sample(set)
  set.delete_at(rand(set.size))
end

def solution(input)
  raise StandardError if !input.filter { |el| el.eql?(:black) }.size.odd?
  do_solution(input)
end

def do_solution(input)
  return input.first if input.size <= 1

  # Drops two samples from array
  sample_1 = fetch_sample(input)
  sample_2 = fetch_sample(input)

  # Recursion
  if sample_1 == sample_2
    do_solution(input)
  else
    new_input = input << :black
    do_solution(new_input)
  end
end

result = (0..1_000).map do |_|
  m = (n = rand(1_000)).odd? ? n : n + 1
  input = prepare_input(m)
  solution(input)
end.uniq

puts "Values: #{result.inspect}"