def selection_sort(array)
  current_minimum = 0
  while current_minimum < 6
    smallest_value_index = find_smallest_value_index(array, current_minimum)
    array[current_minimum], array[smallest_value_index] = array[smallest_value_index], array[current_minimum]
    current_minimum += 1
  end
  return array.join("")
end

def find_smallest_value_index(array, current_minimum)
  smallest_value = array[current_minimum]
  smallest_index = current_minimum
  while current_minimum < array.length
    if array[current_minimum] < smallest_value
      smallest_value = array[current_minimum]
      smallest_index = current_minimum
    end
    current_minimum += 1
  end
  return smallest_index
end
input = "MATEMATYZACJA".split("")
print selection_sort(input)[0..8]

AAAACETY