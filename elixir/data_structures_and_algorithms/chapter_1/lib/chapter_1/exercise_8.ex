defmodule Chapter1.Exercise8 do
 @size 10

 defstruct line_buffer: [], word_buffer: []

 # add word to words
 # check if words are smaller than size
 # if yes, continue
 # else print words & start new words

 def add(words) do
   add_or_print(%__MODULE__{word_buffer: words})
 end

 def add_or_print(%__MODULE__{line_buffer: line_buffer, word_buffer: []}), do: print_buffer(line_buffer)
 def add_or_print(%__MODULE__{line_buffer: prev_line_buffer, word_buffer: [new_word | words]}) do
   new_word_buffer = String.split(new_word, "", trim: true)
   new_line_buffer = new_word_buffer ++ prev_line_buffer

   if length(new_line_buffer) < @size do
     buffer = %__MODULE__{line_buffer: new_line_buffer, word_buffer: words}
     add_or_print(buffer)
   else
     print_buffer(prev_line_buffer)
     buffer = %__MODULE__{line_buffer: new_word_buffer, word_buffer: words}
     add_or_print(buffer)
   end
 end

 defp print_buffer(buffer) do
   buffer
   |> format_lines_buffor(:l)
   |> Enum.join()
   |> IO.inspect()
 end

 defp format_lines_buffor(lines_buffor, _) when length(lines_buffor) == @size, do: lines_buffor
 defp format_lines_buffor(lines_buffor, :r), do: format_lines_buffor(lines_buffor ++ [" "], :l)
 defp format_lines_buffor(lines_buffor, :l), do: format_lines_buffor([" " | lines_buffor], :r)
end

["abc", "def", "ghi", "jkl", "blablablaa", "alablabl"]
|> Chapter1.Exercise8.add()
