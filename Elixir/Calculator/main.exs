defmodule Main do
  def main() do
    # input = IO.gets("Enter: ")
    input = "(3 + 4) * 5 - 1 - 2 + 3 / 2"

    tokens = Tokenizer.tokenize(String.replace(input, "\n", ""))
    IO.inspect(tokens, label: "TOKENS")

    rpn = ShuntingYard.to_rpn(tokens)
    IO.inspect(rpn, label: "RPN")

    result = Parser.parse(rpn, [])
    IO.inspect(result, label: "RESULT")
  end
end

defmodule Parser do
  @operators ["^", "*", "/", "+", "-"]

  def parse([], [result]), do: result

  def parse([op | t], [o1 | [o2 | stack]]) when op in @operators do
    f1 = if(is_float(o1), do: o1, else: to_float(o1))
    f2 = if(is_float(o2), do: o2, else: to_float(o2))
    result = calculate(op, f2, f1)
    parse(t, [result | stack])
  end

  def parse([token | t], stack) do
    parse(t, [token | stack])
  end

  def calculate(op, o1, o2) do
    case op do
      "^" -> :math.pow(o1, o2)
      "*" -> o1 * o2
      "/" -> o1 / o2
      "+" -> o1 + o2
      "-" -> o1 - o2
    end
  end

  defp to_float(str) when is_binary(str) do
    case Float.parse(str) do
      {float, ""} ->
        float

      :error ->
        case Integer.parse(str) do
          {int, ""} -> int * 1.0
          :error -> raise ArgumentError, message: "Cannot convert #{str} to float"
        end
    end
  end
end

defmodule Tokenizer do
  def tokenize(str), do: do_tokenize(String.graphemes(str), "", [])

  defp do_tokenize([], current, acc) do
    acc ++ if current != "", do: [current], else: []
  end

  defp do_tokenize([h | t], current, acc) do
    cond do
      h in [" ", "\t"] ->
        do_tokenize(t, "", acc ++ if(current != "", do: [current], else: []))

      h =~ ~r/^\d*\.?\d*$/ ->
        do_tokenize(t, current <> h, acc)

      h in ["+", "-", "*", "/", "^", "(", ")"] ->
        acc = acc ++ if current != "", do: [current], else: []
        do_tokenize(t, "", acc ++ [h])

      true ->
        raise "Unknown character: |#{h}|"
    end
  end
end

defmodule ShuntingYard do
  @precedences %{"^" => 4, "*" => 3, "/" => 3, "+" => 2, "-" => 2}
  @right_associative ["^"]

  def to_rpn(tokens), do: rpn(tokens, [], [])

  defp rpn([], output, []), do: Enum.reverse(output)
  defp rpn([], output, [op | rest]), do: rpn([], [op | output], rest)

  defp rpn([token | rest], output, stack) do
    cond do
      number?(token) ->
        rpn(rest, [token | output], stack)

      operator?(token) ->
        {stack1, output1} = unwind_operators(token, stack, output)
        rpn(rest, output1, [token | stack1])

      token == "(" ->
        rpn(rest, output, ["(" | stack])

      token == ")" ->
        {stack1, output1} = unwind_until_left_paren(stack, output)
        rpn(rest, output1, stack1)

      true ->
        raise "Unknown token: #{token}"
    end
  end

  defp unwind_operators(op, [top | rest] = stack, output)
       when top != "(" do
    if should_pop?(op, top) do
      unwind_operators(op, rest, [top | output])
    else
      {stack, output}
    end
  end

  defp unwind_operators(_, stack, output), do: {stack, output}

  defp unwind_until_left_paren(["(" | rest], output), do: {rest, output}

  defp unwind_until_left_paren([top | rest], output),
    do: unwind_until_left_paren(rest, [top | output])

  defp should_pop?(op1, op2) do
    p1 = @precedences[op1]
    p2 = @precedences[op2]

    p2 && (p2 > p1 or (p2 == p1 and not right_associative?(op1)))
  end

  defp right_associative?(op), do: op in @right_associative
  defp operator?(op), do: Map.has_key?(@precedences, op)
  defp number?(token), do: token =~ ~r/^\d+\.?\d*$/
end
