Code.require_file("token.exs")

defmodule Lexer do
  defstruct(input: "", curr: 0, peek: 0, char: nil)

  @single_token_map %{
    ?+ => :plus,
    ?- => :dash,
    ?* => :asterisk,
    ?+ => :plus,
    ?* => :asterisk,
    ?/ => :slash,
    ?- => :dash,
    ?+ => :plus,
    ?= => :equal,
    ?< => :lessthan,
    ?> => :greaterthan,
    ?; => :semicolon,
    ?: => :colon,
    ?( => :lparen,
    ?) => :rparen,
    ?{ => :lbrace,
    ?} => :rbrace
  }

  @identifier_map %{
    "let" => :let,
    "fn" => :function,
    "if" => :if,
    "else" => :else,
    "return" => :return,
    "true" => :boolean,
    "false" => :boolean,
    "bool" => :bool,
    "int" => :int
  }

  def new(input) do
    %Lexer{input: input}
    |> advance()
  end

  def parse_while(lexer) do
    {token = %Token{type: type}, new_lexer} = next_token(lexer)

    case type do
      :eof ->
        :ok

      _ ->
        print_token(token)
        parse_while(new_lexer)
    end
  end

  def advance(%Lexer{input: input, peek: p} = lexer) do
    if p >= String.length(input) do
      %Lexer{lexer | char: :eof}
    else
      %Lexer{
        lexer
        | curr: p,
          peek: p + 1,
          char: String.at(input, p) |> to_char_code()
      }
    end
  end

  def next_token(%Lexer{char: c} = lexer) do
    cond do
      is_whitespace?(c) ->
        next_token(advance(lexer))

      is_letter?(c) ->
        {ident, new_lexer} = read_identifier(lexer)
        {%Token{type: Map.get(@identifier_map, ident, :identifier), literal: ident}, new_lexer}

      is_digit?(c) ->
        {num, new_lexer} = read_number(lexer)
        {%Token{type: :int, literal: num}, new_lexer}

      c in Map.keys(@single_token_map) ->
        token_type = Map.get(@single_token_map, c)
        {%Token{type: token_type, literal: <<c>>}, advance(lexer)}

      c == :eof ->
        {%Token{type: :eof}, advance(lexer)}

      true ->
        {%Token{type: :illegal, literal: <<c>>}, advance(lexer)}
    end
  end

  def read_identifier(lexer) do
    read_while(lexer, [], &is_letter?/1)
  end

  def read_number(lexer) do
    read_while(lexer, [], &is_digit?/1)
  end

  defp read_while(%Lexer{} = lexer, acc, predicate) do
    if predicate.(lexer.char) do
      read_while(advance(lexer), [lexer.char | acc], predicate)
    else
      {Enum.reverse(acc) |> IO.iodata_to_binary(), lexer}
    end
  end

  def print_token(%Token{literal: literal, type: type}) do
    IO.puts("Token literal: \"#{literal}\" type: #{type}")
  end

  defp to_char_code(nil), do: nil
  defp to_char_code(<<c::utf8>>), do: c

  defp is_letter?(c), do: c in ?a..?z or c in ?A..?Z or c == ?_
  defp is_digit?(c), do: c in ?0..?9
  defp is_whitespace?(c), do: c in 0..32
end
