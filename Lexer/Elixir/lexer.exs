Code.require_file("token.exs")

defmodule Lexer do
  defstruct(input: "", curr: 0, peek: 0, char: nil)

  def new(input) do
    %Lexer{input: input}
    |> advance()
  end

  def parse_while(lexer) do
    {token = %Token{type: type}, new_lexer} = next_token(lexer)

    case type do
      :eof ->
        :ok

      :whitespace ->
        parse_while(new_lexer)

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

  def next_token(%Lexer{char: ?*} = lexer) do
    {%Token{literal: "*", type: :asterisk}, advance(lexer)}
  end

  def next_token(%Lexer{char: ?+} = lexer) do
    {%Token{literal: "+", type: :plus}, advance(lexer)}
  end

  def next_token(%Lexer{char: ?*} = lexer) do
    {%Token{literal: "*", type: :asterisk}, advance(lexer)}
  end

  def next_token(%Lexer{char: ?/} = lexer) do
    {%Token{literal: "/", type: :slash}, advance(lexer)}
  end

  def next_token(%Lexer{char: ?-} = lexer) do
    {%Token{literal: "-", type: :dash}, advance(lexer)}
  end

  def next_token(%Lexer{char: ?+} = lexer) do
    {%Token{literal: "+", type: :plus}, advance(lexer)}
  end

  def next_token(%Lexer{char: ?=} = lexer) do
    {%Token{literal: "=", type: :equal}, advance(lexer)}
  end

  def next_token(%Lexer{char: ?<} = lexer) do
    {%Token{literal: "<", type: :lessthan}, advance(lexer)}
  end

  def next_token(%Lexer{char: ?>} = lexer) do
    {%Token{literal: ">", type: :greaterthan}, advance(lexer)}
  end

  def next_token(%Lexer{char: ?;} = lexer) do
    {%Token{literal: ";", type: :semicolon}, advance(lexer)}
  end

  def next_token(%Lexer{char: ?:} = lexer) do
    {%Token{literal: ":", type: :colon}, advance(lexer)}
  end

  def next_token(%Lexer{char: ?(} = lexer) do
    {%Token{literal: "(", type: :lparen}, advance(lexer)}
  end

  def next_token(%Lexer{char: ?)} = lexer) do
    {%Token{literal: ")", type: :rparen}, advance(lexer)}
  end

  def next_token(%Lexer{char: ?{} = lexer) do
    {%Token{literal: "{", type: :lbrace}, advance(lexer)}
  end

  def next_token(%Lexer{char: ?}} = lexer) do
    {%Token{literal: "}", type: :rbrace}, advance(lexer)}
  end

  def next_token(%Lexer{char: c} = lexer) when c in 0..32 do
    {%Token{type: :whitespace}, advance(lexer)}
  end

  # alphabetic
  def next_token(%Lexer{char: c} = lexer) when c in ?a..?z or c in ?A..?Z or c == ?_ do
    {ident, newl} = read_identifier(lexer)
    {%Token{literal: ident, type: :identifier}, newl}
  end

  # numeric
  def next_token(%Lexer{char: c} = lexer) when c in ?0..?9 do
    {ident, newl} = read_number(lexer)
    {%Token{literal: ident, type: :number}, newl}
  end

  # EOF
  def next_token(%Lexer{char: :eof} = lexer) do
    {%Token{type: :eof}, advance(lexer)}
  end

  # all else (illegal)
  def next_token(%Lexer{char: c} = lexer) do
    {%Token{literal: c, type: :illegal}, advance(lexer)}
  end

  # identifier
  def read_identifier(%Lexer{char: c} = lexer) when c in ?a..?z or c in ?A..?Z or c == ?_ do
    read_identifier(lexer, [])
  end

  def read_identifier(%Lexer{char: c} = lexer, acc) when c in ?a..?z or c in ?A..?Z or c == ?_ do
    new_lexer = advance(lexer)
    read_identifier(new_lexer, [c | acc])
  end

  def read_identifier(lexer, acc) do
    {Enum.reverse(acc) |> IO.iodata_to_binary(), lexer}
  end

  # number
  def read_number(%Lexer{char: c} = lexer) when c in ?0..?9 do
    read_number(lexer, [])
  end

  def read_number(%Lexer{char: c} = lexer, acc) when c in ?0..?9 do
    new_lexer = advance(lexer)
    read_number(new_lexer, [c | acc])
  end

  def read_number(lexer, acc) do
    {Enum.reverse(acc), lexer}
  end

  def print_token(%Token{literal: literal, type: type}) do
    IO.puts("Token literal: \"#{literal}\" type: #{type}")
  end

  defp to_char_code(nil), do: nil
  defp to_char_code(<<c::utf8>>), do: c
end
