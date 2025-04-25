defmodule Lexer do
  defstruct(input: "", curr: 0, peek: 0, char: nil)

  defmodule Token do
    defstruct literal: "", type: :eof
  end

  @single_token_map %{
    ?! => :exclamation,
    ?@ => :at,
    ?# => :hashtag,
    ?$ => :dollar,
    ?% => :percent,
    ?^ => :caret,
    ?& => :ampersand,
    ?* => :asterisk,
    ?( => :lparen,
    ?) => :rparen,
    ?- => :minus,
    ?_ => :underscore,
    ?+ => :plus,
    ?= => :assign,
    ?[ => :lbracket,
    ?] => :rbracket,
    ?{ => :lbrace,
    ?} => :rbrace,
    ?; => :semicolon,
    ?: => :colon,
    ?' => :char,
    ?" => :string,
    ?, => :comma,
    ?. => :period,
    ?< => :lessthan,
    ?> => :greaterthan,
    ?/ => :slash,
    ?? => :question,
    ?\\ => :backslash,
    ?| => :pipe
  }

  @double_token_map %{
    "==" => :equal,
    "!=" => :not_equal,
    "+=" => :plus_eq,
    "-=" => :minus_eq,
    "*=" => :mult_eq,
    "/=" => :div_eq,
    "<=" => :lt_equal,
    ">=" => :gt_equal,
    "++" => :increment,
    "--" => :decrement,
    "//" => :comment
  }

  @identifier_map %{
    "let" => :let,
    "const" => :const,
    "struct" => :struct,
    "fn" => :function,
    "if" => :if,
    "else" => :else,
    "switch" => :switch,
    "case" => :case,
    "break" => :break,
    "return" => :return,
    "while" => :while,
    "for" => :for,
    "and" => :and,
    "or" => :or,
    "in" => :in,
    "true" => :boolean,
    "false" => :boolean,
    "bool" => :bool,
    "int" => :int,
    "f32" => :f32,
    "str" => :str,
    "nil" => nil,
    "void" => :void
  }

  def new(input) do
    advance(%Lexer{input: input})
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

  def next_token(%Lexer{input: i, curr: c, peek: p, char: ch} = lexer) do
    ds = String.slice(i, c..p)

    cond do
      is_whitespace?(ch) ->
        next_token(advance(lexer))

      ds == "//" ->
        {ident, new_lexer} = read_comment(lexer)
        {%Token{type: :comment, literal: ident}, new_lexer}

      ds in Map.keys(@double_token_map) ->
        token_type = Map.get(@double_token_map, ds)
        {%Token{type: token_type, literal: ds}, advance(advance(lexer))}

      ch == ?" ->
        {ident, new_lexer} = read_string(advance(lexer))
        {%Token{type: :string, literal: ident}, advance(new_lexer)}

      ch == ?' ->
        {ident, new_lexer} = read_char(advance(lexer))
        {%Token{type: :char, literal: ident}, advance(new_lexer)}

      ch in Map.keys(@single_token_map) ->
        token_type = Map.get(@single_token_map, ch)
        {%Token{type: token_type, literal: <<ch>>}, advance(lexer)}

      is_letter?(ch) ->
        {ident, new_lexer} = read_identifier(lexer)
        {%Token{type: Map.get(@identifier_map, ident, :identifier), literal: ident}, new_lexer}

      is_digit?(ch) ->
        {token_type, num, new_lexer} = read_number(lexer)
        {%Token{type: token_type, literal: num}, new_lexer}

      ch == :eof ->
        {%Token{type: :eof}, advance(lexer)}

      true ->
        {%Token{type: :illegal, literal: <<ch>>}, advance(lexer)}
    end
  end

  def read_comment(lexer) do
    read_while(lexer, [], &is_not_crlf/1)
  end

  def read_identifier(lexer) do
    read_while(lexer, [], &is_letter?/1)
  end

  def read_string(lexer) do
    read_while(lexer, [], &(&1 != ?" and is_not_crlf(&1)))
  end

  def read_char(lexer) do
    read_while(lexer, [], &(&1 != ?' and is_not_crlf(&1)))
  end

  def read_number(lexer) do
    {num, new_lexer} = read_while(lexer, [], &is_number?/1)

    {case num |> String.graphemes() |> Enum.count(&(&1 == ".")) do
       0 -> :number
       1 -> :float
       _ -> :illegal
     end, num, new_lexer}
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

  defp is_not_crlf(c), do: c != ?\n and c != ?\r
  defp is_letter?(c), do: c in ?a..?z or c in ?A..?Z or c == ?_
  defp is_digit?(c), do: c in ?0..?9
  defp is_number?(c), do: c in ?0..?9 or c == ?.
  defp is_whitespace?(c), do: c in 0..32
end
