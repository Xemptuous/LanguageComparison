Code.require_file("token.exs")
Code.require_file("lexer.exs")
# import Token

defmodule Main do
  def main do
    input = """
    fn is_gt_ten(num: int) bool {
          if (num > 10) {
              return true;
          } else {
              return false;
          }
      }
      let ten = 5 + 5 * 4 / 2 - 5;
      print(is_big(ten));
    """

    lex = Lexer.new(input)
    Lexer.parse_while(lex)
  end
end

# Main.main()
