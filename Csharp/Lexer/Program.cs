string input = @"
fn is_gt_ten(num: int) bool {
   if (num > 10) {
       return true;
   } else {
       return false;
   }
}
let ten = 5 + 5 * 4 / 2 - 5;
print(is_big(ten));";

Lexer lexer = new Lexer(input);

Token tok = lexer.nextToken();
while (tok.type != TokenType.EOF) {
    Console.WriteLine("Token literal:\"{0}\" type: {1}", tok.literal, tok.type);
    tok = lexer.nextToken();
}
