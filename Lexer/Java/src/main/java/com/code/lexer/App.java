package com.code.lexer;

import com.code.lexer.Token.TokenType;

public class App {
    public static void main(String[] args) {
        String input = "fn is_gt_ten(num: int) bool {\n"
                + "if (num > 10) {\n"
                + "return true;\n"
                + "} else {\n"
                + "return false;\n"
                + "}\n"
                + "}\n"
                + "let ten = 5 + 5 * 4 / 2 - 5;\n"
                + "print(is_big(ten));\n";
        Lexer lexer = new Lexer(input);
        Token tok = lexer.nextToken();
        while (tok.token_type != TokenType.EOF) {
            System.out.println(tok.toString());
            tok = lexer.nextToken();
        }
    }
}
