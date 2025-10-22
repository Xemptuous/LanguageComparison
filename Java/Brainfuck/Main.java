import java.util.HashMap;
import java.util.Scanner;
import java.util.Stack;

public class Main {
    public static HashMap<Integer, Integer> makeJumpTable(String input) {
        HashMap<Integer, Integer> jump = new HashMap<>();
        Stack<Integer> stack = new Stack<>();

        for (int i = 0; i < input.length(); i++) {
            switch (input.charAt(i)) {
                case '[':
                    stack.push(i);
                    break;
                case ']':
                    Integer idx = stack.pop();
                    jump.put(idx, i);
                    jump.put(i, idx);
            }
        }
        return jump;
    }

    public static void main(String[] args) {
        String input = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";

        HashMap<Integer, Integer> jump = makeJumpTable(input);
        Scanner scanner = new Scanner(System.in);

        byte[] tape = new byte[30000];
        Integer dp = 0;
        Integer ip = 0;

        while (ip < input.length()) {
            switch (input.charAt(ip)) {
                case '>':
                    dp++;
                    break;
                case '<':
                    dp--;
                    break;
                case '+':
                    tape[dp]++;
                    break;
                case '-':
                    tape[dp]--;
                    break;
                case '.':
                    System.out.print((char) tape[dp]);
                    break;
                case ',':
                    tape[dp] = scanner.nextByte();
                    break;
                case '[': {
                    if (tape[dp] == 0) {
                        ip = jump.get(ip);
                    }
                    break;
                }
                case ']': {
                    if (tape[dp] != 0) {
                        ip = jump.get(ip);
                    }
                    break;
                }
            }
            ip++;
        }
    }
}
