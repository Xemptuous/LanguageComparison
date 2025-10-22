import std.stdio;
import std.range;

int[int] makeJumpMap(string input) {
    int[int] jump;
    int[] stack;

    for (int i = 0; i < input.length; i++) {
        switch (input[i]) {
        case '[':
            stack ~= i;
            break;
        case ']':
            int idx = stack.back;
            stack.popBack();
            jump[i] = idx;
            jump[idx] = i;
            break;
        default:
            break;
        }
    }
    return jump;
}

void main() {
    string input = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.++"
        ~ "+++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";

    int[int] jump = makeJumpMap(input);
    int[30_000] tape;
    int dp = 0;
    int ip = 0;

    while (ip < input.length) {
        switch (input[ip]) {
        case '>':
            dp += 1;
            break;
        case '<':
            dp -= 1;
            break;
        case '+':
            tape[dp] += 1;
            break;
        case '-':
            tape[dp] -= 1;
            break;
        case '.':
            write(cast(char) tape[dp]);
            break;
        case '[':
            if (tape[dp] == 0) {
                ip = jump[ip];
            }
            break;
        case ']':
            if (tape[dp] != 0) {
                ip = jump[ip];
            }
            break;
        default:
            break;
        }
        ip += 1;
    }
}
