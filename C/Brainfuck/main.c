#include <stdint.h>
#include <stdio.h>
#include <string.h>

int main() {
    char* input = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++"
                  "++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
    uint8_t tape[30000];
    int ip = 0;
    int dp = 0;
    int c  = 0;

    while (ip < strlen(input)) {
        switch (input[ip]) {
            case '>': dp++; break;
            case '<': dp--; break;
            case '+': tape[dp]++; break;
            case '-': tape[dp]--; break;
            case '.': printf("%c", (char)tape[dp]); break;
            case ',': {
                uint8_t input;
                scanf(" %hhu", &input);
                tape[dp] = input;
            }; break;
            case '[':
                if (tape[dp] == 0) {
                    c++;
                    while (input[ip] != ']' || c != 0) {
                        ip++;
                        if (input[ip] == '[') c++;
                        else if (input[ip] == ']') c--;
                    }
                }
                break;
            case ']':
                if (tape[dp] != 0) {
                    c++;
                    while (input[ip] != '[' || c != 0) {
                        ip--;
                        if (input[ip] == ']') c++;
                        else if (input[ip] == '[') c--;
                    }
                }
                break;
        }
        ip++;
    }
}
