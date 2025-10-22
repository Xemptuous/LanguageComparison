#include <cstdint>
#include <iostream>
#include <stack>
#include <string>
#include <unordered_map>

using namespace std;

typedef unordered_map<int, int> JumpTable;

JumpTable make_jump_make(string input) {
    JumpTable jump;
    stack<int> s;
    for (int i = 0; i < input.length(); i++) {
        switch (input[i]) {
            case '[': s.push(i); break;
            case ']': {
                int idx = s.top();
                s.pop();
                jump[idx] = i;
                jump[i]   = idx;
                break;
            }
        }
    }
    return jump;
}

int main() {
    const string input = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>"
                         "---.+++++++..+++.>>.<"
                         "-.<.+++.------.--------.>>+.>++.";
    const JumpTable jump = make_jump_make(input);

    static uint8_t tape[3000];
    int ip = 0;
    int dp = 0;

    while (ip < input.length()) {
        switch (input[ip]) {
            case '>': dp += 1; break;
            case '<': dp -= 1; break;
            case '+': tape[dp] += 1; break;
            case '-': tape[dp] -= 1; break;
            case '.': cout << tape[dp]; break;
            case ',':
                uint8_t i;
                cin >> i;
                tape[dp] = i;
                break;
            case '[':
                if (tape[dp] == 0) ip = jump.at(ip);
                break;
            case ']':
                if (tape[dp] != 0) ip = jump.at(ip);
                break;
        }
        ip += 1;
    }
}
