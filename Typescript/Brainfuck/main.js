function makeJumpTable(input) {
    var jump = {};
    var stack = [];
    for (var i = 0; i < input.length; i++) {
        switch (input[i]) {
            case '[':
                stack.push(i);
                break;
            case ']':
                var idx = stack.pop();
                if (idx != undefined) {
                    jump[idx] = i;
                    jump[i] = idx;
                }
        }
    }
    return jump;
}
var input = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
var jump = makeJumpTable(input);
var tape = [];
for (var i = 0; i < 30000; i++)
    tape[i] = 0;
var dp = 0;
var ip = 0;
while (ip < input.length) {
    switch (input[ip]) {
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
            process.stdout.write(String.fromCharCode(tape[dp]));
            break;
        case '[':
            if (tape[dp] == 0)
                ip = jump[ip];
            break;
        case ']':
            if (tape[dp] != 0)
                ip = jump[ip];
            break;
    }
    ip++;
}
