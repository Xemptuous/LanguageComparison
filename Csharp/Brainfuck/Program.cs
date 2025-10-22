Dictionary<int, int> MakeJumpTable(string input) {
    Dictionary<int, int> jump = new Dictionary<int, int>();
    Stack<int> stack          = new Stack<int>();

    for (int i = 0; i < input.Length; i++) {
        switch (input[i]) {
            case '[': stack.Push(i); break;
            case ']':
                int idx = stack.Pop();
                jump.Add(i, idx);
                jump.Add(idx, i);
                break;
        }
    }
    return jump;
}

const string input = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---."
                     + "+++++++..+++.>>.<-.<"
                     + ".+++.------.--------.>>+.>++.";
Dictionary<int, int> jump = MakeJumpTable(input);

int[] tape = new int[30000];
int ip     = 0;
int dp     = 0;

while (ip < input.Length) {
    switch (input[ip]) {
        case '>': dp++; break;
        case '<': dp--; break;
        case '+': tape[dp]++; break;
        case '-': tape[dp]--; break;
        case '.': Console.Write((char)tape[dp]); break;
        case ',': tape[dp] = Console.Read(); break;
        case '[':
            if (tape[dp] == 0) ip = jump[ip];
            break;
        case ']':
            if (tape[dp] != 0) ip = jump[ip];
            break;
    }
    ip++;
}
