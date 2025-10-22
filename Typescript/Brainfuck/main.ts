function makeJumpTable(input: string) {
  let jump: { [key: number]: number; } = {};
  let stack: number[] = [];
  for (let i = 0; i < input.length; i++) {
    switch (input[i]) {
      case '[':
        stack.push(i);
        break;
      case ']':
        let idx = stack.pop();
        if (idx != undefined) {
          jump[idx] = i;
          jump[i] = idx;
        }
    }
  }
  return jump;
}



const input: string = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";

let jump = makeJumpTable(input);
let tape: number[] = [];
for (let i = 0; i < 30_000; i++)
  tape[i] = 0;

let dp = 0;
let ip = 0;

while (ip < input.length) {
  switch (input[ip]) {
    case '>': dp++; break;
    case '<': dp--; break;
    case '+': tape[dp]++; break;
    case '-': tape[dp]--; break;
    case '.': process.stdout.write(String.fromCharCode(tape[dp])); break;
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

