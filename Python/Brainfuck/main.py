def make_jump_table(input: str) -> dict[int, int]:
    jump = {}
    stack = []

    for i, ch in enumerate(input):
        match ch:
            case "[":
                stack.append(i)
            case "]":
                idx = stack.pop()
                jump[idx] = i
                jump[i] = idx
    return jump


if __name__ == "__main__":
    input = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    jump = make_jump_table(input)

    tape = [0 for _ in range(30_000)]
    dp = 0
    ip = 0

    while ip < len(input):
        match input[ip]:
            case ">":
                dp += 1
            case "<":
                dp -= 1
            case "+":
                tape[dp] += 1
            case "-":
                tape[dp] -= 1
            case ".":
                print(chr(tape[dp]), end="")
            case "[":
                if tape[dp] == 0:
                    ip = jump[ip]
            case "]":
                if tape[dp] != 0:
                    ip = jump[ip]
        ip += 1
