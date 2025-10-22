import tables

proc makeJumpTable(input: string): Table[int, int] =
    var jump = initTable[int, int]()
    var stack: seq[int]
    for i, char in input:
        case char
        of '[': stack.add(i)
        of ']': 
            var idx = stack.pop()
            jump[idx] = i
            jump[i] = idx
        else:
            discard
    return jump


proc main() =
    let input = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    let jump = makeJumpTable(input)

    var tape: array[0..30_000, int]
    var dp: int = 0
    var ip: int = 0

    while ip < len(input):
        case input[ip]
        of '>': dp += 1
        of '<': dp -= 1
        of '+': tape[dp] += 1
        of '-': tape[dp] -= 1
        of '.': stdout.write(tape[dp].char)
        of '[':
            if tape[dp] == 0:
                ip = jump[ip]
        of ']':
            if tape[dp] != 0:
                ip = jump[ip]
        else: discard
        ip += 1

main()

