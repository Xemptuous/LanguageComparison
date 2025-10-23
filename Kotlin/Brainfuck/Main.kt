fun makeJumpTable(input: String): HashMap<Int, Int> {
    val jump = hashMapOf<Int, Int>();
    val stack = ArrayDeque<Int>();

    for ((i, ch) in input.withIndex()) {
        when (ch) {
            '[' -> stack.addLast(i)
            ']' -> {
                val idx = stack.removeLast();
                jump.put(i, idx);
                jump.put(idx, i);
            }
        }
    }
    return jump;

}

@OptIn(ExperimentalUnsignedTypes::class)
fun main() {
	val input = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
    val jump = makeJumpTable(input);
    var tape = UByteArray(30000);
    var ip = 0;
    var dp = 0;

    while (ip < input.length) {
        when (input[ip]) {
            '>' -> dp++
            '<' -> dp--
            '+' -> tape[dp]++
            '-' -> tape[dp]--
            '.' -> print(tape[dp].toInt().toChar())
            ',' -> {
                val inputString = readln();
                val uByteValue: UByte? = inputString.toIntOrNull()?.let { intValue -> 
                    if (intValue in 0..255) {
                        intValue.toUByte();
                    } else {
                        null;
                    }
                }
                if (uByteValue != null) {
                    tape[dp] = uByteValue;
                }
            }
            '[' -> {
                if (tape[dp].toInt() == 0) {
                    ip = jump[ip] ?: 0;
                }
            }
            ']' -> {
                if (tape[dp].toInt() != 0) {
                    ip = jump[ip] ?: 0;
                }
            }
        }
        ip++;
    }
}
