package main

import (
	"bufio"
	"errors"
	"fmt"
	"os"
)

type Stack []int

func (s *Stack) Push(v int) {
	*s = append(*s, v)
}

func (s *Stack) Pop() (int, bool) {
	if len(*s) == 0 {
		return 0, false
	}
	index := len(*s) - 1
	element := (*s)[index]
	*s = (*s)[:index]
	return element, true
}

func makeJumpTable(input string) (map[int]int, error) {
	jump := make(map[int]int)
	stack := make(Stack, 0)
	for i, ch := range input {
		switch ch {
		case '[':
			stack.Push(i)
		case ']':
			idx, ok := stack.Pop()
			if !ok {
				return jump, errors.New("mismatched brackets")
			}
			jump[idx] = i
			jump[i] = idx
		}
	}
	return jump, nil
}

func main() {
	const input = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
	jump, err := makeJumpTable(input)
	if err != nil {
		fmt.Printf("Err: %v\n", err)
		return
	}

	var (
		tape [300]byte
		dp   int
		ip   int
	)
	for ip < len(input) {
		switch input[ip] {
		case '>':
			dp += 1
		case '<':
			dp -= 1
		case '+':
			tape[dp] = tape[dp] + 1
		case '-':
			tape[dp] = tape[dp] - 1
		case '.':
			fmt.Printf("%c", tape[dp])
		case ',':
			reader := bufio.NewReaderSize(os.Stdin, 1)
			inp, err := reader.ReadByte()
			if err != nil {
				fmt.Printf("Couldn't read input: %v\n", err)
				return
			}
			tape[dp] = inp
		case '[':
			{
				if tape[dp] == 0 {
					ip = jump[ip]
				}
			}
		case ']':
			{
				if tape[dp] != 0 {
					ip = jump[ip]
				}
			}
		}
		ip += 1
	}
}
