local function makeJumpTable(input)
    local jump = {}
    local stack = {}
    for i = 0, #input - 1 do
        if input:sub(i, i) == "[" then
            table.insert(stack, i)
        elseif input:sub(i, i) == "]" then
            local idx = table.remove(stack, #stack)
            jump[i] = idx
            jump[idx] = i
        end
    end
    return jump
end

local input = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

local jump = makeJumpTable(input)

local tape = {}
for i = 0, 30000 do
    tape[i] = 0
end

local dp = 0
local ip = 0

while ip < #input do
    local c = input:sub(ip, ip)
    if c == ">" then
        dp = dp + 1
    elseif c == "<" then
        dp = dp - 1
    elseif c == "+" then
        tape[dp] = tape[dp] + 1
    elseif c == "-" then
        tape[dp] = tape[dp] - 1
    elseif c == "." then
        io.write(string.char(tape[dp]))
    elseif c == "[" then
        if tape[dp] == 0 then ip = jump[ip] end
    elseif c == "]" then
        if tape[dp] ~= 0 then ip = jump[ip] end
    end
    ip = ip + 1
end
