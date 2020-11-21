def parse(path):
    return [
        int(word)
        for word in open(path, "r").read().split(",")
    ]


def execute(program, a, b):
    program = program[:]
    program[1] = a
    program[2] = b
    ip = 0
    while ip < len(program):

        op = program[ip]

        if op == 1:
            x = program[program[ip+1]]
            y = program[program[ip+2]]
            z = program[ip+3]
            program[z] = x + y
        elif op == 2:
            x = program[program[ip+1]]
            y = program[program[ip+2]]
            z = program[ip+3]
            program[z] = x * y
        elif op == 99:
            return program[0]
        ip += 4

print(execute(parse("Day2.txt"), 12, 2))

program = parse("Day2.txt")

for i in range(100):
    for j in range(100):
        if execute(program, i, j) == 19690720:
            print(i * 100 + j)
    
