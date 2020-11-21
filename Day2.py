class Machine:

    def __init__(self):
        self._ip = 0
        self._tape = []

    def write_direct(self, addr, value):
        self._tape[addr] = value

    def read_direct_offset(self, offset):
        return self._tape[self._ip + offset]

    def read_indirect_offset(self, offset):
        return self._tape[self._tape[self._ip + offset]]

    def read_direct(self, addr):
        return self._tape[addr]

    def read_indirect(self, addr):
        return self._tape[self._tape[addr]]

    def run(self, program, a, b):

        self._ip = 0

        self._tape = program[:]

        self.write_direct(1, a)
        self.write_direct(2, b)

        while self._ip < len(program):
            op = self.read_direct_offset(0)
            if op == 1:
                x = self.read_indirect_offset(1)
                y = self.read_indirect_offset(2)
                z = self.read_direct_offset(3)
                self.write_direct(z, x + y)
                self._ip += 4
            elif op == 2:
                x = self.read_indirect_offset(1)
                y = self.read_indirect_offset(2)
                z = self.read_direct_offset(3)
                self.write_direct(z, x * y)
                self._ip += 4
            elif op == 99:
                return self.read_direct(0)

def parse(path):
    return [
        int(word)
        for word in open(path, "r").read().split(",")
    ]

def solve(tape, inputs, goal):

    e = Machine()

    for (a, b) in inputs:
        if e.run(tape, a, b) == goal:
            return (a, b, 100*a + b)


if __name__ == "__main__":

    code = parse("Day2.txt")

    machine = Machine()

    print("Part 1:")
    print(machine.run(code, 12, 2))

    inputs = [
        (i, j) for i in range(100) for j in range(100)
    ]

    print("Part 2:")
    print(solve(code, inputs, 19690720))

