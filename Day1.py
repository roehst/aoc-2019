def fuel(mass):
    return mass // 3 - 2


def fuel_(mass):
    total = 0
    while mass > 0:
        total += mass
        mass = fuel(mass)
    return total

def read_masses():
    contents = open("Day1.txt", "r").read().strip()
    lines = contents.split("\n")
    weights = [int(line) for line in lines]
    return weights


if __name__ == "__main__":

    masses = read_masses()

    print(sum(map(fuel, masses)))

    # To get the fuel mass only, we subtract the mass afterwards
    print(sum(map(fuel_, masses)) - sum(masses))
