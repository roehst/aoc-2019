def fuel(mass):
    return mass // 3 - 2


def fuel_recursive(mass, fuel_requirement=0):
    if mass > 0:
        return fuel_recursive(fuel(mass), fuel_requirement + mass)
    else:
        return fuel_requirement


def read_masses():
    contents = open("Day1.txt", "r").read().strip()
    lines = contents.split("\n")
    weights = [int(line) for line in lines]
    return weights


if __name__ == "__main__":

    masses = read_masses()

    print(sum(map(fuel, masses)))

    # To get the fuel mass only, we subtract the mass afterwards
    print(sum(map(fuel_recursive, masses)) - sum(masses))
