from aocd import get_data, submit1, submit2


def problem1(scoreboard, iters):
    elf_pos = [0, 1]
    while len(scoreboard) != iters + 10:
        newscore = 0
        for e in range(len(elf_pos)):
            newscore += int(scoreboard[elf_pos[e]])
        scoreboard += str(newscore)
        for e in range(len(elf_pos)):
            elf_pos[e] = (elf_pos[e] + 1 + int(scoreboard[elf_pos[e]])) % len(scoreboard)

    print(len(scoreboard))
    return scoreboard[iters:iters + 10]


def problem2(scoreboard, iters):
    str_len = len(str(iters))
    elf_pos = [0, 1]
    while scoreboard[-1 * str_len:] != str(iters) and scoreboard[-1 * str_len - 1:-1] != str(iters):
        newscore = 0
        for e in range(len(elf_pos)):
            newscore += int(scoreboard[elf_pos[e]])
        scoreboard += str(newscore)
        for e in range(len(elf_pos)):
            elf_pos[e] = (elf_pos[e] + 1 + int(scoreboard[elf_pos[e]])) % len(scoreboard)

    print(len(scoreboard))
    if scoreboard[-1 * str_len:] != str(iters):
        print(scoreboard[-1 * str_len - 1:])
        return len(scoreboard[:-1 * str_len]) - 1
    else:
        print(scoreboard[-1 * str_len - 1:])
        return len(scoreboard[:-1 * str_len - 1]) - 1


def main():
    data = get_data(day=14, year=2018)
    #ans = problem1('37', int(data))
    #print(ans)
    #submit1(ans)

    ans = problem2('37', data)
    print(ans)
    #submit2(ans)


main()
