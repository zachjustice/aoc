from aocd import get_data
from collections import Counter
data = get_data(day=2, year=2018)

def problem1(data):
    twos = 0
    threes = 0

    for d in data.split("\n"):
        c = Counter(d.strip(" ").strip("\n"))
        for l in c:
            if (c[l] == 3):
                threes += 1
                break
        for l in c:
            if (c[l] == 2):
                twos += 1
                break

    return twos * threes

def char_diff(s1, s2):
    common = []
    diff = []
    if (len(s1) != len(s2)):
        return (None, [])

    for c1, c2 in zip(s1, s2):
            if (c1 == c2):
                common.append(c1)
            else:
                diff.extend([c1, c2])

    return (common, diff)

def problem2(data):
    arr = data.split("\n")
    for d1 in arr:
        for d2 in arr:
            common, diff = char_diff(d1, d2)
            if (len(diff) == 2):
                return(d1, d2, common, diff)

print(problem2(data))
