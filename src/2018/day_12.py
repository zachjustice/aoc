from aocd import get_data, submit1, submit2


def problem1(state, rules):
    orig_len = len(state)
    for _ in range(20):
        next_state = ""
        state = "...." + state + "...."
        for i in range(2, len(state) - 2):
            next_state += rules[state[i-2:i+3]]
        state = next_state

    retval = 0
    diff = (len(state) - orig_len) / 2
    for s in range(len(state)):
        if state[s] == '#':
            retval += (s - diff)
    return retval


def problem2(state, rules):
    orig_len = len(state)
    totals = []
    for iter in range(1, 10000):
        next_state = ""
        state = "...." + state + "...."
        for i in range(2, len(state) - 2):
            next_state += rules[state[i-2:i+3]]
        state = next_state
        tot = sum_plants(state, orig_len)
        totals.append(tot)
        # check convergence
        if len(totals) > 300:
            diffs = []
            for i in range(30):
                diff = abs(totals[len(totals) - i - 1] - totals[len(totals) - i - 2])
                diffs.append(diff)
            all_same = True
            for d in diffs[1:]:
                if d != diffs[0]:
                    all_same = False
            if all_same:
                return iter, tot, diffs[0]


def sum_plants(state, orig_len):
    retval = 0
    diff = (len(state) - orig_len) / 2
    for s in range(len(state)):
        if state[s] == '#':
            retval += (s - diff)
    return int(retval)


def main():
    input = get_data(day=12, year=2018).split("\n")
    init_state = input[0].split(': ')[1]
    rules_arr = input[2:]
    rules_arr = list(map(lambda x: (x.split(' => ')[0], x.split(' => ')[1]), rules_arr))
    rules = {}
    for r, o in rules_arr:
        rules[r] = o
    ans = problem1(init_state, rules)
    submit1(ans)

    iteration, plant_sum, inc = problem2(init_state, rules)
    ans = int(((50000000000 - iteration) * inc) + plant_sum)
    print(iteration, inc, plant_sum, ans)
    submit2(ans)


main()
