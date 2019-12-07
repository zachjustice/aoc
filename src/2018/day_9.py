from aocd import get_data, submit1, submit2
from collections import deque, defaultdict

import re


class REMatcher(object):
    def __init__(self, matchstring):
        self.matchstring = matchstring

    def match(self,regexp):
        self.rematch = re.match(regexp, self.matchstring)
        return bool(self.rematch)

    def group(self,i):
        return self.rematch.group(i)


def problem1(num_players, num_marbles):
    marbles = [0, 1]
    curr_marble_index = 1
    curr_player = 2

    score = {}
    for i in range(num_players):
        score[i] = 0

    for marble in range(2, num_marbles):
        if marble % 23 > 0:
            curr_marble_index += 2
            if curr_marble_index > len(marbles):
                curr_marble_index = curr_marble_index % len(marbles)

            marbles.insert(curr_marble_index, marble)
        else:
            score[curr_player] += marble
            curr_marble_index = (curr_marble_index - 7) % len(marbles)
            score[curr_player] += marbles.pop(curr_marble_index)
        curr_player = (curr_player + 1) % num_players
    return max(score.values())


def problem2(num_players, num_marbles):
    marbles = deque([0])
    score = defaultdict(int)

    for marble in range(1, num_marbles):
        if marble % 23 > 0:
            marbles.rotate(-1)
            marbles.append(marble)
        else:
            marbles.rotate(7)
            score[marble % num_players] += marble + marbles.pop()
            marbles.rotate(-1)
    return max(score.values())

def main():
    input = get_data(day=9, year=2018)
    re_matcher = REMatcher(input)
    re_matcher.match(r"(\d+) players; last marble is worth (\d+) points")

    num_players = int(re_matcher.group(1))
    num_marbles = int(re_matcher.group(2)) + 1

    ans = problem1(num_players, num_marbles)
    submit1(ans)

    ans = problem2(num_players, num_marbles*100)
    submit2(ans)

main()
