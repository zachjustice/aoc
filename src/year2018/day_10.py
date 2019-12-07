from aocd import get_data, submit1, submit2
import time

import re


class REMatcher(object):
    def __init__(self, matchstring):
        self.matchstring = matchstring

    def match(self,regexp):
        self.rematch = re.match(regexp, self.matchstring)
        return bool(self.rematch)

    def group(self,i):
        return self.rematch.group(i)


def problem1(points, velocities):
    t = 0
    while True:
        i = 0
        minx = 9999999
        miny = 9999999
        maxx = -9999999
        maxy = -9999999
        for p, v in zip(points, velocities):
            x, y = p
            points[i] = (x + v[0], y + v[1])
            i += 1
            if x > maxx:
                maxx = x
            if y > maxy:
                maxy = y
            if x < minx:
                minx = x
            if y < miny:
                miny = y
        t += 1
        if maxx - minx < 100 and maxy - miny < 20:
            display = [["." for _ in range(minx - 5, maxx + 5)] for _ in range(miny - 5, maxy + 5)]

            for x, y in points:
                display[y - miny][x - minx] = "#"
            for y in range(len(display)):
                for x in range(len(display[y])):
                    print(display[y][x], end="")
                print("")

            return t


def main():
    input = get_data(day=10, year=2018).split("\n")
    points = []
    velocities = []
    for i in input:
        reg = REMatcher(i)
        reg.match(r"position=\<\s*(-?\d+),\s*(-?\d+)\> velocity=\<(\s*-?\d+),\s*(-?\d+)\>")
        points.append((int(reg.group(1)), int(reg.group(2))))
        velocities.append((int(reg.group(3)), int(reg.group(4))))

    assert len(points) == len(input) == len(velocities)

    ans = problem1(points, velocities)
    #submit1(ans)

    submit2(ans)

main()
