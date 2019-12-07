from aocd import get_data
from collections import Counter
from dateutil.parser import parse
from datetime import datetime

data = list(map(lambda x: (int(x.split(", ")[0]), int(x.split(", ")[1])), get_data(day=6, year=2018).split("\n")))
max_x = max(data, key=lambda x: x[0])[0]
max_y = max(data, key=lambda y: y[1])[1]

def calc_manhattan(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

def my_min(coords, coord_b):
    shortest = big_num
    short_list = []
    for coord_a in coords:
        d = calc_manhattan(coord_a, coord_b)
        if d < shortest:
            short_list = [coord_a]
            shortest = d
        elif d == shortest:
            short_list.append(coord_a)
    return short_list

big_num = 99999999
def problem1(data):
    area = {}
    for c in data:
        area[c] = 0

    for y in range(-2 * max_y, max_y * 2):
        closet_coord = min(data, key=lambda coord: calc_manhattan(coord, (big_num, y)))
        area[closet_coord] = -1
        closet_coord = min(data, key=lambda coord: calc_manhattan(coord, (-1 * big_num, y)))
        area[closet_coord] = -1

    for x in range(-2 * max_x, max_x * 2):
        closet_coord = min(data, key=lambda coord: calc_manhattan(coord, (x, big_num)))
        area[closet_coord] = -1
        closet_coord = min(data, key=lambda coord: calc_manhattan(coord, (x, -1 * big_num)))
        area[closet_coord] = -1

    for x in range(max_x + 1):
        for y in range(max_y + 1):
            closet_coords = my_min(data, (x, y))
            if len(closet_coords) == 1:
                close = closet_coords[0]
                if area[close] >= 0:
                    area[close] += 1

    return area

def problem2(coords):
    good_coords = []
    for x in range(-1 * max_x, max_x + 1):
        for y in range(-1 * max_y, max_y + 1):
            its_good = True
            tot_dist = 0
            for coord in coords:
                tot_dist += calc_manhattan(coord, (x, y))
                if tot_dist >= 10000:
                    its_good = False
                    break
            if its_good:
                good_coords.append((x, y)) 
    return good_coords

ans1 = problem1(data)
print(ans1[max(ans1, key=ans1.get)])
print(len(problem2(data)))
