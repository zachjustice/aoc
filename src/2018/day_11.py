from aocd import get_data, submit1, submit2
import sys


def make_grid(grid_id):
    grid = [[0 for _ in range(300)] for _ in range(300)]
    for x in range(0, 300):
        for y in range(0, 300):
            rack_id = x + 11
            str_num = str(((rack_id * (y + 1)) + grid_id) * rack_id)
            if len(str_num) >= 3:
                grid[x][y] = int(str_num[-3])
            else:
                grid[x][y] = 0
            grid[x][y] -= 5
    return grid


def problem1(grid):
    # answer =[[0 for _ in range(300)] for _ in range(300)]
    max = 0
    coord = (1,1)
    for i in range(300-2):
        for j in range(300-2):
            answer = grid[i][j] \
                + grid[i][j+1] \
                + grid[i][j+2] \
                + grid[i+1][j] \
                + grid[i+1][j+1] \
                + grid[i+1][j+2] \
                + grid[i+2][j] \
                + grid[i+2][j+1] \
                + grid[i+2][j+2]
            if answer > max:
                coord = (i + 1, j + 1)
                max = answer
    return coord


def problem2(grid):
    max = 0
    coord = (1, 1, 1)
    for size in range(0,300):
        print("size", size, flush=True)
        for i in range(300 - size - 1):
            for j in range(300 - size - 1):
                power = calculate_power(grid, i, j, size)
                if power > max:
                    coord = (i + 1, j + 1, size)
                    max = power
        if size - coord[2] > 10:
            return coord
    return coord

def calculate_power(grid, i, j, size):
    power = 0
    for x in range(size):
        for y in range(size):
            power += grid[i + x][j + y]
    return power


def main():
    input = int(get_data(day=11, year=2018))
    grid = make_grid(input)
    ans = problem1(grid)
    submit1(str(ans[0]) + "," + str(ans[1]))

    ans = problem2(grid)
    submit2(str(ans[0]) + "," + str(ans[1]) + "," + str(ans[2]))


main()
