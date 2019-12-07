from aocd import get_data, submit1, submit2


def cmp_to_key(mycmp):
    'Convert a cmp= function into a key= function'

    class K(object):
        def __init__(self, obj, *args):
            self.obj = obj

        def __lt__(self, other):
            return mycmp(self.obj, other.obj) < 0

        def __gt__(self, other):
            return mycmp(self.obj, other.obj) > 0

        def __eq__(self, other):
            return mycmp(self.obj, other.obj) == 0

        def __le__(self, other):
            return mycmp(self.obj, other.obj) <= 0

        def __ge__(self, other):
            return mycmp(self.obj, other.obj) >= 0

        def __ne__(self, other):
            return mycmp(self.obj, other.obj) != 0

    return K


def cmp_coord(a, b):
    if a[1] != b[1]:
        return a[1] - b[1]
    else:
        return a[0] - b[0]


dir_to_delta = {
    '<': (-1, 0),
    '>': (1, 0),
    '^': (0, -1),
    'v': (0, 1),
}
turns = ['l', 's', 'r']
track_turn = {
    '+<l': 'v',
    '+>l': '^',
    '+^l': '<',
    '+vl': '>',

    '+<s': '<',
    '+>s': '>',
    '+^s': '^',
    '+vs': 'v',

    '+<r': '^',
    '+>r': 'v',
    '+^r': '>',
    '+vr': '<',

    '/>': '^',
    '/^': '>',
    '/v': '<',
    '/<': 'v',

    '\\>': 'v',
    '\\^': '<',
    '\\v': '>',
    '\\<': '^',
}


def print_tracks(tracks, carts, deleted=[]):
    ncarts = []
    for cart_index in range(len(carts)):
        if cart_index not in deleted:
            ncarts.append(carts[cart_index])

    carts = ncarts
    coords = set(map(lambda c: (c[0], c[1]), carts))
    for y in range(len(tracks)):
        track = tracks[y]
        for x in range(len(tracks[y])):
            if (x, y) in coords:
                cart = list(filter(lambda c: c[0] == x and c[1] == y, carts))[0]
                track = track[:x] + cart[2] + track[x + 1:]
        print(track)


def problem1(tracks, carts):
    while True:
        sorted(carts, key=cmp_to_key(cmp_coord))
        for c_i in range(len(carts)):
            x, y, dir, turn_index = carts[c_i]
            x += dir_to_delta[dir][0]
            y += dir_to_delta[dir][1]

            next_track = tracks[y][x]
            if next_track == '+':
                dir = track_turn['+' + dir + turns[turn_index]]
                turn_index = (turn_index + 1) % len(turns)
            elif next_track == '/' or next_track == '\\':
                dir = track_turn[next_track + dir]

            carts[c_i] = (x, y, dir, turn_index)
            for i in range(1, len(carts)):
                c = carts[(c_i + i) % len(carts)]
                if c[0] == x and c[1] == y:
                    return c[0], c[1]


def problem2(tracks, carts):
    deleted = []
    num_carts = len(carts)
    num_deleted = 0
    while True:
        sorted(carts, key=cmp_to_key(cmp_coord))
        for c_i in range(len(carts)):
            if c_i in deleted:
                continue

            x, y, dir, turn_index = carts[c_i]
            x += dir_to_delta[dir][0]
            y += dir_to_delta[dir][1]

            next_track = tracks[y][x]
            if next_track == '+':
                dir = track_turn['+' + dir + turns[turn_index]]
                turn_index = (turn_index + 1) % len(turns)
            elif next_track == '/' or next_track == '\\':
                dir = track_turn[next_track + dir]

            carts[c_i] = (x, y, dir, turn_index)
            if num_carts - num_deleted == 1:
                print_tracks(tracks, carts, deleted)
                ncarts = []
                for cart_index in range(len(carts)):
                    if cart_index not in deleted:
                        ncarts.append(carts[cart_index])

                print('FINISHED', ncarts)
                return ncarts[0][0], ncarts[0][1]

            for i in range(1, len(carts)):
                index = (c_i + i) % len(carts)
                if index in deleted:
                    continue
                c = carts[index]
                if c[0] == x and c[1] == y:
                    deleted.append(index)
                    deleted.append(c_i)
                    print("{},{}".format(x, y))
                    num_deleted += 2


        # del_count = 0
        # sorted(deleted)
        # for d in deleted:
        #     del carts[d - del_count]
        #     del_count += 1
        # deleted = []

        # print_tracks(tracks, carts, deleted)
        # print("")


def main():
    tracks = []
    for line in open('13.in'):
        if line:
            tracks.append(line.replace('\n', ''))
    # tracks = get_data(day=13, year=2018).split("\n")
    # tracks = list(filter(lambda t: bool(t), tracks))
    carts = []
    for y in range(len(tracks)):
        for x in range(len(tracks[y])):
            if tracks[y][x] == '<':
                carts.append((x, y, tracks[y][x], 0))
                tracks[y] = tracks[y].replace('<', '-', 1)
            elif tracks[y][x] == '>':
                carts.append((x, y, tracks[y][x], 0))
                tracks[y] = tracks[y].replace('>', '-', 1)
            elif tracks[y][x] == 'v':
                carts.append((x, y, tracks[y][x], 0))
                tracks[y] = tracks[y].replace('v', '|', 1)
            elif tracks[y][x] == '^':
                carts.append((x, y, tracks[y][x], 0))
                tracks[y] = tracks[y].replace('^', '|', 1)
    #ans = problem1(tracks, carts)
    #submit1(str(ans[0]) + ',' + str(ans[1]))

    ans = problem2(tracks, carts)
    print(ans)
    # submit2(str(ans[0]) + ',' + str(ans[1]))


main()
