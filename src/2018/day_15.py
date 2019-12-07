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


def cmp_fighter(a, b):
    if a.y != b.y:
        return a.y - b.y
    else:
        return a.x - b.x


def cmp_coord(a, b):
    if a[1] != b[1]:
        return a[1] - b[1]
    else:
        return a[0] - b[0]


def calc_manhattan(a, b):
    return abs(a[0] - b[0]) + abs(a[1] - b[1])


def world_to_str(world, targets=[], me=(-1, -1)):
    retval = ''
    cave = world.cave
    pos = {}
    fighters = sorted(world.elves + world.goblins, key=cmp_to_key(cmp_fighter))

    for f in fighters:
        if f.is_alive():
            pos[(f.x, f.y)] = f

    for y in range(len(cave)):
        for x in range(len(cave[y])):
            if (x, y) in targets:
                retval += 'X'
            elif (x, y) == me:
                retval += 'M'
            elif (x, y) in pos:
                retval += pos[(x, y)].type
            else:
                retval += cave[y][x]

        retval += '    '
        for f in fighters:
            if f.y == y:
                retval += ', {0} {1}'.format(f.type, f.hp)

        retval += '\n'
    retval += '\n'
    return retval


class World(object):
    def __init__(self, cave, elves, goblins):
        self.cave = cave
        self.elves = elves
        self.goblins = goblins
        self.taken_spaces = {}
        self.calculate_taken_spaces()

    def __str__(self):
        return world_to_str(self)

    def calculate_taken_spaces(self):
        self.taken_spaces = {}
        for f in self.elves + self.goblins:
            if f.is_alive():
                self.taken_spaces[(f.x, f.y)] = f

    def clean_up(self):
        self.elves = list(filter(lambda e: e.is_alive(), self.goblins))
        self.goblins = list(filter(lambda g: g.is_alive(), self.goblins))


def get_next(cave, taken_spaces, curr_coord):
    x, y = curr_coord
    poss_moves = [(x + dx, y + dy) for dx, dy in [(0, -1), (-1, 0), (1, 0), (0, 1)]]
    next_moves = []
    for move in poss_moves:
        next_x, next_y = move
        if move not in taken_spaces \
                and 0 <= next_y < len(cave) \
                and 0 <= next_x < len(cave[next_y]) \
                and cave[next_y][next_x] == '.':
            next_moves.append(move)
    return next_moves


class Fighter(object):
    def __init__(self, type, x, y):
        self.type = type
        self.x = x
        self.y = y
        self.hp = 200
        self.atk = 3
        self.target = None
        self.path = []

    def __str__(self):
        return str((self.x, self.y, self.type))

    def __eq__(self, other):
        return other is not None and self.x == other.x and self.y == other.y and self.type == other.type

    def is_dead(self):
        return self.hp <= 0

    def is_alive(self):
        return self.hp > 0

    def take_action(self, world):
        if self.is_dead():
            return

        # attack if we're in range
        if self.target is not None \
                and self.target.is_alive() \
                and calc_manhattan((self.x, self.y), (self.target.x, self.target.y)) == 1:
            self.attack(self.target)
        else:
            targets = self.find_targets(world)
            path, target = self.find_paths(world, targets)
            self.target = target
            self.path = path

            if len(self.path) > 0 and self.path[0] not in world.taken_spaces:
                self.x, self.y = self.path.pop(0)

            # attack if we're in range
            if self.target is not None and calc_manhattan((self.x, self.y), (self.target.x, self.target.y)) == 1:
                self.attack(self.target)

    def find_targets(self, world):
        # find targets
        targets = world.elves if self.type == 'G' else world.goblins
        targets = list(filter(lambda t: t.is_alive(), targets))
        taken_spaces = world.taken_spaces.copy()
        del taken_spaces[(self.x, self.y)]

        # find reachable spaces
        closest_targets = {}
        for t in targets:
            for coord in [(0, -1), (-1, 0), (1, 0), (0, 1)]:
                new_x, new_y = (t.x + coord[0], t.y + coord[1])
                new_coord = (new_x, new_y)
                if new_coord not in taken_spaces \
                        and 0 <= new_y < len(world.cave) \
                        and 0 <= new_x < len(world.cave[new_y]) \
                        and world.cave[new_y][new_x] == '.':
                    closest_targets[new_coord] = t

        return closest_targets

    def find_paths(self, world, targets):
        # print('targets', targets)
        # print(print_world(world, me=(self.x, self.y), targets=targets))
        if len(targets) == 0:
            return [], None

        if (self.x, self.y) in targets:
            return [], targets[(self.x, self.y)]

        q = list(map(lambda p: (p[0], p[1], [(p[0], p[1])]), get_next(world.cave, world.taken_spaces, (self.x, self.y))))
        visited = set()
        possible_paths = []
        # bfs to target
        while len(q) > 0:
            x, y, path = q.pop(0)
            curr_coord = (x, y)
            if curr_coord in targets:
                return path, targets[curr_coord]
                # if len(possible_paths) > 0 and len(possible_paths[0]) != len(path):
                #     # quit when we find a path that's longer than the paths we've acquired
                #     return possible_paths
                # possible_paths.append((path, targets[curr_coord]))
            next_moves = get_next(world.cave, world.taken_spaces, curr_coord)
            for move in next_moves:
                if move not in visited:
                    visited.add(move)
                    new_path = path + [move]
                    q.append((move[0], move[1], new_path))

        return [], None

    def attack(self, other):
        other.hp -= self.atk


def problem1(world):
    num_turns = 0
    while len(world.elves) > 0 and len(world.goblins) > 0:

        # world.clean_up()
        world.calculate_taken_spaces()
        fighters = sorted(world.goblins + world.elves, key=cmp_to_key(cmp_fighter))
        for f in fighters:
            f.take_action(world)

        num_turns += 1

        print('AFTER {0} ROUNDS'.format(num_turns),
              '. elves alive:', len(list(filter(lambda e: e.is_alive(), world.elves))),
              '. gobs alive:', len(list(filter(lambda e: e.is_alive(), world.goblins))))
        print(world_to_str(world))

        if len(list(filter(lambda e: e.is_alive(), world.elves))) == 0:
            tot_hp = 0
            for g in world.goblins:
                tot_hp += g.hp
            print('Gobs win', num_turns, tot_hp, num_turns * tot_hp)
            return num_turns * tot_hp

        if len(list(filter(lambda g: g.is_alive(), world.goblins))) == 0:
            tot_hp = 0
            for g in world.elves:
                tot_hp += g.hp
            print('Elves win', num_turns, tot_hp, num_turns * tot_hp)
            return num_turns * tot_hp

    return world


def problem2(world):
    return


def main():
    input = get_data(day=15, year=2018).split('\n')
    input = '''\
#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######'''.split('\n')
    cave = [['.' for _ in range(len(input[y]))] for y in range(len(input))]
    elves = []
    goblins = []

    for y in range(len(input)):
        for x in range(len(input[y])):
            if input[y][x] == '#':
                cave[y][x] = '#'
            elif input[y][x] == '.':
                cave[y][x] = '.'
            elif input[y][x] == 'E':
                elves.append(Fighter('E', x, y))
                cave[y][x] = '.'
            elif input[y][x] == 'G':
                goblins.append(Fighter('G', x, y))
                cave[y][x] = '.'

    world = World(cave=cave, elves=elves, goblins=goblins)
    print(world)
    ans = problem1(world)
    print('answer:', ans)
    # submit1(ans)

    # ans = problem2('37', data)
    # print(ans)
    # submit2(ans)


main()
