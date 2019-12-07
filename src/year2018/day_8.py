from aocd import get_data, submit1, submit2


class Node(object):
    def __init__(self):
        self.children = []
        self.metadata = []

    def __str__(self):
        return str([c.__str__() for c in self.children]) + " " + str(self.metadata)


def parse_data(arr, num_nodes, calc_metadata):
    # print("parse data with", num_nodes, "node(s)", arr)
    next_index = 0
    return_nodes = []

    if num_nodes == 0:
        return [], 0

    for _ in range(num_nodes):
        node = Node()
        num_children = arr[next_index]
        len_metadata = arr[next_index + 1]
        # print("  parsing at", next_index, "with", num_children, "children and", len_metadata, "metadata")
        next_index += 2

        children, delta = parse_data(arr[next_index:], num_children, calc_metadata)
        next_index += delta
        node.children = children

        metadata_indices = arr[next_index:next_index + len_metadata]
        # print("  curr node:", len(node.children), node.metadata)
        metadata = calc_metadata(node, metadata_indices)

        node.metadata = metadata
        next_index += len_metadata
        return_nodes.append(node)

    return return_nodes, next_index


def problem1(root, add_func):
    q = [root]
    visited = set()
    tot = 0
    while len(q) > 0:
        curr = q.pop()
        visited.add(curr)
        tot += add_func(curr.metadata)
        for c in curr.children:
            if c not in visited:
                q.append(c)
    return tot


def sum_metadata(node, metadata):
    return sum(metadata)


def calc_metadata_indices(node, metadata_indices):
    metadata = 0
    num_children = len(node.children)
    for mindex in metadata_indices:
        if num_children > 0:
            mindex -= 1
            if mindex < len(node.children):
                metadata += node.children[mindex].metadata
        else:
            metadata += mindex
    return metadata


data = get_data(day=8, year=2018).split(" ")
data = list(map(lambda x: int(x), data))

nodes, _ = parse_data(data, 1, sum_metadata)
ans = problem1(nodes[0], lambda x: x)

nodes, _ = parse_data(data, 1, calc_metadata_indices)
submit2(nodes[0].metadata)
