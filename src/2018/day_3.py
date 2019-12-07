from aocd import get_data
from collections import Counter

data = get_data(day=3, year=2018)

class Rectangle:
    def __init__(self, x1, y1, w, h, id_str):
        self.x1 = x1
        self.x2 = x1 + w
        self.y1 = y1
        self.y2 = y1 + h
        self.width = w
        self.height = h
        self.id = id_str

    def __str__(self):
        return str((self.x1, self.y1, self.width, self.height))

def area_of_intersection(rect1, rect2):
    x_overlap = max(0, min(rect1.x2, rect2.x2) - max(rect1.x1, rect2.x1));
    y_overlap = max(0, min(rect1.y2, rect2.y2) - max(rect1.y1, rect2.y1));
    r = None
    if (x_overlap > 0 and y_overlap > 0 ):
        r = Rectangle(
            max(rect1.x1, rect2.x1),
            max(rect1.y1, rect2.y1),
            x_overlap,
            y_overlap,
            0
        )

    return (x_overlap * y_overlap, r);

def make_rectangle(s):
    s = s.replace(" ", "")
    id_str, coords_demins = s.split("@")
    coords, dimens = coords_demins.split(":")
    x1, y1 = coords.split(",")
    w, h = dimens.split("x")
    return Rectangle(int(x1), int(y1), int(w), int(h), id_str)

def add_intersection(rect, intersection):
    for x in range(rect.x1, rect.x2):
        for y in range(rect.y1, rect.y2):
            intersection.add((x, y))
    return intersection

def problem1(data):
    rectangles = list(sorted(map(make_rectangle, data.split("\n")), key=lambda r: r.x1))
    intersections = set()
    for i in range(len(rectangles)):
        for j in range(i + 1, len(rectangles)):
            area, rect = area_of_intersection(
                rectangles[i],
                rectangles[j]
            )

            if rect is not None:
                intersections = add_intersection(rect, intersections)

    return intersections, rectangles

def promblem2(rectangles, intersections):
    for rect in rectangles:
        alone = True
        for x in range(rect.x1, rect.x2):
            for y in range(rect.y1, rect.y2):
                if (x, y) in intersections:
                    alone = False
        if alone:
            return rect
intersections, rectangles = problem1(data)
print(len(intersections))
print(promblem2(rectangles, intersections).id[1:])
