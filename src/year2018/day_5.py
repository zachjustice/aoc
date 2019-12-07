from aocd import get_data
from collections import Counter
from dateutil.parser import parse
from datetime import datetime

data = get_data(day=5, year=2018)

def problem1(data):
    list_letters = list(data)
    touched = True
    while touched:
        touched = False
        i = 0
        while i < len(list_letters) - 1:
            l = list_letters[i]
            nextl = list_letters[i + 1]
            if l != nextl and (l.upper() == nextl or l == nextl.upper()):
                touched = True
                del list_letters[i] 
                del list_letters[i] 
            else:
                i += 1
    return len(list_letters)

def problem2(data):
    best = len(data)
    for l in list(map(lambda x: str(chr(x)), range(97, 97 + 26))):
        parsed_data = "".join(data).replace(l, '').replace(l.upper(), '')
        result = problem1(parsed_data)
        if result < best:
            best = result
    return best

answer1 = problem1(data)
print(answer1)
print(problem2(data))
