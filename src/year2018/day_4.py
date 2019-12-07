from aocd import get_data
from collections import Counter
from dateutil.parser import parse
from datetime import datetime
import re

class REMatcher(object):
    def __init__(self, matchstring):
        self.matchstring = matchstring

    def match(self,regexp):
        self.rematch = re.match(regexp, self.matchstring)
        return bool(self.rematch)

    def group(self,i):
        return self.rematch.group(i)

data = get_data(day=4, year=2018)

# sort strings chronologically
# count which gaurd has the most sleep
# form hashmap { gaurd : { minute asleep : days asleep for that minute } }
def problem1(data):
    events = []
    for s in data.split("\n"):
        date_str = s[s.index("[") + 1 : s.index("]")]
        d = parse(date_str)
        events.append((d, s[s.index("]") + 2 :]))
    hmap = {}
    events = sorted(events, key=lambda e: e[0])
    i = 0
    curr_guard_id = None
    for date, event_str in events:
        if "shift" in event_str:
            m = REMatcher(event_str)
            m.match(r"Guard #(\d+) begins shift.*")
            gaurd_id = m.group(1)
            curr_guard_id = gaurd_id
            if curr_guard_id not in hmap:
                hmap[ gaurd_id ] = {"total": 0, "minutes_asleep": {}, "id": gaurd_id}
        elif "falls asleep" in event_str:
            start = date
            end = events[i + 1][0]
            hmap[curr_guard_id]["total"] += (end - start).seconds
            for minute in range(start.minute, end.minute):
                if minute not in hmap[curr_guard_id]["minutes_asleep"]:
                    hmap[curr_guard_id]["minutes_asleep"][minute] = 0
                hmap[curr_guard_id]["minutes_asleep"][minute] += 1
        i += 1

    sleepiest = sorted(list(hmap.values()), key=lambda x: x['total'], reverse=True)[0]
    sleepiest_minute = (-1, -1)
    for this_minute in sleepiest["minutes_asleep"]:
        minute, days = sleepiest_minute
        this_days = sleepiest["minutes_asleep"][this_minute]
        if this_days > days:
            sleepiest_minute = (this_minute, this_days)

    return int(sleepiest["id"]) * sleepiest_minute[0], hmap

def problem2(hmap):
    sleepiest_minute = (-1, -1, -1)
    for gaurd_id in hmap:
        sleepiest = hmap[gaurd_id]
        for this_minute in sleepiest["minutes_asleep"]:
            _, minute, days = sleepiest_minute
            this_days = sleepiest["minutes_asleep"][this_minute]
            if this_days > days:
                sleepiest_minute = (gaurd_id, this_minute, this_days)
    return int(sleepiest_minute[0]) * sleepiest_minute[1]

answer1, hmap = problem1(data)
print(answer1)
print(problem2(hmap))
