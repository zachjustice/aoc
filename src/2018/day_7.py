from aocd import get_data, submit1, submit2
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


lines = get_data(day=7, year=2018).split("\n")
g = {}
children = set()
sources = set()
sinks = set()

alph_map = list(map(lambda x: str(chr(x)), range(65, 65 + 26)))
# alph_map = ['A','B','C','D','E','F']

for c in alph_map:
    g[c] = {"children": set(), "parents": set()}

for l in lines:
    reMatcher = REMatcher(l)
    reMatcher.match(r"Step (\w+) must be finished before step (\w+) can begin.")

    first = reMatcher.group(1)
    second = reMatcher.group(2)

    g[first]['children'].add(second)
    g[second]['parents'].add(first)
    children.add(second)

sources = set(alph_map) - children
for node in g:
    if len(g[node]["children"]) == 0:
        sinks.add(node)

def problem1(g, sources, sinks):
    order = ""
    q = sorted(sources)
    visited = set()
    while len(q) > 0:
        q.sort()
        i = 0
        curr_length = len(q)
        while i < curr_length: # look at work queue and determine if anything can be done
            curr_step = q[i]
            prereqs = g[curr_step]['parents']
            if len(prereqs.intersection(visited)) == len(prereqs): # we've acquired all prereqs for this step
                visited.add(curr_step) # visit this step
                order += curr_step # add step to order visited
                q.pop(i) # remove from q
                curr_length -= 1
                for child in g[curr_step]['children']:
                    if child not in visited and child not in q:
                        q.append(child) # we can now look the this step's children
                break
            else: # don't have all prereqs
                i += 1 # try the next one
    return order

def calc_time(x):
    return ord(str(x).upper()) - ord('A') + 61

def work(g, q, processing_nodes, processed_steps, curr_workers, order):
    for node in processing_nodes:
        value = processing_nodes[node]
        if value is not None:
            if value < calc_time(node):
                processing_nodes[node] += 1
            if processing_nodes[node] == calc_time(node):
                processed_steps.add(node)
                curr_workers -= 1
                order += node
                processing_nodes[node] = None
                for child in g[node]['children']:
                    if child not in q and child not in processing_nodes:
                        q.append(child) # we can now look the this step's children
    return g, q, processing_nodes, processed_steps, curr_workers, order 

def problem2(g, sources, sinks):
    order = ""
    q = sorted(sources)
    processed_steps = set() # steps which have been completed
    processing_nodes = {} # keep track of steps as they're worked on {'A': 1}
    tot_workers = 5
    curr_workers = 0
    seconds = 0
    while len(q) > 0 or curr_workers > 0:
        g, q, processing_nodes, processed_steps, curr_workers, order = work(g, q, processing_nodes, processed_steps, curr_workers, order) # work on each node in process
        #print(seconds, processing_nodes, q, processed_steps)
        q.sort()
        i = 0
        curr_length = len(q)
        while i < curr_length and curr_workers < tot_workers: # look at work queue and determine if anything can be done
            curr_step = q[i]
            prereqs = g[curr_step]['parents']
            if len(prereqs.intersection(processed_steps)) == len(prereqs): # we've acquired all prereqs for this step
                processing_nodes[curr_step] = 0
                q.pop(i) # remove from q
                curr_length -= 1
                if curr_workers < tot_workers:
                    curr_workers += 1
                if curr_workers == tot_workers:
                    break
            else: # don't have all prereqs
                i += 1 # try the next one
        seconds += 1
    return seconds - 1

submit1(problem1(g, sources, sinks))
submit2(problem2(g, sources, sinks))
