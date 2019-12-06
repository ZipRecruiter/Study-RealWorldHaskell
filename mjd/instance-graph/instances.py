#!/usr/bin/env python3
from collections import defaultdict
import re
import subprocess
import sys

def split_paren_list(s):
    if s == '()':
        return [s]
    if s.startswith('(') and s.endswith(')'):
        s = s[1:-1]
    return s.split(", ")

def first_words_of(ss):
    r = []
    for s in ss:
        words = s.split()
        r.append(words[0])
    return r

def walk(init):
    graph = defaultdict(set)
    styles = {}
    queue = list(init)
    seen = defaultdict(bool)
    while queue:
        cur, queue = queue[0], queue[1:]

        if seen[cur]:
            continue
        else:
            seen[cur] = True

        print("**", cur, file=sys.stderr)
        result = subprocess.run(['ghci'],
                                input=f':set prompt ""\n:info {cur}',
                                encoding='utf8',
                                stdout=subprocess.PIPE,
                                check=True)

        for ln in result.stdout.splitlines():
            if ln.startswith('Prelude> '):
                ln = ln[9:]
            if ln.startswith(" ") or ln.startswith("Leaving GHCi"):
                continue
            print(">", ln, file=sys.stderr)

            if ln.startswith("class ") and ' => ' in ln:
                match = re.match(r'class (.*) => (\w+)', ln)
                left = first_words_of(split_paren_list(match.group(1)))
                right = match.group(2)

                for lclass in left:
                    print("*c*", lclass, "---->", right, file=sys.stderr)
                    styles[lclass] = "class"
                    graph[lclass].add(right)

            elif ln.startswith("instance ") and " => " not in ln:
                if "-- " not in ln:
                    continue
                ln = re.sub(r' *--.*', "", ln)
                try:
                    _, source, target = ln.split(maxsplit=2)
                except:
                    raise Exception("split failed:", ln)
                sources = first_words_of(split_paren_list(source))

                if target.startswith('(') and target != '()':
                    target = (target[1:].split(maxsplit=1))[0]

                if target == '[a]':
                    continue

                styles[target] = "type"
                for s in  sources:
                    print("*i*", s, "---->", target, file=sys.stderr)
                    styles[source] = "class"
                    graph[source].add(target)
                    queue.append(source)
                queue.append(target)

#        break
    return graph, styles

alt_names = {'()': 'Unit', '(->)': 'Function', '(,)': 'Tuple',
             '[]': 'List', }

def style(s):
    if s == "type":
        return 'shape="rect"'
    elif s == "class":
        return 'shape="ellipse"'
    else:
        raise Exception(f"Unknown style '{s}'")

# g is a graph, represented as a dict mapping node names
# to sets of node names.
#
# Whenever we have all three of a → b, b → c, a → c,
# eliminate the third of these, which is redundant

def simplify(g):
    to_delete = []
    for a in g.keys():
        if a not in g: continue
        for b in g[a]:
            if b not in g: continue
            for c in g[b]:
                if c in g[a]:
                    to_delete.append((a, b, c))
    for s, t, u in to_delete:
        if u in g[s]:
            print(f"-- removing {s} → {t} → {u}", file=sys.stderr)
            g[s].remove(u)

if __name__ == '__main__':
    graph, styles = walk(sys.argv[1:])
    simplify(graph)
    print("digraph {")

    seen = {}
    for t in styles.keys():
        name = alt_names.get(t, t)
        seen[name] = True
        print(f"{alt_names.get(t, t)} [{style(styles[t])}]")
    print()

    for label, name in alt_names.items():
        if name in seen:
            print(f'{name} [label="{label}"]')
    print()

    for k, vs in  graph.items():
        for v in vs:
            print(f'{k} -> {alt_names.get(v, v)}')

    print("}")
