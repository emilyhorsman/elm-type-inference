import os
import sys


with open(sys.argv[1], 'r') as f:
    lines = f.readlines()

in_comment = False
output = []
for line in lines:
    if line.startswith('{-|'):
        in_comment = True
        continue
    elif line.strip() == '-}':
        in_comment = False
        continue
    elif in_comment:
        continue
    elif 'type' in line:
        continue

    if line.count(':') == 1:
        output.append('"{}"'.format(line.strip()))

print("-- GENERATED WITH FilterTypeAnnotations.py {} --".format(sys.argv[1]))
print("-- DO NOT EDIT MANUALLY --")
print("module Elm.{} where\n".format(os.path.splitext(os.path.basename(sys.argv[1]))[0]))
print("source :: [String]")
print("source = [")
for index, line in enumerate(output):
    print("  ", line, end='')
    if index < len(output) - 1:
        print(",")
print("]")
