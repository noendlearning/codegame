#!/user/bin/env pythonimport sys
import math
import time
# Auto-generated code below aims at helping you parse

# the standard input according to the problem statement.
""" 
l = int(input())
h = int(input())
t = input()
for i in range(h):
    row = input()

# Write an action using print

# To debug: print("Debug messages...", file=sys.stderr)

print("answer") """
time.sleep(4)
l = int(input())
h = int(input())
t = input()
t = t.upper()
charList = list(t)
letter = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
for i in range(h):
    row = input()
    portion = ''
    for arr in charList:
        index = letter.find(arr)
        if index > -1:
            portion += row[(index+1)*l-l:(index+1)*l]
        else:
            portion += row[26*l:26*l+l]
    print(portion)
