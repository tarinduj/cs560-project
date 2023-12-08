We want a program to isolate the rightmost 0
```
Enter some assignments of the form 'name = bitvector', and then the desired output by itself
x = 00
01
Enter another example? (y/n)
n
Examples: Map(Map(x -> 00) -> 01)
76209 programs remain. Smallest: 1
Select the desired output for input Map(x -> 1010101) with 'select n', or enter it manually:
0: 11111111
1: 01011000
2: 01010010
3: 11111000
4: 10101100
5: 10100100
6: 00000110
7: 00000111
8: 11111101
9: 11111110
10: 01011010
11: 10101011
12: 00000000
13: 10101010
14: 11111100
0000010
Selected output: 0000010
1239 programs remain. Smallest: bvAdd(bvAnd(x,1),1)
Select the desired output for input Map(x -> 11111111) with 'select n', or enter it manually:
0: 00000000
1: 00000100
2: 11111110
3: 11111100
4: 00000010
select 0
Selected output: 00000000
358 programs remain. Smallest: bvAnd(bvNot(x),bvAdd(x,1))
Select the desired output for input Map(x -> 1101111) with 'select n', or enter it manually:
0: 00000000
1: 00010000
2: 01010000
select 1
Selected output: 00010000
All programs are equivalent. Smallest: bvAnd(bvNot(x),bvAdd(x,1))
```
