# Program:   fst sampler
# Copyright: (c) 2021-2022 Jeffrey Heinz
# License:   MIT

# This program sample uses pynini (Gorman 2016, Gorman and Sproat
# 2021) to sample words of various lengths from a fst

# Usage: python sample.py file.fst n min max
# It reads an fst file and randomly generates n strings whose lengths
# are between min and max inclusive and writes them to standard
# output.

import sys
import pynini

fstfile  = sys.argv[1]
n        = int(sys.argv[2])
lmin     = int(sys.argv[3])
lmax     = int(sys.argv[4])

the_fsa  = pynini.Fst.read(fstfile)
symtable = the_fsa.input_symbols()
i1 = iter(symtable)
next(i1) # this skips over epsilon which is always
         # the first entry in the symbol table

a = next(i1) # this is a pair like (1,'a')

def A(s):
    return pynini.acceptor(s, token_type=symtable)

x = A(a[1])
zero = x-x
zero.optimize()

i2 = iter(symtable)
next(i2) # this skips over epsilon which is always
            # the first entry in the symbol table
alph = ''
for sympair in i2:  # the table entries are pairs of the form (num,symbol)
    alph = alph + sympair[1]

sigma = zero
for c in alph:
    sigma = A(c) | sigma
sigma.optimize()

fsa_dict = {}
for i in range(lmin, lmax+1):
    fsa_dict[i] = pynini.optimize(pynini.intersect(the_fsa, pynini.closure(sigma, i, i)))

def print_string_set(fsa):
    my_list = []
    paths = fsa.paths(input_token_type=symtable, output_token_type=symtable)
    for s in paths.ostrings():
        print(s)


# Here is the main call of this script.

for i in range(lmin, lmax+1):
    i_fsa = pynini.randgen(fsa_dict[i], npath=n,
                           seed=0,
                           select="uniform",
                           max_length=2147483647,
                           weighted=False)
    print_string_set(i_fsa)
