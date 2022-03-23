#!/usr/bin/env python
"""Samples words of various lengths from an FST.
This program sample uses Pynini (Gorman 2016, Gorman and Sproat 2021) to sample
words of various lengths from a FST. It reads an fst file and randomly
generates n strings whose lengths are between min and max inclusive and writes
them to standard output."""
# Program:   fst sampler
# Copyright: (c) 2022 Kyle Gorman
# License:   MIT


import argparse
import pynini
from pynini.lib import rewrite


# Seed for reproducability.
# Set to 0 if repriducability is not desired.
SEED = 11215


def _make_sigma(symbols: pynini.SymbolTable) -> pynini.Fst:
    # One could of course do this via union.
    sigma = pynini.Fst()
    sigma.add_states(2)
    sigma.set_start(0)
    for idx, _ in symbols:
        # Skips over epsilon.
        if not idx:
            continue
        sigma.add_arc(0, pynini.Arc(idx, idx, None, 1))
    sigma.set_final(1)
    return sigma


def main(args: argparse.Namespace) -> None:
    fst = pynini.Fst.read(args.fst)
    # We usually use the convention that if there's just one symbol table,
    # it's the output table, so I followed that here.
    symbols = fst.output_symbols()
    assert symbols, "expected an output symbol table"
    # Computes sigma so we can control length.
    sigma = _make_sigma(symbols)
    # Actually output stuff.
    for i in range(args.min, args.max + 1):
        fsa = pynini.intersect(fst, pynini.closure(sigma, i, i)).optimize()
        rand_fsa = pynini.randgen(fsa, npath=args.n, seed=SEED)
        for string in rewrite.lattice_to_strings(rand_fsa, token_type=symbols):
            print(string)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("fst", help="input FST file")
    parser.add_argument("n", type=int, help="number of strings to generate per length")
    parser.add_argument(
        "min", type=int, help="minimum path length (inclusive)"
    )
    parser.add_argument(
        "max", type=int, help="maximum path length (inclusive)"
    )
    main(parser.parse_args())
