```

 _ ) |  |__|_ _|   \
 _ \ |  |_|   |   _ \
___/\__/_|  ___|_/  _\ 

```

# Overview

This program instantiates the Bottom-Up Factor Inference Algorithm
(BUFIA) [1] for phonotactic learning. Given a list of words, and a
phonological feature system, it will return a list of constraints. The
constraints it returns are guaranteed to have the following
properties.

1. They are all surface true (i.e. no word in the wordlist violates
   any of the constraints)
2. They are pairwise incomparable (i.e. no constraint is structurally
   contained within any other)
3. The program is deterministic, not stochastic. Given the same
   inputs, the same results will be returned every time.

BUFIA's search procedure and some additional guarantees are explained in [1]. 

Additionally, users can choose some other parameter settings to ensure
the following.

3. No constraint subsumes the effects of any other constraint.

See [2] for discussion of abductive principles that are implemented here.

# Background

The Bottom-Up Factor Inference Algorithm (BUFIA) is an algorithm for
identifying forbidden factors in structures [1]. The algorithm is
schematic, in the sense that the user can instantiate different kinds
of representations. As explained in [1], the algorithm utilizes the
fact that representations are partially ordered and it searches factor
space in a bottom-up, breadth-first way which guarantees it only finds
the most general forbidden factors. This document uses the term
*constraint* as a synonym for *forbidden factor*. The terms
*structure, representation* and *factor* are also synonyms here.

The implementation here is specifically for sequential representations
(strings) that are used in phonology. So each element of a sequence is
composed of one or more properties (e.g. phonological features). Two
kinds of factors are implemented, local and piecewise, which
correspond to the successor and predecessor relations in
model-theoretic treatments of words.

# Installation

`bufia` is written in Haskell. Install
[Haskell](https://www.haskell.org/) on your system. I recommend
installing with [GHCup](https://www.haskell.org/ghcup/).

Once Haskell is installed, navigate into the bufia directory and
compile the program

```
$ ghc bufia.lhs
```

On some systems, there may be an error that references bang
patterns. If so please try the following for successful compilation.

```
$ ghc -XBangPatterns bufia.lhs
```

# Usage

Running ./bufia without any arguments will provide basic information
regarding usage.

```
$ ./bufia
Usage: bufia [OPTIONS...] wordfile featurefile
  -k Int                         the max factor width (k-value, k>0, default 3)
  -n Int                         the max number of features in a bundle (n>0, default 3)
  -a Int                         which abductive principle to use {0,1,2} (default 1)
  -f Int                         how feature-values should be ordered {0,1,2} (default 0)
  -m Maybe Int                   the max number of constraints to return (default No max)
  -b Bool                        If 'True' then boundaries '#' are added to all words (default True)
  -o Order                       the order of the word model: 'succ' or 'prec'
                --keep=[String]  a list of features to keep : '["f1","f2","f3"]' (default keep all; incompatible with --drop)
                --drop=[String]  a list of features to drop : '["f1","f2","f3"]' (default drop none; incompatible with --keep)
  -h, -?                         show this help
  -v                             show version number
```

## wordfile (Required)

The wordfile contains a list of words. Each word should be on its own line. 
The symbols which compose the word should be separated by spaces.  Here is an example.

```
d͡ʒ ɛ f
w ʌ z
h i ɹ
```

See the `sanity` folder for more examples.

## featurefile (Required)

The featurefile provides featural information for the symbols used in
the wordlist. It should be a command-delimited file with the symbols
in the first line. Here is an example.

```
,i,u,e,o,a,
back,-,+,-,+,-
low,-,-,-,-,+
high,+,+,-,-,-
```

See the `sanity` folder for more examples.


## -k Int

The `-k` flag determines the maximum width of factors to search. The
larger the number the longer the search will take. The default is 3.

## -n Int

The `-n` flag determines the maximum number of features considered at
each position. In other words, if this is set to 2, then feature
combinations of 3 or more features are not considered. So if this is
set to 2 the factor [+high,+front,+round] will never be
considered. The larger the number the longer the search will take. The
default is 3.

In any case, Bufia will never consider incompatible feature
combinations such as [-voice,+voice] or [+high,+low]. If no symbols
are picked out by some combination of features, that combinations is
never considered.

## -a Int

The `-a` flag determines which abductive principle to use to guide
constraint selection. Currently, the following three are implemented.

* 0 : all pairwise incomparable, surface-true constraints are
  returned. (Even if multiple constraints have the same effect.)
* 1 : A constraint is only added to the grammar if its extension is
  not subsumed by the extension of the current grammar.
* 2 : A constraint is only added to the grammar if its extension is
  disjoint from the extension of the current grammar.

The default is 1. 

For standard phonotactic learning problems, principle 0 returns very
many constraints because the density of the constraint space is so
high. While many of these constraints seem redundant, it is important
to realize that they are all surface true and that they are pairwise
incomparable in factor space. In other words, the empirical data and
the feature theory in use cannot distinguish among the constraints
returned with principle 0.

Abductive principle 1 generally provides many fewer constraints than
principle 0, which facilitates interpretation. The constraints that
are returned are the *earliest* constraints in the search that account
for anything new.

Abductive principle 2 generally returns more constraints than
principle 1 and less than principle 2. The constraints here can become
awfully specific. The constraints that are returned are the *earliest*
constraints in the search whose impacts are *completely* new.

## -f Int

The `-f` flag determines how the search is conducted. A key part of
the BUFIA algorithm is the `nextSupFactors` which takes a factor `x`
and finds those factors which are the *minimal* superfactors of
`x`. In other words, since factor space is partially ordered,
`nextSupFactors` of `x` returns the set `{ y : x < y, ¬(∃z)[ x < z < y
] }`. And in order to conduct the breadth first search, this set of
minimal factors needs to be totally ordered in some way. How to
accomplish this?

If there are pairwise incomparable constraints in the output of
`nextSupFactors` we need a way to order them. The current approaches
order the *features* in some way to resolve this issue. (Here
*feature* refers specifically to a feature-value pair like [+back].)

* 0 : Features are prioritized according to how large their extension
  is. The larger the greater priority. If their extensions are of
  equal size, then they are prioritized according to the order they
  appear in the featurefile (from top to bottom).
* 1 : Features are prioritized according to the order they appear in
  the featurefile (from top to bottom).
* 2 : Features are prioritized according to Haskell's internal
  ordering for strings (e.g. alphabetically)

Broadly speaking, these are additional abductive principles that are
used to guide the search. Hayes and Wilson [3] for example use the 0
option here (see page 394). (In this regard it is worth mentioning
that BUFIA's breadth first search strategy over the partially ordered
factor space guarantees that shorter constraints are always considered
before longer constraints.)

## -m Maybe Int

The `-m` flag determines the maximum number of constraints to be
returned. The default is `None` which means there is no limit to the
number of constraints. If you are only interested in obtaining the
first 100 constraints use `-m 100`.

## -b Bool

The `-b` flag determines whether word boundaries should be added to
both ends of the words in the wordfile. Word boundaries always use the
symbol `"#"`. The default is True. So to turn off word boundaries, use
`-b False`. Note that turning off word boundaries will not remove them
from the feature table.

## -o Order

The `-o` flag determines the ordering relation to use in the
model-theoretic representations of words. The following two orders are
implemented.

* succ : This is the successor relation for constraints that ban
  substrings (cf. Strictly Local formal languages).
* prec : This is the precedence relation for constraints that ban
  subsequences (cf. Strictly Piecewise formal languages).

## --drop='["feature1","feature2","feature3"]'

The `--drop` removes the listed features from consideration. This is
useful when you want to ignore some features.

## --keep='["feature1","feature2","feature3"]'

The `--keep` removes all features except the listed ones from
consideration. This is useful when you want to ignore all but a few
features. For example, when searching for constraints on the
distribution of consonants and vowels it can be useful to just to
focus on just a few features.

## -h, -?

These options just show the basic help.

## -v

Shows the version number.


# References

[1] Jane Chandlee, Rémi Eyraud, Jeffrey Heinz, Adam Jardine, and
Jonathan Rawski. 2019. Learning with Partially Ordered
Representations. In Proceedings of the 16th Meeting on the Mathematics
of Language, pages 91--101. Association for Computational
Linguistics. [ [link](https://www.aclweb.org/anthology/W19-5708) ]

[2] Jonathan Rawski. 2021. Structure and Learning in Natural
Language. Dissertation, Stony Brook University.  [
[link](http://jeffreyheinz.net/advisees/2021_JonRawski_dissertation.pdf)
]

[3] Hayes, Bruce, and Colin Wilson. 2008. A maximum entropy model of
phonotactics and phonotactic learning.  Linguistic Inquiry
39:379–440. [ [link](https://doi.org/10.1162/ling.2008.39.3.379) ]
