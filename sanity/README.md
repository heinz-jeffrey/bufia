# Artificial Examples

Here we present three artificial examples which may be useful to
familiarize oneself with Bufia.  One is extremely simple, one is of
moderately simple, and the other is moderately more complex.

It is important to test new algorithms on artificial examples so that
one be confident they behave as expected.

The simple example is a five vowel system, where the only attested
forms are "i i" and "a a".  I call this example [Ludibrium
1](#ludibrium-1) (in case any other ludibrius examples are created in
the future).

The next example is [Parupa](#parupa) from Mayer 2020 (full citation
below). It is CV language with 5 vowels and 8 consonants. It has some
constraints on the CV sequences as well as vowel harmony. I randomly
generated 600 forms.

The most complex example has 8 vowels, 19 consonants and 2 glides, is
a (C)V(C) syllable structure, and exhibits both long distance and
local constraints. I call this language [Malarky](#malarky).

# Ludibrium 1

Ludibrium has two words "i i" and "a a".

## Feature system for Ludibrium 1

|      | i | u | e | o | a |
|------|---|---|---|---|---|
| back | - | + | - | + | - |
| low  | - | - | - | - | + |
| high | + | + | - | - | - |


# Parupa

Parupa is the artificial language introduced by Mayer (2020).

> Connor Mayer. An algorithm for learning phonological classes from
> distributional similarity.  Phonology 37 (2020)
> 91-131. doi:10.1017/S0952675720000056

On page 96, Mayer writes:

> Parupa has the distributional properties in (3).
> (3) a. All syllables are CV.
>     b. Vowel harmony: words must contain only front (/i e/) or back (/u o/)
>        vowels. /a/ may occur in either case (i.e. it is transparent to harmony).
>     c. Words must begin with /p/ or /b/.
> d. CV co-occurrence restrictions:
>    * /p t k/ must be followed by high vowels or /a/.
>    * /b d g/ must be followed by mid vowels or /a/.
>    * /r/ may be followed by any vowel.
>    * The full set of consonants is only in contrast before /a/.

Below is a simple feature system for Parupa that I made.

|      | i | u | e | o | a | p | t | k | b | d | g | r |
|------|---|---|---|---|---|---|---|---|---|---|---|---|
| back | - | + | - | + | - | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| low  | - | - | - | - | + | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| high | + | + | - | - | - | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| cons | - | - | - | - | - | + | + | + | + | + | + | + |
| son  | + | + | + | + | + | - | - | - | - | - | - | + |
| voic | + | + | + | + | + | - | - | - | + | + | + | + |
| dor  | 0 | 0 | 0 | 0 | 0 | - | - | + | - | - | + | - |
| lab  | 0 | 0 | 0 | 0 | 0 | + | - | - | + | - | - | - |



# Malarky

1800 words in this language were generated randomly in accordance to the
following constraints:
  - Complex consonant clusters are forbidden (*CCC, *#CC, *CC#).
  - Adjacent obstruents are forbidden.
  - Adjacent vowels are forbidden.
  - Nasal - Voiceless Obstruent clusters are forbidden.
  - Sibilants have to agree in anteriority.
  - Nonlow vowels have to agree in backness.


## Feature system of Malarky
|            | i | u | e | o | a | ɑ | y | ø | p | b | t | d | k | g | s | z | ʃ | ʒ | ts | dz | tʃ | dʒ | m | n | ŋ | l | r | j | w |
|------------|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|----|----|----|----|---|---|---|---|---|---|---|
| high       | + | + | - | - | - | - | + | - | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0  | 0  | 0  | 0  | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| low        | - | - | - | - | + | + | - | - | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0  | 0  | 0  | 0  | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| back       | - | + | - | + | - | + | - | - | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0  | 0  | 0  | 0  | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| round      | - | + | - | + | - | - | + | + | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0  | 0  | 0  | 0  | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| vocalic    | + | + | + | + | + | + | + | + | - | - | - | - | - | - | - | - | - | - | -  | -  | -  | -  | - | - | - | - | - | - | - |
| consonant  | - | - | - | - | - | - | - | - | + | + | + | + | + | + | + | + | + | + | +  | +  | +  | +  | + | + | + | + | + | - | - |
| sonorant   | + | + | + | + | + | + | + | + | - | - | - | - | - | - | - | - | - | - | -  | -  | -  | -  | + | + | + | + | + | + | + |
| continuant | + | + | + | + | + | + | + | + | - | - | - | - | - | - | + | + | + | + | +  | +  | +  | +  | - | - | - | + | + | + | + |
| approx     | + | + | + | + | + | + | + | + | - | - | - | - | - | - | - | - | - | - | -  | -  | -  | -  | - | - | - | + | + | + | + |
| lateral    | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0  | 0  | 0  | 0  | 0 | 0 | 0 | + | 0 | 0 | 0 |
| rhotic     | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0  | 0  | 0  | 0  | 0 | 0 | 0 | 0 | + | 0 | 0 |
| nasal      | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0  | 0  | 0  | 0  | + | + | + | 0 | 0 | 0 | 0 |
| delrel     | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | - | - | - | - | +  | +  | +  | +  | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| labial     | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | + | + | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0  | 0  | 0  | 0  | + | 0 | 0 | 0 | 0 | 0 | + |
| coronal    | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | + | + | 0 | 0 | + | + | + | + | +  | +  | +  | +  | 0 | + | 0 | + | + | + | 0 |
| dorsal     | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | + | + | 0 | 0 | 0 | 0 | 0  | 0  | 0  | 0  | 0 | 0 | + | 0 | 0 | 0 | 0 |
| anterior   | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | + | + | 0 | 0 | + | + | - | - | +  | +  | -  | -  | 0 | + | 0 | + | - | - | 0 |
| voice      | + | + | + | + | + | + | + | + | - | + | - | + | - | + | - | + | - | + | -  | +  | -  | +  | + | + | + | + | + | + | + |
