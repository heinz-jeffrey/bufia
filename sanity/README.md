
# Artificial Examples

Here we present two artificial examples which may be useful
to familiarize oneself with Bufia.
One is extremely simple and the other is more complex.

The simple example is a five vowel system,
where the only attested form are "i i" and "a a".
I call this example Ludibrium.

The more complex example has 8 vowels, 19 consonants
and 2 glides. Words in this language were generated randomly
in accordance to the following constraints:
  - Complex consonant clusters are forbidden (*CCC, *#CC, *CC#).
  - Adjacent obstruents are forbidden.
  - Adjacent vowels are forbidden.
  - Nasal - Voiceless obstruent clusters are forbidden.
  - Sibilants have to agree in anteriority.
  - Nonlow vowels have to agree in backness.
I call this language Malarky.

# Ludibrium

## Feature system for Ludibrium

|      | i | u | e | o | a |
|------|---|---|---|---|---|
| back | - | + | - | + | - |
| low  | - | - | - | - | + |
| high | + | + | - | - | - |



# Malarky

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

