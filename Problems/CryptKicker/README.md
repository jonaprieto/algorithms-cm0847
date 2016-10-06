Solution
----

#### A little description of the problem.

Input: We have a list of words of our language.
The cipher is a permutation of the alphabet.
Additionly, in the input, the problem asks us,
to find if it is possible the original text of a list
of ciphered lines.

We notice that if it was ciphered correctly, each word in
the ciphered text corresponds to a valid word of our language.

Therefore, we need to figure out if that correspondence
exists and its valid (the cipher is a permutation of the alphabet).

Explain in detail the solution is out of the scope. Essentially,
the solution is a something better than a brute force.

I used the hash map as a predominant data structure to solve this problem.
I have two maps.

  - `Cipher ∷ Map Char Char`. This represents the permutation.
  - `Dict ∷ Map Wrd Wrd`. This represents the dictionary to do
  the translation.

So, the solve function is `decipher`.

```Haskell
decipher ∷  [Wrd] → Map Wrd [Wrd] → Maybe Dict → Maybe Cipher →  Maybe Dict
decipher words@(w:ws) mapa dict cipher = solution
```
- `words` contains the list of words that we need to decipher
- `mapa` has the possible words that a word `w` can have in the language.
- `dict` has the current dictionary for translation that it's building.
- `cipher` has the current permuation that was used to build the current `dict`.

Then, the first call to `decipher` is an empty dict and empty cipher, and
a well-build `mapa` taking into account the input of the problem. The
list `words` is provide by the input as well.
