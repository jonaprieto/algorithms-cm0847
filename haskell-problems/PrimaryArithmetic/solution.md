Solution
---

The *sum school algorithm* runs from right to left in order to process the digits of the numbers involved. We can first take the right most digit per each number has taken its *residue* module by ten. Summing these residues and then if this sum is greater or equal ten, we count a carry operation in an accumulator else we don't count. We run *again* the algorithm but this time with the carry and the same numbers after divided each one by ten, and applying the *floor function*. The algorithm finishes when there is nothing to sum.


