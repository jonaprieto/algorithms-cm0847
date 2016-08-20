Solution
---

The *sum school algorithm* runs from right to left in order to process the digits of the numbers involved. We can first take the right most digit per each number, and then we take the sum of *residues* module by ten.

Then, if the last sum is greater or equal than ten, we count a carry operation with a accumulator else we don't count it. Finally, We run *again* the algorithm but this time with the new carry and the floor part for each number after divided them by ten. The algorithm finishes when there is nothing to sum or the numbers are zero.
