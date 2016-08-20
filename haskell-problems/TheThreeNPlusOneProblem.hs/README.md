Solution
---

This problem is basically *do what it says* (ad-hoc). Therefore, we need to compute the function 3n+1 for each value belongs to the input interval. Then, find the maximum of these values.

Because of the recursive nature of the function, a few values would be computed *many times*. Therefore, we need to *store previous computation* of the function while we are trying to compute the function for new values. This task is performed by the `collatz` function. This function takes a natural number n and computes the complete sequence given by the 3n+1 function taking into account a parameter that storages previous results of the function.

Then, this function returns a list of pairs, the number *n* and the length of the sequence for this *n*. So finally, we do the same per each number found into the last sequence for  *n* and get the answer.
