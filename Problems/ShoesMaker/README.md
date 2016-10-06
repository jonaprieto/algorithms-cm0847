Solution
---

The solution is a some kind of greedy strategy. After doing some math over
the objective function, the pattern of `time_i * cost_j` appears, so
after trying this criterion with some test cases, I figured it out,
it actually works. So, I sorted the list of *orders* using the above
criterion and the other follows.
