Solution
---

The first solution that I came up was finding the paths from the source
to the source and search for the minimum edge in those paths and that value
minus one will be the most capacity for a trip then use it. (DFS(from:source,to:goal))

Instead, I used another idea, build the maximum spanning tree and find the
minimum edge on it. It a sort of the same of above idea but at first that
was not trivial.

The last approach could be use a dynamic programming. That's fancier than
the first approach. The idea here is to modify the `dp` of floyd-warshall
algorithm at its most inner routine. The `dp[i][j]` will storage the max-minimum
edge in a path go from i to j, then
`dp[i][j][k] = max(dp[i][j][k-1], min(dp[i][k][k-1], dp[j,k][k-1]))`.
The last dimension of this `dp` can be avoided.
