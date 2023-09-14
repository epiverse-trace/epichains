# print.epichains works for simulate_summary output

    Code
      epichains_summary
    Output
      `epichains` object 
      
       [1]   1 Inf Inf Inf Inf   1   2 Inf   1   1
      
       Simulated chain sizes: 
      
      Max: 2
      Min: 1

# print.epichains works for simulate_tree output

    Code
      epichains_tree
    Output
      `epichains` object
      
      < tree head (from first known ancestor) >
      
         chain_id sim_id ancestor generation
      11        1      2        1          2
      13        2      2        1          2
      18        3      2        1          2
      19        4      2        1          2
      22        6      2        1          2
      23        8      2        1          2
      
      < tree tail >
      
         chain_id sim_id ancestor generation
      41        2     17        6          3
      85        6     17        6          4
      42        2     18        6          3
      86        6     18        7          4
      87        6     19        7          4
      88        6     20        7          4
      Chains simulated: 10
      Number of ancestors (known): 9
      Number of generations: 5
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      epichains_tree2
    Output
      `epichains` object
      
      < tree head (from first known ancestor) >
      
         chain_id sim_id ancestor generation time
      11        1      2        1          2    3
      13        2      2        1          2    3
      16        3      2        1          2    3
      17        4      2        1          2    3
      18        5      2        1          2    3
      19        6      2        1          2    3
      
      < tree tail >
      
          chain_id sim_id ancestor generation time
      116        7     20        9          4    9
      128        8     20        9          4    9
      117        7     21        9          4    9
      129        8     21        9          4    9
      130        8     22        9          4    9
      131        8     23        9          4    9
      Chains simulated: 10
      Number of ancestors (known): 9
      Number of generations: 4
      Use `as.data.frame(<object_name>)` to view the full output in the console.

# head and tail methods work

    Code
      head(epichains_tree)
    Output
      < tree head (from first known ancestor) >
      
         chain_id sim_id ancestor generation
      11        1      2        1          2
      13        2      2        1          2
      18        3      2        1          2
      19        4      2        1          2
      22        6      2        1          2
      23        8      2        1          2

---

    Code
      head(epichains_tree2)
    Output
      < tree head (from first known ancestor) >
      
         chain_id sim_id ancestor generation time
      11        1      2        1          2    3
      13        2      2        1          2    3
      16        3      2        1          2    3
      17        4      2        1          2    3
      18        5      2        1          2    3
      19        6      2        1          2    3

---

    Code
      tail(epichains_tree)
    Output
      
      < tree tail >
      
         chain_id sim_id ancestor generation
      41        2     17        6          3
      85        6     17        6          4
      42        2     18        6          3
      86        6     18        7          4
      87        6     19        7          4
      88        6     20        7          4

---

    Code
      tail(epichains_tree2)
    Output
      
      < tree tail >
      
          chain_id sim_id ancestor generation time
      116        7     20        9          4    9
      128        8     20        9          4    9
      117        7     21        9          4    9
      129        8     21        9          4    9
      130        8     22        9          4    9
      131        8     23        9          4    9

