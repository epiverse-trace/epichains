# print.epichains works for simulation functions

    Code
      susc_outbreak_raw
    Output
      `<epichains>` object
      
      < tree head (from first known infector) >
      
         index_case_active infector infectee generation       time
      11                 1        1       11          2 58.2546995
      12                 2        1       12          2  5.4332994
      13                 5        1       13          2  2.0539399
      14                 6        1       14          2  0.5332318
      15                 6        1       15          2 30.2672618
      16                 6        1       16          2  0.1588130
      
      
      Index cases simulated: 10
      Number of infectors (known): 8
      Number of generations: 6
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      susc_outbreak_raw2
    Output
      `<epichains>` object
      
      < tree head (from first known infector) >
      
         index_case_active infector infectee generation       time
      11                 1        1       11          2  6.5291176
      12                 3        1       12          2  4.5366156
      13                 4        1       13          2  0.4951176
      14                 6        1       14          2  8.9518883
      15                 7        1       15          2 20.7565268
      16                 8        1       16          2  8.4553011
      
      
      Index cases simulated: 10
      Number of infectors (known): 4
      Number of generations: 4
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      tree_sim_raw
    Output
      `<epichains>` object
      
      < tree head (from first known infector) >
      
        index_case_active infector infectee generation
      3                 1        1        3          2
      4                 2        1        4          2
      5                 1        2        5          3
      6                 1        2        6          3
      7                 2        2        7          3
      8                 1        4        8          4
      
      
      Index cases simulated: 2
      Number of infectors (known): 60
      Number of generations: 15
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      tree_sim_raw2
    Output
      `<epichains>` object
      
      < tree head (from first known infector) >
      
         index_case_active infector infectee generation      time
      11                 1        1       11          2  1.169061
      12                 1        1       12          2 14.980844
      13                 1        1       13          2  1.417131
      14                 2        1       14          2  1.932143
      15                 2        1       15          2  6.667853
      16                 2        1       16          2  1.127939
      
      
      Index cases simulated: 10
      Number of infectors (known): 9
      Number of generations: 7
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      chain_summary_raw
    Output
      `epichains_summary` object 
      
      [1] 15  5
      
       Simulated tree lengths: 
      
      Max: 15
      Min: 5

---

    Code
      chain_lengths_with_Infs
    Output
      `epichains_summary` object 
      
       [1]   8   2 Inf   8   1 Inf   8 Inf   2   3
      
       Simulated tree lengths: 
      
      Max: >=10
      Min: 1

---

    Code
      chain_lengths_all_Infs
    Output
      `epichains_summary` object 
      
      [1] Inf Inf
      
       Simulated tree lengths: 
      
      Max: >=10
      Min: >=10

# head and tail print output as expected

    Code
      head(susc_outbreak_raw)
    Output
         index_case_active infector infectee generation       time
      11                 2        1       11          2 21.1379640
      12                 2        1       12          2  8.8063185
      13                 4        1       13          2  1.1541073
      14                 7        1       14          2  2.0154108
      15                 9        1       15          2  0.4662312
      16                10        1       16          2  0.2252704

---

    Code
      head(tree_sim_raw2)
    Output
         index_case_active infector infectee generation      time
      11                 1        1       11          2 7.3753537
      12                 1        1       12          2 2.1907300
      13                 1        1       13          2 9.1453972
      14                 1        1       14          2 1.0399607
      15                 2        1       15          2 3.6495519
      16                 3        1       16          2 0.5958979

---

    Code
      tail(susc_outbreak_raw)
    Output
         index_case_active infector infectee generation     time
      37                 4       15       37          8 32.71635
      38                10        9       38          8 43.00686
      39                 4       16       39          9 35.32430
      40                 4       17       40         10 39.61216
      41                 4       18       41         11 50.06649
      42                 4       19       42         12 56.45937

---

    Code
      tail(tree_sim_raw2)
    Output
          index_case_active infector infectee generation      time
      137                 9        6      137          5 35.299172
      138                10        5      138          5 12.281767
      139                10        6      139          5  8.809650
      140                10        6      140          5  2.532314
      141                10        6      141          5  3.215042
      142                10        7      142          5  5.640260

