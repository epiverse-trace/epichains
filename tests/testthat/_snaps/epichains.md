# print.epichains works for simulation functions

    Code
      susc_outbreak_raw
    Output
      `<epichains>` object
      
      < tree head (from first known infector_id) >
      
         infectee_id sim_id infector_id generation       time susc_pop
      11           1      2           1          2 58.2546995       80
      12           2      2           1          2  5.4332994       80
      13           5      2           1          2  2.0539399       80
      14           6      2           1          2  0.5332318       80
      15           7      2           1          2 24.8337836       80
      16           8      2           1          2  0.9674612       80
      
      
      Trees simulated: 10
      Number of infectors (known): 8
      Number of generations: 6
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      susc_outbreak_raw2
    Output
      `<epichains>` object
      
      < tree head (from first known infector_id) >
      
         infectee_id sim_id infector_id generation       time susc_pop
      11           1      2           1          2  6.5291176       82
      12           3      2           1          2  4.5366156       82
      13           4      2           1          2  0.4951176       82
      14           6      2           1          2  8.9518883       82
      15           7      2           1          2 20.7565268       82
      16           8      2           1          2  8.4553011       82
      
      
      Trees simulated: 10
      Number of infectors (known): 4
      Number of generations: 4
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      tree_sim_raw
    Output
      `<epichains>` object
      
      < tree head (from first known infector_id) >
      
        infectee_id sim_id infector_id generation
      3           1      2           1          2
      4           2      2           1          2
      5           1      3           2          3
      6           2      3           2          3
      7           1      4           2          3
      8           2      4           3          4
      
      
      Trees simulated: 2
      Number of infectors (known): 60
      Number of generations: 15
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      tree_sim_raw2
    Output
      `<epichains>` object
      
      < tree head (from first known infector_id) >
      
         infectee_id sim_id infector_id generation       time
      11           1      2           1          2 1.16906106
      12           2      2           1          2 1.93214311
      13           3      2           1          2 7.87852902
      14           4      2           1          2 5.75296347
      15           5      2           1          2 7.19513227
      16           6      2           1          2 0.06998357
      
      
      Trees simulated: 10
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
         infectee_id sim_id infector_id generation       time susc_pop
      11           2      2           1          2 21.1379640       84
      12           4      2           1          2  1.1541073       84
      13           7      2           1          2  2.0154108       84
      14           9      2           1          2  0.4662312       84
      15          10      2           1          2  0.2252704       84
      16           2      3           1          2  8.8063185       84

---

    Code
      head(tree_sim_raw2)
    Output
         infectee_id sim_id infector_id generation       time
      11           1      2           1          2  7.3753537
      12           2      2           1          2  3.6495519
      13           3      2           1          2  0.5958979
      14           4      2           1          2  1.2242869
      15           5      2           1          2 37.4953062
      16           6      2           1          2  1.9372317

---

    Code
      tail(susc_outbreak_raw)
    Output
         infectee_id sim_id infector_id generation     time susc_pop
      37           4     15          14          7 32.20450       64
      38           4     16          15          8 32.71635       62
      39           4     17          16          9 35.32430       61
      40           4     18          17         10 39.61216       60
      41           4     19          18         11 50.06649       59
      42           4     20          19         12 56.45937       58

---

    Code
      tail(tree_sim_raw2)
    Output
          infectee_id sim_id infector_id generation      time
      137           8     20           8          5 17.187201
      138           4     20           9          5 39.079365
      139           8     21           8          5  2.522819
      140           4     21           9          5 21.459874
      141           8     22           8          5  2.315577
      142           8     23           9          5  4.616785

