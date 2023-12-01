# print.epichains works for simulation functions

    Code
      susc_outbreak_raw
    Output
      `epichains` object
      
      < tree head (from first known infector) >
      
      [1] sim_id      infector_id generation  time       
      <0 rows> (or 0-length row.names)
      
      < tree tail >
      
        sim_id infector_id generation time
      1      1          NA          1    0
      Number of infectors (known): 0
      Number of generations: 1
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      susc_outbreak_raw2
    Output
      `epichains` object
      
      < tree head (from first known infector) >
      
        sim_id infector_id generation     time
      2      2           1          2 42.57973
      3      3           2          3 42.80500
      4      4           2          3 42.70415
      5      5           4          4 43.87477
      6      6           4          4 44.00812
      7      7           3          4 78.73481
      
      < tree tail >
      
         sim_id infector_id generation     time
      7       7           3          4 78.73481
      8       8           5          5 47.03948
      9       9           6          5 45.38534
      10     10           9          6 46.14505
      11     11           8          6 48.03103
      12     12           7          5 81.49185
      Number of infectors (known): 9
      Number of generations: 6
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      tree_sim_raw
    Output
      `epichains` object
      
      < tree head (from first known infector) >
      
        infectee_id sim_id infector_id generation
      3           1      2           1          2
      4           2      2           1          2
      5           1      3           1          2
      6           2      3           1          2
      7           1      4           1          2
      8           2      4           2          3
      
      < tree tail >
      
         infectee_id sim_id infector_id generation
      12           1      6           4          3
      13           1      7           4          3
      14           2      7           6          4
      15           2      8           6          4
      16           1      8           7          4
      17           2      9           8          5
      Chains simulated: 2
      Number of infectors (known): 7
      Number of generations: 5
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      tree_sim_raw2
    Output
      `epichains` object
      
      < tree head (from first known infector) >
      
         infectee_id sim_id infector_id generation time
      11           1      2           1          2    3
      12           2      2           1          2    3
      13           3      2           1          2    3
      14           4      2           1          2    3
      15           5      2           1          2    3
      16           6      2           1          2    3
      
      < tree tail >
      
          infectee_id sim_id infector_id generation time
      138          10     19           9          4    9
      139           2     20           6          4    9
      140           4     20           9          4    9
      141           4     21           9          4    9
      142           4     22           9          4    9
      143           4     23           9          4    9
      Chains simulated: 10
      Number of infectors (known): 9
      Number of generations: 5
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      chain_summary_raw
    Output
      `epichains` object 
      
      [1] 1 3
      
       Simulated chain lengths: 
      
      Max: 3
      Min: 1

# head and tail print output as expected

    Code
      head(susc_outbreak_raw)
    Output
      < tree head (from first known infector) >
      
      [1] sim_id      infector_id generation  time       
      <0 rows> (or 0-length row.names)

---

    Code
      head(susc_outbreak_raw2)
    Output
      < tree head (from first known infector) >
      
        sim_id infector_id generation     time
      2      2           1          2 42.57973
      3      3           2          3 42.80500
      4      4           2          3 42.70415
      5      5           4          4 43.87477
      6      6           4          4 44.00812
      7      7           3          4 78.73481

---

    Code
      head(tree_sim_raw)
    Output
      < tree head (from first known infector) >
      
        infectee_id sim_id infector_id generation
      3           1      2           1          2
      4           2      2           1          2
      5           1      3           1          2
      6           2      3           1          2
      7           1      4           1          2
      8           2      4           2          3

---

    Code
      head(tree_sim_raw2)
    Output
      < tree head (from first known infector) >
      
         infectee_id sim_id infector_id generation time
      11           1      2           1          2    3
      12           2      2           1          2    3
      13           3      2           1          2    3
      14           4      2           1          2    3
      15           5      2           1          2    3
      16           6      2           1          2    3

---

    Code
      tail(susc_outbreak_raw)
    Output
      
      < tree tail >
      
        sim_id infector_id generation time
      1      1          NA          1    0

---

    Code
      tail(susc_outbreak_raw2)
    Output
      
      < tree tail >
      
         sim_id infector_id generation     time
      7       7           3          4 78.73481
      8       8           5          5 47.03948
      9       9           6          5 45.38534
      10     10           9          6 46.14505
      11     11           8          6 48.03103
      12     12           7          5 81.49185

---

    Code
      tail(tree_sim_raw)
    Output
      
      < tree tail >
      
         infectee_id sim_id infector_id generation
      12           1      6           4          3
      13           1      7           4          3
      14           2      7           6          4
      15           2      8           6          4
      16           1      8           7          4
      17           2      9           8          5

---

    Code
      tail(tree_sim_raw2)
    Output
      
      < tree tail >
      
          infectee_id sim_id infector_id generation time
      138          10     19           9          4    9
      139           2     20           6          4    9
      140           4     20           9          4    9
      141           4     21           9          4    9
      142           4     22           9          4    9
      143           4     23           9          4    9

