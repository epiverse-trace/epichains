# simulate_summary is numerically correct

    Code
      sim_summary_small_pop
    Output
      `epichains_summary` object 
      
       [1] 1 1 1 1 1 2 1 1 1 1
      
       Simulated tree lengths: 
      
      Max: 2
      Min: 1

# simulate_chains produces expected snapshots

    Code
      sim_chains_finite_pop
    Output
      `<epichains>` object
      
      < tree head (from first known infector_id) >
      
         infectee_id sim_id infector_id generation       time susc_pop
      11           1      2           1          2 1.12793938       25
      12           2      2           1          2 0.32178175       25
      13           3      2           1          2 5.75296347       25
      14           4      2           1          2 4.11764375       25
      15           5      2           1          2 0.06998357       25
      16           6      2           1          2 8.43022733       25
      
      
      Trees simulated: 10
      Number of infectors (known): 6
      Number of generations: 5
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      sim_chains_inf_susc
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
      Number of infectors (known): 98
      Number of generations: 12
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      sim_chains_small_pop
    Output
      `<epichains>` object
      
      < tree head (from first known infector_id) >
      
         infectee_id sim_id infector_id generation susc_pop
      11           6      2           1          2        0
      
      
      Trees simulated: 10
      Number of infectors (known): 2
      Number of generations: 2
      Use `as.data.frame(<object_name>)` to view the full output in the console.

