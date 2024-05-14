# simulate_chain_stats is numerically correct

    Code
      sim_summary_small_pop
    Output
      `epichains_summary` object 
      
       [1] 1 1 1 1 1 2 1 1 1 1
      
       Simulated lengths: 
      
      Max: 2
      Min: 1

# simulate_chains produces expected snapshots

    Code
      sim_chains_finite_pop
    Output
      `<epichains>` object
      
      < tree head (from first known infector) >
      
         chain infector infectee generation      time
      11     1        1        2          2 1.1279394
      12     2        1        2          2 0.3217817
      13     2        1        3          2 7.8785290
      14     2        1        4          2 3.1584048
      15     3        1        2          2 5.7529635
      16     3        1        3          2 7.1951323
      
      
      Number of chains: 10
      Number of infectors (known): 6
      Number of generations: 5
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      sim_chains_inf_susc
    Output
      `<epichains>` object
      
      < tree head (from first known infector) >
      
         chain infector infectee generation      time
      11     1        1        2          2  1.169061
      12     1        1        3          2 14.980844
      13     1        1        4          2  1.417131
      14     2        1        2          2  1.932143
      15     2        1        3          2  6.667853
      16     2        1        4          2  1.127939
      
      
      Number of chains: 10
      Number of infectors (known): 98
      Number of generations: 12
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      sim_chains_small_pop
    Output
      `<epichains>` object
      
      < tree head (from first known infector) >
      
         chain infector infectee generation
      11     6        1        2          2
      
      
      Number of chains: 10
      Number of infectors (known): 2
      Number of generations: 2
      Use `as.data.frame(<object_name>)` to view the full output in the console.

