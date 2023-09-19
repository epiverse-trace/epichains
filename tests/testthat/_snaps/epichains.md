# print.epichains works for simulation functions

    Code
      susc_outbreak_raw
    Output
      `epichains` object
      
      < tree head (from first known ancestor) >
      
      [1] sim_id     ancestor   generation time      
      <0 rows> (or 0-length row.names)
      
      < tree tail >
      
        sim_id ancestor generation time
      1      1       NA          1    0
      Number of ancestors (known): 0
      Number of generations: 1
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      susc_outbreak_raw2
    Output
      `epichains` object
      
      < tree head (from first known ancestor) >
      
        sim_id ancestor generation       time
      2      2        1          2 21.5834705
      3      3        1          2  0.3939008
      4      4        2          3 21.6595273
      
      < tree tail >
      
        sim_id ancestor generation       time
      1      1       NA          1  0.0000000
      2      2        1          2 21.5834705
      3      3        1          2  0.3939008
      4      4        2          3 21.6595273
      Number of ancestors (known): 2
      Number of generations: 3
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      tree_sim_raw
    Output
      `epichains` object
      
      < tree head (from first known ancestor) >
      
        chain_id sim_id ancestor generation
      3        1      2        1          2
      4        1      3        1          2
      
      < tree tail >
      
        chain_id sim_id ancestor generation
      1        1      1       NA          1
      2        2      1       NA          1
      3        1      2        1          2
      4        1      3        1          2
      Chains simulated: 2
      Number of ancestors (known): 1
      Number of generations: 2
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      tree_sim_raw2
    Output
      `epichains` object
      
      < tree head (from first known ancestor) >
      
         chain_id sim_id ancestor generation time
      11        1      2        1          2    3
      13        2      2        1          2    3
      15        3      2        1          2    3
      17        4      2        1          2    3
      19        6      2        1          2    3
      20        7      2        1          2    3
      
      < tree tail >
      
          chain_id sim_id ancestor generation time
      92         9     19        8          4    9
      109        6     19        8          5   12
      93         9     20        9          4    9
      110        6     20        9          5   12
      94         9     21        9          4    9
      111        6     21        9          5   12
      Chains simulated: 10
      Number of ancestors (known): 9
      Number of generations: 5
      Use `as.data.frame(<object_name>)` to view the full output in the console.

---

    Code
      chain_summary_raw
    Output
      `epichains` object 
      
      [1] 4 1
      
       Simulated chain lengths: 
      
      Max: 4
      Min: 1

# head and tail print output as expected

    Code
      head(susc_outbreak_raw)
    Output
      < tree head (from first known ancestor) >
      
      [1] sim_id     ancestor   generation time      
      <0 rows> (or 0-length row.names)

---

    Code
      head(susc_outbreak_raw2)
    Output
      < tree head (from first known ancestor) >
      
        sim_id ancestor generation       time
      2      2        1          2 21.5834705
      3      3        1          2  0.3939008
      4      4        2          3 21.6595273

---

    Code
      head(tree_sim_raw)
    Output
      < tree head (from first known ancestor) >
      
        chain_id sim_id ancestor generation
      3        1      2        1          2
      4        1      3        1          2

---

    Code
      head(tree_sim_raw2)
    Output
      < tree head (from first known ancestor) >
      
         chain_id sim_id ancestor generation time
      11        1      2        1          2    3
      13        2      2        1          2    3
      15        3      2        1          2    3
      17        4      2        1          2    3
      19        6      2        1          2    3
      20        7      2        1          2    3

---

    Code
      tail(susc_outbreak_raw)
    Output
      
      < tree tail >
      
        sim_id ancestor generation time
      1      1       NA          1    0

---

    Code
      tail(susc_outbreak_raw2)
    Output
      
      < tree tail >
      
        sim_id ancestor generation       time
      1      1       NA          1  0.0000000
      2      2        1          2 21.5834705
      3      3        1          2  0.3939008
      4      4        2          3 21.6595273

---

    Code
      tail(tree_sim_raw)
    Output
      
      < tree tail >
      
        chain_id sim_id ancestor generation
      1        1      1       NA          1
      2        2      1       NA          1
      3        1      2        1          2
      4        1      3        1          2

---

    Code
      tail(tree_sim_raw2)
    Output
      
      < tree tail >
      
          chain_id sim_id ancestor generation time
      92         9     19        8          4    9
      109        6     19        8          5   12
      93         9     20        9          4    9
      110        6     20        9          5   12
      94         9     21        9          4    9
      111        6     21        9          5   12

