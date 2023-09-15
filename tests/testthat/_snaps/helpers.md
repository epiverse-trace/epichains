# get_statistic_func snapshots look right

    Code
      body(pois_offspring_func)
    Output
      {
          truncdist::rtrunc(n, spec = "pois", lambda = mean_offspring * 
              susc/pop, b = susc)
      }

---

    Code
      body(nbinom_offspring_func)
    Output
      {
          new_mn <- mean_offspring * susc/pop
          size <- new_mn/(disp_offspring - 1)
          truncdist::rtrunc(n, spec = "nbinom", b = susc, mu = new_mn, 
              size = size)
      }

