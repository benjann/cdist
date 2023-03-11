# cdist
Stata module for counterfactual distribution estimation

To install `cdist` from the SSC Archive, type

    . ssc install cdist, replace

To install `cdist` from GitHub, type

    . net install cdist, replace from(https://raw.githubusercontent.com/benjann/cdist/main/)

---

Main changes:

    11mar2023 (version 0.0.2)
    - factor variable notation now also allowed with method(logit)
    - option jmp added to generate additional location-shifted results for JMP type
      decompositions
    - option qdef() added
    - vce(bootstrap) no longer complains if pweights or iweighs are specified
    
    09mar2023 (version 0.0.1)
    - beta version posted on github
