# Infodemic-replication
Replication files for "Battling the Coronavirus 'Infodemic' Among Social Media Users in Kenya and Nigeria"

- arXived [here](https://osf.io/59sn2)
- Pre-registration [here](https://osf.io/59sn2).

Note: The analysis reported in the paper is implemented using macOS, platform: aarch64-apple-darwin20. Due to differences in compilers across platforms, if this code is run on a Windows machine, the random forests used for policy learning and evaluation may have slightly different splits, even using the same random seed. This behavior is noted in the `grf` package documentation [here](https://grf-labs.github.io/grf/REFERENCE.html#forests-predict-different-values-depending-on-the-platform-even-though-the-seed-is-the-same). As a consequence, point estimates may vary slightly from those reported, and the restricted policy learned may have slightly different assignment rules; our replications suggest that these variations do not change the substantive findings reported in the paper. 
  
**Reproducing results**

Python is required to generate the contextual probabilities, but a copy of the probabilities is saved in this replication repository. 
R is required for primary analysis

1. Save most recent data in the `data/` folder.
2. Generate contextual probabilities for adaptive weights for inference:
   + You may wish to create a conda environment if you are re-generating the contextual probabilities:
    ```
    conda create --name contextual_probs python=3.7
    conda activate contextual_probs
    source code/contextual_probabilities/install.sh    
    ```
   + Then, re-generate contextual probabilities for adaptive weights for inference:
    ```
    cd code/contextual_probabilities
    Python gen_probabilities.py
    Rscript convert_contextual_probs.R
    ```
   + File `code/contextual_probabilities/gen_probabilities.py` creates an object that records treatment assignment probabilities for every observation at every point in time, conditional on an algorithm and observed data. 
      - Depends on: 
        - `code/contextual_probabilities/utils.py` 
        - `data/cleaned-data_*.csv`
      - Generates `data/contextual_probabilities.npy`
   + File `code/contextual_probabilities/convert_contextual_probs.R` converts numpy probability object to rds
      - Depends on `data/contextual_probabilities.npy`
      - Generates `code/objects/contextual_probabilities.RDS`
4. If you are NOT using a conda environment, install R packages outside of conda:
   + Start in the `infodemic_replication` directory
   + Run `Rscript --verbose code/r_packages.R`
5. To replicate analysis, compile
   + `code/misinformation_replication.Rmd` (primary analysis in paper)
      - Depends on:
         - `code/utils.R`
         - `data/cleaned-data_*.RDS`
      - Generates:
         + Resulting html file (`misinformation_replication.html`) will be saved in the same folder.
         + Figures and latex tables will be saved in `figures/` and `tables/` folders respectively.
