# Infodemic-replication
Replication files for "Battling the Coronavirus 'Infodemic' Among Social Media Users in Kenya and Nigeria"

- Pre-registration [here](https://osf.io/wbvyk/?view_only=9db136c1387d4292abad22481e4935ae). 
  
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
   + `code/contextual_probabilities/gen_probabilities.py`
      - Depends on `code/contextual_probabilities/utils.py`
      - Generates `data/contextual_probabilities.npy`
   + Run `Rscript code/contextual_probabilities/convert_contextual_probs.R` to convert numpy object to rds
      - Generates `code/objects/contextual_probabilities.RDS`
4. If you are NOT using a conda environment, install R packages outside of conda:
   + Run `Rscript --verbose r_packages.R`
5. To replicate analysis, run
   + `code/misinformation_replication.Rmd` (primary analysis in paper)
      - Depends on `utils.R`
5. Resulting html files (`misinformation_replication.html`) will be saved in the same folder.
6. Figures and latex tables will be saved in `figures/` and `tables/` folders respectively.
