# Misinformation-replication
Replication files for "Optimal Policies to Battle the Coronavirus 'Infodemic' Among Social Media Users in Sub-Saharan Africa."

- Pre-registration [here](https://osf.io/ny2xc/). 

**Files**
+ `code/` folder
  + Data cleaning script `dataCleaning_evaluation.Rmd`
  + Analysis scripts
    + `misinformation_replication.Rmd` (both learning and evaluation primary analysis)
      - Depends on `utils.R`
    + `misinformation_replication_secondary.Rmd`(both learning and evaluation secondary analysis)
      - Depends on `utils.R`
  
**Reproducing results**

Python is required to generate the contextual probabilities, but a copy of the probabilities is saved in this replication repository. 
R is required for primary analysis

1. Save most recent data in the `data/` folder. 
2. Generate contextual probabilities for adaptive weights for inference:
  - You may wish to create a conda environment: `code/installations.md`
  - Run `code/contextual_probabilities/gen_probabilities.py`
    - Depends on `code/contextual_probabilities/utils.py`
    - Generates `data/contextual_probabilities.npy`
  - Run `code/contextual_probabilities/convert_contextual_probs.R` to convert numpy object to rds
    - Generates `code/objects/contextual_probabilities.RDS`
4. To replicate analysis, run
   + `code/misinformation_replication.Rmd` (primary analysis in paper)
      - Depends on `utils.R`
   + `code/misinformation_replication_secondary.Rmd` (secondary analysis)
      - Depends on `utils.R`
5. Resulting html files (`misinformation_replication.html`, `misinformation_replication_secondary.html`) will be saved in the same folder.
6. Figures and latex tables will be saved in `figures/` and `tables/` folders respectively.
