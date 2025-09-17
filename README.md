# conformal-fs

Conformal prediction for frequency-severity modeling

> Helton Graziadei, Paulo C. Marques F., Eduardo F. L. de Melo and Rodrigo S. Targino

https://arxiv.org/abs/2307.13124

### Description

In this repository, we have three folders: 

- **`synthetic/` – §4.1**  
  Scripts to generate data and run the two-stage frequency–severity pipeline with conformalization (split-conformal baseline; GLM/RF severity variants). 
  Produces summary metrics such as empirical **coverage** and **average width**.

- **`mtpl/` – §4.2**  
  Real-data application to **Motor Third-Party Liability** (Belgium), implementing the same pipeline and evaluation metrics as in the synthetic study.

- **`crop/` – §4.3**  
  Real-data application to **Brazilian crop insurance** (municipality-level aggregation as described in the paper), with the same reporting of coverage and width.

### Methods referenced
- **Split conformal prediction (two-stage)** – main procedure across **§4.1–4.3**.  
- **Out-of-bag (OOB) extension** – **§5**: when the **severity** model is a Random Forest, OOB residuals can replace a held-out calibration set. OOB variants live alongside split-conformal scripts within each folder.
