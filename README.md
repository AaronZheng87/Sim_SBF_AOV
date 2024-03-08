# Sim_SBF_AOV

This is the Repo of project about Simulation-Based Power Analysis of Sequential Bases Factor Analysis for ANOVA.

------

## 1_Simulation file

This file contains main R files for simulation of power analysis.

### one-way three groups ANOVA

effect Size of simulation: $Cohen's \space f = \frac{\frac{{\sqrt{{\sum (\mu - \overline{\mu})^2}}}}{{{N}}}}{\sigma}$

Source: [ Lakens](http://journals.sagepub.com/doi/10.1177/2515245920951503)

- [x] Unrelated groups
  - [x] Check 
  - [x] BF Power
- [x] Related groups
  - [x] Check 
  - [x] BF Power

### 2*2 within factors ANOVA

effect size of simulation: $ Cohen's\space d $

Source: [Langenberg](https://doi.org/10.3758/s13428-022-01902-8)

- [ ] Unrelated groups
  - [ ] Group A main effect
    - [ ] Check 
    - [ ] BF Power
  - [ ] Group B main effect
    - [ ] Check
    - [ ] BF power
  - [ ] Interaction effect
    - [ ] Check
    - [ ] BF power
- [ ] Related groups
  - [x] Group A main effect
    - [x] Check 
    - [ ] BF Power
  - [ ] Group B main effect
    - [ ] Check
    - [ ] BF power
  - [ ] Interaction effect
    - [ ] Check
    - [ ] BF power