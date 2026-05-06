[![INFORMS Journal on Computing Logo](https://INFORMSJoC.github.io/logos/INFORMS_Journal_on_Computing_Header.jpg)](https://pubsonline.informs.org/journal/ijoc)

# Multiply-penalized Decentralized Estimation with an Application to Convex Support Tensor Machines

This archive is distributed in association with the [INFORMS Journal on
Computing](https://pubsonline.informs.org/journal/ijoc) under the [MIT License](LICENSE).

The software and data in this repository are a snapshot of the software and data
that were used in the research reported on in the paper 
[Multiply-penalized Decentralized Estimation with an Application to Convex Support Tensor Machines](https://doi.org/10.1287/ijoc.2025.1777) by B. Zhang, Z. Zheng, and H. Lian. 


## Cite

To cite the contents of this repository, please cite both the paper and this repo, using their respective DOIs.

https://doi.org/10.1287/ijoc.2025.1777

https://doi.org/10.1287/ijoc.2025.1777.cd

Below is the BibTex for citing this snapshot of the repository.

```
@misc{zhang2025,
  author =        {B. Zhang, Z. Zheng, and H. Lian},
  publisher =     {INFORMS Journal on Computing},
  title =         {{Multiply-penalized Decentralized Estimation with an Application to Convex Support Tensor Machines}},
  year =          {2025},
  doi =           {10.1287/ijoc.2025.1777.cd},
  url =           {https://github.com/INFORMSJoC/2025.1777},
  note =          {Available for download at https://github.com/INFORMSJoC/2025.1777},
}  
```

## Description

This repository provides R code for an efficient ADMM-based algorithm presented in the article to address the penalized estimation problem involving multiple convex penalties in a decentralized setting. Specifically, these R scripts focus on a challenging and important example about the convex support tensor machine (STM) for classification based on multiple nuclear norm penalties, the difficulty of which stems from the non-smooth empirical loss, which is approximately strongly convex/smooth in a neighborhood of zero. We simulated our ADMM-based algorithm's performance under various data settings on this model, and it demonstrated superior performance, with results consistent with the theoretical findings presented in our article.

This project contains two folders: `results` and `src`.

<!-- - `data`: This folder includes the data used in the paper.-->
- `src`: This folder contains the source code for the simulations and two empirical applications.
- `results`: This folder contains the results of the experiments.

## Results

The numerical experiment results are available in the `results` folder, which includes Figures 1-7 in the article. Here, [Figure 1](results/images/Figure1.eps) illustrates the decentralized network used in the numerical simulations, and others show the results of our ADMM-based algorithm under various data settings, including variations in dimension, sample size, regularization parameters, and more; for further details, see Section 5 of the paper.

## Replicating

The numerical experiments in the paper are entirely based on generated samples. To reproduce the results, please generate the file based on the data settings in the paper and corresponding R code in this repository, and use `src\STM_plot.R` to visualize the results.

Specifically, to replicate the results in [Figure 2](results/images/Figure2.eps), please run `src/STM_diff_p_lam.R` with `lambda <- 0.1` and change `d` to `c(3,3,3)`, `c(5,5,5)` and `c(7,7,7)`, respectively. To replicate the results in [Figure 3](results/images/Figure3.eps), please run `src/STM_diff_p_lam.R` with `d <- c(5,5,5)` and change `lambda` to `0`, `0.1` and `0.2`, respectively.

To replicate the results in [Figure 4](results/images/Figure4.eps), please run `src/STM_process.R` with `rho_all <- 0.5` and `c_all <- c(10,50,100,500,1000,1)`. And please run the same file  with `rho_all <- c(0.001,0.01,0.1,0.5,1,1.5)` and `c_all <- 10` to replicate the results in [Figure 5](results/images/Figure5.eps).

Without changing any setting, running `src/STM_n_toall.R` directly reproduces the results shown in [Figure 6](results/images/Figure6.eps), while running `src/STM_m_toall.R` directly reproduces the results shown in [Figure 7](results/images/Figure7.eps).

