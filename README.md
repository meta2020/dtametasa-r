## [Reproducible results with R codes] 

# A likelihood‐based sensitivity analysis for publication bias on the summary receiver operating characteristic in meta‐analysis of diagnostic test accuracy

See details:  Zhou Y, Huang A, Hattori S. A likelihood‐based sensitivity analysis for publication bias on the summary receiver operating characteristic in meta‐analysis of diagnostic test accuracy. Statistics in Medicine. 2023 Mar 15;42(6):781–98. 10.1002/sim.9643

This folder contains reproducible R codes of simulation studies and re-analysis of the example data.

The following packages are used

- `mixmeta`, `foreach`, `parallel`, `doSNOW`, `doRNG`, are used in the simulation 

- `latex2exp`, `kableExtra`, `mvmeta`, `meta` are used in example data

If they are not installed, please install from R CRAN `install.packages("package_name")`.
 

## Example

- [Example 1 codes](example-results-legend/HTML-Example-IVD.Rmd)

<!-- - [Example 1 results](example-results/HTML-Example-IVD.html) -->


- [Example 2 codes](example-results-legend/HTML-Example-CD64.Rmd)

<!-- - [Example 2 results](example-results/HTML-Example-CD64.html) -->


## Simulation

- [Scenarios](simulation-results/scenarios/HTML-Table-scenarios.Rmd)

### Large tau

- [SAUC estimates](simulation-results/results-RData-Rmd/t12/htmlSAUC-line-plot-t12.Rmd)

- [Parameter estimates, c11](simulation-results/results-RData-Rmd/t12/htmlTab-other-pars-t12-c11.Rmd)

- [Parameter estimates, c10](simulation-results/results-RData-Rmd/t12/htmlTab-other-pars-t12-c10.Rmd)

- [Parameter estimates, c01](simulation-results/results-RData-Rmd/t12/htmlTab-other-pars-t12-c01.Rmd)

### Small tau

- [SAUC estimates](simulation-results/results-RData-Rmd/t0.7/htmlSAUC-line-plot-t0.7.Rmd)

- [Parameter estimates, c11](simulation-results/results-RData-Rmd/t0.7/htmlTab-other-pars-t0.7-c11.Rmd)

- [Parameter estimates, c10](simulation-results/results-RData-Rmd/t0.7/htmlTab-other-pars-t0.7-c10.Rmd)

- [Parameter estimates, c01](simulation-results/results-RData-Rmd/t0.7/htmlTab-other-pars-t0.7-c01.Rmd)

### Trend in SROC curves

- [Summary point in the SROC curve](simulation-results/HTML-sim-sroc.Rmd)








