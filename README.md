# Sensitivity analysis for publication bias on the SROC in DTA meta-analysis R CODE


This folder contains reproducible R codes of simulation studies and re-analysis of the example data.

The following packages are used in the simulation or example data

- "mixmeta", "foreach", "parallel", "doSNOW", "doRNG", used in the simulations

- "latex2exp", "kable, "Extra; 

If they are not installed, please install from R CRAN `install.packages("package_name")`.
 

## Example

- [Example 1](example-results/HTML-Example-IVD.Rmd)

- [Example 2](example-results/HTML-Example-CD64.html)

## Simulation

- [Scenarios](simulation-results/scenarios/HTML-Table-scenarios.html)

### Large tau

- [SAUC estimates](simulation-results/results-RData-Rmd/t12/htmlSAUC-line-plot-t12.html)

- [Parameter estimates, c11](simulation-results/results-RData-Rmd/t12/htmlTab-other-pars-t12-c11.html)

- [Parameter estimates, c10](simulation-results/results-RData-Rmd/t12/htmlTab-other-pars-t12-c10.html)

- [Parameter estimates, c01](simulation-results/results-RData-Rmd/t12/htmlTab-other-pars-t12-c01.html)

### Small tau

- [SAUC estimates](simulation-results/results-RData-Rmd/t0.7/htmlSAUC-line-plot-t0.7.html)

- [Parameter estimates, c11](simulation-results/results-RData-Rmd/t0.7/htmlTab-other-pars-t0.7-c11.html)

- [Parameter estimates, c10](simulation-results/results-RData-Rmd/t0.7/htmlTab-other-pars-t0.7-c10.html)

- [Parameter estimates, c01](simulation-results/results-RData-Rmd/t0.7/htmlTab-other-pars-t0.7-c01.html)

### Trend in SROC curves

- [Summary point in the SROC curve](simulation-results/HTML-sim-sroc.html)








