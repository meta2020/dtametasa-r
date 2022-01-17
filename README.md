# DTA-META-SA R CODE


This folder contains reproducible R codes of simulation studies and re-analysis of the example data.

The following packages are used in the simulation or example data

- "mvmeta", "foreach", "parallel", "doSNOW", "doRNG", "latex2exp", "kable; 

If they are not installed, please install from R CRAN `install.packages("package_name")`.
 

## example/


- [simfun/](example/simfun/): R functions 

- [data-IVD.csv](example/data-IVD.csv): Example data

- [Example-ivd-color.Rmd](example/Example-ivd-color.Rmd): codes to reproduce Section 3. Example

- [Example-ivd-color.pdf](example/Example-ivd-color.pdf): Fig. 1 2 3, Table S1 (color)

- [Example-ivd-bw.pdf](example/Example-ivd-bw.pdf): Fig. 1 2 3, Table S1 (black and white)

## simulation/

- scenario/ 

	- scenario-t12/: scenarios RData for $(\tau_1^2, \tau_1^2) = (1, 4)$

	- scenario-t0.7/: scenarios RData for $(\tau_1^2, \tau_1^2) = (0.5, 0.5)$

	- [Table-scenario.Rmd](simulation/scenario/Table-scenario.Rmd): codes to reproduce scenario table

	- [Table-scenario.pdf](simulation/scenario/Table-scenario.pdf): Table 2 and Table S2

- [sim-sroc-color.Rmd](simulation/sim-sroc-color.Rmd): codes to reproduce Fig. 4

- [sim-sroc-color.pdf](simulation/sim-sroc-color.pdf): Fig. 4 (color)

- [sim-sroc-bw.pdf](simulation/sim-sroc-bw.pdf): Fig. 4 (black and white)


- [1000-times-sim-par.R](simulation/1000-times-sim-par.R): codes to reproduce simulation



- [simfun/](simulation/simfun/): R functions 

- res-par/

	- t12/: 

		- c11/ or c10/ or c01/: simulated results

		- [Tab3-5-sauc-med-iqr-t12.Rmd](simulation/res-par/t12/Tab3-5-sauc-med-iqr-t12.Rmd): codes to reproduce Table 3-5

		- [Tab3-5-sauc-med-iqr-t12.pdf](simulation/res-par/t12/Tab3-5-sauc-med-iqr-t12.pdf): Table 3-5



	- t0.7/:

		- c11/ or c10/ or c01/: simulated results

		- [TabS3-S5-sauc-med-iqr-t0.7.Rmd](simulation/res-par/t0.7/TabS3-S5-sauc-med-iqr-t0.7.Rmd): codes to reproduce Table S3-S5

		- [TabS3-S5-sauc-med-iqr-t0.7.pdf](simulation/res-par/t0.7/TabS3-S5-sauc-med-iqr-t0.7.pdf): Table S3-S5
















