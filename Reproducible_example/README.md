# Reproducible_example

As the code in `ARL_simulations` is very slow and the data set used in `LROI_appl_sim` is not publicly available, we provide a small reproducible example where we show how to obtain similar results using the [success](https://github.com/d-gomon/success) package on a simulated data set.

This directory contains:

- `SimulationExample.R`: an R script displaying the code for the reproducible example (with some comments).
- `Simulation_Example.Rmd`: an R markdown file displaying the code and describing the simulation procedure + some visual results.
- `Simulation_Example.pdf`: the output obtained by knitting `Simulation_Example.pdf` on my machine (sessionInfo() at the end of the file).
- `success_0.1.2.tar.gz`: Version of the [success](https://github.com/d-gomon/success) package used to run the code in this subdirectory.


The code in `SimulationExample.R` and `Simulation_Example.Rmd` consists of two parts:

- A small simulation study where the power over time of the Bernoulli, BK- and CGR-CUSUM charts is compared when restricting the type I error over time on an in-control sample.
- A small simulation study comparing the average run length (ARL) of the three methods when the in-control ARL is restricted on a sample of in-control hospitals.


