# Success example

<!-- badges: start -->
[![arXiv](https://img.shields.io/badge/stat.AP-arXiv%3A2205.07618-B31B1B)](https://doi.org/10.48550/arXiv.2205.07618)
<!-- badges: end -->

This repository aims to make the results from [this publication](https://arxiv.org/abs/2205.07618) reproducible. 
There are three folders, two of which allow for exact reproducibility of results (ARL_simulations and LROI_appl_sim).
The folder Reproducible_example demonstrates a simple example of the approaches using the publicly available package [success](https://github.com/d-gomon/success) on a simulated data set.

The three folders are described in more detail below.

## ARL_simulations

This folder contains:

- "ALLFUNCTIONS.Rmd": stand-alone functions used to perform the simulation study. 
- "ARL_simulations.Rmd": shows how the simulation study was performed using the functions in "ALLFUNCTIONS.Rmd"

The simulations study consisted of three steps:

- Determine control limits such that the average run length was approximately $15$ years on a simulated sample of $3000$ hospitals.
- Determine control charts on samples of $3000$ simulated out-of-control hospitals, with many different true failure rates.
- Compare average run lengths of the methods over these different failure rates.

Note: the code in this folder is quite slow and cannot be used for other practical applications of the methods. If you would like to construct control charts for your data, please take a look at the [success](https://github.com/d-gomon/success) package. Unfortunately, the [success](https://github.com/d-gomon/success) package does not incorporate ARL restriction. 

## LROI_appl_sim

This folder contains:

- "LROI_Detection.Rmd": Shows how the [success](https://github.com/d-gomon/success) package was used to determine the detection times of hospitals in the Dutch Arthroplasty data set.
- "LROI_Power.Rmd": Shows how the [success](https://github.com/d-gomon/success) package was used to perform a simulation study for power over time for hospitals with comparable failure rates to the Dutch Arthroplasty data set.
- "success_0.1.0.tar.gz": the binary [success](https://github.com/d-gomon/success) package which was used in the two files mentioned above.

The simulation study for power over time consisted of the following steps:

- Determine control limits on a simulated sample of $500$ hospitals such that the type I error over a certain time frame was restricted.
- Determine detection times on simulated samples of $500$ out-of-control hospitals.
- Compare the power over time (true detections over time) of the methods.


## Reproducible_example

As the code in ARL_simulations is very slow and the data set used in LROI_appl_sim is not publicly available, we provide a small reproducible example where we show how to obtain similar results using the [success](https://github.com/d-gomon/success) package on a simulated data set.

This folder contains:

- "SimulationExample.R": an R script displaying the code for the reproducible example (with some comments).
- "Simulation_Example.Rmd": an R markdown file displaying the code and describing the simulation procedure + some visual results.
- "Simulation_Example.pdf": the output obtained by knitting "Simulation_Example.pdf" on my machine (sessionInfo() at the end of the file).
- "success_0.1.2.tar.gz": Version of the [success](https://github.com/d-gomon/success) package used to run the code in this folder.

The aim of this folder is to provide users with a simple example of how to perform simulation studies using the [success](https://github.com/d-gomon/success) package. This is demonstrated on an enclosed simulated data set.






