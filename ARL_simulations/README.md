# ARL_simulations

This directory contains:

- `ALLFUNCTIONS.Rmd`: stand-alone functions used to perform the simulation study. 
- `ARL_simulations.Rmd`: shows how the simulation study was performed using the functions in `ALLFUNCTIONS.Rmd`

The simulations study consisted of three steps:

- Determine control limits such that the average run length was approximately $15$ years on a simulated sample of $3000$ hospitals.
- Determine control charts on samples of $3000$ simulated out-of-control hospitals, with many different true failure rates.
- Compare average run lengths of the methods over these different failure rates.

Note: the code in this directory is quite slow and cannot be used for other practical applications of the methods. If you would like to construct control charts for your data, please take a look at the [success](https://github.com/d-gomon/success) package. Unfortunately, the [success](https://github.com/d-gomon/success) package does not incorporate ARL restriction. 



