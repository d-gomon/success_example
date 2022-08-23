# LROI_appl_sim

This directory contains:

- `LROI_Detection.Rmd`: Shows how the [success](https://github.com/d-gomon/success) package was used to determine the detection times of hospitals in the Dutch Arthroplasty Register (LROI) data set.
- `LROI_Power.Rmd`: Shows how the [success](https://github.com/d-gomon/success) package was used to perform a simulation study for power over time for hospitals with comparable failure rates to the Dutch Arthroplasty Register (LROI) data set.
- `success_0.1.0.tar.gz`: the binary [success](https://github.com/d-gomon/success) package which was used in the two files mentioned above.

The simulation study for power over time consisted of the following steps:

- Determine control limits on a simulated sample of $500$ hospitals such that the type I error over a certain time frame was restricted.
- Determine detection times on simulated samples of $500$ out-of-control hospitals.
- Compare the power over time (true detections over time) of the methods.
