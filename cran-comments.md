## Resubmission -- 0.1.3

This is a resubmission. In this version I have:

* Reformatted the DESCRIPTION file to follow the CRAN guidelines for writing package names.
* Updated code in the `ordinalsimr-options` vignette to reset the options to their default values after the vignette is run.
* Reduced the number of simulation iterations from 50 to 40 in the example for `run_simulations.R` to keep the run time for examples under 5s. Previously, the example took 5.019s to run on r-devel-linux-x86_64-debian-gcc.
* A @return tag was added to the `run_app` function in the `run_app.R` file, and indicates the function is called for its side-effects

Additionally, we are currently working on a manuscript that will be added to the DESCRIPTION and CITATION files in a later release.

