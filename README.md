# Read Me

This repository includes all the code needed to replicate the manuscript "Review and test of reproducibility of sub-decadal resolution palaeoenvironmental reconstructions from microfossil assemblages" which explores the ecological, taphonomic and chronological challenges faced by sub-decadal resolution palaeoenvironmental reconstructions from microfossil assemblages using transfer functions. 

The manuscript also examines the available data for several chironomid-based reconstructions and finds several errors and anomalous patterns.

## Reproducing the manuscript

The code is written in R and used `rmarkdown` to render the manuscript. I recommend using RStudio, but you can reproduce the manuscript directly in R, but you have to take care of the working directory and packrat yourself. 

1) Install non-R dependencies. The code needs `GDAL`. On Windows, this is installed automatically when the `rgdal` package is installed, on Linux and Mac you need to [install it manually](https://stackoverflow.com/questions/15248815/rgdal-package-installation). The manuscript is turned into a pdf by the `tinytex` package. This needs `TinyTeX`, which can be installed by running `tinytex::install_tinytex()` in R.

2) Clone this repo by clicking on the green button above and copying the URL. Now, in RStudio go to `File > New Project... > Version Control > Git`, paste in the URL and fill in the desired folder name and path.

3) The code uses `packrat` to track the versions of the R packages used. `packrat` should start automatically and download packages. This may take some time. If nothing happens, or it the process needs restarting, force `packrat` with `packrat::restore()`.

4) For convenience, all of the data sets required for the analysis have been pushed to GitHub (Data are available under their original license). The exception is the data for Speke Hall Lake which the authors have agreed to archive in 2019.

5) The file "make_limitations_of_high_res_quant_palaeo_manuscript.R" contains the `drake` plan that will run all analyses and render the manuscript as a PDF. Source this file into R with

```
source("make_limitations_of_high_res_quant_palaeo_manuscript.R")
```