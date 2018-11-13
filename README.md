# Read Me

This repository includes all the code needed to replicate the manuscript "Review and test of reproducibility of sub-decadal resolution palaeoenvironmental reconstructions from microfossil assemblages" which explores the ecological, taphonomic and chronological challenges faced by sub-decadal resolution palaeoenvironmental reconstructions from microfossil assemblages using transfer functions. 

The manuscript also examines the available data for several chironomid-based reconstructions and finds several errors and anomalous patterns.

## Reproducing the manuscript

The code is written in R and used rmarkdown to render the manuscript. I recommend using RStudio, but you can reproduce the manuscript directly in R, but you have to take care of the working directory and other things yourself. 

1) Clone this repo by clicking on the green button above and copying the URL. Now, in RStudio go to `File > New Project... > Version Control > Git` and paste in the URL.

2) The code uses `packrat` to track the versions of the R packages used. To download the package versions used, run:

```
packrat::restore()
```
3) For convenience, all of the data sets required for the analysis have been pushed to GitHub and will be automatically retrieved when you clone the repo, or will be downloaded automatically (Data are available under their original license). The exception is the data for Speke Hall Lake which the authors have agreed to archive in 2019.

4) The file "make_limitations_of_high_res_quant_palaeo_manuscript.R" contains the `drake` plan that will run all analyses and render the manuscript as a PDF.

## Additional requirements

Some additional software may be required to convert the markdown file to a PDF. Running `tinytex::install_tinytex()` should install this. 