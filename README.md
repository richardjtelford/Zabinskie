# Read Me

This repository includes all the code needed to replicate the manuscript "Review and test of reproducibility of sub-decadal resolution palaeoenvironmental reconstructions from microfossil assemblages" which explores the ecological, taphonomic and chronological challenges faced by sub-decadal resolution palaeoenvironmental reconstructions from microfossil assemblages using transfer functions. 

The manuscript also examines the available data for several chironomid-based reconstructions and finds several errors and anomalous patterns.

## Reproducing the manuscript

The code uses `packrat` to track the versions of the R packages used. To download the package versions used, run:

```
packrat::restore()
```

The file "make_limitations_of_high_res_quant_palaeo_manuscript.R" contains the `drake` plan that will run all analyses and render the manuscript as a PDF. 

Some data will need to be downloaded from public archives. The code will report when data are missing, where they can be downloaded from, where the files should be placed.


##Additional requirements

A few extra resources are required, mainly to handle the conversion of the rmarkdown file into a PDF.

xelatex
