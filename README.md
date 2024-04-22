# paper-tidyindex

The manuscript is produced in `tidyindex.qmd` and rendered into `tidyindex.pdf`. All the codes in the manuscript is extracted using `knitr::purl()` to `tidyindex.R` and the expected output is provided in `tidyindex.html`. In the `scripts` folder, you can find self-contained codes for each of the three examples in the paper. 


## Install instructions

An R version of 4.3 is needed to run the code and it can be downloaded from https://cran.r-project.org/

The following R packages, and their dependencies, are needed to run the reproducible code:  

```r
packages <- c(
    "tidyverse", "patchwork", "tidyindex", "lmomco", "ozmaps", "rmapshaper", 
    "colorspace", "SPEI", "tsibble", "GGally", "patchwork", "tourr", "magick", 
    "gifski"
)

```

They can be installed using the following code:

```r
install.packages(packages, repos = "https://cran.r-project.org")
```