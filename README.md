actMon
=========================================================
This package provides a set of functions designed to streamline the analysis of datasets produced by TriKinetics hardware using R. Complex operations like syncronizing an experiment to its light cycle or calculating sleep are reduced to simple 1-line functions like `syncLightCycle()`. `actMon` also calculates basic statistics on your data and creates basic plots using the `ggplot2` package. 

## Installation
### Getting the required dependencies
You will need Hadley Wickham's `devtools` package. To install, enter the following command into the R console: 
```{r} 
install.packages("devtools")
``` 

If you are on Windows, building R packages from source requires installing RTools. You can get it from [here](http://cran.r-project.org/bin/windows/Rtools/). Make sure you get the correct version of RTools for your version of R (you can check your version of R from the message you get on starting a new R session).

### Installing actMon
Type the following into the R console:
```{r}
devtools::install_github("kazi11/actMon")
```

That's it! You're done! You can now access the functions provided by this package in R using `library(actMon)`.

## License
This software is licensed under the [GNU-GPL 3 license](https://tldrlegal.com/license/gnu-general-public-license-v3-%28gpl-3%29#summary).
