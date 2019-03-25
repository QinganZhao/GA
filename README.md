<div align=center>

# GA
### An R library for variable selection in regression problems based on genetic algorithm
[Franklin Zhao](http://franklinzhao.top), [Mia Zhong](https://github.com/Mia-Zhong)

[C&#244;me de Lassus Saint Geni&#232;s](https://github.com/ComedeLassus), and [Arman Jabbari](https://github.com/arm4nn)

<div align=left>

**IMPORTANT**: Since some parts in this library were developed with the help of the function *dplyr::sample_n()*, please make sure you have installed [dplyr](https://www.r-pkg.org/pkg/dplyr) library. Also, please make sure you have installed [devtools](https://www.r-pkg.org/pkg/devtools) library so that GA can be imported via Github.

If you would like to use the parallelization technique in this library, please also make sure you have installed the following libraries: [parallel](http://stat.ethz.ch/R-manual/R-devel/library/parallel/doc/parallel.pdf), [doParallel](https://www.r-pkg.org/pkg/doParallel), and [foreach](https://www.r-pkg.org/pkg/foreach).

#### Use the library
Please run the following code in R:
``` r
devtools::install_github('QinganZhao/GA')
library(GA)
```

#### Run the tests in the library
Please make sure you have installed [testthat](https://www.r-pkg.org/pkg/testthat) library, and run the following code in R:
``` r
testthat::test_package('GA')
```

#### Develop the library
The library is welcomed to be refined. Please push the updated library to Github and reinstall it before using.

To update the help pages, please run the following code after changing:
``` r
devtools::document()
```
