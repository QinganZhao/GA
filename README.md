<div align=center>

# GA
### An R package for variable selection in regression problems based on genetic algorithm
[C&#244;me de Lassus Saint Geni&#232;s](https://github.com/ComedeLassus)

[Arman Jabbari](https://github.com/arm4nn)

[Qingan Zhao](https://github.com/QinganZhao)

[Mia Zhong](https://github.com/Mia-Zhong)

(in alphabetical order)

<div align=left>

**IMPORTANT**: Since some parts in this package were developed with the help of the function *dplyr::sample_n()*, please make sure you have installed [dplyr](https://www.r-pkg.org/pkg/dplyr) package. Also, please make sure you have installed [devtools](https://www.r-pkg.org/pkg/devtools) package so that GA can be imported via Github.

If you would like to use the parallelization technique in this package, please also make sure you have installed the following packages: [parallel](http://stat.ethz.ch/R-manual/R-devel/library/parallel/doc/parallel.pdf), [doParallel](https://www.r-pkg.org/pkg/doParallel), and [foreach](https://www.r-pkg.org/pkg/foreach).
<br />
#### Use the package
Please run the following code in R:
> devtools::install_github('arm4nn/GA')<br>
> library(GA)<br />

#### Run the tests in the package
Please make sure you have installed [testthat](https://www.r-pkg.org/pkg/testthat) package, and run the following code in R:
> testthat::test_package('GA')<br />

#### Develop the package
The package is welcomed to be refined. Please push the updated package to Github and reinstall it before using.

To update the help pages, please run the following code after changing:
> devtools::document()




