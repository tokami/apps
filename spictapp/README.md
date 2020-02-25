spictapp: The click-based app for SPiCT
=====

The click-based app for the Stochastic surplus Production model in Continuous
Time (SPiCT; Pedersen and Berg, 2017).

## Installation

The package is hosted on GitHub and the installation requires the R package
'devtools'. Run the following code to install the package from GitHub:

```r
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("tokami/apps/spictapp")
```

## Getting Started

The [spictapp tutorial](https://github.com/tokami/apps/tree/master/spictapp) is
a good place to get started with spictapp. The tutorial explains the app
in-depth and walks you through a full SPiCT assessment using the app step by
step.

## Citation

Please cite the use of this package by:

Mildenberger, T. K. (2020). spictapp: The click-based app for SPiCT. R package
version 1.0.0, URL: https://github.com/tokami/apps/tree/master/spictapp

AND by acknowledging SPiCT by:

Pedersen, M. W., & Berg, C. W. (2017). A stochastic surplus production model in
continuous time. Fish and Fisheries, 18(2), 226-243.

The underlying model used in the package is described in a published
[`paper`](http://onlinelibrary.wiley.com/doi/10.1111/faf.12174/full). The last
non-formatted version of the paper can be found in the [SPiCT
package](https://github.com/DTUAqua/spict/blob/master/spict/inst/spict.pdf).

## License

The spictapp package as a whole is licensed under the GPLv3. See the
[LICENSE](LICENSE) file for more details.

## References

Pedersen, M. W., & Berg, C. W. (2017). A stochastic surplus production model in
continuous time. Fish and Fisheries, 18(2), 226-243.
