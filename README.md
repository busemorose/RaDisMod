# RaDisMod

## Description

RaDisMod is an R package developed by Guillaume Cinkus and Vincent Marc at Avignon Université. It contains one function that launches an R shiny application for modelling single flood events using production and transfer functions.

## Installation

RaDisMod requires an installation of R `4.0.0`**. The instruction for the installation and the download of R can be found on the [CRAN website](https://cran.r-project.org/).

Once R is installed, KarstID can be installed from [GitHub](https://github.com/busemorose/RaDisMod).

``` r
if (!require("devtools")) install.packages("devtools") # install devtools package if needed
devtools::install_github("busemorose/RaDisMod") # install RaDisMod package
```
## Launch

Once the package is installed, the application can be loaded with the `RaDisMod()` function.

``` r
library(RaDisMod)
RaDisMod()
```

or

```
RaDisMod::RaDisMod()
```

## Data import

The data import field allows to load a file with the following conditions:

-   The data must be a plain text file
-   The file must have two columns representing precipitation and observed discharge, respectively named `P` and `obs`
-   The columns must be semicolon-separated and of the same length

## Type of unit hydrogram

Four unit hydrogram are available by default:

- Socose
- Gamma
- Log-normal
- Triangle

It is possible to import a custom unit hydrogram using `Custom` in the drop-down menu. The file for a custom unit hydrogram must be a unique column without header.

The length of the unit hydrogram to consider can be specified with `Length of HU`.

## Production function

SCS, with $P_{cumul}$ the cumulated precipitation and $J$ a parameter to calibrate.

if $P_{cumul} < 0.2 * J$ then $P_{n-cumul} = 0$

else $P_{n-cumul} = \frac{(P_{cumul} - 0.2 * J)^2}{P_{cumul} + 0.8 * J}$

Then, to obtain the net precipitation $P_n$:

```
PN <- diff(c(0, PN_cum))
```

## Model calibration

Two options:

- Manual calibration using the three first sliders (`J` and two other parameters depending on the unit hydrogram)
- Automatic calibration in the "Parameter optimisation" section using the `Run` button at the bottom. It will randomly sample parameters within the specified ranges a number of times equal to the `Iterations` number. The individual simulations are then evaluated with the mentioned performance criteria, either `NSE`, `KGE`, `KGENP` and `RMSE`.

The evaluation range can be specified using the appropriate slider.





