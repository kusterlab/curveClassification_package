# curveClassification_package

For regular users, we recommend the Shiny App version of this tool: https://github.com/kusterlab/curveClassification_shiny

If you want to install the package in R, run the following (tested on R version 3.6.3):

```R
urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"
install.packages(urlPackage, repos=NULL, type="source")

install.packages('devtools');
require(devtools);
devtools::install_github('kusterlab/curveClassification_package', upgrade_dependencies=FALSE, dependencies=TRUE)
```

To load this package, run:

```R
library(CurveClassification)
```
