---
title: "package editData : An RStudio Addin for Editing A 'data.frame'"
author: "Keon-Woong Moon"
date: "2017-09-23"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{editData}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# editData


The 'editData' is an RStudio addin for editing a 'data.frame' or a 'tibble'. Many RStudio users want to edit a data.frame. With this 'editData' package, you can delete, add or update a 'data.frame' without coding. You don't have to use Microsoft excel or a csv editor any more to edit data. You can get resultant data as a 'tibble' or as a 'data.frame'.

## Install package

You can install `editData` package from CRAN.

```r
install.packages("editData")
```

You can install the developmental version of `editData` package from github.


```r
#install.packages("devtools")
devtools::install_github("cardiomoon/editData")
```

After install this `editData` package you can see the `editData` addin in RStudio's addins. (See the second plot).

## Usage: As an RStudio Add-in


This addin can be used to interactively manipulate a `data.frame` or a `tibble`. The intended way to use this is as follows:

1. Highlight a symbol naming a `data.frame` or a `tibble` in your R session, e.g. `mtcars`(1).

<img src="https://raw.githubusercontent.com/cardiomoon/editData/master/man/figures/1.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" width="70%" style="display: block; margin: auto;" />

2. Execute this addin(arrow), to interactively manipulate it.

<img src="https://raw.githubusercontent.com/cardiomoon/editData/master/man/figures/2.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" width="70%" style="display: block; margin: auto;" />

3. You can select and unselect a row by clicking a row in dataTable. You can delete the selected row(1), add a new row(2) or edit a row(3).

<img src="https://raw.githubusercontent.com/cardiomoon/editData/master/man/figures/3.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" width="70%" style="display: block; margin: auto;" />

4. If you press the edit button you can see this window. You can move to the desired row, edit rowname and individual data. You can delete the row or update the data.

<img src="https://raw.githubusercontent.com/cardiomoon/editData/master/man/figures/4.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" width="70%" style="display: block; margin: auto;" />

5. By default, the `sampleData` included in the `editData` package is selected. The `sex` and `bloodType` column are `factor` variables. A `selectInput` is assigned for a column of class factor.

<img src="https://raw.githubusercontent.com/cardiomoon/editData/master/man/figures/5.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" width="70%" style="display: block; margin: auto;" />

6. A `dateInput` is assigend for a column of class `date`.

<img src="https://raw.githubusercontent.com/cardiomoon/editData/master/man/figures/6.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" width="70%" style="display: block; margin: auto;" />



## Usage: As a regular function

You can use the `editData()` function as a regular function, e.g. in a command line.


```r
require(editData)
result <- editData(mtcars)
```

The resultant 'tibble' or 'data.frame' is assigned to the object `result`.
