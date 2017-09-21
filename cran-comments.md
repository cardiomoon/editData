This is the resubmission of the package 'dplyrAssist'

1) I omit the redundant part "The 'dplyrAssist' is'.

2) I add space between 'data' and "(as a 'tibble')".

3) This package has only one function - the 'dplyrAssist' function. It starts a shiny app and waits for the user's interaction. Without user's interaction, the app waits indefinitely. So it is not possible to include executable examples (without user's interaction) for checks on CRAN. I have added examples within \dontrun{}. This is executable example with user's interaction but not for checks on CRAN.

## Test environments
* local OS X install, R 3.4.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

