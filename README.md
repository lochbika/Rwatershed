# R watershed package
## Installation
Installation can be done with the devtools package.

    library(devtools)
    install_github("lochbika/Rwatershed",ref="main")
    
Alternatively, you can download/clone this repository and manually run the installation routines in RStudio.

## Usage
This R package contains functions to compute the watershed segmentation of an input matrix. Call the respective implementation with the wrapper function watershed, for example

    > watershed(x, method = "R")

where x is the input matrix.
    
## More information
The documentation is a good starting point. For a full tutorial and demonstration, check out the [blog post on my personal website](https://lochbihler.nl/practice-your-r-two-options-to-implement-the-watershed-segmentation/).
