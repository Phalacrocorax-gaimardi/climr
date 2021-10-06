# climr

 <!-- badges: start -->
  [![R build status](https://github.com/Phalacrocorax-gaimardi/climr/workflows/R-CMD-check/badge.svg)](https://github.com/Phalacrocorax-gaimardi/climr/actions)
  <!-- badges: end -->

climr calculates national or regional contributions to global warming given historical emissions, and future emissions scenarios. Currently configured to examine methane policy for Ireland.

## Installation

```r
# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("Phalacrocorax-gaimardi/climr")
# major dependencies include the Hector R package
```

## Usage

```r
# calculate the 1745-2000 GSAT contribution for Ireland in domestic scenario "E" and global SSP1-1.9 pathway, with default parameter settings
dgsat <- climr::getIRL_dGSAT("E","ssp1-19")
# plot results
```  

## Data
 

For example, it can identify combinations of methane and co2 emissions pathways lead to climate neutrality before 2050.

![alt text](https://github.com/Phalacrocorax-gaimardi/climr/blob/main/images/hector_results_matrix.png "Scenarios ECS=2.9C")


