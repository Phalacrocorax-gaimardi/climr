context("getIRLContrib")
library(dplyr)
res <- getIRLContrib("B","rcp26",3)
test_that("Test that temperature contribution is reasonable in 2050",{
 expect_lt(dplyr::filter(res, year==2050, variable=="Tgav")$ireland, 0.01)
 expect_gt(dplyr::filter(res, year==2050, variable=="Tgav")$ireland, 0)}
)
