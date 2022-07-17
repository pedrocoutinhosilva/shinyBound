library(testthat)
library(magrittr)
library(rvest)
library(htmltools)
library(shinyBound)

start_time <- Sys.time()
test_check("shinyBound")

print("Run time: ", Sys.time() - start_time)
