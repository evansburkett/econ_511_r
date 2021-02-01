# Clear workspace
rm(list=ls())

# Create filepaths
header <- list()

header$import   <- here::here("git/econ_511_r/import")
header$datasets <- here::here("git/econ_511_r/datasets")
header$export   <- here::here("git/econ_511_r/export")

# Load packages
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(mFilter)
library(openxlsx)

# Create user-defined functions
header$add_worksheet <- function(wb, sheet, title, subtitle, data) {
  addWorksheet(wb, sheet)
  writeData(wb, sheet, startRow = 1, title)
  writeData(wb, sheet, startRow = 2, subtitle)
  writeData(wb, sheet, startRow = 4, data)
}