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
