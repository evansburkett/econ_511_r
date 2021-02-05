################################################################################
# Header
################################################################################
rm(list=ls())

# SET WORKING DIRECTORY HERE
wd <- "C:/Users/evans/OneDrive/Documents/git/econ_511_r/submission/ps_01" 

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, mFilter, openxlsx)

add_worksheet <- function(wb, sheet, title, subtitle, data) {
  addWorksheet(wb, sheet)
  writeData(wb, sheet, startRow = 1, title)
  writeData(wb, sheet, startRow = 2, subtitle)
  writeData(wb, sheet, startRow = 4, data)
}

################################################################################
# 01) Import and build data
################################################################################

raw <- read_csv(
  file.path(wd, "nipa_table_1_1_3.csv"),
  col_names = F,
  skip = 4)

build_1 <- raw %>% 
  mutate(var_name = case_when(
    X2 == "Gross domestic product"            ~ "gdp",
    X2 == "Personal consumption expenditures" ~ "c",
    X2 == "Gross private domestic investment" ~ "investment",
    X2 == "Fixed investment"                  ~ "fixed_i",
    X2 == "Nonresidential"                    ~ "nonresidential",
    X2 == "Structures"                        ~ "structures",
    X2 == "Equipment"                         ~ "equipment",
    X2 == "Intellectual property products"    ~ "intellectual",
    X2 == "Residential"                       ~ "residential")) %>% 
  filter(X1 == "Line" | !is.na(var_name)) %>% 
  select(-X1, -X2)

year_quarter <- c(
  paste0(
    as.character(build_1[1,1:ncol(build_1)-1]), 
    as.character(build_1[2,1:ncol(build_1)-1])),
  "var_name")

build_2 <- build_1 %>% 
  set_names(year_quarter) %>% 
  filter(!is.na(var_name))

nipa <- build_2 %>% 
  gather(key = "date", value = "value", -var_name) %>% 
  mutate(value = as.numeric(value))

################################################################################
# 02) Analyze and export analysis
################################################################################

# 02a) Calculate trends
trend <- nipa %>% 
  mutate(log = log(value)) %>% 
  arrange(var_name, date) %>% 
  group_by(var_name) %>% 
  mutate(hp_log = as.numeric(hpfilter(log, freq = 1600)$trend)) %>% 
  ungroup() %>% 
  mutate(log_dev = log - hp_log)

# 02b) Correlations of log deviations with GDP log deviation
corr_log_dev <- trend %>% 
  select(var_name, log_dev, date) %>% 
  spread(var_name, log_dev) %>% 
  select(-date) %>% 
  cor() %>% 
  as.data.frame()

corr_log_dev <- corr_log_dev[rownames(corr_log_dev) == "gdp",]
rownames(corr_log_dev) <- NULL
corr_log_dev <- gather(corr_log_dev, key = "var_name", value = "corr")

# 02c) Standard deviations of log deviations
stdev_log_dev <- trend %>% 
  group_by(var_name) %>% 
  summarize(stdev = sd(log_dev)) %>% 
  ungroup()

gdp_stdev_log_dev <- stdev_log_dev$stdev[stdev_log_dev$var_name=="gdp"]

stdev_log_dev <- stdev_log_dev %>% 
  mutate(relative_to_gdp = stdev/gdp_stdev_log_dev)

# 02d) Calculate log change
change <- nipa %>% 
  mutate(log = log(value)) %>% 
  arrange(var_name, date) %>% 
  group_by(var_name) %>% 
  mutate(log_change = log - lag(log)) %>% 
  ungroup()

# 02e) Standard deviations of log change
stdev_change <- change %>% 
  group_by(var_name) %>% 
  summarize(stdev = sd(log_change, na.rm = T)) %>% 
  ungroup()

gdp_stdev_change <- stdev_change$stdev[stdev_change$var_name=="gdp"]

stdev_change <- stdev_change %>% 
  mutate(relative_to_gdp = stdev/gdp_stdev_change)

# 02f) Serial correlation of log deviations and log change
fosc_log_dev <- trend %>% 
  group_by(var_name) %>% 
  summarize(
    fosc = acf(
      log_dev, lag.max = 1, plot = F)[["acf"]][2]) %>%  
  ungroup()

fosc_change <- change %>% 
  filter(!is.na(log_change)) %>% 
  group_by(var_name) %>% 
  summarize(
    fosc = acf(
      log_change, lag.max = 1, plot = F)[["acf"]][2]) %>%  
  ungroup()

# 02g) Export
wb <- createWorkbook()

add_worksheet(
  wb, 
  sheet = "Log Dev - Corr", 
  data = corr_log_dev,
  title = "Variables: Log deviation from trend",
  subtitle = "Statistic: Correlation with gdp")

add_worksheet(
  wb, 
  sheet = "Log Dev - StDev", 
  data = stdev_log_dev,
  title = "Variables: Log deviation from trend",
  subtitle = paste0(
    "Statistics: Standard deviation, ", 
    "Standard deviation as share of standard deviation of gdp"))

add_worksheet(
  wb, 
  sheet = "Log Dev - FOSC", 
  data = fosc_log_dev,
  title = "Variables: Log deviation from trend",
  subtitle = "Statistic: First order serial correlation")

add_worksheet(
  wb, 
  sheet = "Change - StDev", 
  data = stdev_change,
  title = "Variables: Log change",
  subtitle = paste0(
    "Statistics: Standard deviation, ", 
    "Standard deviation as share of standard deviation of gdp"))

add_worksheet(
  wb, 
  sheet = "Change - FOSC", 
  data = fosc_change,
  title = "Variables: Log change",
  subtitle = "Statistic: First order serial correlation")

saveWorkbook(
  wb, 
  file.path(wd, "output.xlsx"), 
  overwrite = TRUE)
