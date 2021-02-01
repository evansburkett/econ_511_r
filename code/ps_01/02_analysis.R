# Header
source(here::here("git/econ_511_r/code/header.R"))

################################################################################
# Read in data
################################################################################
nipa <- read_rds(
  file.path(header$datasets, "01_nipa_real_gdp", "nipa_real_gdp.rds"))

################################################################################
# Calculate trends
################################################################################
trend <- nipa %>% 
  mutate(log = log(value)) %>% 
  arrange(var_name, date) %>% 
  group_by(var_name) %>% 
  mutate(hp_log = as.numeric(hpfilter(log, freq = 1600)$trend)) %>% 
  ungroup() %>% 
  mutate(log_dev = log - hp_log)

################################################################################
# Correlations of log deviations with GDP log deviation
################################################################################
corr_log_dev <- trend %>% 
  select(var_name, log_dev, date) %>% 
  spread(var_name, log_dev) %>% 
  select(-date) %>% 
  cor() %>% 
  as.data.frame()

corr_log_dev <- corr_log_dev[rownames(corr_log_dev) == "gdp",]
rownames(corr_log_dev) <- NULL
corr_log_dev <- gather(corr_log_dev, key = "var_name", value = "corr")

################################################################################
# Standard deviations of log deviations
################################################################################
stdev_log_dev <- trend %>% 
  group_by(var_name) %>% 
  summarize(stdev = sd(log_dev)) %>% 
  ungroup()

gdp_stdev_log_dev <- stdev_log_dev$stdev[stdev_log_dev$var_name=="gdp"]

stdev_log_dev <- stdev_log_dev %>% 
  mutate(relative_to_gdp = stdev/gdp_stdev_log_dev)

################################################################################
# Calculate log change
################################################################################
change <- nipa %>% 
  mutate(log = log(value)) %>% 
  arrange(var_name, date) %>% 
  group_by(var_name) %>% 
  mutate(log_change = log - lag(log)) %>% 
  ungroup()

################################################################################
# Standard deviations of log change
################################################################################
stdev_change <- change %>% 
  group_by(var_name) %>% 
  summarize(stdev = sd(log_change, na.rm = T)) %>% 
  ungroup()

gdp_stdev_change <- stdev_change$stdev[stdev_change$var_name=="gdp"]

stdev_change <- stdev_change %>% 
  mutate(relative_to_gdp = stdev/gdp_stdev_change)

################################################################################
# Serial correlation of log deviations and log change
################################################################################
calc_fosc <- function(temp) { # function for first order serial correlation
  cor(temp[1:length(temp)-1], temp[2:length(temp)])}

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

################################################################################
# Export
################################################################################
wb <- createWorkbook()

header$add_worksheet(
  wb, 
  sheet = "Log Dev - Corr", 
  data = corr_log_dev,
  title = "Variables: Log deviation from trend",
  subtitle = "Statistic: Correlation with gdp")

header$add_worksheet(
  wb, 
  sheet = "Log Dev - StDev", 
  data = stdev_log_dev,
  title = "Variables: Log deviation from trend",
  subtitle = paste0(
    "Statistics: Standard deviation, ", 
    "Standard deviation as share of standard deviation of gdp"))

header$add_worksheet(
  wb, 
  sheet = "Log Dev - FOSC", 
  data = fosc_log_dev,
  title = "Variables: Log deviation from trend",
  subtitle = "Statistic: First order serial correlation")

header$add_worksheet(
  wb, 
  sheet = "Change - StDev", 
  data = stdev_change,
  title = "Variables: Log change",
  subtitle = paste0(
    "Statistics: Standard deviation, ", 
    "Standard deviation as share of standard deviation of gdp"))

header$add_worksheet(
  wb, 
  sheet = "Change - FOSC", 
  data = fosc_change,
  title = "Variables: Log change",
  subtitle = "Statistic: First order serial correlation")

saveWorkbook(
  wb, 
  file.path(header$export, "01_ps_01", "rgdp_corr_stdev.xlsx"), 
  overwrite = TRUE)

