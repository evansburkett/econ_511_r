# Header
source(here::here("git/econ_511_r/code/header.R"))

# Import/Build
raw <- read_csv(
  file.path(header$import, "01_nipa_real_gdp", "download.csv"),
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

build_3 <- build_2 %>% 
  gather(key = "date", value = "value", -var_name) %>% 
  mutate(value = as.numeric(value))

# Export
write_csv(
  build_3, 
  file.path(header$datasets, "01_nipa_real_gdp", "nipa_real_gdp.csv"))

write_rds(
  build_3, 
  file.path(header$datasets, "01_nipa_real_gdp", "nipa_real_gdp.rds"))

