rm(list=ls())
# import ------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(reshape2)
  library(data.table)
  library(zoo)
  library(r2dii.utils)
  library(scales)
  library(pacta.scenario.preparation)
  library(pacta.data.preparation)
  library(dotenv)
  library(rlog)
  library(readxl)
})

dotenv::load_dot_env()

output_dir <- Sys.getenv("OUTPUT_DIR")
last_file_used <- Sys.getenv("LAST_FILE_USED")

config <-
  config::get(
    file = "config.yml",
    config = Sys.getenv("R_CONFIG_ACTIVE"),
    use_parent = FALSE
  )

pacta_financial_timestamp <- Sys.getenv("R_CONFIG_ACTIVE")


# Create Power results

power_results_empty <- read_xlsx(Sys.getenv("TEMPLATE_DIR"), sheet = "Electric Utilities", skip = 12)


trajectory_power <- read.csv(file.path(output_dir, paste0("power_", Sys.getenv("R_CONFIG_ACTIVE"),"_aggregate.csv")))

technology_power <- read.csv(file.path(output_dir, paste0("Trajectory_technology_",Sys.getenv("R_CONFIG_ACTIVE"),".csv"))) %>%
  filter(ald_sector == "Power")

power_results <- power_results_empty %>%
  left_join(trajectory_power, by = c('Company' = "company_name")) %>%
  left_join(technology_power, by = c('Company' = "company_name")) %>%
  mutate('1. Capacity Alignment with 1.5°C - value' = ifelse(!is.na(aggregate_score_class),aggregate_score_class,"Not Assessed"),
         '1. traffic light'  = ifelse(is.na(aggregate_score_traffic_light) | aggregate_score_traffic_light == "N.A.", "Not Assessed",aggregate_score_traffic_light),
         '1.1 Coal - value' = ifelse(is.na(Rating_2diU2.CoalCapT) | Rating_2diU2.CoalCapT == "N.A.", "Not Assessed",Rating_2diU2.CoalCapT),
         '1.1 traffic light' = ifelse(is.na(traffic_light_2diU2.CoalCapT) | traffic_light_2diU2.CoalCapT == "N.A.", "Not Assessed",traffic_light_2diU2.CoalCapT),
         '1.3 Oil - value' = ifelse(is.na(Rating_2diU2.OilCapT) | Rating_2diU2.OilCapT == "N.A.", "Not Assessed",Rating_2diU2.OilCapT),
         '1.3 traffic light' = ifelse(is.na(traffic_light_2diU2.OilCapT) | traffic_light_2diU2.OilCapT == "N.A.", "Not Assessed",traffic_light_2diU2.OilCapT),
         '1.2 Natural Gas - value' = ifelse(is.na(Rating_2diU2.GasCapT) | Rating_2diU2.GasCapT == "N.A.", "Not Assessed",Rating_2diU2.GasCapT),
         '1.2 traffic light' = ifelse(is.na(traffic_light_2diU2.GasCapT) | traffic_light_2diU2.GasCapT == "N.A.", "Not Assessed",traffic_light_2diU2.GasCapT),
         '1.4 Nuclear - value' = ifelse(is.na(Rating_2diU2.NuclearCapT) | Rating_2diU2.NuclearCapT == "N.A.", "Not Assessed",Rating_2diU2.NuclearCapT),
         '1.4 traffic light' = ifelse(is.na(traffic_light_2diU2.NuclearCapT) | traffic_light_2diU2.NuclearCapT == "N.A.", "Not Assessed",traffic_light_2diU2.NuclearCapT),
         '1.5 Hydro - value' = ifelse(is.na(Rating_2diU2.HydroCapT) | Rating_2diU2.HydroCapT == "N.A.", "Not Assessed",Rating_2diU2.HydroCapT),
         '1.5 traffic light' = ifelse(is.na(traffic_light_2diU2.HydroCapT) | traffic_light_2diU2.HydroCapT == "N.A.", "Not Assessed",traffic_light_2diU2.HydroCapT),
         '1.6 Renewables - value' = ifelse(is.na(Rating_2diU2.RenewablesCapT) | Rating_2diU2.RenewablesCapT == "N.A.", "Not Assessed",Rating_2diU2.RenewablesCapT),
         '1.6 traffic light'= ifelse(is.na(traffic_light_2diU2.RenewablesCapT) | traffic_light_2diU2.RenewablesCapT == "N.A.", "Not Assessed",traffic_light_2diU2.RenewablesCapT)
  ) %>%
  select(-c("Rating_2diU2.CoalCapT", "Rating_2diU2.OilCapT", "Rating_2diU2.GasCapT", "Rating_2diU2.HydroCapT", "Rating_2diU2.NuclearCapT", "Rating_2diU2.RenewablesCapT", "traffic_light_2diU2.CoalCapT", "traffic_light_2diU2.OilCapT", "traffic_light_2diU2.GasCapT", "traffic_light_2diU2.HydroCapT", "traffic_light_2diU2.NuclearCapT", "traffic_light_2diU2.RenewablesCapT", "single_score_Case8", "aggregate_score_traffic_light", "X.x", "rank", "aggregate_score_class", "X.y", "ald_sector", "Rating_2diA2.ElectricT", "Rating_2diA2.FuelCellT", "Rating_2diA2.HybridT", "Rating_2diA2.ICET", "traffic_light_2diA2.ElectricT", "traffic_light_2diA2.FuelCellT", "traffic_light_2diA2.HybridT", "traffic_light_2diA2.ICET"))


# Create Cement results


cement_results_empty <- read_xlsx(Sys.getenv("TEMPLATE_DIR"), sheet = "Cement", skip = 5)


cement_ei <- read.csv(file.path(Sys.getenv("OUTPUT_DIR"), paste0("/CA100_", pacta_financial_timestamp, "_EI.csv"))) %>%
  filter(ald_sector == "Cement")


cement_results <- cement_results_empty %>%
  left_join(cement_ei, by = c('Company' = "company_name")) %>%
  mutate(
    '1. traffic light' = traffic_light,

    '1. - value' = alignement_classement,

    # keep this line commented if we have to add it back in a next release
    #    '1a. 2DII Company Emissions Intensity in 2021' = paste0(as.character(round(emission_intensity_sector, 2)), "t of CO2 / t of Cement")
  ) %>%
  select(-c("X", "ald_sector", "year.x", "emission_intensity_sector", "scenario", "year.y", "value", "alignment_metric", "alignement_classement", "traffic_light"   ))



# Create Steel results


steel_results_empty <- read_xlsx(Sys.getenv("TEMPLATE_DIR"), sheet = "Steel", skip=5)


steel_ei <- read.csv(file.path(Sys.getenv("OUTPUT_DIR"), paste0("/CA100_", pacta_financial_timestamp, "_EI.csv"))) %>%
  filter(ald_sector == "Steel")


steel_results <- steel_results_empty %>%
  left_join(steel_ei, by = c('Company' = "company_name")) %>%
  mutate('1. traffic light' = ifelse(is.na(traffic_light), '2- 2DII  Alignment with the IEA’s Beyond 2 Degrees Scenario target in 2030 - Traffic Light',traffic_light),

         # keep commented if we need it at some point
         #'1. 2DII Company Emissions Intensity in 2021' = ifelse(is.na(emission_intensity_sector), '1. 2DII Company Emissions Intensity in 2021', paste0(as.character(round(emission_intensity_sector, 2)), "t of CO2 / t of Steel")),

         '1. - value' = ifelse(is.na(alignment_metric), '2. 2DII Alignment with the IEA’s Beyond 2 Degrees Scenario target in 2030', alignement_classement)
  ) %>%
  select(-c("X", "ald_sector", "year.x", "emission_intensity_sector", "scenario", "year.y", "value", "alignment_metric", "alignement_classement", "traffic_light"   ))


# Aviation

aviation_results_empty <- read_xlsx(Sys.getenv("TEMPLATE_DIR"), sheet = "Aviation", skip=5)


aviation_ei <- read.csv(file.path(Sys.getenv("OUTPUT_DIR"), paste0("/CA100_", pacta_financial_timestamp, "_EI.csv"))) %>%
  filter(ald_sector == "Aviation")

# keep if we need 2019 EI at some point
#aviation_ei_2019 <- read_xlsx(last_file_used, sheet = "ALIGNMENT ASSESSMENTS-Aviation")  %>%
#  select("Company Name", "1a. 2DII Company Emissions Intensity in 2019", "1b. Distance from 2019 emissions intensity to alignment with the International Energy Agency’s (IEA) Beyond 2°C Scenario target in 2030 - Value", "1b. Distance from 2019 emissions intensity to alignment with the International Energy Agency’s (IEA) Beyond 2°C Scenario target in 2030  - Traffic Light")


aviation_results <- aviation_results_empty %>%
  left_join(aviation_ei, by = c('Company' = "company_name")) %>%
#  left_join(aviation_ei_2019, by = 'Company Name') %>%
  mutate('1. traffic light' = ifelse(is.na(traffic_light), '2- 2DII  Alignment with the IEA’s Beyond 2 Degrees Scenario target in 2030 - Traffic Light',traffic_light),

         # keep commented if we need this value
         #'2a. 2DII Company Emissions Intensity in 2021' = ifelse(is.na(emission_intensity_sector), '1. 2DII Company Emissions Intensity in 2021', paste0(as.character(round(emission_intensity_sector*1000000, 1)), "g CO2/revenue passenger km")),

         '1. - value' = alignement_classement
  ) %>%
  select(-c("X", "ald_sector", "year.x", "emission_intensity_sector", "scenario", "year.y", "value", "alignment_metric", "alignement_classement", "traffic_light"   ))




# Create Auto results

auto_results_empty <- read_xlsx(Sys.getenv("TEMPLATE_DIR"), sheet = "Autos", skip = 5)


trajectory_auto <- read.csv(file.path(output_dir, paste0("auto_", Sys.getenv("R_CONFIG_ACTIVE"),"_aggregate.csv")))

technology_auto <- read.csv(file.path(output_dir, paste0("Trajectory_technology_",Sys.getenv("R_CONFIG_ACTIVE"),".csv"))) %>%
  filter(ald_sector == "Automotive") %>%
  filter(company_name != "Volvo AB")

# No need for HDV anymore
#trajectory_hdv <- read.csv(file.path(output_dir, paste0("hdv_", Sys.getenv("R_CONFIG_ACTIVE"),"_aggregate.csv")))

#technology_hdv <- read.csv(file.path(output_dir, paste0("Trajectory_technology_",Sys.getenv("R_CONFIG_ACTIVE"),".csv"))) %>%
#  filter(ald_sector == "HDV")

#trajectory_auto <- rbind(trajectory_auto, trajectory_hdv)
#technology_auto <- rbind(technology_auto, technology_hdv)

auto_results <- auto_results_empty %>%
  left_join(trajectory_auto, by = c('Company' = "company_name")) %>%
  left_join(technology_auto, by = c('Company' = "company_name")) %>%
  mutate('1. Production Plan alignment with 1.5°C - value' = ifelse(is.na(aggregate_score_class), "Not Assessed", aggregate_score_class),
         '1. traffic light'  = ifelse(is.na(aggregate_score_traffic_light) | aggregate_score_traffic_light == "N.A.", "Not Assessed", aggregate_score_traffic_light),
         '1.1 ICE - value' = ifelse(is.na(Rating_2diA2.ICET) | Rating_2diA2.ICET == "N.A.", "Not Assessed", Rating_2diA2.ICET),
         '1.1 traffic light' = ifelse(is.na(traffic_light_2diA2.ICET) | traffic_light_2diA2.ICET == "N.A.", "Not Assessed", traffic_light_2diA2.ICET),
         '1.2 Hybrid - value' = ifelse(is.na(Rating_2diA2.HybridT) | Rating_2diA2.HybridT == "N.A.", "Not Assessed", Rating_2diA2.HybridT),
         '1.2 traffic light' = ifelse(is.na(traffic_light_2diA2.HybridT) | traffic_light_2diA2.HybridT == "N.A.", "Not Assessed", traffic_light_2diA2.HybridT),
         '1.3 EVs - value' = ifelse(is.na(Rating_2diA2.ElectricT) | Rating_2diA2.ElectricT == "N.A.", "Not Assessed", Rating_2diA2.ElectricT),
         '1.3 traffic light' = ifelse(is.na(traffic_light_2diA2.ElectricT) | traffic_light_2diA2.ElectricT == "N.A.", "Not Assessed", traffic_light_2diA2.ElectricT)
  ) %>%
  select(-c("Rating_2diU2.CoalCapT", "Rating_2diU2.OilCapT", "Rating_2diU2.GasCapT", "Rating_2diU2.HydroCapT", "Rating_2diU2.NuclearCapT", "Rating_2diU2.RenewablesCapT", "traffic_light_2diU2.CoalCapT", "traffic_light_2diU2.OilCapT", "traffic_light_2diU2.GasCapT", "traffic_light_2diU2.HydroCapT", "traffic_light_2diU2.NuclearCapT", "traffic_light_2diU2.RenewablesCapT", "single_score_Case8", "aggregate_score_traffic_light", "X.x", "rank", "aggregate_score_class", "X.y", "ald_sector", "Rating_2diA2.ElectricT", "Rating_2diA2.FuelCellT", "Rating_2diA2.HybridT", "Rating_2diA2.ICET", "traffic_light_2diA2.ElectricT", "traffic_light_2diA2.FuelCellT", "traffic_light_2diA2.HybridT", "traffic_light_2diA2.ICET"))




ca100 <- openxlsx::loadWorkbook(Sys.getenv("TEMPLATE_DIR"))


openxlsx::writeData(
  ca100,
  "Electric Utilities",
  power_results,
  startCol = 1,
  startRow = 13,
  array = FALSE,
  xy = NULL,
  colNames = TRUE,
  rowNames = FALSE,
  headerStyle = openxlsx_getOp("headerStyle"),
  borders = openxlsx_getOp("borders", "none"),
  borderColour = openxlsx_getOp("borderColour", "black"),
  borderStyle = openxlsx_getOp("borderStyle", "thin"),
  withFilter = openxlsx_getOp("withFilter", FALSE),
  keepNA = openxlsx_getOp("keepNA", FALSE),
  na.string = openxlsx_getOp("na.string"),
  name = NULL,
  sep = ", "
)


writeData(
  ca100,
  "Autos",
  auto_results,
  startCol = 1,
  startRow = 6,
  array = FALSE,
  xy = NULL,
  colNames = TRUE,
  rowNames = FALSE,
  headerStyle = openxlsx_getOp("headerStyle"),
  borders = openxlsx_getOp("borders", "none"),
  borderColour = openxlsx_getOp("borderColour", "black"),
  borderStyle = openxlsx_getOp("borderStyle", "thin"),
  withFilter = openxlsx_getOp("withFilter", FALSE),
  keepNA = openxlsx_getOp("keepNA", FALSE),
  na.string = openxlsx_getOp("na.string"),
  name = NULL,
  sep = ", "
)

writeData(
  ca100,
  "Cement",
  cement_results,
  startCol = 1,
  startRow = 6,
  array = FALSE,
  xy = NULL,
  colNames = TRUE,
  rowNames = FALSE,
  headerStyle = openxlsx_getOp("headerStyle"),
  borders = openxlsx_getOp("borders", "none"),
  borderColour = openxlsx_getOp("borderColour", "black"),
  borderStyle = openxlsx_getOp("borderStyle", "thin"),
  withFilter = openxlsx_getOp("withFilter", FALSE),
  keepNA = openxlsx_getOp("keepNA", FALSE),
  na.string = openxlsx_getOp("na.string"),
  name = NULL,
  sep = ", "
)

writeData(
  ca100,
  "Steel",
  steel_results,
  startCol = 1,
  startRow = 6,
  array = FALSE,
  xy = NULL,
  colNames = TRUE,
  rowNames = FALSE,
  headerStyle = openxlsx_getOp("headerStyle"),
  borders = openxlsx_getOp("borders", "none"),
  borderColour = openxlsx_getOp("borderColour", "black"),
  borderStyle = openxlsx_getOp("borderStyle", "thin"),
  withFilter = openxlsx_getOp("withFilter", FALSE),
  keepNA = openxlsx_getOp("keepNA", FALSE),
  na.string = openxlsx_getOp("na.string"),
  name = NULL,
  sep = ", "
)

writeData(
  ca100,
  "Aviation",
  aviation_results,
  startCol = 1,
  startRow = 6,
  array = FALSE,
  xy = NULL,
  colNames = TRUE,
  rowNames = FALSE,
  headerStyle = openxlsx_getOp("headerStyle"),
  borders = openxlsx_getOp("borders", "none"),
  borderColour = openxlsx_getOp("borderColour", "black"),
  borderStyle = openxlsx_getOp("borderStyle", "thin"),
  withFilter = openxlsx_getOp("withFilter", FALSE),
  keepNA = openxlsx_getOp("keepNA", FALSE),
  na.string = openxlsx_getOp("na.string"),
  name = NULL,
  sep = ", "
)

openxlsx::saveWorkbook(ca100, Sys.getenv("FINAL_FILE"), overwrite = TRUE)
