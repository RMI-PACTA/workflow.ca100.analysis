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
})

dotenv::load_dot_env()

config <-
  config::get(
    file = "config.yml",
    config = Sys.getenv("R_CONFIG_ACTIVE"),
    use_parent = FALSE
  )

data_store_path <- Sys.getenv("DATA_STORE_PATH")
output_dir <- Sys.getenv("OUTPUT_DIR")
dir.create(output_dir)


masterdata_ownership_filename <- config$masterdata_ownership_filename
scenario_sources_list <- config$scenario_sources_list
sector_list <- config$sector_list
other_sector_list <- config$other_sector_list
zero_emission_factor_techs <- config$zero_emission_factor_techs
green_techs <- config$green_techs
scenario_raw_data_to_include <- config$scenario_raw_data_to_include
market_share_target_reference_year <- config$market_share_target_reference_year
tech_exclude <- config$tech_exclude
scenario_geographies_list <- config$scenario_geographies_list
global_aggregate_sector_list <- config$global_aggregate_sector_list
global_aggregate_scenario_sources_list <- config$global_aggregate_scenario_sources_list
time_horizon <- config$time_horizon
pacta_financial_timestamp <- Sys.getenv("R_CONFIG_ACTIVE")


ald_eq_raw <- read.csv(file.path(Sys.getenv("DATA_STORE_PATH"), masterdata_ownership_filename))

# tidy --------------------------------------------------------------------
ald_eq_all <- ald_eq_raw %>% pivot_longer(cols = starts_with("X_"),
                                          names_to = "year",
                                          names_prefix = "X_",
                                          values_to = "ald_production") %>%
  mutate(
    emissions_factor = if_else(
      technology %in% c("Electric", "HydroCap", "NuclearCap", "RenewablesCap") & ald_production > 0,
      0,
      emissions_factor),
    emissions_factor_unit = "-"
  )

ald_eq <- ald_eq_all %>%
  group_by(company_name, !!!syms("company_id"), sector, asset_country, technology,
           year, unit, emissions_factor, emissions_factor_unit) %>%
  summarise(ald_production = sum(ald_production, na.rm = TRUE), .groups = "drop")



ald_eq <- ald_eq %>%
  transmute(
    id = as.character(.data[["company_id"]]),
    id_name = as.character("company_id"),
    ald_sector = as.character(sector),
    ald_location = as.character(asset_country),
    technology = as.character(technology),
    year = as.numeric(year),
    ald_production = as.numeric(ald_production),
    ald_production = if_else(ald_production <= 0, 0, ald_production),
    ald_production_unit = as.character(unit),
    ald_emissions_factor = as.numeric(emissions_factor),
    ald_emissions_factor_unit = as.character(emissions_factor_unit),
    company_name = as.character(company_name)
  ) %>%
  mutate(technology =
           case_when(
             technology == "Oil and Condensate" ~ "Oil",
             ald_sector == "Coal" ~ "Coal",
             TRUE ~ technology
           )) %>%
  mutate(technology = sub("Grade$", "", technology)) %>%
  group_by(across(-ald_production)) %>%
  summarise(ald_production = sum(ald_production, na.rm = TRUE), .groups = "drop")

relevant_years <- sort(
  unique(
    c(
      market_share_target_reference_year:(market_share_target_reference_year + time_horizon)
    )
  )
)
log_info(
  paste(
    "Full time horizon set to:",
    paste0(relevant_years, collapse = ", ")
  )
)

log_info("Catching scenario data... ") # We provide HDV scenario data, and CA100 only wants IEA scenario, so we have to import ETP2017 HDV scenario.
hdv_etp_pathway <- read.csv(Sys.getenv("ETP_PATHWAY")) %>%
  select(-c (tmsr,smsp)) %>%
  filter(year >= market_share_target_reference_year) %>%
  mutate(scenario = case_when(
    scenario == "b2ds" ~ "B2DS",
    scenario == "sds" ~ "SDS",
    scenario == "rts" ~ "NPS",
    )
  )%>%
  rename(scenario_geography = "scenario_region",
         units = "unit") %>%
  mutate(source = "ETP2017",
         indicator = "#") %>%
  mutate(sector = "HDV",
         technology = case_when(
           technology == "electric" ~ "Electric_HDV",
           technology == "hybrid" ~ "Hybrid_HDV",
           technology == "ice" ~ "ICE_HDV"
         ))


log_info("Preparing scenario data... ")
scenarios_analysis_input_path <- file.path(Sys.getenv("OUTPUT_DIR"), "Scenarios_AnalysisInput.csv")
scenario_raw_data_to_include <- lapply(scenario_raw_data_to_include, get, envir = asNamespace("pacta.scenario.preparation"))
scenario_raw_data <- bind_rows(scenario_raw_data_to_include)

# scenario values will be linearly interpolated for each group below
interpolation_groups <- c(
  "source",
  "scenario",
  "sector",
  "technology",
  "scenario_geography",
  "indicator",
  "units"
)

Scenario_AnalysisInput <- scenario_raw_data %>%
  pacta.scenario.preparation::interpolate_yearly(!!!rlang::syms(interpolation_groups)) %>%
  rbind(hdv_etp_pathway) %>%
  filter(.data$year >= .env$market_share_target_reference_year) %>%
  pacta.scenario.preparation::add_market_share_columns(reference_year = market_share_target_reference_year) %>%
  pacta.scenario.preparation::format_p4i(green_techs)


write_csv(Scenario_AnalysisInput, file.path(output_dir,"Scenario_AnalysisInput.csv"), na = "")

# We don't need country of domicile here
ar_company_id__country_of_domicile <- tibble(
  ar_company_id = "",
  country_of_domicile = ""
)

masterdata_ownership_path <- file.path(Sys.getenv("DATA_STORE_PATH"), masterdata_ownership_filename)

masterdata_ownership_datastore <- readr::read_csv(masterdata_ownership_path, na = "", show_col_types = FALSE) %>%
  pacta.data.preparation::prepare_masterdata(
    ar_company_id__country_of_domicile,
    pacta_financial_timestamp,
    zero_emission_factor_techs
  )


saveRDS(masterdata_ownership_datastore, file.path(output_dir, "masterdata_ownership_datastore.rds"))

# Review classification before next release
ca_companies <- tibble(
  company_name = c("The AES Corp.","American Electric Power Co., Inc.","Bayerische Motoren Werke AG","CEZ as","Dominion Energy, Inc.","Duke Energy Corp.","E.ON SE",
                  "Électricité de France SA","Enel SpA","ENGIE SA","Eskom Holdings SOC Ltd.","Exelon Corp.","FirstEnergy Corp.","Fortum Oyj","Iberdrola SA","Korea Electric Power Corp.",
                  "National Grid Plc","NextEra Energy, Inc.","NRG Energy, Inc.","Ntpc Ltd.","PGE Polska Grupa Energetyczna SA","Power Assets Holdings Ltd.","PPL Corp.","RWE AG","SSE Plc","The Southern Co.",
                  "Vistra Corp.","WEC Energy Group, Inc.","Xcel Energy, Inc.","Daimler AG","Ford Motor Co.","General Motors Co.","Honda Motor Co., Ltd.","Nissan Motor Co., Ltd.",
                  "Renault SA","SAIC Motor Corp. Ltd.", "Suzuki Motor Corp.", "Toyota Motor Corp.", "Volkswagen AG", "Volvo AB", "AGL Energy Ltd.","Naturgy Energy Group SA",
                  "Uniper SE", "Berkshire Hathaway, Inc.", "Centrica Plc", "Air France-KLM SA", "American Airlines Group, Inc.", "Anglo American Plc", "Anhui Conch Cement Co., Ltd.", "ArcelorMittal SA",
                  "Bhp Group Plc", "BlueScope Steel Ltd.","Boral Ltd.", "CEMEX SAB de CV", "Dangote Cement Plc", "Delta Air Lines, Inc.", "Heidelberg Materials AG","Hitachi Cement", "Holcim Ltd.",
                  "Martin Marietta Materials, Inc.", "PACCAR, Inc.", "Rio Tinto Ltd.", "Rolls-Royce Group Plc", "thyssenkrupp AG", "UltraTech Cement Ltd.", "United Airlines Holdings, Inc.", "Qantas Airways Ltd.",
                  "ADBRI Ltd.", "China Steel Corp.", "CRH Plc", "NIPPON STEEL CORP.", "POSCO", "Severstal PAO", "Adelaide Brighton", "Grupo Argos SA",
                  "Peugeot Sa", "Fiat Chrysler Automobiles NV", "Stellantis NV", "SSAB AB", "Mercedes-Benz Group AG", "PACCAR, Inc.", "Constellation Energy Generation LLC", "Origin Energy Ltd."),
  sector_assessed = c("Power", "Power", "Automotive", "Power", "Power", "Power", "Power",
                 "Power", "Power", "Power", "Power", "Power", "Power", "Power", "Power", "Power",
                 "Power", "Power", "Power", "Power", "Power", "Power", "Power", "Power", "Power", "Power",
                 "Power", "Power", "Power", "Automotive", "Automotive", "Automotive","Automotive", "Automotive",
                 "Automotive", "Automotive","Automotive", "Automotive","Automotive", "HDV", "Power", "Power",
                 "Power", "Power","Power", "Aviation", "Aviation", "Cement", "Cement", "Steel",
                 "Steel", "Steel", "Cement", "Cement", "Cement",  "Aviation", "Cement", "Cement", "Cement",
                 "Steel", "HDV", "Steel", "Automotive", "Steel", "Cement", "Aviation", "Aviation",
                 "Cement", "Steel", "Cement", "Steel", "Power", "Steel", "Cement", "Cement",
                 "Automotive", "Automotive", "Automotive", "Steel", "Automotive", "Automotive", "Power", "Power")
)

# Try to match by AI company ID here

name_bridge <- ald_eq_raw %>%
  filter(company_name %in% ca_companies$company_name) %>%
  select(company_name, company_id) %>%
  left_join(ca_companies, by= "company_name") %>%
  distinct()

saveRDS(name_bridge, file.path(output_dir, "name_bridge.rds"))

abcd_ownership_ca100 <- masterdata_ownership_datastore %>%
  filter(id %in% name_bridge$company_id)

masterdata_ownership_datastore <-
  abcd_ownership_ca100 %>%
  filter(year %in% relevant_years) %>%
  mutate(technology = case_when(
    technology == "ICE Propane_HDV"   ~ "ICE_HDV",
    technology == "ICE CNG_HDV"   ~ "ICE_HDV",
    technology == "ICE Gasoline_HDV"   ~ "ICE_HDV",
    technology == "Hybrid No-Plug_HDV"   ~ "Hybrid_HDV",
    technology == "ICE Hydrogen_HDV"   ~ "ICE_HDV",
    technology == "ICE Diesel_HDV"   ~ "ICE_HDV",
    TRUE ~ technology
  ))

scenarios_long <- Scenario_AnalysisInput %>%
  inner_join(
    pacta.scenario.preparation::scenario_source_pacta_geography_bridge,
    by = c(
      scenario_source = "source",
      scenario_geography = "scenario_geography_source"
    )
  ) %>%
  select(-"scenario_geography") %>%
  rename(scenario_geography = "scenario_geography_pacta") %>%
  filter(
    .data$scenario_source %in% .env$scenario_sources_list,
    .data$ald_sector %in% c(.env$sector_list, .env$other_sector_list),
    .data$scenario_geography %in% unique(.env$scenario_regions$scenario_geography),
    .data$year %in% unique(
      c(.env$relevant_years)
    )
  ) %>%
  rbind(Scenario_AnalysisInput %>% filter(scenario_source == "ETP2017")) %>%
  mutate(ald_sector = ifelse(technology %in% c("Electric_HDV_HDV", "Hybrid_HDV_HDV", "ICE_HDV_HDV", "NA_HDV"), "HDV", ald_sector),
         technology= case_when(
           technology == "Electric_HDV_HDV" ~ "Electric_HDV",
           technology == "ICE_HDV_HDV" ~ "ICE_HDV",
           technology == "Hybrid_HDV_HDV" ~ "Hybrid_HDV",
           technology == "NA_HDV" ~ "FuelCell_HDV",
           TRUE ~ technology
         )) %>%
  distinct()

index_regions <- tibble(
  country_iso = unique(ald_eq_raw$asset_country),
  equity_market = "GlobalMarket"
)


for (scenario_source in unique(scenarios_long$scenario_source)) {
  filename <- paste0("equity_abcd_scenario_", scenario_source, ".rds")
  scenarios_long_source <- filter(scenarios_long, .data$scenario_source == .env$scenario_source)
  log_info(paste0("Formatting and saving ", filename, "... "))
  pacta.data.preparation::dataprep_abcd_scen_connection(
    abcd_data = masterdata_ownership_datastore,
    scenario_data = scenarios_long_source,
    reference_year = market_share_target_reference_year,
    relevant_years = relevant_years,
    tech_exclude = tech_exclude,
    scenario_geographies_list = scenario_geographies_list,
    sector_list = sector_list,
    other_sector_list = other_sector_list,
    global_aggregate_scenario_sources_list = global_aggregate_scenario_sources_list,
    global_aggregate_sector_list = global_aggregate_sector_list,
    scenario_regions = scenario_regions,
    index_regions = index_regions
  ) %>%
    saveRDS(file.path(output_dir, filename))
}

