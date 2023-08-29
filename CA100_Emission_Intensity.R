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

output_dir <- Sys.getenv("OUTPUT_DIR")

ald_eq <- readRDS(file.path(Sys.getenv("OUTPUT_DIR"), "/masterdata_ownership_datastore.rds"))


b2ds_scen_emission_factors <-
  tibble::tribble(
    ~scenario, ~ald_sector, ~year, ~value,
    "B2DS",    "Cement",    2014,  0.582644415615628,
    "B2DS",    "Cement",    2025,  0.501438531033769,
    "B2DS",    "Cement",    2030,  0.430393853942256,
    "B2DS",    "Cement",    2035,  0.360690452957794,
    "B2DS",    "Cement",    2040,  0.282732742468256,
    "B2DS",    "Cement",    2045,  0.242959859089995,
    "B2DS",    "Cement",    2050,  0.168966172912532,

    "B2DS",    "Steel",     2014,  1.80759846130986,
    "B2DS",    "Steel",     2025,  1.1287448939987,
    "B2DS",    "Steel",     2030,  0.874539236865355,
    "B2DS",    "Steel",     2035,  0.63380312584879,
    "B2DS",    "Steel",     2040,  0.496436820917842,
    "B2DS",    "Steel",     2045,  0.380201231895074,
    "B2DS",    "Steel",     2050,  0.260550997168052,

    "B2DS",    "Aviation",  2014,  0.000104175966876,
    "B2DS",    "Aviation",  2025,  0.000074663078144,
    "B2DS",    "Aviation",  2030,  0.000059621956812,
    "B2DS",    "Aviation",  2035,  0.000044980269423,
    "B2DS",    "Aviation",  2040,  0.000033570948673,
    "B2DS",    "Aviation",  2045,  0.000024032720840,
    "B2DS",    "Aviation",  2050,  0.000018345836437
  )%>%
  filter(year==2030)


nze_scen_emission_factors <-
  tibble::tribble(
    ~scenario, ~ald_sector, ~year, ~value,
    "NZE_2050",    "Cement",    2030,  0.44,

    "NZE_2050",    "Steel",     2030,  1.42,

    "B2DS",    "Aviation",  2030,  0.000059621956812,
  )%>%
  filter(year==2030)

name_bridge <- readRDS(file.path(output_dir, "name_bridge.rds"))

market_share_target_reference_year <- config$market_share_target_reference_year
pacta_financial_timestamp <- Sys.getenv("R_CONFIG_ACTIVE")


ald_ca100 <- ald_eq %>%
  mutate(id = as.double(id)) %>%
  filter(id %in% name_bridge$company_id) %>%
  left_join(name_bridge, by = c(id = "company_id")) %>%
  filter(ald_sector %in% c("Steel", "Cement", "Aviation"))%>%
  filter(technology != "Grinding")%>%
  filter(technology != "Freight")%>%
  filter(year == market_share_target_reference_year) %>%
  group_by(company_name, year, ald_sector) %>%
  mutate(emission_intensity_sector= weighted.mean(ald_emissions_factor,ald_production )) %>%
  select(company_name, year,ald_sector, emission_intensity_sector) %>%
  distinct() %>%
  merge(nze_scen_emission_factors, by="ald_sector") %>%
  mutate(alignment_metric=(value-emission_intensity_sector)/emission_intensity_sector) %>%
  mutate(alignement_classement=
           case_when(
             alignment_metric>-0.15 & ald_sector=="Aviation" ~ "Approaching B2DS (<2.0°C)",
             alignment_metric>-0.3 & ald_sector=="Aviation" ~ "Moderate distance to B2DS (<2.0°C)",
             ald_sector=="Aviation" ~ "Significant distance to B2DS (<2.0°C)",
             alignment_metric>-0.05 & ald_sector=="Cement" ~ "Approaching NZE (1.5°C)",
             alignment_metric>-0.2 & ald_sector=="Cement"~ "Moderate distance NZE (1.5°C)",
             ald_sector=="Cement" ~ "Significant distance NZE (1.5°C)",
             alignment_metric>-0.15 & ald_sector=="Steel" ~ "Approaching NZE (1.5°C)",
             alignment_metric>-0.36 & ald_sector=="Steel"~ "Moderate distance NZE (1.5°C)",
             ald_sector=="Steel" ~ "Significant distance NZE (1.5°C)",
           )
  )%>%
  mutate(traffic_light=
           case_when(
             alignement_classement=="Approaching B2DS (<2.0°C)" ~ "Green",
             alignement_classement=="Moderate distance to B2DS (<2.0°C)" ~ "Amber",
             alignement_classement=="Significant distance to B2DS (<2.0°C)" ~ "Red",
             alignement_classement=="Approaching NZE (1.5°C)" ~ "Green",
             alignement_classement=="Moderate distance NZE (1.5°C)" ~ "Amber",
             alignement_classement=="Significant distance NZE (1.5°C)" ~ "Red",
           )
  )%>%
  mutate(company_name=
           case_when(
             company_name=="ArcelorMittal SA" ~"ArcelorMittal S.A. " ,
             company_name=="Bluescope Steel Ltd." ~"BlueScope Steel Ltd.",
             company_name=="China Steel Corp." ~"China Steel Corp.",
             company_name=="NIPPON STEEL CORP" ~"Nippon Steel Corp.",
             company_name=="POSCO" ~"POSCO Holdings Inc.",
             company_name=="Severstal PAO" ~"Severstal PAO",
             company_name=="Volvo Ab" ~"SSAB AB",
             company_name=="ThyssenKrupp AG" ~"thyssenkrupp AG",
             company_name=="Air France-KLM SA" ~"Air France–KLM S.A.",
             company_name=="ADBRI Ltd."~ "Adbri Ltd.",
             company_name=="American Airlines Group, Inc." ~"American Airlines Group Inc.",
             company_name=="Delta Air Lines, Inc." ~"Delta Air Lines, Inc.",
             company_name=="Qantas Airways Ltd." ~"Qantas Airways Ltd.",
             company_name=="United Airlines Holdings, Inc." ~"United Airlines Holdings, Inc.",
             company_name=="Volvo Ab" ~"Adelaide Brighton",
             company_name=="Anhui Conch Cement Co., Ltd." ~"Anhui Conch Cement Co. Ltd.",
             company_name=="Boral Ltd." ~"Boral Ltd.",
             company_name=="CEMEX SAB de CV" ~"Cemex SAB de CV",
             company_name=="CRH Plc" ~"CRH plc",
             company_name=="Dangote Cement Plc" ~"Dangote Cement plc",
             company_name=="Grupo Argos SA" ~"Grupo Argos S.A.",
             company_name=="Heidelberg Materials AG" ~"Heidelberg Materials AG",
             company_name=="Holcim Ltd." ~"Holcim Ltd",
             company_name=="Martin Marietta Materials, Inc." ~"Martin Marietta Materials, Inc.",
             company_name=="UltraTech Cement Ltd." ~"UltraTech Cement Ltd.",
             company_name=="NIPPON STEEL CORP." ~"Nippon Steel Corp.",
             company_name=="SSAB AB" ~"SSAB AB",
             company_name=="BlueScope Steel Ltd." ~ "BlueScope Steel Ltd.",
             company_name == "thyssenkrupp AG" ~ "thyssenkrupp AG",
             TRUE ~ company_name,
           ))




write.csv(ald_ca100, file.path(Sys.getenv("OUTPUT_DIR"), paste0("/CA100_", Sys.getenv("R_CONFIG_ACTIVE"), "_EI.csv")))

