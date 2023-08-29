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
output_dir <- Sys.getenv("OUTPUT_DIR")
ald_eq <- readRDS(file.path(output_dir, "/masterdata_ownership_datastore.rds"))
ald_scen <- readRDS(file.path(output_dir, "/equity_abcd_scenario_WEO2022.rds"))
ald_scen_hdv <- readRDS(file.path(output_dir, "/equity_abcd_scenario_ETP2017.rds"))

ald_scen <- rbind(ald_scen, ald_scen_hdv)

config <-
  config::get(
    file = "config.yml",
    config = Sys.getenv("R_CONFIG_ACTIVE"),
    use_parent = FALSE
  )

market_share_target_reference_year<- config$market_share_target_reference_year
time_horizon <- config$time_horizon
Most.Relevant.Techs <- config$Most.Relevant.Techs

end_year <- market_share_target_reference_year + time_horizon

scenario_source_agg_score <- config$scenario_source_agg_score
scenario_agg_score <- config$scenario_agg_score

scenario_source_agg_score_auto <- config$scenario_source_agg_score_auto
scenario_agg_score_auto <- config$scenario_agg_score_auto

scenario_source_agg_score_hdv <- config$scenario_source_agg_score_hdv
scenario_agg_score_hdv <- config$scenario_agg_score_hdv

change_in_scenario <-
  tibble::tribble(
    ~technology, ~ald_sector, ~type,
    "CoalCap",    "Power", "Brown",
    "GasCap",    "Power", "Brown",
    "HydroCap",    "Power", "Green",
    "NuclearCap",    "Power", "Green",
    "OilCap",    "Power", "Brown",
    "RenewablesCap",    "Power", "Green",

    "Electric",    "Automotive", "Green",
    "Hybrid",    "Automotive", "Green",
    "ICE",    "Automotive", "Brown",
    "Electric_HDV",    "HDV", "Green",
    "Hybrid_HDV",    "HDV", "Green",
    "ICE_HDV",    "HDV", "Brown",
  )

name_bridge <- readRDS(file.path(output_dir, "name_bridge.rds"))

# transform ---------------------------------------------------------------


scen_ca100 <- ald_scen %>%
  mutate(id = as.double(id)) %>%
  filter(id %in% name_bridge$company_id) %>%
  left_join(name_bridge, by= c(id = "company_id")) %>%
  filter(year %in% c(market_share_target_reference_year, market_share_target_reference_year + time_horizon),
         ald_sector %in% c("Power", "Automotive", "HDV"),
         equity_market == "GlobalMarket"
  ) %>%
  group_by(
    company_name, id,scenario_source, scenario, scenario_geography, ald_sector, year, technology
  ) %>%
  summarize(
    plan_tech_prod = sum(plan_tech_prod,na.rm = T),
    scen_tech_prod = sum(scen_tech_prod,na.rm = T)
  ) %>%
  ungroup() %>%
  group_by(company_name,  id, scenario, scenario_geography, ald_sector, year) %>%
  mutate(plan_sec_prod = sum(plan_tech_prod, na.rm = T),
         scen_sec_prod = sum(scen_tech_prod, na.rm = T)) %>%
  ungroup() %>%
  mutate(plan_tech_share = plan_tech_prod/plan_sec_prod,
         scen_tech_share = scen_tech_prod/scen_sec_prod)

aggregate_score_Case8 <- scen_ca100%>%
  group_by(company_name, id, scenario_source, scenario, scenario_geography, ald_sector, technology) %>%
  dplyr::mutate(
    scenario_change = abs(last(.data$scen_tech_prod) - first(.data$scen_tech_prod)),
    production_change = abs(last(.data$plan_tech_prod) - first(.data$scen_tech_prod))
  ) %>%
  dplyr::mutate(
    scenario_change = dplyr::if_else(
      .data$technology %in% Most.Relevant.Techs &
        .data$production_change > .data$scenario_change,
      .data$production_change,
      .data$scenario_change
    )
  ) %>%
  dplyr::mutate(
    tech_allocation_weight = dplyr::if_else(
      # TODO: add an input that defines these technologies
      .data$technology %in% Most.Relevant.Techs &
        last(.data$plan_tech_prod) > last(.data$scen_tech_prod),
      last(.data$plan_tech_prod),
      last(.data$scen_tech_prod)
    )
  ) %>%
  merge(change_in_scenario, by=c("technology", "ald_sector")) %>%
  ungroup() %>%
  group_by(company_name, id, scenario_source, scenario, scenario_geography, ald_sector, year)%>%
  mutate(trajectory_deviation=if_else(scen_tech_prod==0,ifelse(plan_tech_prod == 0, 0, 1),(plan_tech_prod-scen_tech_prod)/scen_tech_prod))%>%
  mutate(trajectory_alignment=if_else(type=="Green", trajectory_deviation, -1*trajectory_deviation))%>%
  mutate(weighting_factor = scenario_change * tech_allocation_weight)%>%
  mutate(single_score_Case8 = stats::weighted.mean(trajectory_alignment, scenario_change * tech_allocation_weight))%>%
  ungroup() %>%
#  filter(year == end_year) %>%
  filter(scenario_geography=="Global")




score_power <- aggregate_score_Case8 %>%
  filter(year==end_year) %>%
  filter(ald_sector=="Power")%>%
  filter(scenario_source==scenario_source_agg_score)%>%
  filter(scenario==scenario_agg_score) %>%
  mutate(company_name=case_when(company_name=="The AES Corp." ~ "The AES Corp.",
                                company_name=="AGL Energy Ltd." ~ "AGL Energy Ltd.",
                                company_name=="American Electric Power Co., Inc." ~ "American Electric Power Co., Inc.",
                                company_name=="CEZ as" ~ "CEZ, a.s.",
                                company_name=="Dominion Energy, Inc." ~ "Dominion Energy, Inc.",
                                company_name=="Duke Energy Corp." ~ "Duke Energy Corp.",
                                company_name=="E.ON SE" ~ "E.ON SE",
                                company_name=="Électricité de France SA" ~ "Electricité de France S.A.",
                                company_name=="Enel SpA" ~ "Enel SpA",
                                company_name=="ENGIE SA" ~ "Engie S.A.",
                                company_name=="Eskom Holdings SOC Ltd." ~ "Eskom Holdings SOC Ltd.",
                                company_name=="Exelon Corp." ~ "Exelon Corp.",
                                company_name=="FirstEnergy Corp." ~ "FirstEnergy Corp.",
                                company_name=="Fortum Oyj" ~ "Fortum Oyj",
                                company_name=="Iberdrola SA" ~ "Iberdrola, S.A.",
                                company_name=="Korea Electric Power Corp." ~ "Korea Electric Power Corp.",
                                company_name=="National Grid Plc" ~ "National Grid plc",
                                company_name=="NextEra Energy, Inc." ~ "NextEra Energy, Inc.",
                                company_name=="NRG Energy, Inc." ~ "NRG Energy, Inc.",
                                company_name=="Ntpc Ltd." ~ "NTPC Ltd. ",
                                company_name=="PGE Polska Grupa Energetyczna SA" ~ "PGE Polska Grupa Energetyczna S.A.",
                                company_name=="Power Assets Holdings Ltd." ~ "Power Assets Holdings Ltd.",
                                company_name=="PPL Corp." ~ "PPL Corp.",
                                company_name=="RWE AG" ~ "RWE AG",
                                company_name=="SSE Plc" ~ "SSE plc",
                                company_name=="The Southern Co." ~ "The Southern Co.",
                                company_name=="Uniper SE" ~ "Uniper SE",
                                company_name=="Vistra Corp." ~ "Vistra Corp. ",
                                company_name=="WEC Energy Group, Inc." ~ "WEC Energy Group, Inc.",
                                company_name=="Xcel Energy, Inc." ~ "Xcel Energy Inc.",
                                company_name=="Bayerische Motoren Werke AG" ~ "BMW",
                                company_name=="Daimler AG" ~ "Daimler AG",
                                company_name=="Fiat Chrysler Automobiles NV" ~ "Fiat Chrysler Automobiles NV",
                                company_name=="Ford Motor Co." ~ "Ford",
                                company_name=="General Motors Co." ~ "General Motors",
                                company_name=="Honda Motor Co., Ltd." ~ "Honda Motor",
                                company_name=="Nissan Motor Co., Ltd." ~ "Nissan",
                                company_name=="Peugeot Sa" ~ "Peugeot SA",
                                company_name=="Renault SA" ~ "Renault",
                                company_name=="SAIC Motor Corp. Ltd." ~ "Saic Motor",
                                company_name=="Stellantis NV" ~ "Stellantis (formerly Fiat/PSA Groupe)",
                                company_name=="Suzuki Motor Corp." ~ "Suzuki Motor",
                                company_name=="Toyota Motor Corp." ~ "Toyota",
                                company_name=="Volkswagen AG" ~ "Volkswagen",
                                company_name=="Volvo Car AB" ~ "Volvo",
                                company_name=="Mercedes-Benz Group AG" ~ "Mercedes-Benz (formerly Daimler AG)",
                                company_name=="PACCAR, Inc."~"PACCAR, Inc.",
                                company_name=="Constellation Energy Generation LLC"~"Constellation Energy Corp.",
                                company_name == "Origin Energy Ltd." ~ "Origin Energy Ltd.",


                                company_name=="Berkshire Hathaway, Inc."~"Berkshire Hathaway Inc.",
                                company_name=="Centrica Plc" ~ "Centrica plc",
                                company_name == "Origin Energy Ltd." ~ "Origin Energy Ltd.",

                                TRUE ~ company_name
  )) %>%
  select(company_name, single_score_Case8)%>%
  distinct()


score_hdv <- aggregate_score_Case8 %>%
  filter(ald_sector == "HDV") %>%
  filter(year ==end_year) %>%
  filter(scenario_source==scenario_source_agg_score_hdv)%>%
  filter(scenario==scenario_agg_score_hdv)%>%
  mutate(company_name=case_when(company_name=="Volvo AB" ~ "Volvo",
                                TRUE ~ company_name
  )) %>%
  select(company_name, single_score_Case8)%>%
  distinct() %>%
  filter(company_name %in% c("Volvo", "PACCAR, Inc."))


score_auto<-aggregate_score_Case8 %>%
  filter(ald_sector=="Automotive") %>%
  filter(year==end_year) %>%
  filter(scenario_source==scenario_source_agg_score_auto)%>%
  filter(scenario==scenario_agg_score_auto) %>%
  mutate(company_name=case_when(company_name=="The AES Corp." ~ "The AES Corp.",
                                company_name=="AGL Energy Ltd." ~ "AGL Energy Ltd.",
                                company_name=="American Electric Power Co., Inc." ~ "American Electric Power Co., Inc.",
                                company_name=="CEZ as" ~ "CEZ, a.s.",
                                company_name=="Dominion Energy, Inc." ~ "Dominion Energy, Inc.",
                                company_name=="Duke Energy Corp." ~ "Duke Energy Corp.",
                                company_name=="E.ON SE" ~ "E.ON SE",
                                company_name=="Électricité de France SA" ~ "Electricité de France S.A.",
                                company_name=="Enel SpA" ~ "Enel SpA",
                                company_name=="ENGIE SA" ~ "Engie S.A.",
                                company_name=="Eskom Holdings SOC Ltd." ~ "Eskom Holdings SOC Ltd.",
                                company_name=="Exelon Corp." ~ "Exelon Corp.",
                                company_name=="FirstEnergy Corp." ~ "FirstEnergy Corp.",
                                company_name=="Fortum Oyj" ~ "Fortum Oyj",
                                company_name=="Iberdrola SA" ~ "Iberdrola, S.A.",
                                company_name=="Korea Electric Power Corp." ~ "Korea Electric Power Corp.",
                                company_name=="National Grid Plc" ~ "National Grid plc",
                                company_name=="NextEra Energy, Inc." ~ "NextEra Energy, Inc.",
                                company_name=="NRG Energy, Inc." ~ "NRG Energy, Inc.",
                                company_name=="Ntpc Ltd." ~ "NTPC Ltd. ",
                                company_name=="PGE Polska Grupa Energetyczna SA" ~ "PGE Polska Grupa Energetyczna S.A.",
                                company_name=="Power Assets Holdings Ltd." ~ "Power Assets Holdings Ltd.",
                                company_name=="PPL Corp." ~ "PPL Corp.",
                                company_name=="RWE AG" ~ "RWE AG",
                                company_name=="SSE Plc" ~ "SSE plc",
                                company_name=="The Southern Co." ~ "The Southern Co.",
                                company_name=="Uniper SE" ~ "Uniper SE",
                                company_name=="Vistra Corp." ~ "Vistra Corp. ",
                                company_name=="WEC Energy Group, Inc." ~ "WEC Energy Group, Inc.",
                                company_name=="Xcel Energy, Inc." ~ "Xcel Energy Inc.",
                                company_name=="Bayerische Motoren Werke AG" ~ "BMW",
                                company_name=="Daimler AG" ~ "Daimler AG",
                                company_name=="Fiat Chrysler Automobiles NV" ~ "Fiat Chrysler Automobiles NV",
                                company_name=="Ford Motor Co." ~ "Ford",
                                company_name=="General Motors Co." ~ "General Motors",
                                company_name=="Honda Motor Co., Ltd." ~ "Honda Motor",
                                company_name=="Nissan Motor Co., Ltd." ~ "Nissan",
                                company_name=="Peugeot Sa" ~ "Peugeot SA",
                                company_name=="Renault SA" ~ "Renault",
                                company_name=="SAIC Motor Corp. Ltd." ~ "Saic Motor",
                                company_name=="Stellantis NV" ~ "Stellantis (formerly Fiat/PSA Groupe)",
                                company_name=="Suzuki Motor Corp." ~ "Suzuki Motor",
                                company_name=="Toyota Motor Corp." ~ "Toyota",
                                company_name=="Volkswagen AG" ~ "Volkswagen",
                                company_name=="Volvo Car AB" ~ "Volvo",
                                company_name=="Mercedes-Benz Group AG" ~ "Mercedes-Benz (formerly Daimler AG)",
                                company_name=="PACCAR, Inc."~"PACCAR, Inc.",
                                company_name=="Constellation Energy Generation LLC"~"Constellation Energy Corp.",
                                company_name == "Origin Energy Ltd." ~ "Origin Energy Ltd.",


                                company_name=="Berkshire Hathaway, Inc."~"Berkshire Hathaway Inc.",
                                company_name=="Centrica Plc" ~ "Centrica plc",
                                company_name == "Origin Energy Ltd." ~ "Origin Energy Ltd.",

                                TRUE ~ company_name
  )) %>%
  select(company_name, single_score_Case8)%>%
  distinct()


number_power_company <- nrow(score_power) + 1
number_auto_company <- nrow(score_auto) + 1
number_hdv_company <- nrow(score_hdv) + 1


power_final <- score_power %>%
  mutate(rank =number_power_company-rank(single_score_Case8))%>%
  mutate(aggregate_score_class = case_when(
    single_score_Case8 >= 0 ~ "Aligned with NZE  (1.5°C)",
    TRUE ~ "Misaligned with NZE  (1.5°C)",
  ))%>%
  mutate(aggregate_score_traffic_light = case_when(
    single_score_Case8 >= 0 ~ "Green",
    TRUE ~ "Red",
  ))


auto_final<-score_auto %>%
  mutate(rank = number_auto_company-rank(single_score_Case8)) %>%
  mutate(aggregate_score_class = case_when(
    single_score_Case8 >= 0 ~ "Aligned",
    TRUE ~ "Misaligned",
  ))%>%
  mutate(aggregate_score_traffic_light = case_when(
    single_score_Case8 >= 0 ~ "Green",
    TRUE ~ "Red",
  ))

hdv_final <- score_hdv %>%
  mutate(rank = number_hdv_company-rank(single_score_Case8)) %>%
  mutate(aggregate_score_class = case_when(
    single_score_Case8 >= 0 ~ "Aligned",
    TRUE ~ "Misaligned",
  ))%>%
  mutate(aggregate_score_traffic_light = case_when(
    single_score_Case8 >= 0 ~ "Green",
    TRUE ~ "Red",
  ))


comm_plan <- aggregate_score_Case8 %>%
  select("technology", "ald_sector", "company_name", "scenario", "scenario_geography", "year", "plan_tech_prod", "scen_tech_prod")%>%
  group_by(ald_sector, company_name, scenario, scenario_geography, year) %>%
  pivot_longer(cols = c("plan_tech_prod", "scen_tech_prod"), names_to = "metric") %>%
  mutate(metric= paste0(metric, "_", year)) %>%
  ungroup() %>%
  select(- "year") %>%
  pivot_wider(values_from = value, names_from = metric)


comm_plan_2 <- aggregate_score_Case8 %>%
  select("technology", "ald_sector", "company_name", "scenario", "scenario_geography", "year", "plan_tech_share", "scen_tech_share", "scenario_change", "production_change", "tech_allocation_weight", "type", "trajectory_alignment", "weighting_factor", "single_score_Case8") %>%
  filter(year == 2028) %>%
  select(-"year") %>%
  group_by(ald_sector, company_name, scenario, scenario_geography) %>%
  mutate(weighting_factor = weighting_factor/sum(weighting_factor, na.rm=TRUE)) %>%
  right_join(comm_plan, by = c("technology", "ald_sector", "company_name", "scenario", "scenario_geography"))



write.csv(auto_final, file.path(output_dir, paste0("auto_", Sys.getenv("R_CONFIG_ACTIVE"),"_aggregate.csv")))
write.csv(power_final, file.path(output_dir, paste0("power_", Sys.getenv("R_CONFIG_ACTIVE"),"_aggregate.csv")))
write.csv(hdv_final, file.path(output_dir, paste0("hdv_", Sys.getenv("R_CONFIG_ACTIVE"),"_aggregate.csv")))

write.csv(comm_plan_2, file.path(output_dir, "raw_value_communication_plan.csv"))

