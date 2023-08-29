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

ald_eq <- readRDS(file.path(Sys.getenv("OUTPUT_DIR"), "/masterdata_ownership_datastore.rds"))
ald_scen <- readRDS(file.path(Sys.getenv("OUTPUT_DIR"), "/equity_abcd_scenario_WEO2022.rds"))
ald_scen_hdv <- readRDS(file.path(output_dir, "/equity_abcd_scenario_ETP2017.rds"))

ald_scen <- rbind(ald_scen, ald_scen_hdv)

config <-
  config::get(
    file = "config.yml",
    config = Sys.getenv("R_CONFIG_ACTIVE"),
    use_parent = FALSE
  )

market_share_target_reference_year <- config$market_share_target_reference_year
time_horizon <- config$time_horizon

Brown.Techs <- config$Brown.Techs
Most.Relevant.Techs <- config$Most.Relevant.Techs
Other.Relevant.Techs <- config$Other.Relevant.Techs


name_bridge <- readRDS(file.path(output_dir, "name_bridge.rds"))

# Transform --------------

Var2 <- ald_scen %>%
  filter(year == market_share_target_reference_year + time_horizon,
         scenario_geography == ifelse(ald_sector == "Power", "Global", "Global")) %>%
  select(id,scenario,ald_sector,technology, plan_tech_prod, scen_tech_prod) %>%
  group_by(id)

# Here, check how null production are encoded (NA, N.A. or 0)
# Auto & Power are splitted because it might happen that grading are not the same for the two sectors due to scenario availability.
# For exemple for 2023Q2 (STEPS is not available for Auto)

# Split Power and Automotive as the scenario we compare with are not the same

Var2.Power<-Var2%>%filter(ald_sector=="Power")

Var2.Power$Category <- if_else(Var2.Power$technology %in% Brown.Techs, "Brown", "Green")

Scen.Wide <- pivot_wider(Var2.Power, names_from = "scenario", values_from = "scen_tech_prod")

# Overwrite NZE with APS if less ambitious
Scen.Wide <- Scen.Wide %>%
  mutate(
    NZE_2050 = case_when(
      NZE_2050 <= APS & Category == "Green" ~ APS,
      NZE_2050 >= APS & Category == "Brown" ~ APS,
      TRUE ~  NZE_2050
    )
  )


Scen.Green <- Scen.Wide %>% filter(Category == "Green")

# for green techs if the production is greater than B2ds, great,
# if between SDS & B2DS good
Scen.Green$Scen.Rating <- case_when(
  Scen.Green$plan_tech_prod == 0 ~"N.A.",
  Scen.Green$plan_tech_prod >= Scen.Green$STEPS & Scen.Green$plan_tech_prod < Scen.Green$APS ~ "Above APS (>1.7°C)",
  Scen.Green$plan_tech_prod >= Scen.Green$APS & Scen.Green$plan_tech_prod < Scen.Green$NZE_2050 ~ "Aligned with APS (1.5°C - 1.7°C)",
  Scen.Green$plan_tech_prod >= Scen.Green$NZE_2050 ~ "Aligned with/Below NZE (<1.5°C)",
  TRUE ~ "Aligned with /Above STEPS (>2.5°C)"
)


Scen.Brown <- Scen.Wide %>% filter(Category == "Brown")

Scen.Brown$Scen.Rating <- case_when(
  Scen.Brown$plan_tech_prod > Scen.Brown$STEPS  ~ "Aligned with /Above STEPS (>2.5°C)",
  Scen.Brown$plan_tech_prod > Scen.Brown$APS & Scen.Brown$plan_tech_prod < Scen.Brown$STEPS ~ "Above APS (>1.7°C)",
  Scen.Brown$plan_tech_prod > Scen.Brown$NZE_2050 & Scen.Brown$plan_tech_prod < Scen.Brown$APS ~ "Aligned with APS (1.5°C - 1.7°C)",
  Scen.Brown$plan_tech_prod == 0 & Scen.Brown$NZE_2050 == 0 ~ "N.A.",
  TRUE ~ "Aligned with/Below NZE (<1.5°C)"
)


Scen.Results <- rbind(Scen.Green,Scen.Brown)
Scen.Results <- Scen.Results %>%
  mutate(id = as.double(id)) %>%
  left_join(name_bridge, by = c("id" = "company_id")) %>%
  mutate(traffic_light=case_when(
    Scen.Rating=="Aligned with/Below NZE (<1.5°C)" ~ "Green",
    Scen.Rating=="Aligned with APS (1.5°C - 1.7°C)" ~ "Amber",
    Scen.Rating=="Above APS (>1.7°C)" ~ "Red",
    Scen.Rating=="Aligned with /Above STEPS (>2.5°C)" ~ "Red",
  ))

Scen.Results_Power <- Scen.Results %>% arrange(company_name,ald_sector,technology)

### Then Automotive

Var2.Automotive<-Var2%>%filter(ald_sector=="Automotive")

Var2.Automotive$Category <- if_else(Var2.Automotive$technology %in% Brown.Techs, "Brown", "Green")

Scen.Wide <- pivot_wider(Var2.Automotive, names_from = "scenario", values_from = "scen_tech_prod")

# Overwrite NZE_2050 with APS if less ambitious
Scen.Wide <- Scen.Wide %>%
  mutate(
    NZE_2050 = case_when(
      APS <= NZE_2050 & Category == "Green" ~ NZE_2050,
      APS >= NZE_2050 & Category == "Brown" ~ NZE_2050,
      TRUE ~  NZE_2050
    )
  )


Scen.Green <- Scen.Wide %>% filter(Category == "Green")



# for green techs if the production is greater than B2ds, great,
# if between SDS & B2DS good
Scen.Green <- Scen.Green %>%
  mutate(Scen.Rating =
           case_when(
             plan_tech_prod==0 ~ "N.A.",
             plan_tech_prod >= NZE_2050 ~ "Aligned with/Below NZE (<1.5°C)",
             plan_tech_prod >= APS ~ "Aligned with APS (1.5°C - 1.7°C)",
             TRUE ~ "Above APS (>1.7°C)"
             )
         )


Scen.Brown <- Scen.Wide %>% filter(Category == "Brown")

Scen.Brown <- Scen.Brown %>%
  mutate(Scen.Rating =
           case_when(
             plan_tech_prod==0 ~ "N.A.",
             plan_tech_prod <= NZE_2050 ~ "Aligned with/Below NZE (<1.5°C)",
             plan_tech_prod <= APS ~ "Aligned with APS (1.5°C - 1.7°C)",
             TRUE ~ "Above APS (>1.7°C)"
             )
  )


Scen.Results <- rbind(Scen.Green,Scen.Brown)


Scen.Results <- Scen.Results %>%
  mutate(traffic_light=case_when(
    Scen.Rating=="Aligned with/Below NZE (<1.5°C)" ~ "Green",
    Scen.Rating=="Aligned with APS (1.5°C - 1.7°C)" ~ "Amber",
    Scen.Rating=="Above APS (>1.7°C)" ~ "Red",
    Scen.Rating=="Significantly above SDS >2.7°C" ~ "Red",
  ))

Scen.Results_Automotive <- Scen.Results %>%
  mutate(id = as.double(id)) %>%
  left_join(name_bridge, by = c("id" = "company_id")) %>%
  arrange(company_name,ald_sector,technology) %>%
  filter(ald_sector %in% c("Power", "Automotive"))


Scen.Results <-rbind(Scen.Results_Automotive, Scen.Results_Power)

### Then HDV

Var2.HDV<-Var2%>%filter(ald_sector=="HDV")

Var2.HDV$Category <- if_else(Var2.HDV$technology %in% Brown.Techs, "Brown", "Green")

Scen.Wide <- pivot_wider(Var2.HDV, names_from = "scenario", values_from = "scen_tech_prod")

# Overwrite B2DS with SDS if less ambitious
Scen.Wide <- Scen.Wide %>%
  mutate(
    B2DS = case_when(
      SDS <= B2DS & Category == "Green" ~ B2DS,
      SDS >=B2DS & Category == "Brown" ~ B2DS,
      TRUE ~  B2DS
    )
  )


Scen.Green <- Scen.Wide %>% filter(Category == "Green")



# for green techs if the production is greater than B2ds, great,
# if between SDS & B2DS good
Scen.Green$Scen.Rating <- case_when(
  Scen.Green$plan_tech_prod==0 ~"N.A.",
  Scen.Green$plan_tech_prod < Scen.Green$NPS ~ "Significantly above 2DS >2.7°C",
  Scen.Green$plan_tech_prod >= Scen.Green$NPS & Scen.Green$plan_tech_prod < Scen.Green$SDS ~ "Above 2DS >2°C",
  Scen.Green$plan_tech_prod >= Scen.Green$SDS & Scen.Green$plan_tech_prod < Scen.Green$B2DS ~ "Close to 2DS 1.75C-2°C",
  Scen.Green$plan_tech_prod >= Scen.Green$B2DS ~ "Aligned to B2DS <1.75°C",
)


Scen.Brown <- Scen.Wide %>% filter(Category == "Brown")

Scen.Brown$Scen.Rating <- case_when(
  Scen.Brown$plan_tech_prod >= Scen.Brown$NPS  ~ "Significantly above 2DS >2.7°C",
  Scen.Brown$plan_tech_prod >= Scen.Brown$SDS & Scen.Brown$plan_tech_prod < Scen.Brown$NPS ~ "Above 2DS >2°C",
  Scen.Brown$plan_tech_prod >= Scen.Brown$B2DS & Scen.Brown$plan_tech_prod < Scen.Brown$SDS ~ "Close to 2DS 1.75C-2°C",
  Scen.Brown$plan_tech_prod < Scen.Brown$B2DS ~ "Aligned to B2DS <1.75°C",
  Scen.Brown$plan_tech_prod == 0 ~ "N.A."
)


Scen.Results <- rbind(Scen.Green,Scen.Brown)

Scen.Results <- Scen.Results %>%
  mutate(id=as.double(id)) %>%
  left_join(name_bridge, by = c("id" = "company_id"))%>%
  mutate(traffic_light=case_when(
    Scen.Rating=="Aligned with/Below NZE (<1.5°C)" ~ "Green",
    Scen.Rating=="Aligned to B2DS <1.75°C" ~ "Green",
    Scen.Rating=="Close to 2DS 1.75C-2°C" ~ "Amber",
    Scen.Rating=="Above 2DS >2°C" ~ "Red",
    Scen.Rating=="Significantly above 2DS >2.7°C" ~ "Red",
  ))

Scen.Results_HDV <- Scen.Results %>% arrange(company_name,ald_sector,technology)%>%
  filter(sector_assessed %in% c("Power", "Automotive", "HDV"))


Scen.Results<-rbind(Scen.Results_Automotive, Scen.Results_Power, Scen.Results_HDV
                    )%>%
  filter(ald_sector==sector_assessed)


#### SAVE RESULTS


CA100.Var2 <- Scen.Results %>%
  ungroup() %>%
  arrange(company_name,ald_sector,technology) %>%
  mutate(
    SectorKey = if_else(ald_sector == "Automotive", "A", "U"),
    technology = paste0("2di",SectorKey, "2.",technology,"T"),
    Rating = Scen.Rating
  ) %>%
  select(company_name,ald_sector,technology,Rating, traffic_light) %>%
  distinct()

CA100.Var2_vf<- CA100.Var2 %>%
  pivot_wider(names_from=technology, values_from=c(Rating, traffic_light)) %>%
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
  ))


write.csv(CA100.Var2_vf, file.path(output_dir, paste0("Trajectory_technology_",Sys.getenv("R_CONFIG_ACTIVE"),".csv")))
