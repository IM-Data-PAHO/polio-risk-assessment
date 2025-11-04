# AUTHORSHIP ----

# Pan American Health Organization
# Author: Oliver Mazariegos
# Last Update: 2023-10-09
# R 4.3.1
# Editorial ----
# Editor: Rafael León
# Contact: leonraf@paho.org
# Date: 2024-05-17
# Edit: Modified RISK eval to consider other languages, previously the tool
# was not considering even the correct cell to change language
# ---------------
# Editor: Rafael León
# Contact: leonraf@paho.org
# Date: 2024-12-03
# Edit: Modified RISK EVAL to include an excel output to the Data folder
# This excel output includes the process required to produce the summary risk
# table for the country's data
# ---------------


rm(list = ls())
# PATHS ----
resolve_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  matched <- grep(file_arg, cmd_args)
  if (length(matched) > 0) {
    return(normalizePath(sub(file_arg, "", cmd_args[matched][1]), mustWork = FALSE))
  }
  if (!is.null(sys.frames()) && length(sys.frames()) > 0) {
    for (frame in rev(sys.frames())) {
      if (!is.null(frame$ofile)) {
        return(normalizePath(frame$ofile, mustWork = FALSE))
      }
    }
  }
  return(NA_character_)
}

script_path <- resolve_script_path()
script_dir <- if (!is.na(script_path)) {
  normalizePath(dirname(script_path), mustWork = FALSE)
} else {
  normalizePath(getwd(), mustWork = FALSE)
}

candidate_root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
if (dir.exists(file.path(candidate_root, "Data"))) {
  PATH_global <- candidate_root
} else if (dir.exists(file.path(script_dir, "Data"))) {
  PATH_global <- script_dir
} else {
  PATH_global <- normalizePath(getwd(), mustWork = FALSE)
}

PATH_country_data   = file.path(PATH_global,"Data","country_data.xlsx")
PATH_risk_cut_offs  = file.path(PATH_global,"R","risk_cut_offs.xlsx")
PATH_shapefiles     = file.path(PATH_global,"Data","shapefiles")
PATH_translations   = file.path(PATH_global,"R","translations.xlsx")

# SETUP ----
library(readxl)
library(sf)
library(tidyverse)

## 2024-05-17 Major fix ----
LANG <- as.character(read_excel(PATH_country_data,sheet = 1)[3,2])

if(LANG == "SPA"){
  Sys.setlocale(locale = "es_ES.UTF-8")
}

if(LANG == "POR"){
  Sys.setlocale(locale = "es_ES.UTF-8")
}

if(LANG == "ENG"){
  Sys.setlocale(locale = "en_US.UTF-8")
}

if(LANG == "FRA"){
  Sys.setlocale(locale = "fr_FR.UTF-8")
}

# VARS ----

# LANG ----
LANG_TLS <- read_excel(PATH_translations,sheet = "DASHBOARD") %>% 
  select(LABEL,all_of(LANG))
colnames(LANG_TLS) <- c("LABEL","LANG")
lang_label <- function(label) {
  return(LANG_TLS$LANG[LANG_TLS$LABEL == label])
}

get_indicator_risk_level <- function(indicator, risk_points, pfa_vector = NULL) {
  if (is.null(pfa_vector)) {
    pfa_vector <- rep(FALSE, length(risk_points))
  }
  pfa_vector[is.na(pfa_vector)] <- FALSE
  
  lookup_cutoff <- function(level, is_pfa) {
    values <- CUT_OFFS$value[
      CUT_OFFS$RV == indicator &
        CUT_OFFS$risk_level == level &
        CUT_OFFS$PFA == is_pfa
    ]
    if (length(values) == 0) {
      return(NA_real_)
    }
    return(values[[1]])
  }
  
  risk_levels <- vector("character", length(risk_points))
  for (i in seq_along(risk_points)) {
    score_val <- risk_points[[i]]
    is_pfa <- pfa_vector[[i]]
    if (is.na(score_val)) {
      risk_levels[[i]] <- lang_label("no_data")
      next
    }
    
    lr_limit <- lookup_cutoff("LR", is_pfa)
    mr_limit <- lookup_cutoff("MR", is_pfa)
    hr_limit <- lookup_cutoff("HR", is_pfa)
    
    if (!is.na(lr_limit) && score_val <= lr_limit) {
      risk_levels[[i]] <- lang_label("LR")
    } else if (!is.na(mr_limit) && score_val <= mr_limit) {
      risk_levels[[i]] <- lang_label("MR")
    } else if (!is.na(hr_limit) && score_val <= hr_limit) {
      risk_levels[[i]] <- lang_label("HR")
    } else {
      risk_levels[[i]] <- lang_label("VHR")
    }
  }
  
  return(risk_levels)
}

cut_offs_dest <- file.path(PATH_global,"R","Dashboard","www","cut_offs_download.xlsx")
if (LANG == "SPA") {
  file.copy(from = file.path(PATH_global,"R","cut_offs_excel","cut_offs_download_SPA.xlsx"), to = cut_offs_dest, overwrite = TRUE)
} else if (LANG == "ENG") {
  file.copy(from = file.path(PATH_global,"R","cut_offs_excel","cut_offs_download_ENG.xlsx"), to = cut_offs_dest, overwrite = TRUE)
} else if (LANG == "POR") {
  file.copy(from = file.path(PATH_global,"R","cut_offs_excel","cut_offs_download_POR.xlsx"), to = cut_offs_dest, overwrite = TRUE)
} else if (LANG == "FRA") {
  file.copy(from = file.path(PATH_global,"R","cut_offs_excel","cut_offs_download_FRA.xlsx"), to = cut_offs_dest, overwrite = TRUE)
}

rep_label_admin1_name = lang_label("rep_label_admin1_name")
rep_label_admin1_name_plural = lang_label("rep_label_admin1_name_plural")
rep_label_admin2_name = lang_label("rep_label_admin2_name")
rep_label_admin2_name_plural = lang_label("rep_label_admin2_name_plural")

# UTILS ----

# Not in operator
`%!in%` <- Negate(`%in%`)

# Removes accents and uppercases ADMIN1 and ADMIN2 columns
admin_normalizer <- function(admin_df) {
  # MAYUS
  admin_df$ADMIN1 <- toupper(admin_df$ADMIN1)
  admin_df$ADMIN2 <- toupper(admin_df$ADMIN2)
  
  # ACCENTS
  admin_df <- admin_df %>%
    mutate(
      ADMIN1 = gsub("Á","A", ADMIN1),
      ADMIN1 = gsub("É","E", ADMIN1),
      ADMIN1 = gsub("Í","I", ADMIN1),
      ADMIN1 = gsub("Ó","O", ADMIN1),
      ADMIN1 = gsub("Ú","U", ADMIN1),
      ADMIN1 = gsub("Ñ","N", ADMIN1),
      ADMIN1 = gsub("Ü","U", ADMIN1),
      ADMIN2 = gsub("Á","A", ADMIN2),
      ADMIN2 = gsub("É","E", ADMIN2),
      ADMIN2 = gsub("Í","I", ADMIN2),
      ADMIN2 = gsub("Ó","O", ADMIN2),
      ADMIN2 = gsub("Ú","U", ADMIN2),
      ADMIN2 = gsub("Ñ","N", ADMIN2),
      ADMIN2 = gsub("Ü","U", ADMIN2),
    )
  return(admin_df)
}

var_norm <- function(x) {
  x = toupper(x)
  if (is.character(x)) {
    x = gsub("Á","A",x)
    x = gsub("É","E",x)
    x = gsub("Í","I",x)
    x = gsub("Ó","O",x)
    x = gsub("Ú","U",x)
    x = gsub("Ñ","N",x)
    x = gsub("Ü","U",x)
  }
  return(x)
}

geocodes_cleansing <- function(df) {
  df <- df %>% 
    filter(!is.na(`ADMIN1 GEO_ID`) & !is.na(GEO_ID)) %>% 
    mutate(`ADMIN1 GEO_ID` = as.character(`ADMIN1 GEO_ID`),GEO_ID = as.character(GEO_ID))
  return(df)
} 

# SCORING Functions ----

## Population immunity ----

# Population and PFA
population_and_pfa <- function(population_inmunity_df) {
  score <- case_when(
    population_inmunity_df$POB15 >= 100000 | population_inmunity_df$pfa == lang_label("yes") ~ TRUE,
    TRUE ~ FALSE
  )
  return(score)
}

# Score coverage Polio3
score_coverage_polio3 <- function(population_inmunity_df, year_column) {
  population_and_pfa <- population_and_pfa(population_inmunity_df)
  score = case_when(
    population_and_pfa & round(population_inmunity_df[[year_column]],0)  < 80 ~ 8,
    population_and_pfa & (round(population_inmunity_df[[year_column]],0) >= 80 & round(population_inmunity_df[[year_column]],0) < 90) ~ 5,
    population_and_pfa & (round(population_inmunity_df[[year_column]],0) >= 90 & round(population_inmunity_df[[year_column]],0) < 95) ~ 2,
    population_and_pfa & (round(population_inmunity_df[[year_column]],0) >= 95 & round(population_inmunity_df[[year_column]],0) <= 100) ~ 0,
    population_and_pfa & round(population_inmunity_df[[year_column]],0)  > 100 ~ 2,
    !population_and_pfa & round(population_inmunity_df[[year_column]],0)  < 80 ~ 10,
    !population_and_pfa & (round(population_inmunity_df[[year_column]],0) >= 80 & round(population_inmunity_df[[year_column]],0) < 90) ~ 6,
    !population_and_pfa & (round(population_inmunity_df[[year_column]],0) >= 90 & round(population_inmunity_df[[year_column]],0) < 95) ~ 3,
    !population_and_pfa & (round(population_inmunity_df[[year_column]],0) >= 95 & round(population_inmunity_df[[year_column]],0) <= 100) ~ 0,
    !population_and_pfa & round(population_inmunity_df[[year_column]],0)  > 100 ~ 3
  )
  return(score)
}

# Score IPV2
score_ipv2 <- function(population_inmunity_df) {
  population_and_pfa <- population_and_pfa(population_inmunity_df)
  score = case_when(
    population_and_pfa & round(population_inmunity_df[["ipv2"]],0)  < 80 ~ 8,
    population_and_pfa & (round(population_inmunity_df[["ipv2"]],0) >= 80 & round(population_inmunity_df[["ipv2"]],0) < 90) ~ 5,
    population_and_pfa & (round(population_inmunity_df[["ipv2"]],0) >= 90 & round(population_inmunity_df[["ipv2"]],0) < 95) ~ 2,
    population_and_pfa & (round(population_inmunity_df[["ipv2"]],0) >= 95 & round(population_inmunity_df[["ipv2"]],0) <= 100) ~ 0,
    population_and_pfa & round(population_inmunity_df[["ipv2"]],0)  > 100 ~ 2,
    !population_and_pfa & round(population_inmunity_df[["ipv2"]],0)  < 80 ~ 10,
    !population_and_pfa & (round(population_inmunity_df[["ipv2"]],0) >= 80 & round(population_inmunity_df[["ipv2"]],0) < 90) ~ 6,
    !population_and_pfa & (round(population_inmunity_df[["ipv2"]],0) >= 90 & round(population_inmunity_df[["ipv2"]],0) < 95) ~ 3,
    !population_and_pfa & (round(population_inmunity_df[["ipv2"]],0) >= 95 & round(population_inmunity_df[["ipv2"]],0) <= 100) ~ 0,
    !population_and_pfa & round(population_inmunity_df[["ipv2"]],0)  > 100 ~ 3
  )
  return(score)
}

# Score succesfull campaign
score_effective_campaign <- function(population_inmunity_df) {
  population_and_pfa <- population_and_pfa(population_inmunity_df)
  score = case_when(
    population_and_pfa & population_inmunity_df[["effective_campaign"]] == lang_label("no") ~ 6,
    population_and_pfa & population_inmunity_df[["effective_campaign"]] == lang_label("yes") ~ 0,
    !population_and_pfa & population_inmunity_df[["effective_campaign"]] == lang_label("no") ~ 8,
    !population_and_pfa & population_inmunity_df[["effective_campaign"]] == lang_label("yes") ~ 0,
    TRUE ~ 0
  )
  return(score)
}

## Survaillance ----

# Score reporting units
score_compliant_units <- function(surveillance_df) {
  population_and_pfa = population_and_pfa(surveillance_df)
  score = case_when(
    surveillance_df[["compliant_units_percent"]] < 80 ~ 8,
    surveillance_df[["compliant_units_percent"]] >= 80 ~ 0,
    TRUE ~ 8
  )
  return(score)
}

# Score PFA rate
score_pfa_rate <- function(surveillance_df) {
  population_and_pfa <- population_and_pfa(surveillance_df)
  score <- case_when(
    population_and_pfa & surveillance_df[['pfa_rate']] < 1 ~ 8,
    population_and_pfa & surveillance_df[['pfa_rate']] >= 1 ~ 0,
    TRUE ~ NA
  )
  return(score)
}

# Score PFA notified < 14 days
score_pfa_notified <- function(surveillance_df) {
  population_and_pfa <- population_and_pfa(surveillance_df)
  score <- case_when(
    population_and_pfa & surveillance_df[["pfa_notified_percent"]] < 80 ~ 5,
    population_and_pfa & surveillance_df[["pfa_notified_percent"]] >= 80 ~ 0,
    TRUE ~ NA
  )
  return(score)
}

# Score PFA investigated < 48 hr
score_pfa_investigated <- function(surveillance_df) {
  population_and_pfa <- population_and_pfa(surveillance_df)
  score <- case_when(
    population_and_pfa & surveillance_df[["pfa_investigated_percent"]] < 80 ~ 5,
    population_and_pfa & surveillance_df[["pfa_investigated_percent"]] >= 80 ~ 0,
    TRUE ~ NA
  )
  return(score)
}

# Score suitable samples
score_suitable_samples <- function(surveillance_df) {
  population_and_pfa <- population_and_pfa(surveillance_df)
  score <- case_when(
    population_and_pfa & surveillance_df[["suitable_samples_percent"]] < 80 ~ 5,
    population_and_pfa & surveillance_df[["suitable_samples_percent"]] >= 80 ~ 0,
    TRUE ~ NA
  )
  return(score)
}

# Score followups
score_followups <- function(surveillance_df) {
  population_and_pfa <- population_and_pfa(surveillance_df)
  score <- case_when(
    population_and_pfa & surveillance_df[["followups_percent"]] < 80 ~ 5,
    population_and_pfa & surveillance_df[["followups_percent"]] >= 80 ~ 0,
    TRUE ~ NA
  )
  return(score)
}

# Score active search
score_active_search <- function(surveillance_df) {
  population_and_pfa <- population_and_pfa(surveillance_df)
  score <- case_when(
    !population_and_pfa & surveillance_df[["active_search"]] == lang_label("no") ~ 12,
    !population_and_pfa & surveillance_df[["active_search"]] == lang_label("no_upper") ~ 12,
    !population_and_pfa & surveillance_df[["active_search"]] == lang_label("yes") ~ 0,
    !population_and_pfa & surveillance_df[["active_search"]] == lang_label("yes_upper") ~ 0,
    TRUE ~ NA
  )
  return(score)
}

## Determinants ----

# score drinking water
score_drinking_water <- function(determinants_df) {
  population_and_pfa <- population_and_pfa(determinants_df)
  if (mean(determinants_df[["drinking_water_percent"]]) < 1) {
    determinants_df <- determinants_df %>% 
      mutate(
        drinking_water_percent = round(drinking_water_percent * 100,0)
      )
  }
  score <- case_when(
    population_and_pfa & determinants_df[["drinking_water_percent"]] < 90 ~ 5,
    population_and_pfa & determinants_df[["drinking_water_percent"]] >= 90 ~ 0,
    !population_and_pfa & determinants_df[["drinking_water_percent"]] < 90 ~ 6,
    !population_and_pfa & determinants_df[["drinking_water_percent"]] >= 90 ~ 0,
  )
  return(score)
}

# Score sanitation services
score_sanitation_services <- function(determinants_df) {
  population_and_pfa <- population_and_pfa(determinants_df)
  if (mean(determinants_df[["sanitation_services_percent"]]) < 1) {
    determinants_df <- determinants_df %>% 
      mutate(
        sanitation_services_percent = round(sanitation_services_percent * 100,0)
      )
  }
  score <- case_when(
    population_and_pfa & determinants_df[["sanitation_services_percent"]] < 90 ~ 5,
    population_and_pfa & determinants_df[["sanitation_services_percent"]] >= 90 ~ 0,
    !population_and_pfa & determinants_df[["sanitation_services_percent"]] < 90 ~ 6,
    !population_and_pfa & determinants_df[["sanitation_services_percent"]] >= 90 ~ 0,
  )
  return(score)
}


## Outbreaks ----

# Score outbreaks
score_outbreak <- function(outbreaks_df, disease) {
  score <- case_when(
    disease != 'polio' & outbreaks_df[[disease]] == lang_label("yes") ~ 2,
    disease == 'polio' & outbreaks_df[[disease]] == lang_label("yes") ~ 4,
    outbreaks_df[[disease]] == lang_label("no") ~ 0
  )
  return(score)
}


# OPTS ----
## 2024-05-17 Major fix ----
# This section was not referencing the language sheet correctly
OPTS_DF <- read_xlsx(PATH_country_data,sheet = "_ListValues")
sex_opts <- unique(OPTS_DF$Sex)
sex_opts <- sex_opts[!is.na(sex_opts)]
yes_no_opts <- unique(OPTS_DF[2])
yes_no_opts <- yes_no_opts[!is.na(yes_no_opts)]
outbreak_opts <- unique(OPTS_DF[3])
outbreak_opts <- outbreak_opts[!is.na(outbreak_opts)]

# GENERAL ----
id_data <- read_excel(PATH_country_data,sheet = 2) %>% select(1,2,3,4)
colnames(id_data) <- c("ADMIN1 GEO_ID","GEO_ID","ADMIN1","ADMIN2")
id_data <- admin_normalizer(id_data)
id_data$`ADMIN1 GEO_ID` <- as.character(id_data$`ADMIN1 GEO_ID`)
id_data$GEO_ID <- as.character(id_data$GEO_ID)
id_data <- id_data %>% filter(!is.na(`ADMIN1 GEO_ID`) & !is.na(GEO_ID))
config_data <- read_excel(PATH_country_data,sheet = 1)
colnames(config_data) <- c("var","val")
YEAR_EVAL <- as.integer(config_data$val[2])
YEAR_1 = YEAR_EVAL - 4
YEAR_2 = YEAR_EVAL - 3
YEAR_3 = YEAR_EVAL - 2
YEAR_4 = YEAR_EVAL - 1
YEAR_5 = YEAR_EVAL 
YEAR_CAMP_SR <- YEAR_EVAL
COUNTRY_NAME <- config_data$val[1]
REPORT_FILE_FORMAT <- config_data$val[3]
scores_data <- id_data

# RISK CUT OFFS ----
sheet_cut_off <- 'sheet_cut_off'
CUT_OFFS <- read_xlsx(PATH_risk_cut_offs,sheet = sheet_cut_off,n_max = 10)
CUT_OFFS <- CUT_OFFS %>% pivot_longer(!RV:PFA,names_to = "risk_level")

# POPULATION AREA ----
pop_data <- read_excel(PATH_country_data,sheet = 2)
colnames(pop_data) <- c("ADMIN1 GEO_ID","GEO_ID","ADMIN1","ADMIN2",'POB1', 'POB5', "POB15", "POB")
pop_data <- admin_normalizer(pop_data)
pop_data$`ADMIN1 GEO_ID` <- as.character(pop_data$`ADMIN1 GEO_ID`)
pop_data$GEO_ID <- as.character(pop_data$GEO_ID)
pop_data <- pop_data %>% filter(!is.na(`ADMIN1 GEO_ID`) & !is.na(GEO_ID))
pop_data$POB1 <- as.numeric(pop_data$POB1)
pop_data$POB5 <- as.numeric(pop_data$POB5)
pop_data$POB15 <- as.numeric(pop_data$POB15)
pop_data$POB <- as.numeric(pop_data$POB)
ZERO_POB_LIST <- pop_data %>% filter(POB15 <= 0) %>% select(GEO_ID)
ZERO_POB_LIST <- ZERO_POB_LIST$GEO_ID

# POPULATION IMMUNITY ----

## Read data ----
immunity_data <- read_excel(PATH_country_data, sheet = 3, skip = 2, col_names = FALSE)
colnames(immunity_data) <- c('ADMIN1 GEO_ID', 'GEO_ID', 'ADMIN1', 'ADMIN2', 
                             'POB1', 'POB5', 'POB15', 'pfa','year1','year2','year3',
                             'year4','year5','ipv2','effective_campaign')
immunity_data <- admin_normalizer(immunity_data)

## Filter missing GEO codes ----
immunity_data <- geocodes_cleansing(immunity_data)
immunity_data <- admin_normalizer(immunity_data)

## Scores calculation ----
immunity_scores <- immunity_data %>% 
  mutate_at(
    vars(contains('year')), ~ round(.,digits = 0)
  ) %>% 
  mutate(
    ipv2 = round(ipv2, digits = 0),
    population_and_pfa_bool = population_and_pfa(immunity_data),
    year1_score = score_coverage_polio3(immunity_data,'year1'),
    year2_score = score_coverage_polio3(immunity_data,'year2'),
    year3_score = score_coverage_polio3(immunity_data,'year3'),
    year4_score = score_coverage_polio3(immunity_data,'year4'),
    year5_score = score_coverage_polio3(immunity_data,'year5'),
    ipv_score = score_ipv2(immunity_data),
    effective_campaign_score = score_effective_campaign(immunity_data),
  ) %>% 
  rowwise() %>% 
  mutate(
    immunity_score = sum(c_across(matches('score')), na.rm = T) 
  )

immunity_scores <- immunity_scores %>% 
  mutate(
    years_score = year1_score + year2_score + year3_score + year4_score + year5_score,
    .after = year5_score
  )

## Adding to scores_data ----
immunity_scores_join <- immunity_scores %>% 
  select(
    'GEO_ID',
    'POB1',
    'POB5',
    'POB15',
    'pfa',
    'immunity_score'
  )
scores_data <- left_join(scores_data, immunity_scores_join)

# SURVAILLANCE ----

## Read data ----
surveillance_data <- read_excel(PATH_country_data, sheet = 4, skip = 2, col_names = FALSE)
colnames(surveillance_data) <- c('ADMIN1 GEO_ID', 'GEO_ID', 'ADMIN1', 'ADMIN2', 
                                 'POB1', 'POB5', 'POB15', 'pfa', 'compliant_units_percent', 'pfa_rate', 
                                 'pfa_notified_percent', 'pfa_investigated_percent', 
                                 'suitable_samples_percent', 'followups_percent', 'active_search')
surveillance_data <- admin_normalizer(surveillance_data)

## Filtering missing GEO codes ----
surveillance_data <- geocodes_cleansing(surveillance_data)

## Scores calculation ----
surveillance_scores <- surveillance_data %>% 
  mutate_at(
    vars(contains('percent')), ~ round(.,digits = 0)
  ) %>% 
  mutate(
    population_and_pfa_bool = population_and_pfa(surveillance_data),
    compliant_units_score = score_compliant_units(surveillance_data),
    pfa_rate_score = score_pfa_rate(surveillance_data),
    pfa_notified_score = score_pfa_notified(surveillance_data),
    pfa_investigated_score = score_pfa_investigated(surveillance_data),
    suitable_samples_score = score_suitable_samples(surveillance_data),
    followups_score = score_followups(surveillance_data),
    active_search = case_when(
      population_and_pfa_bool ~ NA,
      !population_and_pfa_bool ~ active_search
    ),
    active_search_score = score_active_search(surveillance_data),
    
  ) %>% 
  rowwise() %>% 
  mutate(
    surveillance_score = sum(c_across(matches('score')), na.rm = T) 
  )

## Adding to scores_data ----
surveillance_scores_join <- surveillance_scores %>% 
  select(
    'GEO_ID',
    'surveillance_score'
  )
scores_data <- left_join(scores_data, surveillance_scores_join)

# DETERMINANTS ----

## Read data ----
determinants_data <- read_excel(PATH_country_data, sheet = 5, skip = 2, col_names = FALSE)
colnames(determinants_data) <- c('ADMIN1 GEO_ID', 'GEO_ID', 'ADMIN1', 'ADMIN2', 
                                 'POB1', 'POB5', 'POB15', 'pfa', 'drinking_water_percent', 'sanitation_services_percent')
determinants_data <- admin_normalizer(determinants_data)

## Filtering missing GEO codes ----
determinants_data <- geocodes_cleansing(determinants_data)

## Scores calculation ----
determinants_scores <-  determinants_data %>% 
  mutate(
    drinking_water_percent = case_when(
      mean(drinking_water_percent) < 1 ~ round(drinking_water_percent*100, digits = 0),
      T ~ round(drinking_water_percent,digits = 0)
    ),
    sanitation_services_percent = case_when(
      mean(sanitation_services_percent) < 1 ~ round(sanitation_services_percent*100, digits = 0),
      T ~ round(sanitation_services_percent,digits = 0)
    ),
    population_and_pfa_bool = population_and_pfa(determinants_data),
    drinking_water_score = score_drinking_water(determinants_data),
    sanitation_services_score = score_sanitation_services(determinants_data)
  ) %>% 
  rowwise() %>% 
  mutate(
    determinants_score = sum(c_across(matches('score')), na.rm = T) 
  )


## Adding to scores_data ----
determinants_scores_join <- determinants_scores %>% 
  select(
    'GEO_ID',
    'determinants_score'
  )
scores_data <- left_join(scores_data, determinants_scores_join)

# OUTBREAKS ----

## Read data ----
outbreaks_data = read_excel(PATH_country_data, sheet = 6, skip = 2, col_names = FALSE)
colnames(outbreaks_data) =  c('ADMIN1 GEO_ID', 'GEO_ID', 'ADMIN1', 'ADMIN2',
                              'POB1', 'POB5', 'POB15', 'pfa',
                              'polio', 'measles', 'rubella', 'diphtheria', 'yellow_fever', 'tetanus')
outbreaks_data <- admin_normalizer(outbreaks_data)

## Filtering missing GEO codes ----
outbreaks_data = geocodes_cleansing(outbreaks_data)

## Scores calculation ----
outbreaks_scores <- outbreaks_data %>% 
  mutate(
    polio_score = score_outbreak(outbreaks_data, 'polio'),
    measles_score = score_outbreak(outbreaks_data, 'measles'),
    rubella_score = score_outbreak(outbreaks_data, 'rubella'),
    diphtheria_score = score_outbreak(outbreaks_data, 'diphtheria'),
    yellow_fever_score = score_outbreak(outbreaks_data, 'yellow_fever'),
    tetanus_score = score_outbreak(outbreaks_data, 'tetanus'),
    population_and_pfa_bool = population_and_pfa(outbreaks_data)
  ) %>% 
  rowwise() %>% 
  mutate(
    outbreaks_score = sum(c_across(matches('score')), na.rm = T) 
  )

## Adding to scores_data ----
outbreaks_scores_join <- outbreaks_scores %>% 
  select(
    'GEO_ID',
    'outbreaks_score'
  )
scores_data <- left_join(scores_data, outbreaks_scores_join)

# TOTAL SCORE ----
scores_data <- scores_data %>% 
  rowwise() %>% 
  mutate(
    total_score = sum(c_across(matches('score')), na.rm = T) 
  )
# Save dashboard tables to Excel workbook ----
pfa_general_vector <- population_and_pfa(scores_data)

general_risk_export <- scores_data %>%
  mutate(
    immunity_score = round(immunity_score, 0),
    surveillance_score = round(surveillance_score, 0),
    determinants_score = round(determinants_score, 0),
    outbreaks_score = round(outbreaks_score, 0),
    total_score = round(total_score, 0),
    qualitative_risk = get_indicator_risk_level("total_score", total_score, pfa_general_vector)
  ) %>%
  select(
    ADMIN1,
    ADMIN2,
    immunity_score,
    surveillance_score,
    determinants_score,
    outbreaks_score,
    total_score,
    qualitative_risk
  )

colnames(general_risk_export) <- c(
  lang_label("table_admin1_name"),
  lang_label("table_admin2_name"),
  lang_label("menuitem_immunity"),
  lang_label("menuitem_surveillance"),
  lang_label("menuitem_determinants"),
  lang_label("menuitem_outbreaks"),
  lang_label("risk_points"),
  lang_label("risk_level")
)

population_immunity_export <- immunity_scores %>%
  mutate(
    year1 = round(year1, 0),
    year2 = round(year2, 0),
    year3 = round(year3, 0),
    year4 = round(year4, 0),
    year5 = round(year5, 0),
    ipv2 = round(ipv2, 0),
    years_score = round(years_score, 0),
    ipv_score = round(ipv_score, 0),
    effective_campaign_score = round(effective_campaign_score, 0),
    immunity_score = round(immunity_score, 0),
    qualitative_risk = get_indicator_risk_level("immunity_score", immunity_score, population_and_pfa_bool)
  ) %>%
  select(
    ADMIN1,
    ADMIN2,
    POB1,
    POB5,
    POB15,
    year1,
    year2,
    year3,
    year4,
    year5,
    years_score,
    ipv2,
    ipv_score,
    effective_campaign,
    effective_campaign_score,
    immunity_score,
    qualitative_risk
  )

colnames(population_immunity_export) <- c(
  lang_label("table_admin1_name"),
  lang_label("table_admin2_name"),
  "POB1",
  "POB5",
  "POB15",
  paste(lang_label("immunity_polio_cob"), YEAR_1, "(%)"),
  paste(lang_label("immunity_polio_cob"), YEAR_2, "(%)"),
  paste(lang_label("immunity_polio_cob"), YEAR_3, "(%)"),
  paste(lang_label("immunity_polio_cob"), YEAR_4, "(%)"),
  paste(lang_label("immunity_polio_cob"), YEAR_5, "(%)"),
  lang_label("immunity_polio_score"),
  lang_label("immunity_ipv2_cob"),
  lang_label("immunity_ipv2_score"),
  lang_label("immunity_effective_cob"),
  lang_label("immunity_effective_score"),
  lang_label("total_pr"),
  lang_label("risk_level")
)

quality_of_surveillance_export <- surveillance_scores %>%
  mutate(
    compliant_units_percent = round(compliant_units_percent, 0),
    pfa_rate = round(pfa_rate, 2),
    pfa_notified_percent = round(pfa_notified_percent, 0),
    pfa_investigated_percent = round(pfa_investigated_percent, 0),
    suitable_samples_percent = round(suitable_samples_percent, 0),
    followups_percent = round(followups_percent, 0),
    surveillance_score = round(surveillance_score, 0),
    qualitative_risk = get_indicator_risk_level("surveillance_score", surveillance_score, population_and_pfa_bool)
  ) %>%
  select(
    ADMIN1,
    ADMIN2,
    surveillance_score,
    compliant_units_percent,
    compliant_units_score,
    pfa_rate,
    pfa_rate_score,
    pfa_notified_percent,
    pfa_notified_score,
    pfa_investigated_percent,
    pfa_investigated_score,
    suitable_samples_percent,
    suitable_samples_score,
    followups_percent,
    followups_score,
    active_search,
    active_search_score,
    qualitative_risk
  )

colnames(quality_of_surveillance_export) <- c(
  lang_label("table_admin1_name"),
  lang_label("table_admin2_name"),
  lang_label("total_pr"),
  lang_label("surveillance_title_map_reporting_units"),
  lang_label("surveillance_reporting_units_score"),
  lang_label("surveillance_pfa_rate"),
  lang_label("surveillance_pfa_rate_score"),
  lang_label("surveillance_title_map_pfa_notification"),
  lang_label("surveillance_pfa_notification_score"),
  lang_label("surveillance_title_map_pfa_investigated"),
  lang_label("surveillance_pfa_investigated_score"),
  lang_label("surveillance_title_map_suitable_samples"),
  lang_label("surveillance_suitable_samples_score"),
  lang_label("surveillance_title_map_followups"),
  lang_label("surveillance_followups_score"),
  lang_label("surveillance_title_map_active_search"),
  lang_label("surveillance_active_search_score"),
  lang_label("risk_level")
)

rio::export(
  list(
    general_risk = general_risk_export,
    population_immunity = population_immunity_export,
    quality_of_surveillance = quality_of_surveillance_export
  ),
  file.path(PATH_global,"Data","polio_results.xlsx")
)




# SHAPEFILES ----
country_shapes <- st_read(PATH_shapefiles,layer = "admin2")
country_shapes <- admin_normalizer(country_shapes)
if ("ADMIN1_" %in% colnames(country_shapes)) {
  country_shapes <- country_shapes %>% rename("ADMIN1_GEO_ID" = "ADMIN1_")
  country_shapes <- country_shapes %>% 
    mutate(`ADMIN1_GEO_ID` = as.character(`ADMIN1_GEO_ID`),GEO_ID = as.character(GEO_ID))
}
country_shapes <- country_shapes[!duplicated(country_shapes$GEO_ID),]
data_shapes_id <- immunity_scores$GEO_ID
country_shapes <- country_shapes %>% 
  filter(GEO_ID %in% data_shapes_id)
# DBD VARS ----
YEAR_LIST <- c(YEAR_1,YEAR_2,YEAR_3,YEAR_4,YEAR_5)
admin1_list <- c(toupper(lang_label("rep_label_all")),sort(unique(id_data$ADMIN1)))
if ("ADMIN1_GEO_ID" %in% colnames(country_shapes)) {
  admin1_geo_id_df <- id_data %>% select(`ADMIN1 GEO_ID`,ADMIN1) %>% unique()
  admin1_geo_id_df <- rbind(admin1_geo_id_df,c(0,toupper(lang_label("rep_label_all"))))
} else {
  admin1_geo_id_df <- id_data %>% select(GEO_ID, ADMIN1) %>% unique()
  admin1_geo_id_df <- rbind(admin1_geo_id_df,c(0,toupper(lang_label("rep_label_all"))))
}

if (("ADMIN1_GEO_ID" %in% colnames(country_shapes)) & ("ADMIN1 GEO_ID" %!in% colnames(country_shapes))) {
  country_shapes <- country_shapes %>% 
    rename(
      "ADMIN1 GEO_ID" = "ADMIN1_GEO_ID"
    )
  
}


# SAVE ----
rm(determinants_scores_join,
   immunity_scores_join,
   outbreaks_scores_join,
   surveillance_scores_join,
   sheet_cut_off)
save.image(file = file.path(PATH_global, "R","Dashboard","POLIO.RData"))
# CLEAN ----
rm(list = ls())
