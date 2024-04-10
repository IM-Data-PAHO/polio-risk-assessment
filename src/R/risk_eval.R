# AUTHORSHIP ----

# Pan American Health Organization
# Author: Oliver Mazariegos
# Last Update: 2023-10-09
# R 4.3.1

# SETUP ----

Sys.setlocale(locale = "es_ES.UTF-8")


library(readxl)
library(sf)
library(tidyverse)

rm(list = ls())

# PATHS ----
file_path = rstudioapi::getSourceEditorContext()$path
file_path_index = unlist(gregexec('R/risk_eval.R',file_path))[1]
PATH_global = substr(file_path,1,file_path_index - 1)
PATH_country_data   = paste0(PATH_global,"Data/country_data.xlsx")
PATH_risk_cut_offs  = paste0(PATH_global,"R/risk_cut_offs.xlsx")
PATH_shapefiles     = paste0(PATH_global,"Data/shapefiles/")
PATH_translations   = paste0(PATH_global,"R/translations.xlsx")

# VARS ----
LANG <- as.character(read_excel(PATH_country_data,sheet = 1)[3,2])

# LANG ----
LANG_TLS <- read_excel(PATH_translations,sheet = "DASHBOARD") %>% 
  select(LABEL,all_of(LANG))
colnames(LANG_TLS) <- c("LABEL","LANG")
lang_label <- function(label) {
  return(LANG_TLS$LANG[LANG_TLS$LABEL == label])
}

if (LANG == "SPA") {
  file.copy(from = "R/cut_offs_excel/cut_offs_download_SPA.xlsx",to ="R/Dashboard/www/cut_offs_download.xlsx",overwrite = TRUE)
} else if (LANG == "ENG") {
  file.copy(from = "R/cut_offs_excel/cut_offs_download_ENG.xlsx",to ="R/Dashboard/www/cut_offs_download.xlsx",overwrite = TRUE)
} else if (LANG == "POR") {
  file.copy(from = "R/cut_offs_excel/cut_offs_download_POR.xlsx",to ="R/Dashboard/www/cut_offs_download.xlsx",overwrite = TRUE)
} else if (LANG == "FRA") {
  file.copy(from = "R/cut_offs_excel/cut_offs_download_FRA.xlsx",to ="R/Dashboard/www/cut_offs_download.xlsx",overwrite = TRUE)
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
OPTS_DF <- read_xlsx(PATH_country_data,sheet = "_ListValues")
sex_opts <- unique(OPTS_DF$Sex)
sex_opts <- sex_opts[!is.na(sex_opts)]
yes_no_opts <- unique(OPTS_DF$`Yes No`)
yes_no_opts <- yes_no_opts[!is.na(yes_no_opts)]
outbreak_opts <- unique(OPTS_DF$Outbreaks)
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
YEAR_1 = YEAR_EVAL - 5
YEAR_2 = YEAR_EVAL - 4
YEAR_3 = YEAR_EVAL - 3
YEAR_4 = YEAR_EVAL - 2
YEAR_5 = YEAR_EVAL - 1
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
immunity_data <- read_excel(
  PATH_country_data, sheet = 3, skip = 2, col_names = FALSE, col_types = c(
    "guess", "guess", "guess", "guess", "numeric", "numeric", "numeric",
    "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
    "text"
  )
)
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
surveillance_data <- read_excel(
  PATH_country_data, sheet = 4, skip = 2, col_names = FALSE, col_types = c(
    "guess", "guess", "guess", "guess", "numeric", "numeric", "numeric",
    "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
    "text"
  )
)
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
determinants_data <- read_excel(
  PATH_country_data, sheet = 5, skip = 2, col_names = FALSE, col_types = c(
    "guess", "guess", "guess", "guess", "numeric", "numeric", "numeric",
    "text", "numeric", "numeric"
  )
)
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
outbreaks_data = read_excel(
  PATH_country_data, sheet = 6, skip = 2, col_names = FALSE, col_types = c(
    "guess", "guess", "guess", "guess", "numeric", "numeric", "numeric",
    "text", "text", "text", "text", "text", "text", "text"
  )
)
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

# SHAPEFILES ----
country_shapes <- st_read(PATH_shapefiles,layer = "admin2")
country_shapes <- admin_normalizer(country_shapes)
if ("ADMIN1_" %in% colnames(country_shapes)) {
  country_shapes <- country_shapes %>% rename("ADMIN1_GEO_ID" = "ADMIN1_")
  country_shapes <- country_shapes %>% 
    mutate(`ADMIN1_GEO_ID` = as.character(`ADMIN1_GEO_ID`),GEO_ID = as.character(GEO_ID))
}
country_shapes <- country_shapes[!duplicated(country_shapes$GEO_ID),]

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
save.image(file = paste0(PATH_global, "R/Dashboard/POLIO.RData"))

# CLEAN ----
rm(list = ls())


