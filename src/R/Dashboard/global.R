# AUTHORSHIP ----

# Pan American Health Organization
# Author: Luiz Quezada & Oliver Mazariegos
# Last Update: 2023-10-09
# R 4.3.1


## 2024-05-17 Major fix ----
#Sys.setlocale(locale = "es_ES.UTF-8")

# LIBS ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinycssloaders)
library(fontawesome)
library(plotly)
library(leaflet)
library(readxl)
library(data.table)
library(tidyr)
library(DT)
library(janitor)
library(RColorBrewer)
library(sf)
library(htmltools)
library(tidyverse)
library(scales)
library(mapview)
library(webshot)


webshot::install_phantomjs(force = TRUE)
Sys.setenv(OPENSSL_CONF = "/dev/null")
options(shiny.fullstacktrace = TRUE)


# LOAD DATA ----
load(file = "POLIO.RData")
# FUNCS ----
get_a1_geo_id <- function(admin1) {
  return(unique(admin1_geo_id_df$`ADMIN1 GEO_ID`[admin1_geo_id_df$ADMIN1 == admin1]))
}

lang_label <- function(label) {
  return(LANG_TLS$LANG[LANG_TLS$LABEL == label])
}

admin1_transform <- function(LANG_TLS,COUNTRY_NAME,admin1) {
  if (admin1 == toupper(lang_label_tls(LANG_TLS,"rep_label_all"))) {
    return(paste0("- ",toupper(COUNTRY_NAME)))
  } else {
    return(paste0("- ",admin1,", ",toupper(COUNTRY_NAME)))
  }
}

plot_pie_data <- function(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,indicator,data,admin1_id,pop_filter, return_table=F) {
  data <- data %>% rename('var' = all_of(indicator))
  data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,indicator,data$var, data$population_and_pfa_bool)
  
  if (pop_filter != lang_label("filter_all")) {
    if (pop_filter == lang_label("less_than_100000")) {
      data <- data %>% 
        mutate(
          risk_level = case_when(
            POB15 >= 100000 ~ lang_label("na"),
            T ~ risk_level
          )
        )
    } else if (pop_filter == lang_label("greater_than_100000")) {
      data <- data %>%
        mutate(
          risk_level = case_when(
            POB15 < 100000 ~ lang_label("na"),
            T ~ risk_level
          )
        )
    }
  }
  
  if (admin1_id == 0) {
    pie_data <- data %>% filter(!is.na(var)) %>% select(risk_level) %>% count(risk_level)
  } else {
    pie_data <- data %>% filter(`ADMIN1 GEO_ID` == admin1_id,!is.na(var)) %>% select(risk_level) %>% count(risk_level)
  }
  
  if (sum(is.na(data$var)) > 0) {pie_data <- rbind(pie_data,c(lang_label_tls(LANG_TLS,"no_data"),sum(is.na(data$var))))}
  if (length(ZERO_POB_LIST) > 0) {pie_data <- rbind(pie_data,c(lang_label_tls(LANG_TLS,"no_hab"),length(ZERO_POB_LIST)))}
  
  pie_data <- pie_data %>% mutate(
    risk_level_word = case_when(
      risk_level == lang_label_tls(LANG_TLS,"LR") ~ lang_label_tls(LANG_TLS,"cut_offs_LR"),
      risk_level == lang_label_tls(LANG_TLS,"MR") ~ lang_label_tls(LANG_TLS,"cut_offs_MR"),
      risk_level == lang_label_tls(LANG_TLS,"HR") ~ lang_label_tls(LANG_TLS,"cut_offs_HR"),
      risk_level == lang_label_tls(LANG_TLS,"VHR") ~ lang_label_tls(LANG_TLS,"cut_offs_VHR"),
      risk_level == "NA" | is.na(risk_level) ~ lang_label_tls(LANG_TLS,"no_data"),
      risk_level == "NO_HAB" ~ lang_label_tls(LANG_TLS,"no_hab"),
      risk_level == lang_label("na") ~ lang_label("na")
    ),
    clas_color = case_when(
      risk_level == lang_label_tls(LANG_TLS,"LR") ~ "rgba(146, 208, 80, 0.7)",
      risk_level == lang_label_tls(LANG_TLS,"MR") ~ "rgba(254, 192, 0, 0.7)",
      risk_level == lang_label_tls(LANG_TLS,"HR") ~ "rgba(232, 19, 43, 0.7)",
      risk_level == lang_label_tls(LANG_TLS,"VHR") ~ "rgba(146, 0, 0, 0.7)",
      risk_level == "NA" | is.na(risk_level) ~ "rgba(0, 0, 0, 0.5)",
      risk_level == "NO_HAB" ~ "rgba(155, 194, 230, 0.7)", 
      risk_level == lang_label("na") ~ "rgba(17, 17, 17, 0.7)"
    ),
    clas_order = case_when(
      risk_level == lang_label_tls(LANG_TLS,"LR") ~ 1,
      risk_level == lang_label_tls(LANG_TLS,"MR") ~ 2,
      risk_level == lang_label_tls(LANG_TLS,"HR") ~ 3,
      risk_level == lang_label_tls(LANG_TLS,"VHR") ~ 4,
      risk_level == "NA" | is.na(risk_level) ~ 5,
      risk_level == "NO_HAB" ~ 6,
      risk_level == lang_label("na") ~ 7
    )
  ) %>% arrange(clas_order)
  
  
  
  if (!return_table) {
    fig <- plot_ly(
      pie_data, labels = ~risk_level_word, values = ~n, 
      type = 'pie',
      textposition="inside",
      textinfo="value+percent",
      texttemplate="%{value}<br>%{percent:.1%}",
      sort = FALSE,
      marker = list(colors = ~clas_color)) %>% 
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      layout(legend = list(orientation = 'h',x = 0.05, y = 5, bgcolor = 'rgba(0,0,0,0)', font = list(size = 12))) %>%
      config(displaylogo = FALSE) %>%
      config(modeBarButtonsToRemove = c("sendDataToCloud", "editInChartStudio","pan2d","select2d","drawclosedpath",
                                        "drawline","drawrect","drawopenpath","drawcircle","eraseshape","autoScale2d",
                                        "zoomIn2d","zoomOut2d","toggleSpikelines","lasso2d")) %>% 
      layout(hovermode = 'x')
    
    return(fig)
  } else {
    pie_data <- pie_data %>% select(risk_level_word,n)
    colnames(pie_data) = c(lang_label_tls(LANG_TLS,"risk_level"),"n")
    
    datos_table <- pie_data %>%
      adorn_totals("row") %>% 
      datatable(
        rownames = F,
        extensions = 'Buttons',
        options = list(
          scrollX=TRUE, scrollCollapse=TRUE,
          language = list(
            info = paste0(lang_label_tls(LANG_TLS,"data_table_showing")," _START_ ",lang_label_tls(LANG_TLS,"data_table_to")," _END_ ",lang_label_tls(LANG_TLS,"data_table_of")," _TOTAL_ ",lang_label_tls(LANG_TLS,"data_table_rows")),
            paginate = list(previous = lang_label_tls(LANG_TLS,"data_table_prev"), `next` = lang_label_tls(LANG_TLS,"data_table_next"))
          ),
          searching = TRUE,fixedColumns = TRUE,autoWidth = FALSE,
          ordering = TRUE,scrollY = TRUE,pageLength = 8,
          dom = 'Brtip',
          buttons = list(
            list(extend = "copy",text = lang_label_tls(LANG_TLS,"button_copy")),
            list(extend='csv',filename=paste(lang_label_tls(LANG_TLS,"distribution"),lang_label_tls(LANG_TLS,indicator),admin1_id)),
            list(extend='excel', filename=paste(lang_label_tls(LANG_TLS,"distribution"),lang_label_tls(LANG_TLS,indicator),admin1_id))
          ),
          class = "display"
        )
      )
  }
}

apply_admin_pop_filters <- function(data, admin1_filter, pop_filter) {
  data_filtered <- data
  if (admin1_filter != lang_label("filter_all")) {
    data_filtered <- data_filtered %>% 
      filter(`ADMIN1 GEO_ID` == admin1_filter)
  }
  
  if (pop_filter != lang_label("filter_all")) {
    if (pop_filter == lang_label("less_than_100000")) {
      data_filtered <- data_filtered %>% filter(POB15 < 100000)
    } else if (pop_filter == lang_label("greater_than_100000")) {
      data_filtered <- data_filtered %>% filter(POB15 >= 100000)
    }
  }
  
  return(data_filtered)
}


# TITLES ----
title_map_box <- function(indicator,admin1) {
  indicator_as_is <- indicator
  indicator = tolower(indicator)
  if (indicator == tolower(lang_label("menuitem_general_label"))) {
    indicator = lang_label("total")
  } else if (indicator_as_is == lang_label("outbreaks_score")) {
    indicator = paste0("(", indicator_as_is, ")")
  } else {
    indicator = paste0("(",indicator,")")
  }
  if (admin1 == toupper(lang_label("rep_label_all"))) {
    title_text <- paste0(lang_label("general_title_map_box")," - ",toupper(COUNTRY_NAME))
  } else {
    title_text <- paste0(lang_label("general_title_map_box")," - ",admin1,", ",toupper(COUNTRY_NAME))
  }
  return(title_text)
}

title_bar_box <- function(indicator,admin1) {
  indicator = tolower(indicator)
  if (indicator == tolower(lang_label("menuitem_general_label"))) {
    indicator = lang_label("total")
  } else {
    indicator = paste0("(",indicator,")")
  }
  if (admin1 == toupper(lang_label("rep_label_all"))) {
    title_text <- paste0(lang_label("general_title_bar_box")," ",indicator," - ",toupper(COUNTRY_NAME))
  } else {
    title_text <- paste0(lang_label("general_title_bar_box")," ",indicator," - ",admin1,", ",toupper(COUNTRY_NAME))
  }
  return(title_text)
}

title_data_box <- function(indicator,admin1) {
  indicator = tolower(indicator)
  if (indicator == tolower(lang_label("menuitem_general_label"))) {
    indicator = lang_label("total")
  } else {
    indicator = paste0("(",indicator,")")
  }
  if (admin1 == toupper(lang_label("rep_label_all"))) {
    title_text <- paste0(lang_label("general_title_data_box")," ",indicator," - ",toupper(COUNTRY_NAME))
  } else {
    title_text <- paste0(lang_label("general_title_data_box")," ",indicator," - ",admin1,", ",toupper(COUNTRY_NAME))
  }
  return(title_text)
}

title_pie_box <- function(indicator,admin1) {
  indicator = tolower(indicator)
  if (admin1 == toupper(lang_label("rep_label_all"))) {
    title_text <- paste0(lang_label("title_pie_box")," (",indicator,") - ",toupper(COUNTRY_NAME))
  } else {
    title_text <- paste0(lang_label("title_pie_box")," (",indicator,") - ",admin1,", ",toupper(COUNTRY_NAME))
  }
  return(title_text)
}




# SOURCE ----
source("general.R")
source("immunity.R")
source("surveillance.R")
source("determinants.R")
source("outbreaks.R")

