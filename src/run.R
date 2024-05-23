# Herramienta digital Análisis de Riesgo
# Organización Panamericana de la Salud
# Autor: Oliver Mazariegos
# Última fecha de modificación: 2023-11-10
# R 4.3.1

## 2024-05-17 Major fix ----
#Sys.setlocale(locale = "es_ES.UTF-8")

# SETUP                             -------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())

library(pacman)
p_load(devtools,webshot,lubridate,forcats,stringr,dplyr,purrr,readr,tibble,
       tidyverse,tidyr,shinydashboard,shinyBS,shiny,shinycssloaders,sf,scales,
       rmarkdown,readxl,RColorBrewer,plotly,ggplot2,mapview,leaflet,janitor, kableExtra,
       htmltools,fontawesome,data.table,knitr,geojsonio,rmapshaper,sp,
       tinytex,DT, flextable)
if (!webshot::is_phantomjs_installed()) {
  webshot::install_phantomjs(force=TRUE)
}


# 1. FLAG                 ----------------------------------------
file.copy(from = "Data/country_flag.png",to ="R/Dashboard/www/country_flag.png",overwrite = TRUE)

# 2. EVAL                 ----------------------------------------
source("R/risk_eval.R")


# 3. REPORT               ----------------------------------------
# HTML Report
rmarkdown::render("R/report_html.Rmd", "html_document")
file.copy(from = "R/report_html.html",to ="R/Dashboard/www/report_html.html",overwrite = TRUE)
file.remove(from = "R/report_html.html")
# WORD Report
rmarkdown::render("R/report_word.Rmd", "word_document")
file.copy(from = "R/report_word.docx",to ="R/Dashboard/www/report_word.docx",overwrite = TRUE)
file.remove(from = "R/report_word.docx")

# 4. DASHBOARD            ----------------------------------------
shiny::runApp("R/Dashboard")

