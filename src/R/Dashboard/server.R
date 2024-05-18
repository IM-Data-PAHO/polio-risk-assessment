# AUTHORSHIP ----

# Pan American Health Organization
# Author: Oliver Mazariegos
# Last Update: 2023-10-09
# R 4.3.1



# SERVER ----
function(input, output, session) {
  
  # REPORTS ----
  # DOWNLOAD Report ----
  output$download_report_word <- downloadHandler(
    filename = paste0(lang_label("report_filename")," ",toupper(COUNTRY_NAME),".docx"),
    content = function(file) {
      file.copy("www/report_word.docx", file)
    }
  )
  
  output$download_report_html <- downloadHandler(
    filename = paste0(lang_label("report_filename")," ",toupper(COUNTRY_NAME),".html"),
    content = function(file) {
      file.copy("www/report_html.html", file)
    }
  )
  
  # GENERAL FUNCTIONS ----
  ind_rename <- function(selected_ind) {
    renamed <- case_when(
      lang_label("menuitem_general_label") == selected_ind ~ "total_score",
      lang_label("menuitem_immunity") == selected_ind ~ "immunity_score",
      lang_label("menuitem_surveillance") == selected_ind ~ "surveillance_score",
      lang_label("menuitem_determinants") == selected_ind ~ "determinants_score",
      lang_label("menuitem_outbreaks") == selected_ind ~ "outbreaks_score",
      lang_label("general_select_immunity") == selected_ind ~ "immunity_score",
      lang_label("general_select_surveillance") == selected_ind ~ "surveillance_score",
      lang_label("general_select_determinants") == selected_ind ~ "determinants_score",
      lang_label("general_select_outbreaks") == selected_ind ~ "outbreaks_score"
    )
    return(renamed)
  }
  
  risk_rename <- function(selected_risk) {
    return(
      case_when(
        toupper(lang_label("rep_label_all")) == selected_risk ~ "ALL",
        lang_label("cut_offs_VHR") == selected_risk ~ "VHR",
        lang_label("cut_offs_HR") == selected_risk ~ "HR",
        lang_label("cut_offs_MR") == selected_risk ~ "MR",
        lang_label("cut_offs_LR") == selected_risk ~ "LR"
      )
    )
  }
  
  risk_filter_rename <- function(risk_filter) {
    return(
      case_when(
        toupper(lang_label("rep_label_all")) == risk_filter ~ toupper(lang_label("filter_all")),
        lang_label("cut_offs_VHR") == risk_filter ~ lang_label("VHR"),
        lang_label("cut_offs_HR") == risk_filter ~ lang_label("HR"),
        lang_label("cut_offs_MR") == risk_filter ~ lang_label("MR"),
        lang_label("cut_offs_LR") == risk_filter ~ lang_label("LR")
      )
    )
  }
  
  box_lugar <- function(admin1) {
    if (admin1 == toupper(lang_label("rep_label_all"))) {
      return(toupper(COUNTRY_NAME))
    } else {
      return(toupper(admin1))
    }
  }
  
  
  ## VALUE BOXES ----
  box_data <- reactiveValues()
  box_data$a1 <- 0
  box_data$a2 <- 0
  box_data$a3 <- 0
  box_data$a4 <- 0
  box_data$at <- 0
  
  observeEvent(input$indicadores_select_indicador, {
    new_box_data <- datos_boxes(LANG_TLS,indicadores_prep_box_data())
    box_data$a1 <- new_box_data[1]
    box_data$a2 <- new_box_data[2]
    box_data$a3 <- new_box_data[3]
    box_data$a4 <- new_box_data[4]
    box_data$at <- new_box_data[5]
  })
  
  observeEvent(input$admin1_filter, {
    new_box_data <- datos_boxes(LANG_TLS,indicadores_prep_box_data())
    box_data$a1 <- new_box_data[1]
    box_data$a2 <- new_box_data[2]
    box_data$a3 <- new_box_data[3]
    box_data$a4 <- new_box_data[4]
    box_data$at <- new_box_data[5]
  })
  
  observeEvent(input$population15_filter, {
    new_box_data <- datos_boxes(LANG_TLS,indicadores_prep_box_data())
    box_data$a1 <- new_box_data[1]
    box_data$a2 <- new_box_data[2]
    box_data$a3 <- new_box_data[3]
    box_data$a4 <- new_box_data[4]
    box_data$at <- new_box_data[5]
  })
  
  indicadores_prep_box_data <- reactive({
    ind_prep_box_data(LANG_TLS,
                      CUT_OFFS,
                      scores_data,
                      ind_rename(input$indicadores_select_indicador),
                      unique(get_a1_geo_id(input$admin1_filter)),
                      input$population15_filter)
  })
  
  output$ind_box_1 <- renderValueBox({
    valueBox(
      VB_style(get_box_text(box_data$a1,box_data$at,"LR"),"font-size: 90%;"),
      VB_style(paste(lang_label("box_LR_admin2"),box_lugar(input$admin1_filter)),"font-size: 95%;"),
      icon = icon('ok-sign', lib = 'glyphicon'),
      color = "purple"
    )
  })
  
  output$ind_box_2 <- renderValueBox({
    valueBox(
      VB_style(get_box_text(box_data$a2,box_data$at,"MR"),"font-size: 85%;"),
      VB_style(paste(lang_label("box_MR_admin2"),box_lugar(input$admin1_filter)),"font-size: 95%;"),
      icon = icon('minus-sign', lib = 'glyphicon'),
      color = "purple"
    )
  })
  
  output$ind_box_3 <- renderValueBox({
    valueBox(
      VB_style(get_box_text(box_data$a3,box_data$at,"HR"),"font-size: 85%;"),
      VB_style(paste(lang_label("box_HR_admin2"),box_lugar(input$admin1_filter)),"font-size: 95%;"),
      icon = icon('exclamation-sign', lib = 'glyphicon'),
      color = "purple"
    )
  })
  
  output$ind_box_4 <- renderValueBox({
    valueBox(
      VB_style(get_box_text(box_data$a4,box_data$at,"VHR"),"font-size: 85%;"),
      VB_style(paste(lang_label("box_VHR_admin2"),box_lugar(input$admin1_filter)),"font-size: 95%;"),
      icon = icon('alert', lib = 'glyphicon'),
      color = "purple"
    )
  })
  
  output$indicadores_title <- renderText({
    input$indicadores_select_indicador
  })
  
  ## MAP ----
  indicadores_prep_map_data <- reactive({
    ind_prep_map_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,country_shapes,scores_data,ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$admin1_filter),risk_rename(input$indicadores_select_risk), input$population15_filter)
  })
  
  
  ind_map <- reactiveValues(dat = 0)
  ind_map_2 <- reactiveValues(dat = 0)
  
  
  output$indicadores_title_map_box <- renderText({
    text_title <- paste(
      lang_label(
        "general_title_map_box"),"-",
      ifelse(input$admin1_filter == lang_label("filter_all"), COUNTRY_NAME, input$admin1_filter)
    )
    text_title <- paste0(text_title," (",YEAR_EVAL,")")
    text_title
  })
  
  output$indicadores_title_map_box_2 <- renderText({
    text_title <- title_map_box(lang_label("general_title_map_box"),input$admin1_filter)
    text_title <- paste0(text_title," (",YEAR_EVAL,")")
    text_title
  })
  
  ### TOTALS ----
  output$indicadores_plot_map <- renderLeaflet({
    ind_map$dat <- ind_plot_map_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,indicadores_prep_map_data(),ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$admin1_filter),risk_rename(input$indicadores_select_risk), input$admin1_filter)
    ind_map$dat
  })
  
  output$dl_indicadores_plot_map <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",toupper(COUNTRY_NAME)," ",input$indicadores_select_indicador," (",YEAR_EVAL,").png")
    },
    content = function(file) {
      mapshot(ind_map$dat, file = file)
    }
  )
  
  output$indicadores_plot_map_2 <- renderLeaflet({
    ind_map_2$dat <- ind_plot_map_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,indicadores_prep_map_data(),ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$admin1_filter),risk_rename(input$indicadores_select_risk), input$admin1_filter)
    ind_map_2$dat
  })
  
  output$dl_indicadores_plot_map_2 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",input$indicadores_select_indicador," (",YEAR_EVAL,").png")
    },
    content = function(file) {
      mapshot(ind_map_2$dat, file = file)
    }
  )
  
  ### IMMUNITY ----
  inmu_map_total <- reactiveValues(dat = 0)
  
  output$indicadores_plot_map_immunity <- renderLeaflet({
    inmu_map_total$dat <- inmu_plot_map_data(
      LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,
      CUT_OFFS,country_shapes,immunity_scores,"immunity_score",
      input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, 
      input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    inmu_map_total$dat
  })
  
  output$dl_indicadores_plot_map_immunity <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",toupper(COUNTRY_NAME)," ",lang_label("general_select_immunity")," (",YEAR_EVAL,").png")
    },
    content = function(file) {
      mapshot(inmu_map_total$dat, file = file)
    }
  )
  
  inmu_map_total_2 <- reactiveValues(dat = 0)
  
  output$indicadores_plot_map_immunity_2 <- renderLeaflet({
    inmu_map_total$dat <- inmu_plot_map_data(
      LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,
      CUT_OFFS,country_shapes,immunity_scores,"immunity_score",
      input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, 
      input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    inmu_map_total$dat
  })
  
  output$dl_indicadores_plot_map_immunity_2 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("general_select_immunity")," (",YEAR_EVAL,").png")
    },
    content = function(file) {
      mapshot(inmu_map_total_2$dat, file = file)
    }
  )
  
  ### SURVEILLANCE ----
  cal_map_total <- reactiveValues(dat = 0)
  
  output$indicadores_plot_map_surveillance <- renderLeaflet({
    cal_map_total$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,surveillance_scores,"surveillance_score",input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    cal_map_total$dat
  })
  
  output$dl_indicadores_plot_map_surveillance <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",toupper(COUNTRY_NAME)," ",lang_label("general_select_surveillance")," (",YEAR_EVAL,").png")
    },
    content = function(file) {
      mapshot(cal_map_total$dat, file = file)
    }
  )
  
  cal_map_total_2 <- reactiveValues(dat = 0)
  
  output$indicadores_plot_map_surveillance_2 <- renderLeaflet({
    cal_map_total$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,surveillance_scores,"surveillance_score",input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    cal_map_total$dat
  })
  
  output$dl_indicadores_plot_map_immunity_2 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("general_select_surveillance")," (",YEAR_EVAL,").png")
    },
    content = function(file) {
      mapshot(cal_map_total_2$dat, file = file)
    }
  )
  
  
  ### DETERMINANTS ----
  determinants_map_total <- reactiveValues(dat = 0)
  
  output$indicadores_plot_map_determinants <- renderLeaflet({
    determinants_map_total$dat <- determinants_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,determinants_scores,"determinants_score",input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    determinants_map_total$dat
  })
  
  output$dl_indicadores_plot_map_determinants <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",toupper(COUNTRY_NAME)," ",lang_label("general_select_determinants")," (",YEAR_EVAL,").png")
    },
    content = function(file) {
      mapshot(determinants_map_total$dat, file = file)
    }
  )
  
  determinants_map_total_2 <- reactiveValues(dat = 0)
  
  output$indicadores_plot_map_determinants_2 <- renderLeaflet({
    determinants_map_total_2$dat <- determinants_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,determinants_scores,"determinants_score",input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    determinants_map_total_2$dat
  })
  
  output$dl_indicadores_plot_map_determinants_2 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("general_select_determinants")," (",YEAR_EVAL,").png")
    },
    content = function(file) {
      mapshot(determinants_map_total_2$dat, file = file)
    }
  )
  
  
  ### OUTBREAKS ----
  outbreaks_map_total <- reactiveValues(dat = 0)
  
  output$indicadores_plot_map_outbreaks <- renderLeaflet({
    outbreaks_map_total$dat <- outbreaks_plot_map_data(
      LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,
      CUT_OFFS,country_shapes,outbreaks_scores,"outbreaks_score",
      input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, 
      input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    outbreaks_map_total$dat
  })
  
  output$dl_indicadores_plot_map_outbreaks <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",toupper(COUNTRY_NAME)," ",lang_label("general_select_outbreaks")," (",YEAR_EVAL,").png")
    },
    content = function(file) {
      mapshot(outbreaks_map_total$dat, file = file)
    }
  )
  
  outbreaks_map_total_2 <- reactiveValues(dat = 0)
  
  output$indicadores_plot_map_outbreaks_2 <- renderLeaflet({
    outbreaks_map_total$dat <- outbreaks_plot_map_data(
      LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,
      CUT_OFFS,country_shapes,outbreaks_scores,"outbreaks_score",
      input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, 
      input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    outbreaks_map_total$dat
  })
  
  output$dl_indicadores_plot_map_outbreaks_2 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("general_select_outbreaks")," (",YEAR_EVAL,").png")
    },
    content = function(file) {
      mapshot(outbreaks_map_total_2$dat, file = file)
    }
  )
  
  
  ### TABLE ----
  output$general_title_data_box <- renderText({
    title_data_box(lang_label("total_score"),input$admin1_filter)
  })
  
  
  output$indicadores_table <- DT::renderDataTable(server = FALSE,{
    ind_get_bar_table(LANG_TLS,CUT_OFFS,scores_data,ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$admin1_filter),risk_rename(input$indicadores_select_risk), input$population15_filter)
  })
  
  output$general_title_data_box2 <- renderText({
    title_data_box(lang_label("total_score"),input$admin1_filter)
  })
  
  
  output$indicadores_table_2 <- DT::renderDataTable(server = FALSE,{
    ind_get_bar_table(LANG_TLS,CUT_OFFS,scores_data,ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$admin1_filter),risk_rename(input$indicadores_select_risk), input$population15_filter)
  })
  
  ### TOTAL POINTS ----
  indicadores_prep_bar_data <- reactive({
    ind_prep_bar_data(LANG_TLS,CUT_OFFS,scores_data,ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$admin1_filter),risk_rename(input$indicadores_select_risk), input$population15_filter)
  })
  
  
  output$indicadores_title_bar_box <- renderText({
    text_title <- title_bar_box(input$indicadores_select_indicador,input$admin1_filter)
    text_title <- paste0(text_title," (",YEAR_EVAL,")")
    text_title
  })
  
  output$indicadores_plot_bar <- renderPlotly({
    pfa_filter <- case_when(
      input$general_title_plot_bar_filter == lang_label("population_pfa_filter") ~ TRUE,
      input$general_title_plot_bar_filter == lang_label("population_pfa_no_filter") ~ FALSE,
    )
    ind_plot_bar_data(LANG_TLS,CUT_OFFS,indicadores_prep_bar_data(),ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$admin1_filter), pfa_filter)
  })
  
  output$indicadores_plot_multibar <- renderPlotly({
    pfa_filter <- case_when(
      input$general_title_plot_multibar_filter == lang_label("population_pfa_filter") ~ TRUE,
      input$general_title_plot_multibar_filter == lang_label("population_pfa_no_filter") ~ FALSE,
    )
    ind_plot_multibar_data(LANG_TLS,CUT_OFFS,scores_data,get_a1_geo_id(input$admin1_filter),ind_rename(input$indicadores_select_indicador),risk_rename(input$indicadores_select_risk), pfa_filter, input$population15_filter)
  })
  
  ### INDICATORS CHEAT SHEET ----
  output$indicadores_rangos_table <- DT::renderDataTable(server = FALSE,{
    pfa_filter <- case_when(
      input$general_limits_table_filter == lang_label("population_pfa_filter") ~ TRUE,
      input$general_limits_table_filter == lang_label("population_pfa_no_filter") ~ FALSE,
    )
    ind_rangos_table(LANG_TLS,CUT_OFFS,ind_rename(input$indicadores_select_indicador), pfa_filter)
  })
  
  output$download_cutoffs_excel <- downloadHandler(
    filename = paste0(lang_label("cutoffs_excel_filename")," ",toupper(COUNTRY_NAME),".xlsx"),
    content = function(file) {
      file.copy("www/cut_offs_download.xlsx", file)
    }
  )
  
  # IMMUNITY ----
  
  ## MAP ----
  ### IMMUNITY SCORE ----
  output$inmunidad_title_map_box <- renderText({
    title_map_box(lang_label("INM_POB"),input$admin1_filter)
  })
  
  
  output$inmunidad_map_total <- renderLeaflet({
    inmu_map_total$dat <- inmu_plot_map_data(
      LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,
      CUT_OFFS,country_shapes,immunity_scores,"immunity_score",
      input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, 
      input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    inmu_map_total$dat
  })
  
  output$dl_inmunidad_map_total <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("INM_POB")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(inmu_map_total$dat, file = file)
    }
  )
  
  ### POLIO COVERAGE ----
  inmu_map_cob_1 <- reactiveValues(dat = 0)
  
  output$inmunidad_map_cob_1 <- renderLeaflet({
    var_srp1 <- case_when(
      input$radio_inmunidad_cob_1 == paste(lang_label("vac_coverage"),YEAR_1) ~ "year1",
      input$radio_inmunidad_cob_1 == paste(lang_label("vac_coverage"),YEAR_2) ~ "year2",
      input$radio_inmunidad_cob_1 == paste(lang_label("vac_coverage"),YEAR_3) ~ "year3",
      input$radio_inmunidad_cob_1 == paste(lang_label("vac_coverage"),YEAR_4) ~ "year4",
      input$radio_inmunidad_cob_1 == paste(lang_label("vac_coverage"),YEAR_5) ~ "year5",
      input$radio_inmunidad_cob_1 == lang_label("risk_points") ~ "SRP1_PR"
    )
    inmu_map_cob_1$dat <- inmu_plot_map_data(
      LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,
      CUT_OFFS,country_shapes,immunity_scores,var_srp1,input$admin1_filter,
      get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, 
      input$population15_filter, risk_filter_rename(input$indicadores_select_risk)
    )
    inmu_map_cob_1$dat
  })
  
  output$dl_inmunidad_map_cob_1 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("immunity_polio_cob")," (",input$radio_inmunidad_cob_1,").png")
    },
    content = function(file) {
      mapshot(inmu_map_cob_1$dat, file = file)
    }
  )
  
  ### IPV2 COVERAGE ----
  inmu_map_cob_2 <- reactiveValues(dat = 0)
  
  output$inmunidad_map_cob_2 <- renderLeaflet({
    inmu_map_cob_2$dat <- inmu_plot_map_data(
      LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,
      CUT_OFFS,country_shapes,immunity_scores,"ipv2",input$admin1_filter,
      get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, 
      input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    inmu_map_cob_2$dat
  })
  
  output$dl_inmunidad_map_cob_2 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("immunity_ipv2_cob")," (",YEAR_EVAL,").png")
    },
    content = function(file) {
      mapshot(inmu_map_cob_2$dat, file = file)
    }
  )
  
  ### EFFECTIVE COVERAGE ----
  inmu_map_effective <- reactiveValues(dat = 0)
  
  output$inmunidad_map_effective <- renderLeaflet({
    inmu_map_effective$dat <- inmu_plot_map_data(LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,immunity_scores,"effective_campaign",input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    inmu_map_effective$dat
  })
  
  output$dl_inmunidad_map_effective <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("immunity_effective_cob")," (",YEAR_1, "-", YEAR_EVAL,").png")
    },
    content = function(file) {
      mapshot(inmu_map_effective$dat, file = file)
    }
  )
  
  ## PIE ----
  output$inmunidad_title_pie_box <- renderText({
    title_pie_box(lang_label("INM_POB"),input$admin1_filter)
  })
  
  output$inmunidad_plot_pie <- renderPlotly({
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"immunity_score",immunity_scores,get_a1_geo_id(input$admin1_filter), input$population15_filter,return_table = F)
  })
  
  output$inmunidad_table_dist <- DT::renderDataTable(server = FALSE,{
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"immunity_score",immunity_scores,get_a1_geo_id(input$admin1_filter), input$population15_filter,return_table = T)
  })
  
  ## DATATABLE ----
  output$inmunidad_title_data_box <- renderText({
    title_data_box(lang_label("INM_POB"),input$admin1_filter)
  })
  
  output$inmunidad_table <- DT::renderDataTable(server = FALSE,{
    inmu_get_data_table(LANG_TLS,YEAR_LIST,CUT_OFFS,immunity_scores,get_a1_geo_id(input$admin1_filter), input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
  })
  
  ## CHEAT SHEET ----
  output$inmu_rangos_table <- DT::renderDataTable(server = FALSE,{
    pfa_filter <- case_when(
      input$immunity_limits_table_filter == lang_label("population_pfa_filter") ~ TRUE,
      input$immunity_limits_table_filter == lang_label("population_pfa_no_filter") ~ FALSE,
    )
    ind_rangos_table(LANG_TLS,CUT_OFFS,"immunity_score", pfa_filter)
  })
  
  # SURVEILLANCE ----
  ## MAP ----
  output$surveillance_title_map_box <- renderText({
    title_map_box(lang_label("SURV_QUAL"),input$admin1_filter)
  })
  
  ### SURVEILLANCE SCORE ----
  
  output$calidad_map_total <- renderLeaflet({
    cal_map_total$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,surveillance_scores,"surveillance_score",input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    cal_map_total$dat
  })
  
  output$dl_surveillance_map_total <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("SURV_QUAL")," (",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(cal_map_total$dat, file = file)
    }
  )
  
  ### REPORTING UNITS ----
  cal_map_2 <- reactiveValues(dat = 0)
  output$calidad_map_2 <- renderLeaflet({
    cal_map_2$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,surveillance_scores,"compliant_units_percent",input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    cal_map_2$dat
  })
  
  output$dl_calidad_map_2 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("surveillance_title_map_reporting_units")," (",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(cal_map_2$dat, file = file)
    }
  )
  
  ### PFA RATE ----
  cal_map_3 <- reactiveValues(dat = 0)
  output$calidad_map_3 <- renderLeaflet({
    cal_map_3$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,surveillance_scores,"pfa_rate",input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    cal_map_3$dat
  })
  
  output$dl_calidad_map_3 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("surveillance_pfa_rate")," (",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(cal_map_3$dat, file = file)
    }
  )
  
  ### PFA NOTIFICATION ----
  cal_map_4 <- reactiveValues(dat = 0)
  output$calidad_map_4 <- renderLeaflet({
    cal_map_4$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,surveillance_scores,"pfa_notified_percent",input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    cal_map_4$dat
  })
  
  output$dl_calidad_map_4 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("surveillance_pfa_notification")," (",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(cal_map_4$dat, file = file)
    }
  )
  
  ### PFA INVESTIGATED ----
  cal_map_5 <- reactiveValues(dat = 0)
  output$calidad_map_5 <- renderLeaflet({
    cal_map_5$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,surveillance_scores,"pfa_investigated_percent",input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    cal_map_5$dat
  })
  
  output$dl_calidad_map_5 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("surveillance_pfa_investigated")," (",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(cal_map_5$dat, file = file)
    }
  )
  
  ### SUITABLE SAMPLES ----
  cal_map_6 <- reactiveValues(dat = 0)
  output$calidad_map_6 <- renderLeaflet({
    cal_map_6$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,surveillance_scores,"suitable_samples_percent",input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    cal_map_6$dat
  })
  
  output$dl_calidad_map_6 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("surveillance_suitable_samples")," (",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(cal_map_6$dat, file = file)
    }
  )
  
  ### FOLLOWUPS ----
  cal_map_7 <- reactiveValues(dat = 0)
  output$calidad_map_7 <- renderLeaflet({
    cal_map_7$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,surveillance_scores,"followups_percent",input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    cal_map_7$dat
  })
  
  output$dl_calidad_map_7 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("surveillance_followups")," (",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(cal_map_7$dat, file = file)
    }
  )
  
  ### ACTIVE SEARCH ----
  cal_map_8 <- reactiveValues(dat = 0)
  output$calidad_map_8 <- renderLeaflet({
    cal_map_8$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,surveillance_scores,"active_search",input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    cal_map_8$dat
  })
  
  output$dl_calidad_map_8 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("surveillance_active_search")," (",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(cal_map_8$dat, file = file)
    }
  )
  
  ## PIE ----
  output$surveillance_title_pie_box <- renderText({
    title_pie_box(lang_label("SURV_QUAL"),input$admin1_filter)
  })
  
  output$calidad_plot_pie <- renderPlotly({
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"surveillance_score",surveillance_scores,get_a1_geo_id(input$admin1_filter), input$population15_filter,return_table = F)
  })
  
  output$calidad_table_dist <- DT::renderDataTable(server = FALSE,{
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"surveillance_score",surveillance_scores,get_a1_geo_id(input$admin1_filter), input$population15_filter,return_table = T)
  })
  
  ## DATATABLE ----
  output$calidad_title_data_box <- renderText({
    title_data_box(lang_label("SURV_QUAL"),input$admin1_filter)
  })
  
  output$calidad_table <- DT::renderDataTable(server = FALSE,{
    cal_get_data_table(LANG_TLS,CUT_OFFS,surveillance_scores,get_a1_geo_id(input$admin1_filter), input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
  })
  
  ## CHEAT SHEET ----
  output$surveillance_rangos_table <- DT::renderDataTable(server = FALSE,{
    pfa_filter <- case_when(
      input$surveillance_limits_table_filter == lang_label("population_pfa_filter") ~ TRUE,
      input$surveillance_limits_table_filter == lang_label("population_pfa_no_filter") ~ FALSE,
    )
    ind_rangos_table(LANG_TLS,CUT_OFFS,"surveillance_score", pfa_filter)
  })
  
  # DETERMINANTS ----
  ## MAP ----
  output$determinants_map_box_title <- renderText({
    title_map_box(lang_label("determinants_score"),input$admin1_filter)
  })
  
  ### DETERMINANTS SCORE ----
  output$determinants_map_total <- renderLeaflet({
    determinants_map_total$dat <- determinants_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,determinants_scores,"determinants_score",input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    determinants_map_total$dat
  })
  
  output$dl_determinants_map_total <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("determinants_score")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(determinants_map_total$dat, file = file)
    }
  )
  
  ### DRINKING WATER ----
  determinants_map_water <- reactiveValues(dat = 0)
  
  output$determinants_map_water <- renderLeaflet({
    determinants_map_water$dat <- determinants_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,determinants_scores,"drinking_water_percent",input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    determinants_map_water$dat
  })
  
  output$dl_determinants_map_water <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("determinants_drinking_water")," (", YEAR_5,").png")
    },
    content = function(file) {
      mapshot(determinants_map_water$dat, file = file)
    }
  )
  
  ### SANITATION SERVICES ----
  determinants_map_sanitation <- reactiveValues(dat = 0)
  
  output$determinants_map_sanitation <- renderLeaflet({
    determinants_map_sanitation$dat <- determinants_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,determinants_scores,"sanitation_services_percent",input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    determinants_map_sanitation$dat
  })
  
  output$dl_determinants_map_sanitation <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("determinants_sanitation_services")," (", YEAR_5,").png")
    },
    content = function(file) {
      mapshot(determinants_map_sanitation$dat, file = file)
    }
  )
  
  ## PIE ----
  output$determinants_title_pie_box <- renderText({
    title_pie_box(lang_label("determinants_score"),input$admin1_filter)
  })
  
  output$determinants_title_pie_box <- renderText({
    title_pie_box(lang_label("determinants_score"),input$admin1_filter)
  })
  
  output$determinants_plot_pie <- renderPlotly({
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"determinants_score",determinants_scores,get_a1_geo_id(input$admin1_filter), input$population15_filter,return_table = F)
  })
  
  output$determinants_table_dist <- DT::renderDataTable(server = FALSE,{
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"determinants_score",determinants_scores,get_a1_geo_id(input$admin1_filter), input$population15_filter,return_table = T)
  })
  
  ## DATATABLE ----
  output$determinants_title_data_box <- renderText({
    title_data_box(lang_label("determinants_score"),input$admin1_filter)
  })
  
  output$determinants_table <- DT::renderDataTable(server = FALSE,{
    determinants_get_data_table(LANG_TLS,CUT_OFFS,determinants_scores,get_a1_geo_id(input$admin1_filter), input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
  })
  
  ## CHEAT SHEET ----
  output$determinants_rangos_table <- DT::renderDataTable(server = FALSE,{
    pfa_filter <- case_when(
      input$determinants_limits_table_filter == lang_label("population_pfa_filter") ~ TRUE,
      input$determinants_limits_table_filter == lang_label("population_pfa_no_filter") ~ FALSE,
    )
    ind_rangos_table(LANG_TLS,CUT_OFFS,"determinants_score", pfa_filter)
  })
  
  # OUTBREAKS ----
  ## MAP ----
  output$outbreaks_map_box_title <- renderText({
    title_map_box(lang_label("outbreaks_score"),input$admin1_filter)
  })
  
  ### OUTBREAK SCORE ----
  
  output$outbreaks_map_total <- renderLeaflet({
    outbreaks_map_total$dat <- outbreaks_plot_map_data(
      LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,
      CUT_OFFS,country_shapes,outbreaks_scores,"outbreaks_score",
      input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, 
      input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
    outbreaks_map_total$dat
  })
  
  output$dl_outbreaks_map_total <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("outbreaks_score")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(outbreaks_map_total$dat, file = file)
    }
  )
  
  ### DISEASES ----
  outbreaks_diseases <- reactiveValues(dat = 0)
  
  output$outbreaks_disease_map <- renderLeaflet({
    which_var <- case_when(
      input$outbreaks_disease_filter == lang_label("outbreaks_polio") ~ "polio",
      input$outbreaks_disease_filter == lang_label("outbreaks_measles") ~ "measles",
      input$outbreaks_disease_filter == lang_label("outbreaks_rubella") ~ "rubella",
      input$outbreaks_disease_filter == lang_label("outbreaks_diphtheria") ~ "diphtheria",
      input$outbreaks_disease_filter == lang_label("outbreaks_yellow_fever") ~ "yellow_fever",
      input$outbreaks_disease_filter == lang_label("outbreaks_tetanus") ~ "tetanus"
    )
    outbreaks_diseases$dat <- outbreaks_plot_map_data(
      LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,
      CUT_OFFS,country_shapes,outbreaks_scores,which_var,
      input$admin1_filter,get_a1_geo_id(input$admin1_filter),admin1_geo_id_df, 
      input$population15_filter, risk_filter_rename(input$indicadores_select_risk),
      input$outbreaks_disease_filter)
    outbreaks_diseases$dat
  })
  
  output$dl_outbreaks_disease_map <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$admin1_filter," ",toupper(COUNTRY_NAME)," ",lang_label("outbreaks_title_map_disease")," (",input$outbreaks_disease_filter,").png")
    },
    content = function(file) {
      mapshot(outbreaks_diseases$dat, file = file)
    }
  )
  
  ## PIE ----
  output$outbreaks_title_pie_box <- renderText({
    title_pie_box(lang_label("outbreaks_score"),input$admin1_filter)
  })
  
  output$outbreaks_plot_pie <- renderPlotly({
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"outbreaks_score",outbreaks_scores,get_a1_geo_id(input$admin1_filter), input$population15_filter,return_table = F)
  })
  
  output$outbreaks_table_dist <- DT::renderDataTable(server = FALSE,{
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"outbreaks_score",outbreaks_scores,get_a1_geo_id(input$admin1_filter), input$population15_filter,return_table = T)
  })
  
  ## DATATABLE ----
  output$outbreaks_title_data_box <- renderText({
    title_data_box(lang_label("outbreaks_score"),input$admin1_filter)
  })
  
  output$outbreaks_table <- DT::renderDataTable(server = FALSE,{
    outbreaks_get_data_table(LANG_TLS,YEAR_LIST,CUT_OFFS,outbreaks_scores,get_a1_geo_id(input$admin1_filter), input$population15_filter, risk_filter_rename(input$indicadores_select_risk))
  })
  
  ## CHEAT SHEET ----
  output$outbreaks_rangos_table <- DT::renderDataTable(server = FALSE,{
    pfa_filter <- case_when(
      input$outbreaks_limits_table_filter == lang_label("population_pfa_filter") ~ TRUE,
      input$outbreaks_limits_table_filter == lang_label("population_pfa_no_filter") ~ FALSE,
    )
    ind_rangos_table(LANG_TLS,CUT_OFFS,"outbreaks_score", pfa_filter)
  })
  
  
  
  
  
  
  
  
}
