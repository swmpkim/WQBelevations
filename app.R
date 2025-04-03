library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(naniar)
library(khroma)
library(plotly)
# library(DT)
library(reactable)
library(skimr)


# UI ----
ui <- page_navbar(
    title = "WQB Elevations App",
    theme = bs_theme(version = 5),
    bg = "#477FB0",
    inverse = TRUE,
    underline = TRUE,
    
    # Sidebar - read in data files and control options
    sidebar = sidebar(
        title = "File Inputs",
        # elevation file
        fileInput("file.elevs", 
                  span(
                      "Upload elevation file", 
                      tooltip(
                          bsicons::bs_icon("info-circle"),
                          "The file must be a csv file following Waquoit Bay NERR's formatting.",
                          placement = "right"
                      )
                  ),  
                  multiple = FALSE,
                  accept = ".csv"),
        # column selection for elevation file
        selectInput("elevColSel", label = "Which column(s) represent elevation measurements?",
                    choices = NULL,
                    multiple = TRUE),
        selectInput("elevAvgSel", label = "Is there a column representing the average of elevation measurements?",
                    choices = NULL,
                    multiple = FALSE),
        # veg data file
        fileInput("file.veg", 
                  span(
                      "Upload vegetation file", 
                      tooltip(
                          bsicons::bs_icon("info-circle"),
                          "The file must be an Excel file in the Namaste project format.",
                          placement = "right"
                      )
                  ),  
                  multiple = FALSE,
                  accept = ".xlsx")
    ),  # end sidebar
    
    # Elevation data panel ----
    nav_panel("Elevations",
              layout_sidebar(
                  sidebar = sidebar(
                      title = NULL,
                      span(
                          h5("Options for time series and transect profile graphics"),
                          tooltip(
                              bsicons::bs_icon("info-circle"),
                              "The time series and transect profile graphs allow detailed examination of one parameter at one site at a time. Select (and change) either or both here and choices will apply in the tabs for both graphic types.",
                              placement = "right"
                          )
                      ),
                      selectInput("selected_site", "Select Site:", 
                                  choices = NULL)
                  ),
              navset_card_tab(
                  nav_panel(
                      title = "Elevation data preview",
                      htmltools::tags$small("This table reflects the input data. The table is interactive. Columns can be sorted by clicking on their name, or filtered by typing into the box below the name."),
                      reactableOutput("dt.elevs")
                  ),
                  nav_panel(
                      title = "Elevation column summary",
                      htmltools::tags$small("This table provides information about each column in the data. The  table is interactive. Columns can be sorted by clicking on their name, or filtered by typing into the box below the name."),
                      reactableOutput("dt.elevs.skimr")
                  ),
                  nav_panel(
                      title = "Elevation histograms",
                      card(
                          full_screen = TRUE,  # Optional fullscreen card
                          div(
                              class = "container",  # Bootstrap container for the grid
                              fluidRow(
                                  column(6, plotlyOutput("p_elevMean", height = "300px")),  # Top Left
                                  column(6, plotlyOutput("p_elevSD", height = "300px"))   # Top Right
                              ),
                              fluidRow(
                                  column(6, plotlyOutput("p_elevAll", height = "300px")),  # Bottom Left
                                  column(6, div(class = "p-3", ""))   # Bottom Right
                              )
                          )
                      )
                  ),
                  nav_panel(
                      title = "Elevation time series",
                      card(
                          full_screen = TRUE,
                          layout_columns(
                              col_widths = c(3, 9),
                              layout_column_wrap(
                                  checkboxInput("show_errorbars", "Show error bars", value = TRUE)),
                              card(
                                  layout_columns(
                                      col_widths = c(10, 2),
                                      checkboxGroupInput("selected_plots", "Included vegetation plots:",
                                                         choices = NULL,
                                                         inline = TRUE),
                                      actionButton("uncheck_all", "Uncheck All", 
                                                   class = "btn-sm",
                                                   style = "margin-top: 25px;")  # adjust margin to align with checkboxes
                                  )
                              )
                              
                          ),
                          plotlyOutput("p_elevTimeSeries")
                      )
                  ),
                  nav_panel(
                      title = "Elevation Transect Profiles",
                      card(
                          full_screen = TRUE,
                          layout_columns(
                              col_widths = c(3, 9),
                              layout_column_wrap(
                                  checkboxInput("show_errorbars_elevProfile", "Show error bars", value = FALSE)),
                          card(
                              layout_columns(
                                  col_widths = c(10, 2),
                                  checkboxGroupInput("selected_years.elev", "Select Year(s):",
                                                     choices = NULL,
                                                     inline = TRUE),
                                  actionButton("uncheck_all_years.elev", "Uncheck All", 
                                               class = "btn-sm",
                                               style = "margin-top: 25px;")
                              )
                          )
                          ),
                          plotlyOutput("p_elevTransectProfile")
                      )
                  )
                  
              ) # end elevation navset_card_tabs
              ) # end sidebar layout
    ), # end elevation nav panel
    
    # Vegetation data panel ----
    nav_panel("Vegetation",
              layout_sidebar(
                  sidebar = sidebar(
                      title = NULL,
                      span(
                          h5("Options for time series and transect profile graphics"),
                          tooltip(
                              bsicons::bs_icon("info-circle"),
                              "The time series and transect profile graphs allow detailed examination of one parameter at one site at a time. Select (and change) either or both here and choices will apply in the tabs for both graphic types.",
                              placement = "right"
                          )
                      ),
                      selectInput("selected_site.veg", "Select Site:", 
                                  choices = NULL),
                      selectInput("selected_column.veg", "Select Column:", 
                                  choices = NULL)
                  ),
              
              navset_card_tab(
                  
                  nav_panel(
                      title = "Vegetation data preview",
                      htmltools::tags$small("This table reflects the input data. The table is interactive. Columns can be sorted by clicking on their name, or filtered by typing into the box below the name."),
                      reactableOutput("dt.veg")
                  ),
                  
                  nav_panel(
                      title = "Vegetation column summary",
                      htmltools::tags$small("This table provides information about each column in the data. The table is interactive. Columns can be sorted by clicking on their name, or filtered by typing into the box below the name."),
                      reactableOutput("dt.veg.skimr")
                  ),
                  
                  nav_panel(
                      title = "Vegetation sampling summary",
                      htmltools::tags$small("This table is interactive. Columns can be sorted by clicking on their name, or filtered by typing into the box below the name. Rows highlighted in orange represent sampling events where no Cover readings were recorded."),
                      reactableOutput("dt.veg_samples")
                  ),
                  
                  nav_panel(
                      title = "Vegetation time series",
                      card(
                          full_screen = TRUE,
                          card(
                              layout_columns(
                                  col_widths = c(10, 2),
                                  checkboxGroupInput("selected_plots.veg", "Included vegetation plots:",
                                                     choices = NULL,
                                                     inline = TRUE),
                                  actionButton("uncheck_all.veg", "Uncheck All", 
                                               class = "btn-sm",
                                               style = "margin-top: 25px;")
                              )
                          ),
                          plotlyOutput("p_vegTimeSeries")
                      )
                  ),
                  nav_panel(
                      title = "Vegetation Transect Profiles",
                      card(
                          full_screen = TRUE,
                          card(
                              layout_columns(
                                  col_widths = c(10, 2),
                                  checkboxGroupInput("selected_years.veg", "Select Year(s):",
                                                     choices = NULL,
                                                     inline = TRUE),
                                  actionButton("uncheck_all_years.veg", "Uncheck All", 
                                               class = "btn-sm",
                                               style = "margin-top: 25px;")
                              )
                          ),
                          plotlyOutput("p_vegTransectProfile")
                      )
                  )
              ) # end navset_tab
              ) # end layout_sidebar
    ), # end nav_panel
    
    # Combined data panel ----
    nav_panel("Combined",
              layout_sidebar(
                  sidebar = sidebar(
                      title = "Choices for Time Series and Transect Profile tabs",
                      selectInput("selected_site.comb", "Select Site:", 
                                  choices = NULL),
                      selectInput("selected_cols.comb", "Select focal columns from veg data:",
                                  choices = NULL,
                                  multiple = TRUE)
                  ),
                  
                  navset_card_tab(
                      
                      nav_panel(
                          title = "Combined Data preview",
                          htmltools::tags$small("This table is interactive. Columns can be sorted by clicking on their name, or filtered by typing into the box below the name."),
                          reactableOutput("dt.comb")
                      ),
                      
                      nav_panel(
                          title = "Combined time series",
                          card(
                              full_screen = TRUE,
                              fill = FALSE,
                              p("Plot selection option is below the graph panels."),
                              
                              plotlyOutput("p_combTimeSeries",
                                           height = "600px"),
                              card(
                                  layout_columns(
                                      col_widths = c(10, 2),
                                      checkboxGroupInput("selected_plots.comb", "Select Plot ID(s):",
                                                         choices = NULL,
                                                         inline = TRUE),
                                      actionButton("uncheck_all.comb", "Uncheck All", 
                                                   class = "btn-sm",
                                                   style = "margin-top: 25px;")
                                  )
                              )
                          )
                      ),
                      nav_panel(
                          title = "Combined Transect Profiles",
                          card(
                              full_screen = TRUE,
                              fill = FALSE,
                              p("Year selection option is below the graph panels."),
                              plotlyOutput("p_combTransectProfile",
                                           height = "600px"),
                              card(
                                  layout_columns(
                                      col_widths = c(10, 2),
                                      checkboxGroupInput("selected_years.comb", "Select Year(s):",
                                                         choices = NULL,
                                                         inline = TRUE),
                                      actionButton("uncheck_all_years.comb", "Uncheck All", 
                                                   class = "btn-sm",
                                                   style = "margin-top: 25px;")
                                  )
                              )
                          )
                      ),
                      nav_panel(
                          title = "Correlation Scatterplots",
                          card(
                              full_screen = TRUE,
                              fill = TRUE,
                              layout_columns(
                                  col_widths = c(4, 4, 4),
                                  
                                  selectInput("corr.comb.x", "Select x-axis variable:",
                                              choices = NULL,
                                              multiple = FALSE),
                                  selectInput("corr.comb.y", "Select y-axis variable:",
                                              choices = NULL,
                                              multiple = FALSE),
                                  actionButton("corr.choices.made", "Use these choices")
                                  
                              ),
                              layout_columns(
                                  col_widths = c(10, 2),
                                  card(plotOutput("p_corr.comb"),
                                       full_screen = TRUE),
                                  list(
                                      p(strong("Correlation Coefficients:")),
                                      textOutput("correlation_text.pear"),
                                      textOutput("correlation_text.spear"),
                                      checkboxInput("add.corr.line",
                                                    "Add line?",
                                                    value = FALSE)
                                  )
                              )
                          )
                      )
                  ) # end navset_tab
              ) # end layout_sidebar
    ), # end nav_panel
    
    
    nav_spacer(),
    nav_item(tags$a(shiny::icon("github"), 
                    "Source Code", 
                    href = "https://github.com/swmpkim/WQBelevations", 
                    target = "_blank")
             )
) # end UI

# Server ----
server <- function(input, output, session){
    
    # data frames ----
    elevs <- reactive({
        req(input$file.elevs)
        df <- read.csv(input$file.elevs$datapath)
        df |>
            mutate(Date.Elevation = lubridate::ymd(paste(Year, Month, Day)),
                   PlotIdFull = paste(SiteID, TransectID, PlotID, sep = "-")) |> 
            select(-Month, -Day)
    })
    
    veg <- reactive({
        req(input$file.veg)
        readxl::read_xlsx(input$file.veg$datapath,
                          sheet = "Cover") |> 
            mutate(PlotIdFull = paste(SiteID, TransectID, PlotID, sep = "-")) |> 
            relocate(PlotIdFull)
    })
    
    veg_samples <- reactive({
        req(veg())
        
        # ID cols
        cols_ID <- c("PlotIdFull", "SiteID", "TransectID", "PlotID",
                     "Year", "Month", "Day", "Total")
        cols_ID_ind <- which(names(veg()) %in% cols_ID)
        
        # columns with veg or abiotic covers recorded
        a <- which(stringr::str_starts(names(veg()), "Density"))
        b <- which(names(veg()) == "Total")
        diff <- min(a[a > b])  # the smallest index of a column starting with "Density" and to the right of "total" (was originally for F_ cols)
        cols_veg <- seq(b + 1, diff - 1)  # all the columns b/t Total and Density_
        cols_veg_names <- names(veg())[cols_veg]  # the names of those columns
        # columns containing "Height", so it can be used by other reserves too
        ht_cols <- names(veg())[stringr::str_detect(names(veg()), "Height")]
        ht_cols <- ht_cols[!(ht_cols %in% c("Orthometric_Height", "Height_Relative_to_MLLW"))]
        
        # tally up readings by type for each sample
        df <- veg() |> 
            rowwise() |> 
            mutate(nSpecies_Cover_measurements = sum(!is.na(c_across(all_of(cols_veg_names)))),
                   nSpecies_Density_measurements = sum(!is.na(c_across(all_of(starts_with("Density"))))),
                   nSpecies_Height_measurements = sum(!is.na(c_across(all_of(ht_cols))))) |>
            ungroup() |> 
            select(all_of(cols_ID), 
                   -Month, -Day, -PlotID, -Total,
                   nSpecies_Cover_measurements, 
                   nSpecies_Density_measurements, 
                   nSpecies_Height_measurements) |> 
            mutate(Cover_completed = case_when(nSpecies_Cover_measurements > 0 ~ TRUE,
                                               .default = FALSE),
                   Density_completed = case_when(nSpecies_Density_measurements > 0 ~ TRUE,
                                                 .default = FALSE),
                   Heights_completed = case_when(nSpecies_Height_measurements > 0 ~ TRUE,
                                                 .default = FALSE),
                   Site.Transect = paste(SiteID, TransectID, sep = "-")) |> 
            select(-SiteID, -TransectID)
    })
    
    comb <- reactive({
        req(veg(), elev_renamed())
        full_join(veg(), elev_renamed(),
                  by = c("Year", "PlotIdFull", "SiteID", "TransectID", "PlotID"))
    })
    
    # more data framing ----
    
    # rename and subset elevations
    elev_renamed <- reactive({
        req(elevs(), input$elevColSel, input$elevAvgSel)
        
        df <- elevs()
        names(df) <- make.names(names(df), unique = TRUE)  # Ensure unique names
        
        # Check for duplicate selections
        if (anyDuplicated(input$elevColSel)) stop("Error: Duplicate columns selected.")
        
        # Check for duplicate column names in the data frame
        if (anyDuplicated(names(df))) {
            stop("Error: Duplicate column names exist in the data frame.")
        }
        
        # Proceed with renaming
        new_names <- paste0("elevation", seq_along(input$elevColSel))

        indices <- which(names(df) %in% input$elevColSel)
        if (length(indices) != length(input$elevColSel)) stop("Error: Column mismatch.")
        names(df)[indices] <- new_names
        
        if(input$elevAvgSel != "none"){
            names(df)[names(df) == input$elevAvgSel] <- "elev_avg_fromCSV"
        }
        
        df <- df |> 
            dplyr::select(Year, 
                          Date.Elevation,
                          PlotIdFull,
                          SiteID, 
                          TransectID, 
                          PlotID,
                          starts_with("elev")) |> 
            dplyr::rowwise() |> 
            dplyr::mutate(elev_mean = round(mean(c_across(starts_with("elevation")), na.rm = TRUE), 4),
                          elev_sd = round(sd(c_across(starts_with("elevation")), na.rm = TRUE), 4),
                          nReadings =  sum(!is.na(c_across(all_of(starts_with("elevation")))))) |> 
            dplyr::ungroup() |> 
            dplyr::relocate(c(nReadings, elev_mean, elev_sd),
                            .after = PlotIdFull)
    })
    
    # pivot elevations
    elev_long <- reactive({
        elev_renamed() |> 
            tidyr::pivot_longer(cols = all_of(starts_with("elevation")),
                                names_to = "rep",
                                values_to = "value")
    })
    
    
    # subset combined data frame to focal species + elevations
    comb_subset <- reactive({
        req(comb(), input$selected_cols.comb)
        comb()  |> 
            dplyr::select(PlotIdFull,
                          SiteID, TransectID, PlotID,
                          Year, 
                          elev_mean, elev_sd,
                          any_of(input$selected_cols.comb))
    })
    
    # pivot combined
    comb_long <- reactive({
        req(comb_subset())
        comb_subset() |> 
            tidyr::pivot_longer(-(PlotIdFull:Year),
                         names_to = "Measurement",
                         values_to = "Value") |> 
            mutate(Measurement = forcats::fct_relevel(Measurement,
                                                      c("elev_mean", "elev_sd"),
                                                      after = Inf))
    })
    
    
    
    # observers ----
    # Observe when the elevs data frame changes and update selection choices
    observe({
        req(elevs())  
        
        # column selections
        nms <- names(elevs())
        # remove the date and site id columns from choice options
        excluded_cols <- c("Year", "Month", "Day", "Date",
                           "SiteID", "TransectID", "PlotID")
        filtered_nms <- setdiff(nms, excluded_cols)
        
        updateSelectInput(session, "elevColSel", choices = filtered_nms)
    })
    
    
    # observer to update elevAgvSel choices based on elevColSel choices
    observeEvent(input$elevColSel, {
        req(elevs()) 
        req(input$elevColSel)
        nms <- names(elevs())
        # remove the date and site id columns from choice options
        excluded_cols <- c("Year", "Month", "Day", "Date",
                           "SiteID", "TransectID", "PlotID")
        filtered_nms <- setdiff(nms, excluded_cols)
        
        # provide choices for avg column that do not include choices selected in colSel.
        avg_choices <- setdiff(filtered_nms, input$elevColSel)
        updateSelectInput(session, "elevAvgSel", 
                          choices = c("none", avg_choices)) #,
                          # selected = if (input$elevAvgSel %in% c("none", avg_choices)) input$elevAvgSel else "none")
    })
    
    # observer for site selection (elevation time series)
    observe({
        req(elev_renamed())
        updateSelectInput(session,
                          "selected_site",
                          choices = unique(elev_renamed()$SiteID))
    })
    
    # observer for plot selection (elevation time series)
    observe({
        req(elev_renamed(), input$selected_site)
        updateCheckboxGroupInput(session,
                          "selected_plots",
                          choices = sort(unique(elev_renamed()$PlotID)),
                          selected = sort(unique(elev_renamed()$PlotID)))
    })
    
    # observer for uncheck all button (elevation time series)
    observeEvent(input$uncheck_all, {
        updateCheckboxGroupInput(session,
                                 "selected_plots",
                                 selected = character(0))  # empty selection
    })
    
    # observer for year selection (elev transect profiles)
    observe({
        req(elev_renamed(), input$selected_site)
        updateCheckboxGroupInput(session,
                                 "selected_years.elev",
                                 choices = sort(unique(elev_renamed()$Year)),
                                 selected = sort(unique(elev_renamed()$Year)))
    })
    
    
    # observer for uncheck all button (elev transect profiles - years)
    observeEvent(input$uncheck_all_years.elev, {
        updateCheckboxGroupInput(session,
                                 "selected_years.elev",
                                 selected = character(0))  # empty selection
    })
    
    
    # observer for site selection (veg time series)
    observe({
        req(veg())
        updateSelectInput(session,
                          "selected_site.veg",
                          choices = unique(veg()$SiteID))
    })
    
    # observer for plot selection (veg time series)
    observe({
        req(veg(), input$selected_site.veg)
        updateCheckboxGroupInput(session,
                                 "selected_plots.veg",
                                 choices = sort(unique(veg()$PlotID)),
                                 selected = sort(unique(veg()$PlotID)))
    })
    
    # observer for uncheck all button (veg time series - plots)
    observeEvent(input$uncheck_all.veg, {
        updateCheckboxGroupInput(session,
                                 "selected_plots.veg",
                                 selected = character(0))  # empty selection
    })
    
    # observer for column selection (veg time series)
    observe({
        req(veg())
        
        # only grab numeric columns
        numeric_cols <- sapply(veg(), is.numeric)
        cols.veg <- names(veg())[numeric_cols]
        
        # exclude any columns that come before the PlotID field (including PlotID)
        col.cutoff <- which(names(veg()) == "PlotID")
        cols.exclude <- names(veg())[1:col.cutoff]
        cols.veg <- cols.veg[!(cols.veg %in% cols.exclude)]
        
        updateSelectInput(session,
                          "selected_column.veg",
                          choices = cols.veg)
    })
    
# observer for year selection (veg transect profiles)
    observe({
        req(veg(), input$selected_site.veg)
        updateCheckboxGroupInput(session,
                                 "selected_years.veg",
                                 choices = sort(unique(veg()$Year)),
                                 selected = sort(unique(veg()$Year)))
    })
    
    
    # observer for uncheck all button (veg transect profiles - years)
    observeEvent(input$uncheck_all_years.veg, {
        updateCheckboxGroupInput(session,
                                 "selected_years.veg",
                                 selected = character(0))  # empty selection
    })
    
    
    # observer for site selection (combined time series)
    observe({
        req(comb())
        updateSelectInput(session,
                          "selected_site.comb",
                          choices = unique(comb()$SiteID))
    })
    
    # observer for plot selection (combined time series)
    observe({
        req(comb(), input$selected_site.comb)
        updateCheckboxGroupInput(session,
                                 "selected_plots.comb",
                                 choices = sort(unique(comb()$PlotID)),
                                 selected = sort(unique(comb()$PlotID)))
    })
    
    # observer for column selection (combined graphics)
    observe({
        req(comb())
        
        # only grab numeric columns
        numeric_cols <- sapply(comb(), is.numeric)
        cols.comb <- names(comb())[numeric_cols]
        
        # for paneled graphs, exclude any that start with 'elev'; these will be included always
        cols.comb <- cols.comb[which(!stringr::str_starts(cols.comb, "elev"))]
        
        # exclude any columns that come before the PlotID field (including PlotID)
        col.cutoff <- which(cols.comb == "PlotID")
        cols.comb <- cols.comb[(col.cutoff + 1):length(cols.comb)]
        
        # add elev_mean and elev_sd back in
        cols.comb.all <- c("elev_mean", "elev_sd", cols.comb)
        
        # use these column names for various selection buttons
        # columns for time series and transect profiles
        updateSelectInput(session,
                          "selected_cols.comb",
                          choices = cols.comb)
        # columns for correlations
        updateSelectInput(session,
                          "corr.comb.x",
                          choices = cols.comb.all)
        updateSelectInput(session,
                          "corr.comb.y",
                          choices = cols.comb.all)
    })
    
    # observer for uncheck all button (combined time series)
    observeEvent(input$uncheck_all.comb, {
        updateCheckboxGroupInput(session,
                                 "selected_plots.comb",
                                 selected = character(0))  # empty selection
    })
    
    # observer for year selection (combined transect profiles)
    observe({
        req(comb(), input$selected_site.comb)
        updateCheckboxGroupInput(session,
                                 "selected_years.comb",
                                 choices = sort(unique(comb()$Year)),
                                 selected = sort(unique(comb()$Year)))
    })
    
    
    # observer for uncheck all button (combined transect profiles - years)
    observeEvent(input$uncheck_all_years.comb, {
        updateCheckboxGroupInput(session,
                                 "selected_years.comb",
                                 selected = character(0))  # empty selection
    })
    
    
    # tables ----
    output$dt.elevs <- renderReactable({
        tmp <- elev_renamed() |> 
            select(Year, PlotIdFull, nReadings, starts_with("elev"))|> 
            mutate(across(starts_with("elev"), function(x) round(x, 4)))
        
        if(input$elevAvgSel != "none"){
            tmp <- tmp |> 
                relocate(elev_avg_fromCSV, .before = elev_mean) 
        }
        
        reactable(tmp,
                  # searchable = TRUE,
                  filterable = TRUE,
                  pagination = FALSE,
                  striped = TRUE,
                  highlight = TRUE,
                  bordered = TRUE,
                  resizable = TRUE,
                  # fullWidth = FALSE,
                  columns = list(
                      Year = colDef(sticky = "left"),
                      PlotIdFull = colDef(sticky = "left")
                  ),
                  defaultColDef = colDef(
                      vAlign = "center", 
                      headerVAlign = "bottom",
                      sortNALast = TRUE)
        )
    })
    
    output$dt.elevs.skimr <- renderReactable({
        tmp <- elev_renamed() |> 
            select(Year, PlotIdFull, nReadings, starts_with("elev"))|> 
            mutate(across(starts_with("elev"), function(x) round(x, 4)))
        
        if(input$elevAvgSel != "none"){
            tmp <- tmp |> 
                relocate(elev_avg_fromCSV, .before = elev_mean) 
        }
        
        tmp.skimr <- skim_without_charts(tmp) |> 
            mutate(across(c(numeric.mean, numeric.sd),
                          function(x) round(x, 4)),
                   complete_rate = round(complete_rate, 3))
        
        reactable(tmp.skimr,
                  # searchable = TRUE,
                  filterable = TRUE,
                  pagination = FALSE,
                  striped = TRUE,
                  highlight = TRUE,
                  bordered = TRUE,
                  resizable = TRUE,
                  # fullWidth = FALSE,
                  columns = list(
                      skim_type = colDef(sticky = "left"),
                      skim_variable = colDef(sticky = "left"),
                      complete_rate = colDef(sticky = "left")
                  ),
                  defaultColDef = colDef(
                      vAlign = "center", 
                      headerVAlign = "bottom",
                      sortNALast = TRUE)
        )
    })
    
    output$dt.veg <- renderReactable({
        reactable(veg(),
                  # searchable = TRUE,
                  filterable = TRUE,
                  pagination = TRUE,
                  striped = TRUE,
                  highlight = TRUE,
                  bordered = TRUE,
                  resizable = TRUE,
                  # fullWidth = FALSE,
                  rowStyle = list(
                      maxHeight = "80px"       
                  ),
                  columns = list(
                      Year = colDef(sticky = "left"),
                      PlotIdFull = colDef(sticky = "left"),
                      Notes = colDef(minWidth = 200,
                                     vAlign = "top"),
                      Unique_ID = colDef(minWidth = 200)
                  ),
                  defaultColDef = colDef(
                      headerStyle = list(
                          maxHeight = "50px",        # Limit the height of the header
                          whiteSpace = "nowrap",     # Prevent wrapping
                          overflow = "hidden",       # Hide overflow
                          textOverflow = "ellipsis"  # Add ellipsis for truncated text
                      ),
                      vAlign = "center", 
                      headerVAlign = "bottom",
                      sortNALast = TRUE
                  )
        )
    })
    
    output$dt.veg.skimr <- renderReactable({
        tmp.skimr <- skim_without_charts(veg()) |> 
            mutate(across(c(starts_with("numeric")),
                          function(x) round(x, 2)),
                   complete_rate = round(complete_rate, 3))
        
        reactable(tmp.skimr,
                  # searchable = TRUE,
                  filterable = TRUE,
                  pagination = FALSE,
                  striped = TRUE,
                  highlight = TRUE,
                  bordered = TRUE,
                  resizable = TRUE,
                  # fullWidth = FALSE,
                  columns = list(
                      skim_type = colDef(sticky = "left"),
                      skim_variable = colDef(sticky = "left"),
                      complete_rate = colDef(sticky = "left")
                  ),
                  defaultColDef = colDef(
                      vAlign = "center", 
                      headerVAlign = "bottom",
                      sortNALast = TRUE)
        )
        
    })
    
    output$dt.veg_samples <- renderReactable({
        reactable(veg_samples(),
                  groupBy = c("Year", "Site.Transect"),
                  # searchable = TRUE,
                  filterable = TRUE,
                  pagination = TRUE,
                  striped = TRUE,
                  highlight = TRUE,
                  bordered = TRUE,
                  resizable = TRUE,
                  # fullWidth = FALSE,
                  columns = list(
                      Year = colDef(sticky = "left",
                                    minWidth = 115),
                      Site.Transect = colDef(minWidth = 115,
                                             sticky = "left"),
                      PlotIdFull = colDef(sticky = "left",
                                          minWidth = 125),
                      nSpecies_Cover_measurements = colDef(aggregate = "sum"),
                      nSpecies_Density_measurements = colDef(aggregate = "sum"),
                      nSpecies_Height_measurements = colDef(aggregate = "sum"),
                      Cover_completed = colDef(aggregate = "frequency"),
                      Density_completed = colDef(aggregate = "frequency"),
                      Heights_completed = colDef(aggregate = "frequency")
                  ),
                  defaultColDef = colDef(
                      vAlign = "center", 
                      headerVAlign = "top",
                      sortNALast = TRUE,
                      headerStyle = list(
                          maxHeight = "80px", 
                          overflow = "hidden"
                      ),
                      style = JS("function(rowInfo) {
       // Initialize the style object
      var style = {};

      // Check if the row is aggregated
      if (rowInfo.aggregated) {
        style.fontWeight = 'bold'; // Bold font for aggregated rows

        // Check if Cover_completed contains 'false' for aggregated rows
        if (rowInfo.row['Cover_completed'] && rowInfo.row['Cover_completed'].toString().includes('false')) {
          style.backgroundColor = '#ffd27f'; // Orange background for aggregated rows containing 'false'
        }
      } else {
        // For non-aggregated rows, check if Cover_completed is exactly false
        if (rowInfo.row['Cover_completed'] === false) {
          style.backgroundColor = '#ffdb99'; // Orange background for non-aggregated rows where Cover_completed is false
        style.fontWeight = 'bold';
        }
      }

      return style;
      }")
                  )
        )
    })
    
    output$dt.comb <- renderReactable({
        reactable(comb_subset(),
                  searchable = TRUE,
                  filterable = TRUE,
                  pagination = TRUE,
                  striped = TRUE,
                  highlight = TRUE,
                  bordered = TRUE,
                  resizable = TRUE,
                  # fullWidth = FALSE,
                  columns = list(
                      Year = colDef(sticky = "left"),
                      PlotIdFull = colDef(sticky = "left")
                  ),
                  defaultColDef = colDef(
                      headerStyle = list(
                          maxHeight = "50px",        # Limit the height of the header
                          whiteSpace = "nowrap",     # Prevent wrapping
                          overflow = "hidden",       # Hide overflow
                          textOverflow = "ellipsis"  # Add ellipsis for truncated text
                      ),
                      vAlign = "center", 
                      headerVAlign = "bottom",
                      sortNALast = TRUE
                  )
        )
    })
    
    
    
    # histograms ----
    output$p_elevMean <- renderPlotly({
        p <- ggplot(elev_renamed(),
                    aes(x = elev_mean)) +
            geom_histogram(bins = 30,
                           fill = "navy",
                           col = "gray") +
            theme_bw() +
            labs(title = "Histogram of mean plot elevation",
                 x = "Mean Elevation (NAVD88)",
                 y = "Count")
        ggplotly(p)
    })
    
    output$p_elevSD <- renderPlotly({
        p <- ggplot(elev_renamed(),
                    aes(x = elev_sd)) +
            geom_histogram(bins = 30,
                           fill = "navy",
                           col = "gray") +
            theme_bw() +
            labs(title = "Histogram of stdev by plot",
                 x = "Standard Deviation of elevation readings",
                 y = "Count")
        ggplotly(p)
    })
    
    output$p_elevAll <- renderPlotly({
        p <- ggplot(elev_long(),
                    aes(x = value)) +
            geom_histogram(bins = 30,
                           fill = "navy",
                           col = "gray") +
            theme_bw()+
            labs(title = "Histogram of all elevation readings",
                 x = "Elevation (NAVD88)",
                 y = "Count")
        ggplotly(p)
    })
    
    # time series ----
    output$p_elevTimeSeries <- renderPlotly({
        req(elev_renamed(), input$selected_site, input$selected_plots)
        cols <- khroma::color("batlow")(length(unique(elev_renamed()$PlotID)))
        names(cols) <- sort(unique(elev_renamed()$PlotID))
        p <- elev_renamed() |> 
            filter(SiteID == input$selected_site,
                   PlotID %in% input$selected_plots) |> 
            ggplot(
                aes(x = Year,
                    y = elev_mean,
                    col = as.factor(PlotID),
                    group = PlotID)) +
            geom_line() +
            geom_point(size = 1.5) +
            scale_color_manual(values = cols) +
            facet_wrap(~TransectID) +
            labs(y = "Mean Elevation (NAVD88) +/- 1 SD",
                 col = "Plot ID") +
            theme_bw() +
            theme(legend.position = "bottom")
        
        # Add error bars if checkbox is checked
        if(input$show_errorbars) {
            p <- p + geom_linerange(aes(ymin = elev_mean - elev_sd,
                                        ymax = elev_mean + elev_sd))
        }
        
        ggplotly(p)
    })
    
    # veg time series
    output$p_vegTimeSeries <- renderPlotly({
        req(veg(), input$selected_site.veg, input$selected_plots.veg, 
            input$selected_column.veg)
        cols <- khroma::color("batlow")(length(unique(veg()$PlotID)))
        names(cols) <- sort(unique(veg()$PlotID))
        tmp <- veg() |> 
            filter(SiteID == input$selected_site.veg,
                   PlotID %in% input$selected_plots.veg) |> 
            rename("Selected" = input$selected_column.veg)
        p <- ggplot(tmp,
                aes(x = Year,
                    y = Selected,
                    col = as.factor(PlotID),
                    group = PlotID)) +
            geom_line() +
            geom_point(size = 1.5) +
            scale_color_manual(values = cols) +
            facet_wrap(~TransectID) +
            labs(y = input$selected_column.veg,
                 col = "Plot ID") +
            theme_bw() +
            theme(legend.position = "bottom")
        
        ggplotly(p)
    })
    
    # combined elev + veg time series
    output$p_combTimeSeries <- renderPlotly({
        req(comb_long(), input$selected_site.comb, input$selected_plots.comb)
        cols <- khroma::color("batlow")(length(unique(comb_long()$PlotID)))
        names(cols) <- sort(unique(comb_long()$PlotID))
        p <- comb_long() |> 
            filter(SiteID == input$selected_site.comb,
                   PlotID %in% input$selected_plots.comb) |> 
            ggplot(
                aes(x = Year,
                    y = Value,
                    col = as.factor(PlotID),
                    group = PlotID)) +
            geom_line() +
            geom_point(size = 1.5) +
            scale_color_manual(values = cols) +
            facet_grid(Measurement ~ TransectID, scales = "free_y") +
            labs(col = "Plot ID") +
            theme_bw() +
            theme(legend.position = "bottom")
        
        ggplotly(p)
    })
    
    
    # transect profiles ----
    # elevation
    output$p_elevTransectProfile <- renderPlotly({
        req(elev_renamed(), input$selected_site, input$selected_years.elev)
        
        min_year <- min(elev_renamed()$Year, na.rm = TRUE)
        max_year <- max(elev_renamed()$Year, na.rm = TRUE)
        mid_year <- mean(c(min_year, max_year))
        
        tmp <- elev_renamed() |> 
            filter(SiteID == input$selected_site,
                   Year %in% input$selected_years.elev)
        
        p <- ggplot(tmp,
                    aes(x = PlotID, y = elev_mean,
                        group = Year,
                        col = Year,
                        fill = Year)) +
            geom_point(size = 2,
                       col = "gray30",
                       shape = 21) +
            geom_line() +
            facet_grid(TransectID ~ SiteID, scales = "free_x") +
            theme_bw() +
            theme(panel.grid.major = element_line(linetype = "dashed"),
                  panel.grid.minor = element_line(linetype = "blank")) + 
            scale_color_nightfall(reverse = TRUE,
                                  limits = c(min_year, max_year),
                                  midpoint = mid_year) +
            scale_fill_nightfall(reverse = TRUE,
                                 limits = c(min_year, max_year),
                                 midpoint = mid_year)
        
        # Add error bars if checkbox is checked
        if(input$show_errorbars_elevProfile) {
            p <- p + geom_linerange(aes(ymin = elev_mean - elev_sd,
                                        ymax = elev_mean + elev_sd))
        }
        
        ggplotly(p)
    })
    
    # veg
    output$p_vegTransectProfile <- renderPlotly({
        req(veg(), input$selected_site.veg, input$selected_years.veg)
        
        min_year <- min(veg()$Year, na.rm = TRUE)
        max_year <- max(veg()$Year, na.rm = TRUE)
        mid_year <- mean(c(min_year, max_year))
        
        tmp <- veg() |> 
            filter(SiteID == input$selected_site.veg,
                   Year %in% input$selected_years.veg) |> 
            rename("Selected" = input$selected_column.veg)
        p <- ggplot(tmp,
                    aes(x = PlotID, y = Selected,
                        group = Year,
                        col = Year,
                        fill = Year)) +
            geom_point(size = 2,
                       col = "gray30",
                       shape = 21) +
            geom_line() +
            facet_grid(TransectID ~ SiteID, scales = "free_x") +
            theme_bw() +
            theme(panel.grid.major = element_line(linetype = "dashed"),
                  panel.grid.minor = element_line(linetype = "blank")) + 
            scale_color_nightfall(reverse = TRUE,
                                  limits = c(min_year, max_year),
                                  midpoint = mid_year) +
            scale_fill_nightfall(reverse = TRUE,
                                 limits = c(min_year, max_year),
                                 midpoint = mid_year) +
            labs(y = input$selected_column.veg)
        
        
        ggplotly(p)
    })
    
    
    # combined
    # NEEDS SOME LOVE - can probably do interpolating elsewhere
    output$p_combTransectProfile <- renderPlotly({
        req(comb_subset(),
            input$selected_site.comb, input$selected_years.comb,
            input$selected_cols.comb)
        
        min_year <- min(comb_subset()$Year, na.rm = TRUE)
        max_year <- max(comb_subset()$Year, na.rm = TRUE)
        mid_year <- mean(c(min_year, max_year))
        
        tmp <- comb_subset() |> 
            arrange(PlotIdFull, Year) |> 
            group_by(PlotIdFull) |> 
            complete(Year = full_seq(Year, 1)) |> 
            tidyr::separate(PlotIdFull, into = c("Site2", "Transect2", "Plot2"),
                            sep = "-", remove = FALSE) |> 
            mutate(elev_interp = zoo::na.approx(elev_mean, Year, na.rm = FALSE),
                   SiteID = case_when(is.na(SiteID) ~ Site2,
                                      .default = SiteID),
                   TransectID = case_when(is.na(TransectID) ~ Transect2,
                                          .default = TransectID),
                   PlotID = case_when(is.na(PlotID) ~ as.numeric(Plot2),
                                      .default = PlotID)) |> 
            tidyr::fill(elev_interp, .direction = "down") |> 
            select(-Site2, -Transect2, -Plot2) |> 
            filter(Year %in% input$selected_years.comb,
                   SiteID  == input$selected_site.comb)
        
        tmp2 <- tmp |> 
            pivot_longer(cols = any_of(input$selected_cols.comb),
                         names_to = "Species",
                         values_to = "Cover")
        
        p <- ggplot(tmp,
                    aes(x = PlotID, y = elev_mean,
                        group = Year,
                        col = Year,
                        fill = Year)) +
            geom_point(size = 0.2,
                       shape = 19) +
            geom_line(alpha = 0.6) +
            geom_point(data = tmp2,
                       aes(size = Cover,
                           shape = Species,
                           y = elev_interp),
                       alpha = 0.8) +
            geom_line(data = tmp2,
                       aes(y = elev_interp),
                      linetype = "dotted",
                       alpha = 0.6) +
            facet_grid(TransectID ~ SiteID, scales = "free_x") +
            theme_bw() +
            theme(panel.grid.major = element_line(linetype = "dashed"),
                  panel.grid.minor = element_line(linetype = "blank")) + 
            scale_color_nightfall(reverse = TRUE,
                                  limits = c(min_year, max_year),
                                  midpoint = mid_year) +
            scale_fill_nightfall(reverse = TRUE,
                                 limits = c(min_year, max_year),
                                 midpoint = mid_year) +
            scale_shape_manual(values = c(0, 2, 6, 1, 3, 4, 5)) + 
            labs(y = "mean plot elevation (dashed line = interpolated)",
                 shape = "Species or cover",
                 size = "Cover")
        
        ggplotly(p)
    })
    
    # Correlations ----
    # make strings when input button is clicked
    corr.comb.x <- eventReactive(input$corr.choices.made, {
        x <- input$corr.comb.x
        x
    })
    
    corr.comb.y <- eventReactive(input$corr.choices.made, {
        y <- input$corr.comb.y
        y
    })
    
    # generate plot
    output$p_corr.comb <- renderPlot({
            req(comb(), corr.comb.x(), corr.comb.y())
            
            p <- ggplot(comb(),
                        aes(x = .data[[corr.comb.x()]],
                            y = .data[[corr.comb.y()]])) +
                geom_miss_point(aes(shape = SiteID),
                                size = 3) +
                scale_shape_manual(values = c(0, 2, 6, 1, 3, 4, 5)) +
                scale_color_brewer(palette = "Set1") +
                theme_bw() 
            
            if(input$add.corr.line == TRUE){
                p <- p +
                    geom_smooth(method = "lm",
                                se = FALSE,
                                na.rm = TRUE,
                                col = "gray20",
                                linetype = "dashed")
            }
            
            p
            
        })
        
        # correlation coefficients
        output$correlation_text.pear <- renderText({
            req(comb(), corr.comb.x(), corr.comb.y())
            
            corr.pear <- cor(comb()[[corr.comb.x()]], comb()[[corr.comb.y()]],
                             use = "pairwise.complete.obs",
                             method = "pearson")
            
            paste0("Pearson: ", round(corr.pear, 2))
            
        })
        
        output$correlation_text.spear <- renderText({
            req(comb(), corr.comb.x(), corr.comb.y())
            
            corr.spear <- cor(comb()[[corr.comb.x()]], comb()[[corr.comb.y()]],
                              use = "pairwise.complete.obs",
                              method = "spearman")
            
            paste0("Spearman: ", round(corr.spear, 2))
            
        })

}  # end server function

# Run App ----
shinyApp(ui, server)