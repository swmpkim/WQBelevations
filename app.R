library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(khroma)
library(plotly)
library(DT)

# to-dos
# make table of # plots per transect by sample year
# number of missing observations
# in the data preview/renamed df - make column for # readings in the plot that day

# UI ----
ui <- page_navbar(
    title = "WQB Elevations App",
    theme = bs_theme(version = 5),
    bg = "#0062cc",
    underline = TRUE,
    
    # Sidebar - read in data files and control options
    sidebar = sidebar(
        title = "File Inputs",
        # elevation file
        fileInput("file.elevs", "Which file has elevation data?", 
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
        fileInput("file.veg", "Which file has vegetation data?", 
                  multiple = FALSE,
                  accept = ".xlsx")
    ),  # end sidebar
    
    # Elevation data panel ----
    nav_panel("Elevations",
              layout_sidebar(
                  sidebar = sidebar(
                      title = "Choices for Time Series and Transect Profile tabs",
                      selectInput("selected_site", "Select Site:", 
                                  choices = NULL)
                  ),
              navset_tab(
                  nav_panel(
                      title = "Elevation data preview",
                      p("These tables are interactive. It is a good idea to sort by various columns to make sure you don't have any anomalous values."),
                      DTOutput("dt.elevs", height = "450px")
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
                                      checkboxGroupInput("selected_plots", "Select Plot ID(s):",
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
                      title = "Choices for Time Series and Transect Profile tabs",
                      selectInput("selected_site.veg", "Select Site:", 
                                  choices = NULL),
                      selectInput("selected_column.veg", "Select Column:", 
                                  choices = NULL)
                  ),
              
              navset_tab(
                  
                  nav_panel(
                      title = "Vegetation Data preview",
                      p("These tables are interactive. It is a good idea to sort by various columns to make sure you don't have any anomalous values."),
                      card(
                          DTOutput("dt.veg")
                      )
                  ),
                  nav_panel(
                      title = "Vegetation time series",
                      card(
                          full_screen = TRUE,
                          card(
                              layout_columns(
                                  col_widths = c(10, 2),
                                  checkboxGroupInput("selected_plots.veg", "Select Plot ID(s):",
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
                  
                  navset_tab(
                      
                      nav_panel(
                          title = "Combined Data preview",
                          p("These tables are interactive. It is a good idea to sort by various columns to make sure you don't have any anomalous values."),
                          card(
                              DTOutput("dt.comb")
                          )
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
            mutate(Date = lubridate::ymd(paste(Year, Month, Day)),
                   PlotIdFull = paste(SiteID, TransectID, PlotID, sep = "-"))
    })
    
    veg <- reactive({
        req(input$file.veg)
        readxl::read_xlsx(input$file.veg$datapath,
                          sheet = "Cover")
    })
    
    comb <- reactive({
        req(input$file.elevs, input$file.veg, elev_renamed())
        veg2 <- veg() |> 
            mutate(PlotIdFull = paste(SiteID, TransectID, PlotID, sep = "-")) |> 
            relocate(PlotIdFull) 
        full_join(veg2, elev_renamed(),
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
        # print("New names:")
        # print(new_names)
        indices <- which(names(df) %in% input$elevColSel)
        if (length(indices) != length(input$elevColSel)) stop("Error: Column mismatch.")
        names(df)[indices] <- new_names
        # print("Updated column names:")
        # print(names(df))
        # 
        
        if(input$elevAvgSel != "none"){
            names(df)[names(df) == input$elevAvgSel] <- "elev_avg_fromCSV"
        }
        
        df <- df |> 
            dplyr::select(Year, Month, Day,
                          Date,
                          PlotIdFull,
                          SiteID, 
                          TransectID, 
                          PlotID,
                          starts_with("elev")) |> 
            dplyr::rowwise() |> 
            dplyr::mutate(elev_mean = mean(c_across(starts_with("elevation")), na.rm = TRUE),
                          elev_sd = sd(c_across(starts_with("elevation")), na.rm = TRUE)) |> 
            dplyr::ungroup() |> 
            dplyr::relocate(c(elev_mean, elev_sd),
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
        
        # exclude any that start with 'elev'; these will be included always
        cols.comb <- cols.comb[which(!stringr::str_starts(cols.comb, "elev"))]
        
        # exclude any columns that come before the PlotID field (including PlotID)
        col.cutoff <- which(cols.comb == "PlotID")
        cols.comb <- cols.comb[(col.cutoff + 1):length(cols.comb)]
        
        updateSelectInput(session,
                          "selected_cols.comb",
                          choices = cols.comb)
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
    output$dt.elevs <- renderDT({
        tmp <- elev_renamed() |> 
            select(Year, PlotIdFull, starts_with("elev"))
        if(input$elevAvgSel != "none"){
            tmp <- tmp |> 
                relocate(elev_avg_fromCSV, .before = elev_mean)
        }
        DT::datatable(tmp,
                      options = list(
                          scrollX = TRUE,      # Enable horizontal scrolling
                          scrollY = "350px",   # Enable vertical scrolling with fixed height
                          # pageLength = 15,     # Number of rows per page
                          pageLength = nrow(tmp),
                          dom = "frti"        # Control which elements appear (f=filter, r=processing, t=table, i=info, p=pagination)
                      )) |> 
            DT::formatRound(columns = names(select(tmp, starts_with("elev"))),
                            digits = 4)
    })
    
    output$dt.veg <- renderDT({
        DT::datatable(veg())
    })
    
    output$dt.comb <- renderDT({
        DT::datatable(comb_subset())
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
                                  midpoint = mean(elev_renamed()$Year, na.rm = TRUE)) +
            scale_fill_nightfall(reverse = TRUE,
                                 midpoint = mean(elev_renamed()$Year, na.rm = TRUE))
        
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
                                  midpoint = mean(veg()$Year, na.rm = TRUE)) +
            scale_fill_nightfall(reverse = TRUE,
                                 midpoint = mean(veg()$Year, na.rm = TRUE)) +
            labs(y = paste0(input$selected_column.veg, " Cover"))
        
        
        ggplotly(p)
    })
    
    
    # combined
    # NEEDS SOME LOVE - can probably do interpolating elsewhere
    output$p_combTransectProfile <- renderPlotly({
        req(comb_subset(),
            input$selected_site.comb, input$selected_years.comb,
            input$selected_cols.comb)
        
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
            geom_point(size = 0.7,
                       col = "gray30",
                       shape = 21) +
            geom_line() +
            geom_point(data = tmp2,
                       aes(size = Cover,
                           shape = Species,
                           y = elev_interp),
                       alpha = 0.6) +
            geom_line(data = tmp2,
                       aes(y = elev_interp),
                      linetype = "dashed",
                       alpha = 0.6) +
            facet_grid(TransectID ~ SiteID, scales = "free_x") +
            theme_bw() +
            theme(panel.grid.major = element_line(linetype = "dashed"),
                  panel.grid.minor = element_line(linetype = "blank")) + 
            scale_color_nightfall(reverse = TRUE,
                                  midpoint = mean(comb_subset()$Year, na.rm = TRUE)) +
            scale_fill_nightfall(reverse = TRUE,
                                 midpoint = mean(comb_subset()$Year, na.rm = TRUE)) +
            labs(y = "mean plot elevation (may be interpolated)",
                 shape = "Species or cover")
        
        ggplotly(p)
    })
    
}

# Run App ----
shinyApp(ui, server)