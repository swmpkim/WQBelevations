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
              
              navset_card_tab(
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
                                  selectInput("selected_site", "Select Site:", 
                                              choices = NULL),
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
                      title = "Transect Profiles",
                      card(
                          full_screen = TRUE,
                          plotlyOutput("p_elevTransectProfile")
                      )
                  )
              ) # end elevation navset_card_tabs
    ), # end elevation nav panel
    
    # Vegetation data panel ----
    nav_panel("Vegetation",
              navset_card_tab(
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
                          layout_columns(
                              col_widths = c(3, 9),
                              layout_column_wrap(
                                  selectInput("selected_site.veg", "Select Site:", 
                                              choices = NULL),
                                  selectInput("selected_column.veg", "Select Column:", 
                                              choices = NULL)
                                  ),
                              card(
                                  layout_columns(
                                      col_widths = c(10, 2),
                                      checkboxGroupInput("selected_plots.veg", "Select Plot ID(s):",
                                                         choices = NULL,
                                                         inline = TRUE),
                                      actionButton("uncheck_all.veg", "Uncheck All", 
                                                   class = "btn-sm",
                                                   style = "margin-top: 25px;")  # adjust margin to align with checkboxes
                                  )
                              )
                              
                          ),
                          plotlyOutput("p_vegTimeSeries")
                      )
                  ),
                  nav_panel(
                      title = "Vegetation Transect Profiles",
                      card(
                          full_screen = TRUE,
                          plotlyOutput("p_vegTransectProfile")
                      )
                  )
              ) # end veg navset_card_tabs
    ), # end veg nav panel
    
    
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
    
    # observer for uncheck all button (veg time series)
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
        
        updateSelectInput(session,
                          "selected_column.veg",
                          choices = cols.veg)
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
    
    
    # transect profile ----
    output$p_elevTransectProfile <- renderPlotly({
        p <- ggplot(elev_renamed(),
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
        
        
        ggplotly(p)
    })
    
    output$p_vegTransectProfile <- renderPlotly({
        tmp <- veg() |> 
            filter(SiteID == input$selected_site.veg,
                   PlotID %in% input$selected_plots.veg) |> 
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
    
}

# Run App ----
shinyApp(ui, server)