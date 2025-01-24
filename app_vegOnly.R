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
ui <- page_sidebar(
    title = "Veg Data Exploration App",
    theme = bs_theme(version = 5),
    underline = TRUE,
    
    
    sidebar = sidebar(
        title = "Choices",
        # veg data file
        fileInput("file.veg", "Which file has vegetation data?", 
                  multiple = FALSE,
                  accept = ".xlsx"),
        h5("For Time Series and Transect Profiles:"),
        selectInput("selected_site.veg", "Select Site:", 
                    choices = NULL),
        selectInput("selected_column.veg", "Select Column:", 
                    choices = NULL)
    ),
    
    # Vegetation data panel ----
    # nav_panel("Vegetation",
               navset_card_tab(
                  
                  nav_panel(
                      title = "Vegetation Data preview",
                      htmltools::tags$small("This table is interactive. Columns can be sorted by clicking on their name, or filtered by typing into the box below the name."),
                      reactableOutput("dt.veg")
                  ),
                  
                  nav_panel(
                      title = "Vegetation Data summary",
                      htmltools::tags$small("This table is interactive. Columns can be sorted by clicking on their name, or filtered by typing into the box below the name. Rows highlighted in orange represent sampling events where no Cover readings were recorded."),
                      reactableOutput("dt.veg.skimr")
                  ),
                  
                  nav_panel(
                      title = "Vegetation Sampling summary",
                      htmltools::tags$small("This table is interactive. Columns can be sorted by clicking on their name, or filtered by typing into the box below the name."),
                      reactableOutput("dt.veg_samples")
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
    # ), # end nav_panel
    

) # end UI

# Server ----
server <- function(input, output, session){
    
    # data frames ----
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
    
    
    # observers ----
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
        
        # columns for correlations
        updateSelectInput(session,
                          "corr.comb.x",
                          choices = cols.veg)
        updateSelectInput(session,
                          "corr.comb.y",
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
    
    
    # tables ----
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
    
     # time series ----
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
    
    # transect profiles ----
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
            req(veg(), corr.comb.x(), corr.comb.y())
            
            p <- ggplot(veg(),
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
            req(veg(), corr.comb.x(), corr.comb.y())
            
            corr.pear <- cor(veg()[[corr.comb.x()]], veg()[[corr.comb.y()]],
                             use = "pairwise.complete.obs",
                             method = "pearson")
            
            paste0("Pearson: ", round(corr.pear, 2))
            
        })
        
        output$correlation_text.spear <- renderText({
            req(veg(), corr.comb.x(), corr.comb.y())
            
            corr.spear <- cor(veg()[[corr.comb.x()]], veg()[[corr.comb.y()]],
                              use = "pairwise.complete.obs",
                              method = "spearman")
            
            paste0("Spearman: ", round(corr.spear, 2))
            
        })

}  # end server function

# Run App ----
shinyApp(ui, server)