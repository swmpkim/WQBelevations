# this app is published at catbirdstats.shinyapps.io/vegData_exploration


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

# description text ----
# store as variables so the wording can be used in multiple places  
data_preview_desc1 <- "View, sort, filter, and search the raw data in the 'Cover' worksheet of your file. This table is laid out exactly the same as your original spreadsheet." 
data_preview_desc2 <- HTML(
    "<p>Columns can be sorted by clicking on their name, or filtered by typing into the box below the name.</p>
    <p>You probably only need this table if you see anything unexpected in your data via the other tables and graphs. Search for the values here without having to return to your original spreadsheet. Any data updates however will need to be made in the original spreadsheet.</p>"
)

column_summary_desc1 <- "This table shows you how R 'sees' your data. This table is good to look through to make sure values in your columns align with your expectations (e.g. you do not have any vegetation cover values of 500)."
column_summary_desc2 <- HTML(
    "<p>The table contains one row for each column of the data. It shows you what each column type is and summarizes the values in the column. Note, empty columns are typically seen as 'logical' (true or false). Every column type displays information about how many cells are full and empty, and what the completeness rate is (number of non-empty cells divided by number of rows).</p>
    <p>For character columns, you see how many unique entries exist. For numeric columns, you see numeric summaries like the min, mean, median, and max.</p>"
)

sampling_summary_desc1 <- "This table provides a summary of sampling events and flags any vegetation plot-date combinations where there is no vegetation cover recorded."
sampling_summary_desc2 <- HTML(
    "<p>For each vegetation plot on each date, true or false is assigned to denote whether each of cover, height, and density were collected. If cover has a 'false' value, the row is orange to draw your attention.</p>
    <p>Rows are initially shown at only the site/date level, and can be expanded all the way down to vegetation plot level so you can find which row is causing the flagging. Any issues you find need to be addressed in the data file."
)

time_series_desc1 <- "See how a variable changes over time at a site. In the sidebar, choose your site and any numeric variable from your file."
time_series_desc2 <- HTML(
    "<p>x-axis: date</p>
    <p>y-axis: the selected variable's value</p>
    <p>points: one for each vegetation plot on each date, showing the variable's value</p>
    <p>lines: one for each vegetation plot, showing the variable through time</p>
    <p>panels: each panel represents a transect, and contains all vegetation plots in that transect</p>
    <p>selections: vegetation plots can be removed and added using the checkboxes, if you want to focus on one or a few.</p>"
)

transect_profiles_desc1 <- "See how a variable changes along a cross-section of your transect. In the sidebar, choose your site and any numeric variable from your file."
transect_profiles_desc2 <- HTML(
    "<p>x-axis: vegetation plot (numerically ordered; presumably either water-to-upland or vice versa)</p>
    <p>y-axis: the selected variable's value</p>
    <p>points: one for each vegetation plot on each date, showing the variable's value</p>
    <p>lines: one for each year</p>
    <p>panels: each panel represents a transect, and contains all vegetation plots in that transect</p>
    <p>selections: years can be removed and added using the checkboxes, if you want to focus on one or a few.</p>"
)

correlation_scatterplots_desc1 <- "Explore relationships between variables, across all sites. This graph only updates when you click the 'Use these choices' button. This is the only graph that is not interactive."
correlation_scatterplots_desc2 <- HTML(
    "<p>You choose the variables to display on each axis.</p>
    <p>points: one for each vegetation plot on each date</p>
    <p>shape: represents site - are there differences in the relationship between sites?</p>
    <p>color: represents missing vs. non-missing values. If a 'missing' colored point is near the origin, it is missing for both variables. If a value is missing for only one of the two variables, it will be near 0 for the variable that is missing but at the appropriate value for the axis where a variable is present. e.g., if a missing value is placed at 80 along the x-axis, and is near the axis, the y-variable was not measured (and is presumably 0, unless it was truly missing data).</p>
    <p>line: if selected, a linear regression line is added to the graph.</p>"
)

table_interactivity_desc <- "This table is interactive. Columns can be sorted by clicking on their name or filtered by typing into the box below the name."



# UI ----
ui <- page_navbar(
    title = "Veg Data Exploration",
    theme = bs_theme(version = 5),
    # bg = "#0062cc",
    bg = "#477FB0",
    inverse = TRUE,
    underline = TRUE,

    
    sidebar = sidebar(
        title = NULL,
        # veg data file
        fileInput("file.veg", 
                  span(
                      h5("Upload vegetation file"), 
                      tooltip(
                          bsicons::bs_icon("info-circle"),
                          "The file must be an Excel file in the Namaste project format.",
                          placement = "right"
                      )
                  ), 
                  multiple = FALSE,
                  accept = ".xlsx"),
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
    
    # About panel ----
    nav_panel("About",
              card(
                  card_header("About this app"),
                  p("This application allows users to explore vegetation data when it is in the format provided by the Namaste project."),
                  p("Most pieces of this app are interactive. Tables can be searched and sorted; graphs can have features added or removed; and hovering over a point on a graph will show that point's values."),
                  p("Information icons (", bsicons::bs_icon("info-circle"), ") are throughout the app to provide more details about specific features. In sidebars you generally hover to see the information, and in the main sections of content you need to click on them."),
                  h4("How to use this app:"),
                  tags$ol(
                      tags$li(strong("Upload your vegetation data file"), "using the sidebar. This information will not be retained by the app once you close the session."),
                      tags$li(
                          span(strong("See tabular summaries"), " of your data by selecting 'Tables' from the navigation bar at the top of the app."),
                          tags$ol(
                              style = "list-style-type: lower-alpha; margin-top: 8px;",
                              tags$li(em(strong("Data preview:")), " ", data_preview_desc1),
                              tags$li(em(strong("Column summary:")), " ", column_summary_desc1),
                              tags$li(em(strong("Sampling summary:")), " ", sampling_summary_desc1)
                          )
                      ),
                      tags$li(
                          span(strong("Explore graphs"), " of your data by selecting 'Graphs' from the navigation bar at the top of the app."),
                          tags$ol(
                              style = "list-style-type: lower-alpha; margin-top: 8px;",
                              tags$li(em(strong("Time series:")), " ", time_series_desc1),
                              tags$li(em(strong("Transect Profiles:")), " ", transect_profiles_desc1),
                              tags$li(em(strong("Correlation Scatterplots:")), " ", correlation_scatterplots_desc1)
                          )
                      )
                      
                  ),
                  hr(),
                  p("This app was developed in support of the National Estuarine Research Reserve System, a partnership between the National Oceanic and Atmospheric Administration and coastal states. However, it is not an official NERRS or CDMO tool, and is available as a courtesy."),
                  p("Funding was provided by the NERRS Science Collaborative under the Namaste project. For more information on Namaste, see our ", tags$a("Marsh Response to Sea Level Rise", href = "https://www.nerra.org/science-tools/marsh-response-to-sea-level-rise/", target = "_blank"), "page. For more information on the NERRS Science Collaborative, see ", tags$a("the Science Collaborative", href = "https://https://nerrssciencecollaborative.org/", target = "_blank"), " page."),
                  p("Developed by ", tags$a("Catbird Stats, LLC", href = "https://www.catbirdstats.com", target = "_blank"), ". For questions about this app, please contact ", tags$a("kim@catbirdstats.com", href = "mailto:kim@catbirdstats.com"), ".")
              )
    ),
    
    # Vegetation data panel ----
    nav_panel("Tables",
               navset_card_tab(
                  
                  nav_panel(
                      title = "Data preview",
                      htmltools::tags$small(
                          data_preview_desc1,
                          table_interactivity_desc,
                          # "This table reflects the input data. The table is interactive. Columns can be sorted by clicking on their name, or filtered by typing into the box below the name.",
                          actionLink("data_preview_info", bsicons::bs_icon("info-circle"))
                          ),
                      reactableOutput("dt.veg")
                  ),
                  
                  nav_panel(
                      title = "Column summary",
                      htmltools::tags$small(
                          column_summary_desc1,
                          table_interactivity_desc,
                          # "This table provides information about each column in the data. The table is interactive. Columns can be sorted by clicking on their name, or filtered by typing into the box below the name.",
                          actionLink("column_summary_info", bsicons::bs_icon("info-circle"))
                          ),
                      reactableOutput("dt.veg.skimr")
                  ),
                  
                  nav_panel(
                      title = "Sampling summary",
                      htmltools::tags$small(
                          sampling_summary_desc1,
                          table_interactivity_desc,
                          # "This table is interactive. Columns can be sorted by clicking on their name, or filtered by typing into the box below the name. Rows highlighted in orange represent sampling events where no Cover readings were recorded.",
                          actionLink("sampling_summary_info", bsicons::bs_icon("info-circle"))
                          ),
                      reactableOutput("dt.veg_samples")
                  )
                  
               ) # end tabbed panel
    ), # end Tables panel
    
    # Graphs panel ----
    nav_panel("Graphs",
              navset_card_tab(
                  
                  nav_panel(
                      title = "Time series",
                      card(
                          htmltools::tags$small(
                              time_series_desc1,
                              actionLink("time_series_info", bsicons::bs_icon("info-circle"))
                          ),
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
                      title = "Transect Profiles",
                      card(
                          htmltools::tags$small(
                              transect_profiles_desc1,
                              actionLink("transect_profiles_info", bsicons::bs_icon("info-circle"))
                          ),
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
                          htmltools::tags$small(
                              correlation_scatterplots_desc1,
                              actionLink("correlation_scatterplots_info", bsicons::bs_icon("info-circle"))
                          ),
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
    ) # end nav_panel
    

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
        
        # Modals ----
        # for detailed descriptions of everything to be pop-ups
        observeEvent(input$data_preview_info, {
            showModal(modalDialog(
                title = "Data Preview Table",
                data_preview_desc2,
                footer = modalButton("Close"),
                easyClose = TRUE,
                size = "m"
            ))
        })
        
        observeEvent(input$column_summary_info, {
            showModal(modalDialog(
                title = "Column Summary Table",
                column_summary_desc2,
                footer = modalButton("Close"),
                easyClose = TRUE,
                size = "m"
            ))
        })
        
        observeEvent(input$sampling_summary_info, {
            showModal(modalDialog(
                title = "Sampling Summary Table",
                sampling_summary_desc2,
                footer = modalButton("Close"),
                easyClose = TRUE,
                size = "m"
            ))
        })
        
        observeEvent(input$time_series_info, {
            showModal(modalDialog(
                title = "Time Series Graph",
                time_series_desc2,
                footer = modalButton("Close"),
                easyClose = TRUE,
                size = "m"
            ))
        })
        
        observeEvent(input$transect_profiles_info, {
            showModal(modalDialog(
                title = "Transect Profile Graph",
                transect_profiles_desc2,
                footer = modalButton("Close"),
                easyClose = TRUE,
                size = "m"
            ))
        })
        
        observeEvent(input$correlation_scatterplots_info, {
            showModal(modalDialog(
                title = "Correlation Scatterplots",
                correlation_scatterplots_desc2,
                footer = modalButton("Close"),
                easyClose = TRUE,
                size = "m"
            ))
        })

}  # end server function

# Run App ----
shinyApp(ui, server)