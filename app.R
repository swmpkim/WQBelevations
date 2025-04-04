library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(naniar)
library(khroma)
library(plotly)
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
    "<p><strong>x-axis:</strong> date</p>
    <p><strong>y-axis:</strong> the selected variable's value</p>
    <p><strong>points:</strong> one for each vegetation plot on each date, showing the variable's value</p>
    <p><strong>lines:</strong> one for each vegetation plot, showing the variable through time</p>
    <p><strong>panels:</strong> each panel represents a transect, and contains all vegetation plots in that transect</p>
    <p><strong>selections:</strong> vegetation plots can be removed and added using the checkboxes, if you want to focus on one or a few.</p>"
)

transect_profiles_desc1 <- "See how a variable changes along a cross-section of your transect. In the sidebar, choose your site and any numeric variable from your file."
transect_profiles_desc2 <- HTML(
    "<p><strong>x-axis:</strong> vegetation plot (numerically ordered; presumably either water-to-upland or vice versa)</p>
    <p><strong>y-axis:</strong> the selected variable's value</p>
    <p><strong>points:</strong> one for each vegetation plot on each date, showing the variable's value</p>
    <p><strong>lines:</strong> one for each year</p>
    <p><strong>panels:</strong> each panel represents a transect, and contains all vegetation plots in that transect</p>
    <p><strong>selections:</strong> years can be removed and added using the checkboxes, if you want to focus on one or a few.</p>"
)
transect_profiles_combined_desc <- ("In the combined window, there is still one line per year for elevation. Points now represent vegetation data. Shape represents the species or group, and size of the point represents the cover value (i.e. very small = low cover; large = high cover).")

elevation_histograms_desc1 <- "See the distribution of elevation readings in your data file and find any anomalously high values that might indicate a typo in data entry."
elevation_histograms_desc2 <- HTML(
    "<p><strong>Mean plot elevation:</strong> Mean elevation by plot on each date, as calculated by app. Look for any anomalous mean values.</p>
    <p><strong>Stdev by plot:</strong> Standard deviation of individual measurements within each plot on each date. If you notice anomalously high values, hover over the bar to see what the value is. Then you can go to the 'data preview' tab and search in the 'elev_sd' colum to find the plot, and look at the individual elevation readings.</p>
    <p><strong>All elevation readings:</strong> Each individual elevation reading in the file. Look for anomalous individual readings, and hover over the bar to see the values.</p>"
)

correlation_scatterplots_desc1 <- "Explore relationships between variables, across all sites. This graph only updates when you click the 'Use these choices' button. This is the only graph that is not interactive."
correlation_scatterplots_desc2 <- HTML(
    "<p>You choose the variables to display on each axis.</p>
    <p><strong>points:</strong> one for each vegetation plot on each date</p>
    <p><strong>shape:</strong> represents site - are there differences in the relationship between sites?</p>
    <p><strong>color:</strong> represents missing vs. non-missing values. If a 'missing' colored point is near the origin, it is missing for both variables. If a value is missing for only one of the two variables, it will be near 0 for the variable that is missing but at the appropriate value for the axis where a variable is present. e.g., if a missing value is placed at 80 along the x-axis, and is near the axis, the y-variable was not measured (and is presumably 0, unless it was truly missing data).</p>
    <p><strong>line:</strong> if selected, a linear regression line is added to the graph.</p>"
)

table_interactivity_desc <- "This table is interactive. Columns can be sorted by clicking on their name or filtered by typing into the box below the name."

time_series_and_transect_profile_desc <- HTML(
    "<p>The time series and transect profile graphs allow detailed examination of one parameter at one site at a time. Select (and change) either or both here and choices will apply in the tabs for both graphic types.</p> 
    <p>Only Site can be updated in the Elevation page, because elevation is the only parameter option.</p>"
)

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
        selectInput("elevColSel", 
                    span(
                        tags$small("Which column(s) represent elevation measurements?"),
                        tooltip(
                            bsicons::bs_icon("info-circle"),
                            "Select the column or columns that contain elevation readings.",
                            placement = "right"
                        )
                    ),
                    choices = NULL,
                    multiple = TRUE),
        selectInput("elevAvgSel", 
                    span(
                        tags$small("Is there a column representing the average of elevation measurements?"),
                        tooltip(
                            bsicons::bs_icon("info-circle"),
                            "If you calcluated the average elevation for your plots in your spreadsheet, select that column here. It will be placed next to the app's calculated mean column so you can check for any errors.",
                            placement = "right"
                        )
                    ),
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
    
    # About panel ----
    nav_panel("About",
              card(
                  card_header("About this app"),
                  p("This application allows users to explore elevation data at vegetation monitoring sites , vegetation data when it is in the format provided by the Namaste project, and the combination of the two datasets together."),
                  p("Most pieces of this app are interactive. Tables can be searched and sorted; graphs can have features added or removed; and hovering over a point on a graph will show that point's values."),
                  p("Information icons (", bsicons::bs_icon("info-circle"), ") are throughout the app to provide more details about specific features. In sidebars you generally hover to see the information, and in the main sections of content you need to click on them."),
                  
                  h4("Importing your data"),
                  tags$ol(
                      tags$li(strong("Upload your elevation data file"), "using the sidebar. This information will not be retained by the app once you close the session."),
                      tags$li(strong("Choose elevation columns"), "using the sidebar. The identified columns will be renamed, and a mean for each plot on each date will be calculated. If a column is identified as representing the average as calculated in the spreadsheet, that column will be moved next to the app's calculated mean for easy comparison. This information will not be retained by the app once you close the session."),
                      tags$li(strong("Upload your vegetation data file"), "using the sidebar. This information will not be retained by the app once you close the session."),
                      tags$li(strong("Collapse the sidebar"), "using the toggle arrow in the upper right corner of the sidebar. All sidebars can be toggled in this way, to allow more space for tables and graphs once choices are made, but also allow you to see and modify your previous choices."),
                  ),
                  
                  h4("Pages in top navigation bar"),
                  p("and some notes on what is needed in the files"),
                  tags$ol(
                      tags$li(strong("Elevation:"), "This app was developed using Waquoit Bay NERR's elevation data file, so other files that may be used must be csv files and in the same format as Waquoit Bay's."),
                      tags$li(strong("Vegetation"), "The vegetation file should be an Excel file as produced from the Namaste project."),
                      tags$li(strong("Combined:"), " In order to explore elevation and vegetation data for a site, the SiteID, TransectID, and PlotID names ", strong("MUST"), " be the same in both files."),
                      
                  ),
                  
                  h4("Tabs within each page"),
                  tags$ol(
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
    
    
    # Elevation data panel ----
    nav_panel("Elevations",
              layout_sidebar(
                  sidebar = sidebar(
                      title = NULL,
                      span(
                          h5("Options for time series and transect profile graphics"),
                          tooltip(
                              bsicons::bs_icon("info-circle"),
                              time_series_and_transect_profile_desc,
                              placement = "right"
                          )
                      ),
                      selectInput("selected_site", 
                                  htmltools::tags$small("Select Site:"), 
                                  choices = NULL)
                  ),
              navset_card_tab(
                  nav_panel(
                      title = "Elevation data preview",
                      htmltools::tags$small(
                          data_preview_desc1,
                          table_interactivity_desc,
                          actionLink("data_preview_info", bsicons::bs_icon("info-circle"))
                      ),
                      reactableOutput("dt.elevs")
                  ),
                  nav_panel(
                      title = "Elevation column summary",
                      htmltools::tags$small(
                          column_summary_desc1,
                          table_interactivity_desc,
                          actionLink("column_summary_info", bsicons::bs_icon("info-circle"))
                      ),
                      reactableOutput("dt.elevs.skimr")
                  ),
                  nav_panel(
                      title = "Elevation histograms",
                      card(
                          htmltools::tags$small(
                              elevation_histograms_desc1,
                              actionLink("elevation_histograms_info", bsicons::bs_icon("info-circle"))
                          ),
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
                          htmltools::tags$small(
                              time_series_desc1,
                              actionLink("time_series_info", bsicons::bs_icon("info-circle"))
                          ),
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
                          htmltools::tags$small(
                              transect_profiles_desc1,
                              actionLink("transect_profiles_info", bsicons::bs_icon("info-circle"))
                          ),
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
                              time_series_and_transect_profile_desc,
                              placement = "right"
                          )
                      ),
                      selectInput("selected_site.veg", 
                                  htmltools::tags$small("Select Site:"), 
                                  choices = NULL),
                      selectInput("selected_column.veg", 
                                  htmltools::tags$small("Select Variable:"), 
                                  choices = NULL)
                  ),
              
              navset_card_tab(
                  
                  nav_panel(
                      title = "Vegetation data preview",
                      htmltools::tags$small(
                          data_preview_desc1,
                          table_interactivity_desc,
                          actionLink("data_preview_info", bsicons::bs_icon("info-circle"))
                      ),
                      reactableOutput("dt.veg")
                  ),
                  
                  nav_panel(
                      title = "Vegetation column summary",
                      htmltools::tags$small(
                          column_summary_desc1,
                          table_interactivity_desc,
                          actionLink("column_summary_info", bsicons::bs_icon("info-circle"))
                      ),
                      reactableOutput("dt.veg.skimr")
                  ),
                  
                  nav_panel(
                      title = "Vegetation sampling summary",
                      htmltools::tags$small(
                          sampling_summary_desc1,
                          actionLink("sampling_summary_info", bsicons::bs_icon("info-circle"))
                      ),
                      reactableOutput("dt.veg_samples")
                  ),
                  
                  nav_panel(
                      title = "Vegetation time series",
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
                      title = "Vegetation Transect Profiles",
                      card(
                          full_screen = TRUE,
                          card(
                              htmltools::tags$small(
                                  transect_profiles_desc1,
                                  actionLink("transect_profiles_info", bsicons::bs_icon("info-circle"))
                              ),
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
                      selectInput("selected_site.comb", 
                                  htmltools::tags$small("Select Site:"), 
                                  choices = NULL),
                      selectInput("selected_cols.comb", 
                                  htmltools::tags$small("Select parameter(s) from veg data:",
                                                        tooltip(bsicons::bs_icon("info-circle"),
                                                                "Multiple columns can be selected for comparison with each other and with elevation data.")),
                                  choices = NULL,
                                  multiple = TRUE)
                  ),
                  
                  navset_card_tab(
                      
                      nav_panel(
                          title = "Combined Data preview",
                          htmltools::tags$small(
                              data_preview_desc1,
                              table_interactivity_desc,
                              actionLink("data_preview_info", bsicons::bs_icon("info-circle"))
                          ),
                          reactableOutput("dt.comb")
                      ),
                      
                      nav_panel(
                          title = "Combined time series",
                          card(
                              htmltools::tags$small(
                                  time_series_desc1,
                                  actionLink("time_series_info", bsicons::bs_icon("info-circle"))
                              ),
                              full_screen = TRUE,
                              fill = FALSE,
                              htmltools::tags$small("Vegetation plot selection option is below the graph panels."),
                              
                              plotlyOutput("p_combTimeSeries",
                                           height = "600px"),
                              card(
                                  layout_columns(
                                      col_widths = c(10, 2),
                                      checkboxGroupInput("selected_plots.comb", "Included vegetation plots:",
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
                              htmltools::tags$small(
                                  transect_profiles_desc1,
                                  actionLink("transect_profiles_info2", bsicons::bs_icon("info-circle"))
                              ),
                              full_screen = TRUE,
                              fill = FALSE,
                              htmltools::tags$small("Year selection option is below the graph panels."),
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
        
        observeEvent(input$transect_profiles_info2, {
            showModal(modalDialog(
                title = "Transect Profile Graph",
                transect_profiles_desc2,
                transect_profiles_combined_desc,
                footer = modalButton("Close"),
                easyClose = TRUE,
                size = "m"
            ))
        })
        
        observeEvent(input$elevation_histograms_info, {
            showModal(modalDialog(
                title = "Elevation histograms",
                elevation_histograms_desc2,
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