library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# inputs ----
input.elev <- here::here("VegPlotElevationsAllYearsAllPlots.csv")
input.veg <- here::here("WQB_veg.xlsx")
input.elevColSel <- c("X1..LW.Left..m.NAVD88",
                      "X2..LW.Right..m.NAVD88",
                      "X3..SW.Right..m.NAVD88",
                      "X4..SW.Left..m.NAVD88",
                      "C..Center..m.NAVD88")
input.elevAvgSel <- "Elev_Avg"

# data frame setup ----
elevs <- read.csv(input.elev) |> 
    mutate(Date = lubridate::ymd(paste(Year, Month, Day)),
           PlotIdFull = paste(SiteID, TransectID, PlotID, sep = "-"))

veg <- readxl::read_xlsx(input.veg,
                         sheet = "Cover")


# data renaming, subsetting, pivoting ----
new_names <- paste0("elevation", seq_along(input.elevColSel))

df <- elevs
names(df)[match(input.elevColSel, names(df))] <- new_names

if(input.elevAvgSel != "none"){
    names(df)[names(df) == input.elevAvgSel] <- "elev_avg_fromCSV"
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

elev_renamed <- df
rm(df)

elev_long <- elev_renamed |> 
    tidyr::pivot_longer(cols = all_of(starts_with("elevation")),
                        names_to = "rep",
                        values_to = "value")

# tables ----

DT::datatable(elev_renamed) |> 
    DT::formatRound(columns = names(select(elev_renamed, starts_with("elev"))),
                digits = 4)

# histograms ----

p_elevMean <- ggplot(elev_renamed,
                   aes(x = elev_mean)) +
    geom_histogram(bins = 30,
                   fill = "navy",
                   col = "gray") +
    theme_bw() +
    labs(title = "Histogram of mean plot elevation",
         x = "Mean Elevation (NAVD88)",
         y = "Count")
ggplotly(p_elevMean)

p_elevSD <- ggplot(elev_renamed,
                     aes(x = elev_sd)) +
    geom_histogram(bins = 30,
                   fill = "navy",
                   col = "gray") +
    theme_bw() +
    labs(title = "Histogram of stdev by plot",
         x = "Standard Deviation of elevation readings",
         y = "Count")
ggplotly(p_elevSD)

p_elevAll <- ggplot(elev_long,
       aes(x = value)) +
    geom_histogram(bins = 30,
                   fill = "navy",
                   col = "gray") +
    theme_bw()+
    labs(title = "Histogram of all elevation readings",
         x = "Elevation (NAVD88)",
         y = "Count")
ggplotly(p_elevAll)


# time series graphs ----  
cols <- khroma::color("batlow")(length(unique(elev_renamed$PlotID)))
# want to make an input to let someone choose the site
# checkbox to turn linerange on and off
p_timeSeries <- elev_renamed |> 
    filter(SiteID == "S1") |> 
    ggplot(
        aes(x = Year,
            y = elev_mean,
            col = as.factor(PlotID),
            group = PlotID)) +
    geom_line() +
    geom_point(size = 1.5) +
    geom_linerange(aes(ymin = elev_mean - elev_sd,
                        ymax = elev_mean + elev_sd)) +
    scale_color_manual(values = cols) +
    facet_wrap(~TransectID) +
    labs(y = "Mean Elevation (NAVD88) +/- 1 SD",
         col = "Plot ID") +
    theme_bw() +
    theme(legend.position = "bottom")
ggplotly(p_timeSeries)

# transect cross-sections ----
# select a year
elev_renamed |> 
    filter(Year == 2018) |> 
    ggplot(aes(x = PlotID,
               y = elev_mean)) +
    geom_point() +
    geom_line() +
    facet_grid(TransectID ~ SiteID, scales = "free_x")

ggplot(elev_renamed,
       aes(x = PlotID,
           y = elev_mean,
           col = Year)) +
    geom_point() +
    geom_line() +
    khroma::scale_color_nightfall() +
    facet_grid(TransectID ~ SiteID, scales = "free_x") +
    theme_bw()

p <- ggplot(elev_renamed,
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
                          midpoint = mean(elev_renamed$Year, na.rm = TRUE)) +
    scale_fill_nightfall(reverse = TRUE,
                         midpoint = mean(elev_renamed$Year, na.rm = TRUE))
    

ggplotly(p)
