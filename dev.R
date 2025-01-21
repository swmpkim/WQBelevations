library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# inputs ----
input.elev <- here::here("VegPlotElevationsAllYearsAllPlots.csv")
input.veg <- here::here("WQB_veg - Copy.xlsx")
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

# binding data frames together ----
# deal with elevation df
elev_renamed2 <- elev_renamed |> 
    rename(Date_elevation = Date) |> 
    select(-Month, -Day)
if(exists("elev_avg_fromCSV", elev_renamed2)){
    elev_renamed2$elev_avg_fromCSV <- NULL
}
# deal with veg df
veg2 <- veg |> 
    mutate(PlotIdFull = paste(SiteID, TransectID, PlotID, sep = "-")) |> 
    relocate(PlotIdFull) 

vegAndElev <- full_join(veg2, elev_renamed2,
                        by = c("Year", "PlotIdFull", "SiteID", "TransectID", "PlotID"))

test <- anti_join(elev_renamed2, veg2,
                  by = c("Year", "PlotIdFull"))

vegAndElev2 <- vegAndElev |> 
    rowwise() |> 
    mutate(sumFs = sum(!is.na(c_across(starts_with("F_")))))

# time series of elevation and algae/wrack?
ts_test <- vegAndElev2 |> 
    select(PlotIdFull:Total, SiteID, TransectID, PlotID,
           Bare, Dead, Wrack, Water, Algae,
           sumFs, starts_with("elev"))
ts_long <- ts_test |> 
    pivot_longer(c(Bare, Dead, Wrack, Water, Algae, elev_mean, elev_sd),
                 names_to = "Measurement",
                 values_to = "Value")

ts_long |> 
    filter(Measurement %in% c("Wrack", "Algae", "elev_mean", "elev_sd")) |> 
    ggplot(aes(x = Year, y = Value, group = PlotIdFull, col = PlotID, shape = TransectID)) +
    geom_point() +
    geom_line() +
    facet_grid(Measurement ~ SiteID, scales = "free_y")

# or choose a site and facet by transect
ts_long |> 
    filter(Measurement %in% c("Wrack", "Algae", "Bare", "elev_mean", "elev_sd"),
           SiteID == "S2") |> 
    ggplot(aes(x = Year, y = Value, group = PlotIdFull, col = PlotID, fill = PlotID)) +
    geom_point(size = 2,
               col = "gray30",
               shape = 21) +
    geom_line() +
    khroma::scale_color_nightfall(reverse = TRUE, midpoint = 7) +
    khroma::scale_fill_nightfall(reverse = TRUE, midpoint = 7) +
    facet_grid(Measurement ~ TransectID, scales = "free_y") +
    theme_bw()

# correlations b/t elevation and algae/wrack? what about lags,
# or when there aren't readings of one or the other?
library(naniar)
ggplot(ts_test,
       aes(x = Wrack,
           y = Algae)) +
    geom_miss_point()

ts_test2 <- ts_test |> 
    mutate(across(c(Bare, Dead, Wrack, Water, Algae),
                  function(x) ifelse(is.na(x), 0, x)))
ggplot(ts_test2,
       aes(x = Wrack,
           y = Algae)) +
    geom_point(size = 3, alpha = 0.3)

ggplot(ts_test2,
       aes(x = Algae,
           y = elev_mean)) +
    geom_point(size = 3, alpha = 0.3)

ggplot(ts_test2,
       aes(x = Wrack,
           y = elev_mean)) +
    geom_point(size = 3, alpha = 0.3)

ggplot(ts_test2,
       aes(x = Bare,
           y = elev_mean)) +
    geom_point(size = 3, alpha = 0.3)

ggplot(ts_test2,
       aes(x = Bare,
           y = elev_sd)) +
    geom_point(size = 3, alpha = 0.3)

ggplot(ts_test2,
       aes(x = Algae,
           y = elev_sd)) +
    geom_point(size = 3, alpha = 0.3)

ggplot(ts_test2,
       aes(x = Wrack,
           y = elev_sd)) +
    geom_point(size = 3, alpha = 0.3)

ts_test3 <- ts_test2 |>
    ungroup() |> 
    select(PlotIdFull:Algae,
           elev_mean, elev_sd) |> 
    mutate(elev_sd = case_when(is.na(elev_sd) ~ 0,
                               .default = elev_sd)) |>
    mutate(across(c(Total, Bare, Dead, Wrack, Water, Algae, elev_mean, elev_sd),
           function(x) as.vector(scale(x)))) |> 
    mutate(across(c(Total:elev_sd),
           function(x) ifelse(is.na(x), 0, x)))

pca_test <- princomp(ts_test3[, c(18:24)])
screeplot(pca_test, type = "l")
summary(pca_test)

biplot(pca_test,
       cex = c(0.5, 1),
       col = c("gray40", "darkred"))



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
