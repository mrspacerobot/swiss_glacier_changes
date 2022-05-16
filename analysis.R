#####################################################
## This script imports the data, does some EDA,    ##
## summarizes the data and fits the proposed model ##
## as well as some others to the data              ##
#####################################################

# This script depends on:
# - A more or less recent version of R
# - A working installation of pdflatex
# - The imagemagick command-line tools


# Load libraries -----------------------------------------------------------------------------------------------------------
# import self written functions
source("functions.R")

# install necessary libraries
used_pkgs <- c("readr", "dplyr", "ggplot2", "scales", "purrr", "tidyr", "kableExtra", "texreg") # list of used packages
install_pkgs(used_pkgs) # If this function returns 0 for all packages, everything is fine

# load packages
for(i in used_pkgs) library(i, character.only = TRUE)
rm(i)

# Setup tasks -----------------------------------------------------------------------------------------------------------

# create a directory to store figures in
if(!dir.exists("./figures")) dir.create("./figures")

# Import data ---------------------------------------------------------------------------------------------------------------

## import the three csv files for glacier data and split into data and metadata ----
datasets <- c("data/lengthchange.csv", "data/massbalance_observation.csv", "data/Volumechange.csv")
csv <- lapply(datasets, function(x){
  # read files line by line
  dat <- readLines(x)
  # filter out the intro text
  dat <- dat[c(7:length(dat))]
  # parse data
  dat <- suppressMessages(readr::read_delim(paste0(dat, collapse = "\n"), delim = ";", col_types = readr::cols()))
  colnames(dat) <- gsub("\\s+", "_", colnames(dat))
  # separate meta data
  meta_dat <- dat[1:2, ]
  dat <- dat[-1L*1:2, ]
  # determine column data type
  if(x == datasets[1]){
    date_cols <- c(3, 5)
    num_cols <- 7:8
  } else if(x == datasets[2]){
    date_cols <- 3:5
    num_cols <- 6:13
  } else if(x == datasets[3]){
    date_cols <- c(3, 5)
    num_cols <- 7:11
  }
  # set data type
  dat[] <- lapply(seq_along(dat), function(x){
    out <- dat[[x]]
    if(x %in% date_cols) out <- as.Date(dat[[x]])
    if(x %in% num_cols) out <- as.numeric(gsub("^None$", NA_character_, dat[[x]]))
    out
  })
  # calculate duration of observation period in days
  dat$obs_period <- as.numeric(difftime(dat$end_date_of_observation, dat$start_date_of_observation, units = "days"))
  # add average length change per year to the length change data set
  # as observation periods are not equally long
  if(x == "data/lengthchange.csv") dat$dL_yearly <- dat$length_change / dat$obs_period * 365
  # Keep only rows that have at least one non-na value
  dat <- dat[vapply(seq_len(nrow(dat)), function(x) !all(is.na(unlist(dat[x, ]))), logical(1L)), ]
  # return
  list(data = dat, metadata = meta_dat)
})

# Assign output list contents to the global environment
names(csv) <- gsub(".csv","", basename(datasets))
out <- lapply(csv, `[[`, i = "data")
list2env(out, envir = .GlobalEnv); rm(out)
out <- lapply(csv, `[[`, i = "metadata")
names(out) <- gsub(".csv","_meta", basename(datasets))
list2env(out, envir = .GlobalEnv); rm(out, csv)
rm(datasets)

## import excel sheets for greenhouse gas emissions --------
sheets <- c("Total", "CO2", "CH4", "N2O", "HFC, PFC, SF6, NF3")
out <- lapply(sheets, function(x){
  # get rows with year and total emission
  dat <- suppressMessages(readxl::read_xlsx("data/Evolution_GHG_since_1990.xlsx", sheet = x))
  years <- suppressWarnings(as.numeric(unlist(dat[grep("^Cat\\..*$", dat[[1L]]), ], use.names = FALSE)))
  if(x == "HFC, PFC, SF6, NF3") col <- 2L else col <- 1L
  data_row <- grep("^Total.*$", dat[[col]])
  if(length(data_row) != 1L) data_row <- data_row[which.min(vapply(data_row, 
                                                                   function(x) sum(is.na(suppressWarnings(as.numeric(unlist(dat[x, ]))))), 
                                                                   integer(1L)))]
  data <- suppressWarnings(as.numeric(unlist(dat[data_row, ], use.names = FALSE)))
  # throw out NA rows
  non_na_rows <- !is.na(years)
  # put everything in a data frame
  res <- tibble::tibble(year = years[non_na_rows], emission = data[non_na_rows], gas = x)
  # note different order of magnitude of data (million vs thousand tonnes of emission)
  if(x %in% c("Total", "CO2")) res$emission <- res$emission * 1e6 else res$emission <- res$emission * 1e3
  # add unit of data
  if(x %in% c("Total", "HFC, PFC, SF6, NF3")) res$unit <- "CO2 equivalent" else res$unit <- "native"
  res
})

# put everything into one data frame
emissions <- do.call(rbind, out); rm(out, sheets)

## import temperature data ----

# fetch csv with links to the actual data sets
t <- read.csv("https://data.geo.admin.ch/ch.meteoschweiz.klima/nbcn-tageswerte/liste-download-nbcn-d.csv",
              sep = ";", encoding = "latin1")

# clean this a bit
t <- t[t$Station != "", ]
t <- t[, which(names(t) %in% c("Station", "station.location", "Latitude",
                               "Longitude", "Climate.region",
                               "Station.height.m..a..sea.level", "Canton",
                               "URL.Previous.years..verified.data.", "URL.Current.year"))]

# import data from all of the weather stations
nms <- t$Station
temp <- lapply(1:nrow(t), function(x){
  cat("\rImporting temperature data from station:", t$Station[x])
  # read data from the single stations
  df <- read.csv(t$URL.Previous.years..verified.data.[x], sep = ";")
  # throw out data that is not in our study period
  idx <- as.integer(gsub("^(\\d{4}).+$", "\\1", df$date)) >= min(emissions$year)
  df <- df[idx, ]
  # keep location, date and temperature columns
  df <- df[, which(names(df) %in% c("station.location", "date", "tre200d0"))]
  # convert date to date format
  df$date <- as.Date(gsub("^(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3", df$date))
  # convert temperature to numeric
  df$tre200d0 <- as.numeric(df$tre200d0)
  names(df)[names(df) == "tre200d0"] <- "temperature"
  # Add other information from the index csv
  df$station_name <- t$Station[x]
  df$canton <- t$Canton[x]
  df$latitude <- t$Latitude[x]
  df$longitude <- t$Longitude[x]
  df$elevation <- t$Station.height.m..a..sea.level[x]
  df$climate_region <- t$Climate.region[x]
  # return
  return(df)
})
names(temp) <- nms; rm(t, nms)

# investigate NAs
nas <- sapply(temp, function(x) sapply(x, function(y) any(is.na(y))))
nas <- which(nas, arr.ind = TRUE)[, 2] # which time series contain NAs
# count how many NAs there are. In one time series there is 1 and in the other 15.
# That should be no problem since this is daily data and we calculate the monthly mean
# temperature anyway.
sapply(nas, function(x) sapply(temp[[x]], function(y) sum(is.na(y))))

# put everything inside a data frame
temp <- do.call(rbind, temp)

# EDA -----------------------------------------------------------------------------------------------------------------------------

## EDA for greenhouse gas emissions -----

## Check that there is an entry for each emission type ("total", "CO2", "CH4", "N2O", "HFC, PFC, SF6, NF3") in
## every year
n_entries <- tapply(emissions$gas, emissions$year, length)
all(n_entries == n_entries[1]) # should be TRUE

## Check that there are no missing years
emission_years <- sort(as.integer(names(n_entries)))
all.equal(emission_years, seq(min(emission_years), max(emission_years), 1L)) # should be TRUE

rm(n_entries)

## Show emissions over time
emissions %>% 
  mutate(emission = emission / 1e3,
         gas = factor(gas, levels = unique(gas), 
                      labels = make_labels(gas)),
         unit = factor(unit, levels = unique(unit), 
                       labels = make_labels(unit))) %>% 
  ggplot(aes(x = year, y = emission, color = unit)) +
  geom_point() +
  geom_line() +
  facet_wrap(~gas, scales = "free_y", labeller = label_parsed) + 
  scale_color_discrete(labels = parse_format()) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.margin = unit(c(0.2, 0.75, 0.2, 0.2), "cm")) +
  labs(title = "Greenhouse gas emissions in Switzerland",
       y = "Emission [1000 t]", x = "Year",
       color = "Unit")
ggsave("./figures/greenhouse.pdf",
       height = 8,
       width = 7,
       units = "in")

## strong negative correlation between year and total gg-emissions
emissions %>% 
  filter(gas == "Total") %>% 
  with(cor(year, emission, method = "pearson"))


## EDA for glacier data -----

# Trim dataset to observation within our time period of interest.
# Corresponds to where we actually have data for greenhouse gases
lengthchange <- lengthchange %>% 
  filter(end_date_of_observation > as.Date(paste0(min(emission_years), "-01-01")), 
         !is.na(length_change), !is.nan(length_change))

# Look at the columns of the data frame
summary(lengthchange)


# First, plot the data
pairs(lengthchange[, sapply(lengthchange, is.numeric)])

# There is some weird stuff going. Length changes of -1000m?
# Observation periods of 25000 days?


# Note:
# 1 NA in the length_change variable (This is ok since it is actually NaN in the original data)
# 9000 NAs in the elevation_of_glacier_tongue variable
# Longest observation_period is almost 25'000 days

# Check suspicious cases (length_change per year of more than +-150 m). Boundary was arbitrarily chosen.
sus <- lengthchange %>% 
  filter(abs(dL_yearly) > 150) %>% 
  pull(glacier_id) %>% 
  unique

lengthchange %>% 
  filter(glacier_id %in% sus) %>% 
  select(glacier_name, start_date_of_observation, end_date_of_observation, length_change) %>% 
  arrange(start_date_of_observation) %>% 
  ggplot() +
  geom_segment(aes(x = start_date_of_observation,
                   xend = end_date_of_observation,
                   y = length_change,
                   yend = length_change)) +
  theme_minimal() +
  facet_wrap(~ glacier_name, scales = "free_y", ncol = 4) +
  labs(x = "Year",
       y = "Total length change during period [m]") +
  theme(plot.margin = unit(c(0.2, 0.75, 0.2, 0.2), "cm"))
ggsave("./figures/lengthchange_outlier.pdf",
       height = 8,
       width = 10,
       units = "in")
  
# Hard to say, some actually look reasonable, others look like clear mistakes

# Check if we see a general trend when we plot all of the glaciers
lengthchange %>% 
  select(glacier_name, start_date_of_observation, end_date_of_observation, dL_yearly) %>% 
  arrange(start_date_of_observation) %>% 
  ggplot() +
  geom_segment(aes(x = start_date_of_observation,
                   xend = end_date_of_observation,
                   y = dL_yearly,
                   yend = dL_yearly,
                   colour = glacier_name
  ), alpha = 0.4) +
  theme_minimal() +
  scale_color_discrete(type = "viridis") +
  labs(x = "year",
       y = "Average yearly length change [m/year of observation]",
       colour = "Average yearly \nlength change [m]") +
  theme(legend.position = "none",
        plot.margin = unit(c(0.2, 0.75, 0.2, 0.2), "cm"))
ggsave("./figures/average_lengthchange.pdf",
       height = 6,
       width = 8,
       units = "in")

# How many glaciers do we actually have within our period of interest?
length(unique(lengthchange$glacier_id))

# How are the observation periods distributed? 

# Minimum 273 days, maximum 9519 days
summary(lengthchange$obs_period)

# Check distribution (especially of the shorter periods). 
# Ideally we want all observations to cover more or less entire years.
data.frame(observation_periods = get_deviation(lengthchange$obs_period, 365L)) %>% 
  ggplot() +
  geom_histogram(aes(x = observation_periods), bins = 30, fill = "skyblue") +
  labs(title = "Observation periods",
       x = "Deviation from the closest multiple of 365 [d]",
       y = "Count") +
  theme_minimal() +
  theme(plot.margin = unit(c(0.2, 0.75, 0.2, 0.2), "cm"))
ggsave("./figures/obs_period_distribution.pdf",
       height = 7,
       width = 6,
       units = "in")

# In which months did the observations start/end?
start_months <- as.integer(gsub("^.+-(\\d{2})-.+$", "\\1", as.character(lengthchange$start_date_of_observation)))
end_months <- as.integer(gsub("^.+-(\\d{2})-.+$", "\\1", as.character(lengthchange$end_date_of_observation)))

bind_rows(tibble(vis_months = start_months,
                 title = "Distribution of observation starts"),
          tibble(vis_months = end_months,
                 title = "Distribution of observation ends")) %>% 
  mutate(month_name = month.name[vis_months]) %>% 
  group_by(month_name, title) %>% 
  summarise(freq = length(vis_months), .groups = "drop") %>% 
  mutate(month_name = factor(month_name, levels = month.name, labels = substr(month.name, 1, 3), ordered = TRUE),
         title = factor(title, levels = c("Distribution of observation starts", 
                                          "Distribution of observation ends"), ordered = TRUE)) %>% 
  ggplot(aes(y = freq, x = month_name)) +
  geom_col(fill = "skyblue") +
  theme_minimal() +
  facet_wrap(~title) +
  labs(x = "",
       y = "Frequency")
ggsave("./figures/obs_period_start_end.pdf",
       height = 7,
       width = 6,
       units = "in")

## These plots confirm that all observation periods started between July and November
## and all observation periods end in between July and November

## EDA for temperature data -----

# Calculate monthly average temperatures for all of Switzerland
temp <- temp %>% 
  mutate(month = gsub("-\\d{2}$", "", as.character(date))) %>% 
  group_by(month) %>% 
  summarise(temperature = mean(temperature, na.rm = TRUE), .groups = "drop") %>% 
  mutate(year = gsub("-.+$", "", month),
         month_char = gsub("^.+-", "", month),
         month_int = as.integer(month_char),
         month_name = month.name[month_int]) %>% 
  select(month, month_char, month_int, month_name, year, temperature)

# plot the temperature data
temp %>% 
  mutate(date = as.Date(paste0(month, "-15"))) %>% 
  ggplot(aes(x = date, y = temperature, color = "whatever")) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Date", y = "Average monthly temperature [\u00B0C]",
       title = "Average temperature")
ggsave("./figures/avg_temperature.pdf",
       height = 7,
       width = 6,
       units = "in")

temp %>% 
  mutate(date = as.Date(paste0(month, "-15")),
         month_name = factor(month_name, levels = month.name, 
                             labels = substr(month.name, 1, 3), ordered = TRUE)) %>% 
  ggplot(aes(x = month_name, y = temperature, color = year, group = year, alpha = 0.2)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "Average temperature [\u00B0C]",
       title = "Average temperature by month")
ggsave("./figures/avg_temperature_by_month.pdf",
       height = 7,
       width = 6,
       units = "in")

# Preprocess data set such that we can use it to fit the model we want -----------------------------------------

## As all observations start between July and November and all 
## observations end between July and November as well, we can
## assign an observation to the year of the first January after the start of the observation.
## In case of observations periods of more than 1 year, we will divide the lengthchange by the number
## of years in the observation period.
data_clean <- purrr::map_dfr(seq_len(nrow(lengthchange)), function(x){
  # loop through each row
  obs_orig <- lengthchange %>% slice(x)
  # get start and end year
  year_start <- as.integer(format(obs_orig$start_date_of_observation, format = "%Y")) + 1L
  year_end <- as.integer(format(obs_orig$end_date_of_observation, format = "%Y")) + 1L
  # if difference between start and end year is 1: treat as one observation.
  n_years <- year_end - year_start
  out <- obs_orig %>% 
    select(glacier_name, glacier_id, elevation_of_glacier_tongue, length_change) %>% 
    mutate(year = year_start)
  # Otherwise calculate the number of years and divide length change by the number of years
  if(n_years != 1L){
    out <- as_tibble(lapply(out, rep, times = n_years)) %>% 
      mutate(year = seq(year_start, year_start + n_years - 1L, 1L),
             length_change = length_change / n_years)
  }
  out
})

# Calculate average elevation of glacier tongue for each glacier (such that we may use this as a covariate)
elevation <- data_clean %>% 
  group_by(glacier_id) %>% 
  summarise(avg_elevation = mean(elevation_of_glacier_tongue, na.rm = TRUE))

data_clean <- data_clean %>% 
  left_join(y = elevation, by = "glacier_id") %>% 
  select(-elevation_of_glacier_tongue)

# Check whether anything is NA (some glaciers have no indication about elevation)
sapply(data_clean, function(x) any(is.na(x))) # ideally should be all FALSE

# Which glaciers do not have elevation indication? It's only 7.
data_clean %>% filter(is.na(avg_elevation)) %>% pull(glacier_name) %>% unique()

# Add these by hand
## Mittelaletschgletscher - source: https://doi.glamos.ch/pubs/glrep/glrep_121-122.pdf
## Bas Glacier d'Arolla - source: https://doi.glamos.ch/pubs/glrep/glrep_121-122.pdf
data_clean$avg_elevation[data_clean$glacier_name == "Mittelaletschgletscher"] <- 2294
data_clean$avg_elevation[data_clean$glacier_name == "Bas Glacier d'Arolla"] <- 2135
## For these, we use the values provided on a map from swisstopo
## Bella Tola Gletscher - source: https://map.geo.admin.ch/?selectedNode=node_ch.swisstopo.fixpunkte-lfp11&zoom=10&bgLayer=ch.swisstopo.pixelkarte-farbe&layers=ch.swisstopo.fixpunkte-hfp2,ch.swisstopo.fixpunkte-hfp1,ch.swisstopo.fixpunkte-lfp2,ch.swisstopo.fixpunkte-lfp1&layers_visibility=false,true,false,false&time_current=latest&lang=de&topic=ech&E=2616178.63&N=1121298.44
## Tällibodengletscher - source: https://map.geo.admin.ch/?selectedNode=node_ch.swisstopo.fixpunkte-lfp11&zoom=10&bgLayer=ch.swisstopo.pixelkarte-farbe&layers=ch.swisstopo.fixpunkte-hfp2,ch.swisstopo.fixpunkte-hfp1,ch.swisstopo.fixpunkte-lfp2,ch.swisstopo.fixpunkte-lfp1&layers_visibility=false,true,false,false&time_current=latest&lang=de&topic=ech&E=2642415.75&N=1094540.50
## Ofentalgletscher - source: https://map.geo.admin.ch/?selectedNode=node_ch.swisstopo.fixpunkte-lfp11&zoom=10&bgLayer=ch.swisstopo.pixelkarte-farbe&layers=ch.swisstopo.fixpunkte-hfp2,ch.swisstopo.fixpunkte-hfp1,ch.swisstopo.fixpunkte-lfp2,ch.swisstopo.fixpunkte-lfp1&layers_visibility=false,true,false,false&time_current=latest&lang=de&topic=ech&E=2643895.12&N=1096251.84
## Grand Plan Névé - source: https://map.geo.admin.ch/?selectedNode=node_ch.swisstopo.fixpunkte-lfp11&zoom=10&bgLayer=ch.swisstopo.pixelkarte-farbe&layers=ch.swisstopo.fixpunkte-hfp2,ch.swisstopo.fixpunkte-hfp1,ch.swisstopo.fixpunkte-lfp2,ch.swisstopo.fixpunkte-lfp1&layers_visibility=false,true,false,false&time_current=latest&lang=de&topic=ech&E=2576572.25&N=1121159.63
## Glacier de Paneirosse - source: https://map.geo.admin.ch/?selectedNode=node_ch.swisstopo.fixpunkte-lfp11&zoom=10&bgLayer=ch.swisstopo.pixelkarte-farbe&layers=ch.swisstopo.fixpunkte-hfp2,ch.swisstopo.fixpunkte-hfp1,ch.swisstopo.fixpunkte-lfp2,ch.swisstopo.fixpunkte-lfp1&layers_visibility=false,true,false,false&time_current=latest&lang=de&topic=ech&E=2576572.25&N=1121159.63
data_clean$avg_elevation[data_clean$glacier_name == "Bella Tola Gletscher"] <- 2760
data_clean$avg_elevation[data_clean$glacier_name == "Tällibodengletscher"] <- 2660
data_clean$avg_elevation[data_clean$glacier_name == "Ofentalgletscher"] <- 2760
data_clean$avg_elevation[data_clean$glacier_name == "Grand Plan Névé"] <- 2400
data_clean$avg_elevation[data_clean$glacier_name == "Glacier de Paneirosse"] <- 2440

# Filter again to the time period we need
data_clean <- data_clean %>% 
  filter(year >= min(emissions$year))

# Add column with cumulative length change
data_clean <- data_clean %>% 
  group_by(glacier_id) %>% 
  mutate(cum_length_change = cumsum(length_change)) %>% 
  ungroup()

# Plot the preprocessed data
data_clean %>% 
  ggplot(aes(x = year, y = cum_length_change, color = glacier_name, alpha = 0.2)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Year", 
       y = "Cumulative length change [m]",
       title = "Length change trajectories")
ggsave("./figures/cumulative_length_change.pdf",
       height = 7,
       width = 6,
       units = "in")

# Summarize data and save csv ------------------------------------------------------------------------
# Get all the data into a data frame
model_df <- data_clean %>% 
  group_by(year) %>% 
  summarise(length_change = mean(length_change),
            cum_length_change = mean(cum_length_change)) %>% 
  left_join(emissions %>% 
              tidyr::pivot_wider(id_cols = year,
                                 values_from = emission,
                                 names_from = gas,
                                 names_prefix = "emission_"), by = "year") %>% 
  left_join(temp %>% 
              select(year, month_name, temperature) %>% 
              mutate(year = as.integer(year)) %>% 
              tidyr::pivot_wider(id_cols = year, 
                                 names_from = month_name, 
                                 names_prefix = "temp_", 
                                 values_from = temperature),
            by = "year") %>%
  # remove rows with NAs (2020)
  tidyr::drop_na() %>% 
  # add yearly average temperature as the mean of all monthly mean temperatures
  rowwise() %>% 
  mutate(temp_year = mean(c_across(starts_with("temp_")))) %>% 
  ungroup()

# Write csv
model_df %>% 
  write.csv(file = "./data/data_cleaned.csv")

# Add some more plots
## plot cumulative length change vs. temperature
model_df %>% 
  ggplot(aes(x = temp_year, y = length_change)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() + 
  labs(y = "Length change [m]",
       x = "Yearly average temperature [\u00B0C]",
       title = "Length change vs. temperature")
ggsave(filename = "figures/length_change_vs_temperature.pdf",
       height = 6,
       width = 7,
       units = "in")

## plot cumulative length change vs. temperature
model_df %>% 
  ggplot(aes(x = temp_year, y = cum_length_change)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() + 
  labs(y = "Cumulative length change [m]",
       x = "Yearly average temperature [\u00B0C]",
       title = "Cumulative length change vs. temperature")
ggsave(filename = "figures/cum_length_change_vs_temperature.pdf",
       height = 6,
       width = 7,
       units = "in")

## plot length change vs. emission
## This will probably look problematic in model diagnostics
model_df %>% 
  mutate(emission_Total = emission_Total / 1e6) %>% 
  ggplot(aes(x = emission_Total, y = length_change)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() + 
  labs(x = "Total greenhouse gas emission [Mio. t]",
       y = "Length change [m]",
       title = "Length change vs. Greenhouse gas emissions")
ggsave(filename = "figures/length_change_vs_emission.pdf",
       height = 6,
       width = 7,
       units = "in")

## plot cumulative length change vs. emission
model_df %>% 
  mutate(emission_Total = emission_Total / 1e6) %>% 
  ggplot(aes(x = emission_Total, y = cum_length_change)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() + 
  labs(x = "Total greenhouse gas emission [Mio. t]",
       y = "Cumulative length change [m]",
       title = "Cumulative length change vs. Greenhouse gas emissions")
ggsave(filename = "figures/cum_length_change_vs_emission.pdf",
       height = 6,
       width = 7,
       units = "in")

## plot length change vs. time trend
model_df %>% 
  ggplot(aes(x = year, y = length_change)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() + 
  labs(x = "Year",
       y = "Length change [m]",
       title = "Length change vs. linear time trend")
ggsave(filename = "figures/length_change_vs_lin_time_trend.pdf",
       height = 6,
       width = 7,
       units = "in")

## plot cumulative length change vs. time trend
## This will probably look problematic in model diagnostics due to residual dependence
model_df %>% 
  ggplot(aes(x = year, y = cum_length_change)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() + 
  labs(x = "Year",
       y = "Cumulative length change [m]",
       title = "Cumulative length change vs. linear time trend")
ggsave(filename = "figures/cum_length_change_vs_lin_time_trend.pdf",
       height = 6,
       width = 7,
       units = "in")

# Try model fits -------------------------------------------------------------------------------------

# possible predictors
predictors <- list(
  month_temps = paste0("temp_", month.name), # monthly average temperatures
  year_temps = "temp_year", # yearly average temperature
  time_trend = "year", # add a time trend
  emission_total = "emission_Total" # add emission
)

# construct a grid containing all desired predictor combinations
grid <- lapply(seq_along(predictors), function(...) c(FALSE, TRUE))
names(grid) <- names(predictors)
grid <- expand.grid(grid)
# do not include yearly mean temperature and monthly temperature in the same model
grid <- grid[!(grid$month_temps & grid$year_temps), ]

# use data until 2015 for training, the remaining four years for validation
train_idx <- model_df$year <= 2015L
train <- model_df[train_idx, ]
valid <- model_df[!train_idx, ]

# what should be the response variable?
response <- "length_change"
#response <- "cum_length_change"

# fit models according to grid parameters
models <- lapply(seq_len(nrow(grid)), function(x){
  # get grid covariates
  covariates <- names(grid)[unlist(grid[x, ])]
  covariates <- do.call(`c`, predictors[covariates])
  # construct a formula object
  if(length(covariates) == 0L){
    # if no predictors fit intercept only model
    formula <- as.formula(paste0(response, " ~ 1"))
  } else {
    # otherwise create a formula
    formula <- as.formula(paste0(response, " ~ ", paste0(covariates, collapse = " + ")))
  }
  # fit the linear model to the data up until 2015
  model <- lm(formula = formula, data = train)
  fitted <- predict(model, interval = "confidence") %>% 
    as_tibble() %>% 
    mutate(Component = "Fit")
  # predict the remaining 4 years
  prediction <- predict(model, newdata = valid, interval = "prediction")%>% 
    as_tibble() %>% 
    mutate(Component = "Prediction")
  # stuff everything into data frame
  res <- bind_rows(fitted, prediction)
  # Add actual observed and year as a column
  res$observed <- c(train[[response]], valid[[response]])
  res$year <- c(train[["year"]], valid[["year"]])
  
  # return everything
  list(predictors = if(length(covariates) == 0L) "Intercept" else c("Intercept", covariates),
       model = model,
       AIC = AIC(model),
       BIC = BIC(model),
       df = res)
})

# Get the model we proposed in the proposal
prop_mod <- models[[which(with(grid, year_temps & time_trend & emission_total))]]

# look at diagnostic plots
# For response = "length_change", this looks fine to me - given that there are only 26 observations.
# However, if response = "cum_length_change", there is very clear structure
oldpar <- par(no.readonly = TRUE)
pdf(file = "figures/model_diagnostics.pdf")
par(mfrow = c(2, 2))
plot(prop_mod$model)
dev.off()
par(oldpar)

# plot data and model
prop_mod$df %>% 
  ggplot() +
  # add observed
  geom_point(aes(x = year, y = observed)) +
  geom_line(aes(x = year, y = observed)) +
  # add model
  geom_point(aes(x = year, y = fit, color = Component)) + # point
  geom_line(aes(x = year, y = fit, color = Component, linetype = Component)) + # line
  geom_ribbon(aes(x = year, ymin = lwr, ymax = upr, fill = Component),
              alpha = 0.5) +
  theme_minimal() +
  labs(title = "Model fit and prediction",
       x = "",
       y = if(response == "length_change") "Length change" else "Cumulative length change",
       color = "Model",
       linetype = "Model",
       fill = "Model")
ggsave(paste0("./figures/fit_proposed_model_", response, ".pdf"),
       height = 6,
       width = 7,
       units = "in")

# Note: the intervals are different because the red line corresponds to the
# model fit so the area represents a confidence interval. The blue curve is
# the prediction so the blue area is a prediction interval.

# We can see that this does not look very good. However, this was to be expected since:
# 1) Our model is not very sophisticated
# 2) Emissions in Switzerland are probably less relevant than global emissions
# 3) We do not take dependence structure into account
  
# Render coefficient table (I don't )
latex <- paste0("\\documentclass{article}\n\\usepackage{booktabs}\n\\begin{document}\n\\thispagestyle{empty}", 
    texreg::texreg(prop_mod$model, 
                   custom.coef.map = list("(Intercept)" = "Intercept",
                                          "temp_year" = "Yearly avg. temp.",
                                          "year" = "Time trend",
                                          "emission_Total" = "Gg emissions"),
                   digits = 7,
                   booktabs = TRUE,
                   custom.model.names = "Proposed model",
                   use.packages = FALSE,
                   single.row = TRUE),
    "\n\\end{document}")
# Remove caption and label
latex <- gsub("\\n\\\\caption\\{.+?\\}", "", latex)
latex <- gsub("\\n\\\\label\\{.+?\\}", "", latex)
# write table to file and compile it
cat(latex, file = "regression_table.tex")
# compile table
system("pdflatex regression_table.tex")
# crop table
system("convert -density 300 -trim +repage regression_table.pdf regression_table.pdf")

# Leave this out as it is almost the same as the model above
# # Look at other models (lowest AIC)
# models[[which.min(sapply(models, `[[`, i = "AIC"))]]$df %>% 
#   ggplot() +
#   # add observed
#   geom_point(aes(x = year, y = observed)) +
#   geom_line(aes(x = year, y = observed)) +
#   # add model
#   geom_point(aes(x = year, y = fit, color = Component)) + # point
#   geom_line(aes(x = year, y = fit, color = Component, linetype = Component)) + # line
#   geom_ribbon(aes(x = year, ymin = lwr, ymax = upr, fill = Component),
#               alpha = 0.5) +
#   theme_minimal() +
#   labs(title = "Model fit and prediction",
#        x = "",
#        y = if(response == "length_change") "Length change" else "Cumulative length change",
#        color = "Model",
#        linetype = "Model",
#        fill = "Model")
# ggsave(paste0("./figures/fit_best_model_AIC_", response, ".pdf"),
#        height = 6,
#        width = 7,
#        units = "in")

