#####################################################
## This script imports the data, does some EDA,    ##
## summarizes the data and writes it to a csv file ##
#####################################################

# Load libraries -----------------------------------------------------------------------------------------------------------
# import self written functions
source("functions.R")

# install necessary libraries
used_pkgs <- c("readr", "dplyr", "ggplot2", "viridisLite", "scales") # list of used packages
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
  dat <- readLines(x)
  dat <- dat[c(7:length(dat))]
  dat <- suppressMessages(readr::read_delim(paste0(dat, collapse = "\n"), delim = ";", col_types = readr::cols()))
  colnames(dat) <- gsub("\\s+", "_", colnames(dat))
  meta_dat <- dat[1:2, ]
  dat <- dat[-1L*1:2, ]
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
  dat[] <- lapply(seq_along(dat), function(x){
    out <- dat[[x]]
    if(x %in% date_cols) out <- as.Date(dat[[x]])
    if(x %in% num_cols) out <- as.numeric(gsub("^None$", NA_character_, dat[[x]]))
    out
  })
  dat$obs_period <- as.numeric(difftime(dat$end_date_of_observation, dat$start_date_of_observation, units = "days"))
  if(x == "data/lengthchange.csv") dat$dL_yearly <- dat$length_change / dat$obs_period * 365
  dat <- dat[vapply(seq_len(nrow(dat)), function(x) !all(is.na(unlist(dat[x, ]))), logical(1L)), ]
  list(data = dat, metadata = meta_dat)
})
names(csv) <- gsub(".csv","", basename(datasets))
out <- lapply(csv, `[[`, i = "data")
list2env(out, envir = .GlobalEnv); rm(out)
out <- lapply(csv, `[[`, i = "metadata")
names(out) <- gsub(".csv","_meta", basename(datasets))
list2env(out, envir = .GlobalEnv); rm(out, csv)
rm(datasets)


## import excel file for greenhouse gas emissions --------
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
emissions <- do.call(rbind, out); rm(out, sheets)

## import temperature data ----
t <- read.csv("https://data.geo.admin.ch/ch.meteoschweiz.klima/nbcn-tageswerte/liste-download-nbcn-d.csv",
              sep = ";", encoding = "latin1")

# clean this
t <- t[t$Station != "", ]
t <- t[, which(names(t) %in% c("Station", "station.location", "Latitude",
                               "Longitude", "Climate.region",
                               "Station.height.m..a..sea.level", "Canton",
                               "URL.Previous.years..verified.data.", "URL.Current.year"))]

# import all of the weather stations
nms <- t$Station
temp <- lapply(1:nrow(t), function(x){
  df <- read.csv(t$URL.Previous.years..verified.data.[x], sep = ";")
  idx <- as.integer(gsub("^(\\d{4}).+$", "\\1", df$date)) >= min(emissions$year)
  df <- df[idx, ]
  df <- df[, which(names(df) %in% c("station.location", "date", "tre200d0"))]
  df$date <- as.Date(gsub("^(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3", df$date))
  df$tre200d0 <- as.numeric(df$tre200d0)
  names(df)[names(df) == "tre200d0"] <- "temperature"
  df$station_name <- t$Station[x]
  df$canton <- t$Canton[x]
  df$latitude <- t$Latitude[x]
  df$longitude <- t$Longitude[x]
  df$elevation <- t$Station.height.m..a..sea.level[x]
  df$climate_region <- t$Climate.region[x]
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

# Trim dataset to observation within our time period of interest (where we actually have data for greenhouse gases)
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
       y = "Average yearly length change [m/year]",
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
  mutate(month_name = factor(month_name, levels = month.name, labels = month.name, ordered = TRUE),
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
         month_name = factor(month_name, levels = month.name, ordered = TRUE)) %>% 
  ggplot(aes(x = month_name, y = temperature, color = year, group = year, alpha = 0.2)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Date", y = "Average temperature [\u00B0C]",
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
left_join(data_clean %>% 
            group_by(year) %>% 
            summarise(length_change = mean(length_change)),
          emissions %>% 
            tidyr::pivot_wider(id_cols = year,
                               values_from = emission,
                               names_from = gas,
                               names_prefix = "emission_"),
          by = "year") %>%
  left_join(temp %>% 
              select(year, month_name, temperature) %>% 
              mutate(year = as.integer(year)) %>% 
              tidyr::pivot_wider(id_cols = year, 
                                 names_from = month_name, 
                                 names_prefix = "temp_", 
                                 values_from = temperature),
            by = "year") %>% 
  write.csv(file = "./data/lengthchange_cleaned.csv")




