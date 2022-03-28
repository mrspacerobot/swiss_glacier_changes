# This script imports the data, cleans it, and does some EDA


# Load libraries -----------------------------------------------------------------------------------------------------------
# import self written functions
source("functions.R")

# install necessary libraries
used_pkgs <- c("readr", "dplyr", "ggplot2", "GGally", "viridisLite") # list of used packages
install_pkgs(used_pkgs) # If this function returns 0 for all packages, everything is fine

# load packages
for(i in used_pkgs) library(i, character.only = TRUE)
rm(i)

# Import data ---------------------------------------------------------------------------------------------------------------

# import the three csv files and split into data and metadata
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


# import excel file
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

# EDA -----------------------------------------------------------------------------------------------------------------------------

## EDA for greenhouse gas emissions -----

## Show emissions over time
ggplot(emissions, aes(x = year, y = emission, color = unit)) +
  geom_point() +
  geom_line() +
  facet_wrap(~gas, scales = "free_y") + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Greenhouse gas emissions",
       y = "emission [t]", x = "year")

## Note the strong negative correlation between year and total emission in CO2 equivalent
ggpairs(emissions[emissions$gas == "Total", c("year", "emission")])


## EDA for glacier data -----

### lengthchange data set -----

# First, plot the data
ggpairs(lengthchange[, sapply(lengthchange, is.numeric)])

# There is some weird stuff going. Length changes of -1000m?
# Observation periods of 25000 days?

# Look at the columns of the data frame
summary(lengthchange)

# Note:
# 1 NA in the length_change variable
# 9000 NAs in the elevation_of_glacier_tongue variable
# Longest observation_period is almost 25'000 days

# Check suspicious cases (length_change per year of more than +-150 m). Boundary was arbitrarily chosen.
sus <- lengthchange %>% 
  filter(abs(dL_yearly) > 150) %>% 
  pull(glacier_id) %>% 
  unique

lengthchange %>% 
  filter(glacier_id %in% sus) %>% 
  select(glacier_name, start_date_of_observation, end_date_of_observation, dL_yearly) %>% 
  arrange(start_date_of_observation) %>% 
  group_by(glacier_name) %>% 
  mutate(yaxis = 1:length(glacier_name)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_segment(aes(x = start_date_of_observation,
                   xend = end_date_of_observation,
                   y = dL_yearly,
                   yend = dL_yearly,
                   colour = dL_yearly)) +
  theme_minimal() +
  facet_wrap(~ glacier_name, scales = "free_y") +
  scale_color_continuous(type = "viridis") +
  labs(x = "year",
       y = "Average yearly length change [m]",
       colour = "Average yearly \nlength change [m]")
  
# Hard to say, some actually look reasonable, others look like clear mistakes

### massbalance data set -----
ggpairs(massbalance_observation[, sapply(massbalance_observation, is.numeric)])


### Volumechange data set -----
ggpairs(Volumechange[, sapply(Volumechange, is.numeric)])

