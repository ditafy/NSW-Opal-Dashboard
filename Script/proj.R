library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(viridis)

#origin and destination datasets
#merge four files to a single long format (raw_all)
file_path <- list.files(path = '.', pattern = 'origin_destination_.*\\.csv', full.names = TRUE)
extract_snapshot <- function(path){
  str_extract(basename(path), '(nov|feb|mar|aug)')
}
raw_all <- map_df(file_path, ~{
  opal <- read_csv(.x, show_col_types = FALSE)
  opal$snapshot <- extract_snapshot(.x)
  opal
})

#cleaning merged data
#unify station name format ()
normalize_station <- function(x){
  x %>%
    str_replace_all('_', ' ') %>%
    str_squish() %>%
    str_to_title()
}

#modify date & TZcode...
opal_all <- raw_all %>% clean_names() %>% mutate(
  date = ymd(date),
  
  t_zname_on = normalize_station(t_zname_on),
  t_zname_off = normalize_station(t_zname_off),
  
  t_zcode_on = if_else(t_zcode_on == 0, NA_real_, t_zcode_on),
  t_zcode_off = if_else(t_zcode_off == 0, NA_real_, t_zcode_off),
  
  mode = as.character(mode),
  count = as.numeric(count)
) %>%
  mutate(across(where(is.character), ~na_if(trimws(.), ''))) %>% distinct()

glimpse(opal_all)

#time datasets
#same as above (merge and all)
file_path_time <- list.files(path = '.', pattern = 'time_.*\\.csv', full.names = TRUE)
extract_snapshot_time <- function(path){
  str_extract(basename(path), '(nov|feb|mar|aug)')
}
raw_time <- map_df(file_path_time, ~{
  time <- read_csv(.x, show_col_types = FALSE)
  time$snapshot <- extract_snapshot_time(.x)
  time
})

#data cleaning
time_clean <- raw_time %>% clean_names() %>% mutate(
  date = ymd(date),
  
  time_min = hour(time) * 60 + minute(time),
  time_hour = time_min / 60,
  
  tap = as.character(tap),
  mode = as.character(mode),
  count = as.numeric(count)
) %>% 
  select(snapshot, date, tap, mode, time, time_hour, count)

glimpse(time_clean)


#data aggregation 
#daily trend (all and modes)
daily_trend_all <- time_clean %>% filter(tap == 'on') %>% 
  mutate(
    weekday = wday(date, label = TRUE, abbr = TRUE, week_start = 1)
  ) %>% 
  group_by(snapshot, weekday) %>% summarise(total_count = sum(count), .groups = 'drop')
saveRDS(daily_trend_all, 'daily_trend_all.rds')

daily_trend_by_mode <- time_clean %>% filter(tap == 'on') %>% 
  mutate(
    weekday = wday(date, label = TRUE, abbr = TRUE, week_start = 1)
  ) %>% 
  group_by(snapshot, mode, weekday) %>% summarise(total_count = sum(count), .groups = 'drop')
saveRDS(daily_trend_by_mode, 'daily_trend_by_mode.rds')

#hourly heatmap
hourly_heatmap <- time_clean %>% mutate(
  weekday = wday(date, label = TRUE, abbr = TRUE, week_start = 1)
) %>% group_by(snapshot, time_hour,weekday, tap, mode) %>% summarise(total_count = sum(count), .groups = 'drop')
saveRDS(hourly_heatmap, 'hourly_heatmap.rds')

#mode pie chart
mode_pie <- time_clean %>% group_by(snapshot, mode, tap) %>% summarise(total_count = sum(count), .groups = 'drop')
saveRDS(mode_pie, 'mode_pie.rds')

mode_pie_on <- mode_pie %>% filter(tap == 'on')
saveRDS(mode_pie_on, 'mode_pie_on.rds')

#summary boxes
summary_box <- time_clean %>% group_by(snapshot, tap) %>% summarise(total_count = sum(count), .groups = 'drop')
saveRDS(summary_box, 'summary_box.rds')


#read travel zone shapefile
tz_shp <- st_read('TZ_NSW_2016.shp')
tz_shp_wgs <- st_transform(tz_shp, 4326)

opal_all <- opal_all %>%
  mutate(t_zcode_on = as.character(t_zcode_on))

tz_shp_wgs <- tz_shp_wgs %>%
  mutate(TZ16_CODE = as.character(TZ16_CODE))

zone_volume <- opal_all %>%
  filter(!is.na(t_zcode_on)) %>%
  group_by(snapshot, mode, t_zcode_on) %>%
  summarise(total = sum(count), .groups = 'drop')

#merge two datasets
tz_map_full <- tz_shp_wgs %>%
  left_join(zone_volume, by = c('TZ16_CODE' = 't_zcode_on'))

tz_map_full$total[is.na(tz_map_full$total)] <- 0

#zoom in the map
sydney_map <- st_crop(
  tz_map_full,
  xmin = 150.5, xmax = 151.5,
  ymin = -34.2, ymax = -33.6
)

#hit a complicate problem here while trying to deploy the website, shiny for some reason can not deal with 'sf' package
#leave me no choice but change data to a dataframe.
sydney_map1 <- sydney_map %>%
  st_cast('MULTIPOLYGON') %>%
  st_cast('POLYGON') %>%
  st_cast('LINESTRING') %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(long = X, lat = Y) 

sydney_map1 <- sydney_map1 %>%
  mutate(TZ16_CODE = sydney_map$TZ16_CODE[.[["L1"]]],
         total = sydney_map$total[.[["L1"]]],
         snapshot = sydney_map$snapshot[.[["L1"]]],
         mode = sydney_map$mode[.[["L1"]]])

glimpse(sydney_map)
glimpse(sydney_map1)


saveRDS(sydney_map, 'sydney_map.rds')
saveRDS(sydney_map1, "sydney_map1.rds")

