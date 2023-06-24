Data Collection
================
Antoine Thomas
8/3/23

Data collection involves identifying and consolidating the data sources
required. For air quality (AQ) prediction, historical data on air
pollutants are the most important. In addition, coherent data can be
brought in. AQ is known to be strongly influenced by the local weather,
but also by factors such as traffic intensity, local road construction
sites, ongoing construction projects or industrial enterprises operating
in the surrounding area. AQ monitoring data, being particulate matter
(PM<sub>2.5</sub>/PM<sub>10</sub>), ozone (O<sub>3</sub>) and nitrogen
dioxide (NO<sub>2</sub>), from the city of Berlin and weather data from
the open source provider *open-meteo.com* were selected as primary data
sources. In addition, data from the Berlin traffic detection (*Berlin
Open Data*) were included.

### Reading in air quality monitoring data

``` r
# Reading in the master data for air quality measurement stations
airquality_stations <- GET("https://luftdaten.berlin.de/api/stations") %>% 
  parse_json(simplifyVector = T) %>% 
  as_tibble() %>%
  mutate(lat = as.numeric(lat),
         lng = as.numeric(lng),
         stationgroups = unlist(stationgroups),
         components = as.character(components),
         activeComponents = as.character(activeComponents),
         partials = unlist(partials),
         lqis = as.character(lqis),
         exceeds = as.character(exceeds))

# Reading in the names/paths of the files to be imported
files_pm25 <- list.files(path = "Daten/Luftguetemessung/Feinstaub_PM2")
files_pm10 <- list.files(path = "Daten/Luftguetemessung/Feinstaub_PM10")
files_O3 <- list.files(path = "Daten/Luftguetemessung/Ozon_O3")
files_NO2 <- list.files(path = "Daten/Luftguetemessung/Stickstoffdioxid_NO2")

# Function to read in monitoring data of a single pollutant data set
read_pollutants <- function(x, folder) {
  read_csv2(file = paste0("Daten/Luftguetemessung/", folder, "/", x)) %>%
    .[-(1:3),] %>%
    mutate(Station = dmy_hm(Station))
}

# Reading in data monitoring PM2.5 
values_pm25 <- sapply(files_pm25, read_pollutants, folder = "Feinstaub_PM2") %>%
  bind_rows() %>%
  unique() %>%
  rename("date" = Station) %>%
  gather(key = "Station", value = "pm25", 2:15)

# Reading in data monitoring PM10
values_pm10 <- sapply(files_pm10, read_pollutants, folder = "Feinstaub_PM10") %>%
  bind_rows() %>%
  unique() %>%
  rename("date" = Station) %>%
  gather(key = "Station", value = "pm10", 2:15)

# Reading in data monitoring O3
values_O3 <- sapply(files_O3, read_pollutants, folder = "Ozon_O3") %>%
  bind_rows() %>%
  unique() %>%
  rename("date" = Station) %>%
  gather(key = "Station", value = "O3", 2:11)

# Reading in data monitoring NO2
values_NO2 <- sapply(files_NO2, read_pollutants, folder = "Stickstoffdioxid_NO2") %>%
  bind_rows() %>%
  unique() %>%
  rename("date" = Station) %>%
  gather(key = "Station", value = "NO2", 2:21)


## Gathering all air quality monitoring data  
airquality_df <- purrr::reduce(list(values_pm25,
                                values_pm10,
                                values_O3,
                                values_NO2), dplyr::full_join, by = c("date", "Station")) %>%
  mutate(pm25 = as.numeric(pm25),
         pm10 = as.numeric(pm10),
         O3 = as.numeric(O3),
         NO2 = as.numeric(NO2))

## Removing not needed data and functions
remove(values_pm25,
       values_pm10,
       values_O3,
       values_NO2,
       files_NO2,
       files_O3,
       files_pm10,
       files_pm25,
       read_pollutants)
```

After the import process, a data set `airquality_df` with AQ data for
the period from January 2016 to April 2023 is available. In addition, a
master data set `monitoring_stations` is available, which contains basic
and general information about the various monitoring stations in Berlin.

``` r
glimpse(airquality_df)
```

    Rows: 1,401,480
    Columns: 6
    $ date    <dttm> 2017-01-01 01:00:00, 2017-01-01 02:00:00, 2017-01-01 03:00:00…
    $ Station <chr> "010 Wedding", "010 Wedding", "010 Wedding", "010 Wedding", "0…
    $ pm25    <dbl> 175, 99, 63, 29, 20, 20, 26, 27, 29, 23, 19, 17, 15, 14, 14, 1…
    $ pm10    <dbl> 185, 104, 67, 31, 22, 23, 28, 30, 32, 25, 21, 20, 17, 16, 17, …
    $ O3      <dbl> 8, 19, 22, 32, 34, 30, 26, 26, 25, 34, 38, 39, 41, 43, 39, 36,…
    $ NO2     <dbl> 48, 37, NA, NA, 21, 24, 24, 25, 26, 22, 22, 21, 21, 19, 22, 24…

``` r
glimpse(airquality_stations)
```

    Rows: 20
    Columns: 18
    $ name             <chr> "010 Wedding", "018 Schöneberg", "027 Marienfelde", "…
    $ code             <chr> "mc010", "mc018", "mc027", "mc032", "mc042", "mc077",…
    $ codeEu           <chr> "DEBE010", "DEBE018", "DEBE027", "DEBE032", "DEBE034"…
    $ address          <chr> "13353 Wedding, Amrumer Str./Limburger Str.", "10823 …
    $ lat              <dbl> 52.54291, 52.48579, 52.39840, 52.47319, 52.48945, 52.…
    $ lng              <dbl> 13.34926, 13.34885, 13.36807, 13.22514, 13.43084, 13.…
    $ active           <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,…
    $ stationgroups    <chr> "background", "background", "suburb", "suburb", "back…
    $ measuringStart   <chr> "1984-11-01T00:00:00+01:00", "1986-11-01T00:00:00+01:…
    $ measuringEnd     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ measuringHeight  <int> NA, NA, NA, 3, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ url              <chr> "http://www.stadtentwicklung.berlin.de/umwelt/umwelta…
    $ information      <chr> "Die Station befindet sich an der Beuth Hochschule fü…
    $ components       <chr> "c(\"pm10_1h\", \"pm10_24h\", \"pm10_24hg\", \"pm10_1…
    $ activeComponents <chr> "c(\"pm10_1h\", \"pm10_24h\", \"pm10_24hg\", \"pm10_1…
    $ partials         <chr> "pm2_gravi_24h", "pm2_gravi_1m", "pm2_gravi_1y", "pm2…
    $ lqis             <chr> "c(\"lqi\", \"o3\", \"pm10\", \"no2\")", "c(\"lqi\", …
    $ exceeds          <chr> "c(\"no2_1h\", \"o3_1h\", \"o3_8hg\", \"pm10_24h\", \…

### Reading in weather data

``` r
# Variables to import:
# - hourly: temperature, humidity, dewpoint, surface pressure, precipitation, 
# windspeed, winddirection
# - daily: sunrise time, sunset time
# Timeframe: 01.01.2016 - 30.05.2023
weather_df_hourly <- GET("https://archive-api.open-meteo.com/v1/archive?latitude=52.52&longitude=13.41&start_date=2016-01-01&end_date=2023-05-30&hourly=temperature_2m,relativehumidity_2m,dewpoint_2m,surface_pressure,precipitation,windspeed_100m,winddirection_100m&timezone=Europe%2FBerlin&windspeed_unit=ms") %>% 
  parse_json(simplifyVector = T)

# Reading in daily weather data
weather_df_daily <- GET("https://archive-api.open-meteo.com/v1/archive?latitude=52.52&longitude=13.41&start_date=2016-01-01&end_date=2023-05-30&daily=sunrise,sunset&timezone=Europe%2FBerlin") %>% 
  parse_json(simplifyVector = T)

# Wrangling data from the API to a consistent data frame
weather_df_hourly$hourly_units <- names(weather_df_hourly$hourly_units)
weather_df_hourly <- weather_df_hourly %>%  
  as_tibble() %>%
  spread(hourly_units, hourly) %>% # change df to long format 
  unnest(cols = names(.)[7:length(names(.))]) %>% # unnest weather data variables
  relocate(time, .after = elevation) %>%
  mutate(time = ymd_hm(time)) %>%
  select(-latitude, -longitude, -utc_offset_seconds, -generationtime_ms, -timezone, -timezone_abbreviation, -elevation)

weather_df_daily$daily_units <- names(weather_df_daily$daily_units)
weather_df_daily <- weather_df_daily %>%  
  as_tibble() %>%
  spread(daily_units, daily) %>% # change df to long format 
  unnest(cols = names(.)[7:length(names(.))]) %>% # unnest weather data variables
  relocate(time, .after = elevation) %>%
  mutate(time = ymd(time),
         sunrise = ymd_hm(sunrise),
         sunset = ymd_hm(sunset),
         # Compute duration of sunlight on a specific day based on sunrise and sunset times
         duration_sunlight = as.numeric(difftime(sunset, sunrise, units="mins"))) %>%
  select(time, duration_sunlight)

# Gathering hourly and daily weather data
weather_df <- weather_df_hourly %>%
  mutate(date = floor_date(time, unit = "days")) %>%
  left_join(weather_df_daily, by = c("date" = "time")) %>%
  select(-date)

# Removing not needed data
remove(weather_df_daily,
       weather_df_hourly)
```

After the import process, a data set `weather_df` with hourly and daily
weather data for the period from January 2016 to May 2023 is available.

``` r
glimpse(weather_df)
```

    Rows: 64,968
    Columns: 9
    $ time                <dttm> 2016-01-01 00:00:00, 2016-01-01 01:00:00, 2016-01…
    $ dewpoint_2m         <dbl> 1.1, 1.3, 1.3, 1.2, 0.9, 0.7, 0.7, 0.5, 0.1, -0.3,…
    $ precipitation       <dbl> 0.1, 0.2, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, …
    $ relativehumidity_2m <int> 95, 95, 96, 97, 98, 98, 98, 97, 98, 98, 98, 98, 95…
    $ surface_pressure    <dbl> 1020.1, 1020.1, 1020.2, 1020.0, 1020.3, 1020.4, 10…
    $ temperature_2m      <dbl> 1.8, 2.0, 1.9, 1.7, 1.3, 1.1, 1.0, 0.9, 0.4, -0.1,…
    $ winddirection_100m  <int> 238, 230, 225, 228, 234, 243, 253, 260, 265, 261, …
    $ windspeed_100m      <dbl> 3.55, 3.12, 3.25, 3.75, 4.08, 4.70, 5.03, 4.67, 4.…
    $ duration_sunlight   <dbl> 469, 469, 469, 469, 469, 469, 469, 469, 469, 469, …

### Reading in traffic data

``` r
# Reading in traffic detector master data
traffic_detectors <- readxl::read_xlsx("Daten/Verkehrsdaten/Stammdaten_Verkehrsdetektion.xlsx") %>%
  select(MQ_KURZNAME, # mq = Messquerschnitt
         STRASSE,
         POSITION,
         POS_DETAIL,
         RICHTUNG,
         `LÄNGE (WGS84)`,
         `BREITE (WGS84)`,
         INBETRIEBNAHME,
         DEINSTALLIERT,
         KOMMENTAR) %>%
  rename("cs_shortname" = MQ_KURZNAME, #cs = cross-section
         "street" = STRASSE,
         "position" = POSITION,
         "position_detail" = POS_DETAIL,
         "direction" = RICHTUNG,
         "lng" = `LÄNGE (WGS84)`,
         "lat" = `BREITE (WGS84)`,
         "launch_date" = INBETRIEBNAHME,
         "uninstallment_date" = DEINSTALLIERT,
         "comment" = KOMMENTAR) %>%
  unique()


plan(multisession, workers = 4)
# Create a dataframe for each month and year from January 2017 to April 2023
traffic_df <- expand.grid(c(01:12),c(2017:2023)) %>%
  as_tibble() %>%
  filter(!(Var1 > 4 & Var2 == 2023)) %>%
  mutate(Var1 = ifelse(Var1 < 10, paste0("0",Var1), Var1),
         # Mutating all file URLs - File names tend to be slightly different
         # depending on the year.
         file_url = paste0("https://mdhopendata.blob.core.windows.net/verkehrsdetektion/", Var2, "/Messquerschnitt%20(fahrtrichtungsbezogen)/mq_hr_", Var2, "_", Var1, ".csv.gz")) %>%
  mutate(file_url = ifelse(Var2 == 2021, str_replace(file_url, "%20\\(fahrtrichtungsbezogen\\)", ""),file_url),
         file_url = ifelse(Var2 == 2023, str_replace(file_url, "Messquerschnitt", "Messquerschnitte"),file_url),
         data = future_map(.x = file_url, .f = read_csv2)) %>%
  pull(data) %>%
  bind_rows() %>%
  mutate(date = as.Date(tag, format = "%d.%m.%Y") + hours(stunde)) %>%
  relocate(date, .after = mq_name) %>%
  select(-tag, -stunde) %>%
  rename("cs_shortname" = mq_name,
         "quality" = qualitaet)
```

After the import process, a data set `traffic_df` with hourly traffic
data for the period from January 2017 to April 2023 is available. In
addition, a master data set `traffic_detectors` is available, which
contains basic and general information about the various traffic
detection sensors in Berlin.

``` r
glimpse(traffic_df)
```

    Rows: 12,465,926
    Columns: 9
    $ cs_shortname <chr> "TE001", "TE001", "TE001", "TE001", "TE001", "TE001", "TE…
    $ date         <dttm> 2017-01-01 00:00:00, 2017-01-01 01:00:00, 2017-01-01 02:…
    $ quality      <dbl> 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 1…
    $ q_kfz_mq_hr  <dbl> 87, 319, 271, 180, 111, 77, 98, 108, 166, 317, 808, 1287,…
    $ v_kfz_mq_hr  <dbl> 77, 74, 80, 83, 88, 86, 78, 73, 68, 72, 63, 65, 88, 78, 7…
    $ q_pkw_mq_hr  <dbl> 86, 311, 268, 174, 108, 73, 93, 105, 147, 306, 781, 1269,…
    $ v_pkw_mq_hr  <dbl> 78, 76, 81, 84, 89, 86, 78, 73, 68, 72, 62, 65, 89, 78, 7…
    $ q_lkw_mq_hr  <dbl> 1, 8, 3, 6, 3, 4, 5, 3, 19, 11, 27, 18, 28, 22, 22, 17, 2…
    $ v_lkw_mq_hr  <dbl> 0, 11, 3, 56, 30, 83, 75, 83, 74, 73, 77, 76, 52, 63, 83,…

``` r
glimpse(traffic_detectors)
```

    Rows: 314
    Columns: 10
    $ cs_shortname       <chr> "TE001", "TE002", "TE004", "TE005", "TE006", "TE009…
    $ street             <chr> "A115", "A115", "Clayallee", "Berliner Straße", "Te…
    $ position           <chr> "AS Spanische Allee – Brücke", "AS Spanische Allee …
    $ position_detail    <chr> "AK Zehlendorf", "AD Funkturm", "Potsdamer Chaussee…
    $ direction          <chr> "Südwest", "Nordost", "Süd", "West", "Nord", "Nordo…
    $ lng                <dbl> 13.19258, 13.19275, 13.26130, 13.26311, 13.25988, 1…
    $ lat                <dbl> 52.43387, 52.43381, 52.43664, 52.43511, 52.43374, 5…
    $ launch_date        <dttm> 2003-02-18, 2003-02-18, 2003-02-18, 2003-02-19, 20…
    $ uninstallment_date <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    $ comment            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…

### Gathering and exporting data

In order to make effective use of the imported data, it is necessary to
combine and merge individual data sets. In particular, weather data must
be combined with air quality data because they will always be used
together in further analysis. Traffic data is not combined with air
quality or weather data in the data collection step. The first step is
to identify individual air quality monitoring stations that could be
considered for such a link.

``` r
# Joining air quality data with weather data
air_weather_df <- airquality_df %>%
  left_join(weather_df, by = c("date" = "time")) %>%
  left_join(airquality_stations %>% select(name, stationgroups), by = c("Station" = "name")) %>%
  relocate(stationgroups, .after = Station)


# Exporting the combined dataset as .csv  
write.csv2(air_weather_df, file = "Daten/DataCollection/air_weather_df.csv")

# Exporting other data sets as .csv
write.csv2(traffic_df, file = "Daten/DataCollection/traffic_df.csv")
write.csv2(traffic_detectors, file = "Daten/DataCollection/traffic_detectors.csv")
write.csv2(airquality_stations, file = "Daten/DataCollection/airquality_stations.csv")
```

After the gathering process, a data set `air_weather_df` combining air
quality measurement data and weather data is available. This data set is
exported as .csv for usage within the following analysis. The previously
collected data such as `traffic_df`, `traffic_detectors` and
`airquality_stations` are exported as well.

### Illustration of air quality monitoring stations and traffic sensors

![](data_collection_files/figure-commonmark/Illustration%20air%20quality%20and%20traffic%20sensors-1.png)

### Session info

    R version 4.2.1 (2022-06-23)
    Platform: aarch64-apple-darwin20 (64-bit)
    Running under: macOS Ventura 13.3

    Matrix products: default
    BLAS:   /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRblas.0.dylib
    LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib

    locale:
    [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
     [1] ggmap_3.0.1        RColorBrewer_1.1-3 furrr_0.3.1        future_1.28.0     
     [5] lubridate_1.8.0    jsonlite_1.8.3     httr_1.4.4         forcats_0.5.2     
     [9] stringr_1.4.1      dplyr_1.1.2        purrr_0.3.5        readr_2.1.3       
    [13] tidyr_1.2.1        tibble_3.2.1       ggplot2_3.4.0      tidyverse_1.3.2   

    loaded via a namespace (and not attached):
     [1] bit64_4.0.5         vroom_1.6.0         modelr_0.1.9       
     [4] assertthat_0.2.1    sp_1.5-1            googlesheets4_1.0.1
     [7] cellranger_1.1.0    yaml_2.3.6          globals_0.16.1     
    [10] pillar_1.9.0        backports_1.4.1     lattice_0.20-45    
    [13] glue_1.6.2          digest_0.6.30       rvest_1.0.3        
    [16] colorspace_2.0-3    htmltools_0.5.5     plyr_1.8.7         
    [19] pkgconfig_2.0.3     broom_1.0.1         listenv_0.8.0      
    [22] haven_2.5.1         scales_1.2.1        jpeg_0.1-9         
    [25] tzdb_0.3.0          googledrive_2.0.0   farver_2.1.1       
    [28] generics_0.1.3      ellipsis_0.3.2      withr_2.5.0        
    [31] cli_3.6.1           magrittr_2.0.3      crayon_1.5.2       
    [34] readxl_1.4.1        evaluate_0.17       fs_1.6.2           
    [37] fansi_1.0.3         parallelly_1.32.1   xml2_1.3.3         
    [40] tools_4.2.1         hms_1.1.2           RgoogleMaps_1.4.5.3
    [43] gargle_1.2.1        lifecycle_1.0.3     munsell_0.5.0      
    [46] reprex_2.0.2        compiler_4.2.1      rlang_1.1.1        
    [49] grid_4.2.1          rstudioapi_0.14     labeling_0.4.2     
    [52] bitops_1.0-7        rmarkdown_2.17      gtable_0.3.1       
    [55] codetools_0.2-18    DBI_1.1.3           curl_4.3.3         
    [58] R6_2.5.1            knitr_1.40          bit_4.0.4          
    [61] fastmap_1.1.0       utf8_1.2.2          stringi_1.7.8      
    [64] parallel_4.2.1      Rcpp_1.0.9          vctrs_0.6.3        
    [67] png_0.1-7           dbplyr_2.2.1        tidyselect_1.2.0   
    [70] xfun_0.39          
