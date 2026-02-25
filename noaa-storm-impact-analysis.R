######################## LIBRARIES

library(data.table)   # read large .bz2 file faster than base R
library(lubridate)    # parse and manipulate date variables
library(dplyr)        # data manipulation and filtering
library(ggplot2)      # plotting
library(gridExtra)    # arrange multiple plots in a grid
library(grid)         # text grob for plot titles
library(gganimate)    # animate ggplot objects
library(gifski)       # render gganimate output as GIF
library(plotly)       # interactive choropleth maps
library(htmlwidgets)  # save interactive plots as standalone HTML


######################## LOAD DATA

file_url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(file_url, "stormData.bz2", method = "curl")

storm_raw = fread("stormData.bz2", na.strings = "?")


######################## REDUCE DATA

# Keep only post-1995 records -- earlier years are incomplete and skew toward
# tornado-only data, which would falsely inflate tornado impact rankings.
# Full data runs 1950-2011; analysis uses 1996-2011.

storm_raw$BGN_DATE = mdy_hms(storm_raw$BGN_DATE)
storm_sub = subset(storm_raw, BGN_DATE > "1995-12-31")

# Retain columns needed for analysis -- include STATE and year for map and temporal
storm_sub = storm_sub |>
    dplyr::select(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP,
                  CROPDMG, CROPDMGEXP, STATE, BGN_DATE)

storm_sub$year = year(storm_sub$BGN_DATE)


######################## WRANGLE DAMAGE DATA

# Property and crop damage are each split across two columns:
# a numeric value and a string exponent (K = thousands, M = millions, B = billions).
# One data entry error (exponent recorded as "0") is removed.
# All relevant columns are lowercased for consistent handling downstream.

table(storm_sub$PROPDMGEXP)
table(storm_sub$CROPDMGEXP)

storm_sub = storm_sub[!(storm_sub$PROPDMGEXP == "0"), ]

names(storm_sub)     = tolower(names(storm_sub))
storm_sub$evtype     = tolower(storm_sub$evtype)
storm_sub$propdmgexp = tolower(storm_sub$propdmgexp)
storm_sub$cropdmgexp = tolower(storm_sub$cropdmgexp)

storm_sub = storm_sub |>
    mutate(
        propdmgexp2 = case_when(
            propdmgexp == "b" ~ 1000000000,
            propdmgexp == "m" ~ 1000000,
            propdmgexp == "k" ~ 1000
        ),
        cropdmgexp2 = case_when(
            cropdmgexp == "b" ~ 1000000000,
            cropdmgexp == "m" ~ 1000000,
            cropdmgexp == "k" ~ 1000
        ),
        damage_prop = propdmg * propdmgexp2,
        damage_crop = cropdmg * cropdmgexp2
    )

storm_clean = storm_sub |>
    dplyr::select(evtype, fatalities, injuries, damage_prop, damage_crop, state, year)


######################## WRANGLE EVENT TYPES

# The raw data has 400+ unique evtype values due to data entry inconsistencies. NOAA's official storm data documentation (Table 2.1.1) lists 48 event types.

storm_clean = storm_clean |> arrange(evtype)

storm = storm_clean |>
    mutate(evtype = case_when(
        evtype == "   high surf advisory"           ~ "high surf",
        evtype == " coastal flood"                  ~ "flood",
        evtype == " flash flood"                    ~ "flood",
        evtype == " lightning"                      ~ "other",
        evtype == " tstm wind"                      ~ "thunderstorm wind",
        evtype == " tstm wind (g45)"                ~ "thunderstorm wind",
        evtype == " waterspout"                     ~ "other",
        evtype == " wind"                           ~ "other",
        evtype == "abnormal warmth"                 ~ "heat",
        evtype == "abnormally dry"                  ~ "drought",
        evtype == "abnormally wet"                  ~ "other",
        evtype == "accumulated snowfall"            ~ "winter weather",
        evtype == "agricultural freeze"             ~ "winter weather",
        evtype == "astronomical high tide"          ~ "astronomical tide",
        evtype == "astronomical low tide"           ~ "astronomical tide",
        evtype == "beach erosion"                   ~ "other",
        evtype == "bitter wind chill"               ~ "extreme cold/wind chill",
        evtype == "bitter wind chill temperatures"  ~ "extreme cold/wind chill",
        evtype == "black ice"                       ~ "ice",
        evtype == "blizzard summary"                ~ "blizzard",
        evtype == "blow-out tide"                   ~ "astronomical tide",
        evtype == "blow-out tides"                  ~ "astronomical tide",
        evtype == "blowing dust"                    ~ "dust storm",
        evtype == "blowing snow"                    ~ "winter weather",
        evtype == "brush fire"                      ~ "wildfire",
        evtype == "coastal  flooding/erosion"       ~ "flood",
        evtype == "coastal erosion"                 ~ "other",
        evtype == "coastal flood"                   ~ "flood",
        evtype == "coastal flooding"                ~ "flood",
        evtype == "coastal flooding/erosion"        ~ "flood",
        evtype == "coastal storm"                   ~ "heavy rain",
        evtype == "coastalflood"                    ~ "flood",
        evtype == "coastalstorm"                    ~ "heavy rain",
        evtype == "cold"                            ~ "extreme cold/wind chill",
        evtype == "cold and frost"                  ~ "frost/freeze",
        evtype == "cold and snow"                   ~ "winter weather",
        evtype == "cold temperature"                ~ "extreme cold/wind chill",
        evtype == "cold temperatures"               ~ "extreme cold/wind chill",
        evtype == "cold weather"                    ~ "winter weather",
        evtype == "cold wind chill temperatures"    ~ "extreme cold/wind chill",
        evtype == "cold/wind chill"                 ~ "extreme cold/wind chill",
        evtype == "cool spell"                      ~ "extreme cold/wind chill",
        evtype == "cstl flooding/erosion"           ~ "flood",
        evtype == "dam break"                       ~ "flood",
        evtype == "damaging freeze"                 ~ "frost/freeze",
        evtype == "downburst"                       ~ "heavy wind",
        evtype == "driest month"                    ~ "drought",
        evtype == "drifting snow"                   ~ "winter weather",
        evtype == "drowning"                        ~ "other",
        evtype == "dry"                             ~ "drought",
        evtype == "dry conditions"                  ~ "drought",
        evtype == "dry microburst"                  ~ "high wind",
        evtype == "dry spell"                       ~ "drought",
        evtype == "dry weather"                     ~ "drought",
        evtype == "dryness"                         ~ "drought",
        evtype == "dust devel"                      ~ "dust devil",
        evtype == "early frost"                     ~ "frost/freeze",
        evtype == "early rain"                      ~ "other",
        evtype == "early snowfall"                  ~ "winter weather",
        evtype == "erosion/cstl flood"              ~ "flood",
        evtype == "excessive cold"                  ~ "extreme cold/wind chill",
        evtype == "excessive rain"                  ~ "heavy rain",
        evtype == "excessive rainfall"              ~ "heavy rain",
        evtype == "excessive snow"                  ~ "heavy snow",
        evtype == "excessively dry"                 ~ "drought",
        evtype == "extended cold"                   ~ "extreme cold/wind chill",
        evtype == "extreme cold"                    ~ "extreme cold/wind chill",
        evtype == "extreme wind chill"              ~ "extreme cold/wind chill",
        evtype == "extreme windchill"               ~ "extreme cold/wind chill",
        evtype == "extreme windchill temperatures"  ~ "extreme cold/wind chill",
        evtype == "extremely wet"                   ~ "other",
        evtype == "falling snow/ice" ~               "winter weather",
        evtype == "first frost"                     ~ "frost/freeze",
        evtype == "first snow"                      ~ "winter weather",
        evtype == "flash flood"                     ~ "flood",
        evtype == "flash flood/flood"               ~ "flood",
        evtype == "flash flooding"                  ~ "flood",
        evtype == "flood/flash flood"               ~ "flood",
        evtype == "flood/flash/flood"               ~ "flood",
        evtype == "flood/strong wind"               ~ "flood",
        evtype == "fog"                             ~ "dense fog",
        evtype == "freeze"                          ~ "frost/freeze",
        evtype == "freezing drizzle"                ~ "sleet",
        evtype == "freezing rain"                   ~ "sleet",
        evtype == "freezing rain/sleet"             ~ "sleet",
        evtype == "freezing spray"                  ~ "sleet",
        evtype == "frost"                           ~ "frost/freeze",
        evtype == "funnel clouds"                   ~ "funnel cloud",
        evtype == "glaze"                           ~ "other",
        evtype == "gradient wind"                   ~ "heavy wind",
        evtype == "gusty lake wind"                 ~ "heavy wind",
        evtype == "gusty thunderstorm wind"         ~ "thunderstorm wind",
        evtype == "gusty thunderstorm winds"        ~ "thunderstorm wind",
        evtype == "gusty wind"                      ~ "heavy wind",
        evtype == "gusty winds"                     ~ "heavy wind",
        evtype == "hail(0.75)"                      ~ "hail",
        evtype == "hard freeze"                     ~ "frost/freeze",
        evtype == "hazardous surf"                  ~ "high surf",
        evtype == "heat"                            ~ "heat",
        evtype == "heat wave"                       ~ "heat",
        evtype == "heatburst"                       ~ "heat",
        evtype == "heavy precipitation"             ~ "heavy rain",
        evtype == "heavy rain effects"              ~ "heavy rain",
        evtype == "heavy rainfall"                  ~ "heavy rain",
        evtype == "heavy seas"                      ~ "high surf",
        evtype == "heavy snow shower"               ~ "heavy snow",
        evtype == "heavy snow squalls"              ~ "heavy snow",
        evtype == "heavy surf"                      ~ "high surf",
        evtype == "heavy surf/high surf"            ~ "high surf",
        evtype == "high  swells"                    ~ "high surf",
        evtype == "high seas"                       ~ "high surf",
        evtype == "high surf advisories"            ~ "high surf",
        evtype == "high surf advisory"              ~ "high surf",
        evtype == "high swells"                     ~ "high surf",
        evtype == "high water"                      ~ "flood",
        evtype == "high waves"                      ~ "high surf",
        evtype == "high wind (g40)"                 ~ "high wind",
        evtype == "high wind and seas"              ~ "high wind",
        evtype == "high wind damage"                ~ "high wind",
        evtype == "high winds"                      ~ "high wind",
        evtype == "high winds heavy rains"          ~ "high wind",
        evtype == "hurricane"                       ~ "hurricane/typhoon",
        evtype == "hurricane edouard"               ~ "hurricane/typhoon",
        evtype == "hurricane emily"                 ~ "hurricane/typhoon",
        evtype == "hurricane erin"                  ~ "hurricane/typhoon",
        evtype == "hurricane felix"                 ~ "hurricane/typhoon",
        evtype == "hurricane opal"                  ~ "hurricane/typhoon",
        evtype == "hurricane opal/high winds"       ~ "hurricane/typhoon",
        evtype == "hyperthermia/exposure"           ~ "heat",
        evtype == "hypothermia"                     ~ "extreme cold/wind chill",
        evtype == "hypothermia/exposure"            ~ "extreme cold/wind chill",
        evtype == "ice"                             ~ "ice",
        evtype == "ice floes"                       ~ "ice",
        evtype == "ice fog"                         ~ "freezing fog",
        evtype == "ice jam flood (minor"            ~ "flood",
        evtype == "ice on road"                     ~ "ice",
        evtype == "ice roads"                       ~ "ice",
        evtype == "ice storm/flash flood"           ~ "ice storm",
        evtype == "icy roads"                       ~ "ice",
        evtype == "lack of snow"                    ~ "other",
        evtype == "lake effect snow"                ~ "lake-effect snow",
        evtype == "lake flood"                      ~ "flood",
        evtype == "lakeshore flood"                 ~ "flood",
        evtype == "landslide"                       ~ "other",
        evtype == "landslides"                      ~ "other",
        evtype == "landslump"                       ~ "other",
        evtype == "large wall cloud"                ~ "other",
        evtype == "late freeze"                     ~ "frost/freeze",
        evtype == "late season snow"                ~ "winter weather",
        evtype == "late snow"                       ~ "winter weather",
        evtype == "light freezing rain"             ~ "sleet",
        evtype == "light snow"                      ~ "winter weather",
        evtype == "light snowfall"                  ~ "winter weather",
        evtype == "lighting"                        ~ "lightning",
        evtype == "ligntning"                       ~ "lightning",
        evtype == "low temperature"                 ~ "extreme cold/wind chill",
        evtype == "low temperature record"          ~ "extreme cold/wind chill",
        evtype == "major flood"                     ~ "flood",
        evtype == "marine accident"                 ~ "other",
        evtype == "marine mishap"                   ~ "other",
        evtype == "metro storm, may 26"             ~ "other",
        evtype == "microburst"                      ~ "thunderstorm wind",
        evtype == "microburst winds"                ~ "thunderstorm wind",
        evtype == "mild and dry pattern"            ~ "other",
        evtype == "mild pattern"                    ~ "other",
        evtype == "mild/dry pattern"                ~ "other",
        evtype == "minor flooding"                  ~ "flood",
        evtype == "mixed precip"                    ~ "winter weather",
        evtype == "mixed precipitation"             ~ "winter weather",
        evtype == "monthly precipitation"           ~ "other",
        evtype == "monthly rainfall"                ~ "other",
        evtype == "monthly snowfall"                ~ "other",
        evtype == "monthly temperature"             ~ "other",
        evtype == "mountain snows"                  ~ "heavy snow",
        evtype == "mud slide"                       ~ "other",
        evtype == "mud slides"                      ~ "other",
        evtype == "mud slides urban flooding"       ~ "other",
        evtype == "mudslide"                        ~ "other",
        evtype == "mudslides"                       ~ "other",
        evtype == "near record snow"                ~ "heavy snow",
        evtype == "no severe weather"               ~ "other",
        evtype == "non severe hail"                 ~ "hail",
        evtype == "non-severe wind damage"          ~ "heavy wind",
        evtype == "non-tstm wind"                   ~ "heavy wind",
        evtype == "none"                            ~ "other",
        evtype == "northern lights"                 ~ "other",
        evtype == "other"                           ~ "other",
        evtype == "patchy dense fog"                ~ "dense fog",
        evtype == "patchy ice"                      ~ "ice",
        evtype == "prolong cold"                    ~ "extreme cold/wind chill",
        evtype == "prolong cold/snow"               ~ "winter weather",
        evtype == "prolong warmth"                  ~ "heat",
        evtype == "prolonged rain"                  ~ "heavy rain",
        evtype == "rain"                            ~ "heavy rain",
        evtype == "rain (heavy)"                    ~ "heavy rain",
        evtype == "rain and wind"                   ~ "heavy rain",
        evtype == "rain damage"                     ~ "heavy rain",
        evtype == "rain/snow"                       ~ "winter weather",
        evtype == "rain/wind"                       ~ "heavy rain",
        evtype == "rainstorm"                       ~ "heavy rain",
        evtype == "rapidly rising water"            ~ "flood",
        evtype == "record cold"                     ~ "extreme cold/wind chill",
        evtype == "record cold and high wind"       ~ "extreme cold/wind chill",
        evtype == "record cool"                     ~ "extreme cold/wind chill",
        evtype == "record dry month"                ~ "drought",
        evtype == "record dryness"                  ~ "drought",
        evtype == "record heat"                     ~ "heat",
        evtype == "record heat wave"                ~ "heat",
        evtype == "record high"                     ~ "heat",
        evtype == "record high temperature"         ~ "heat",
        evtype == "record high temperatures"        ~ "heat",
        evtype == "record low"                      ~ "extreme cold/wind chill",
        evtype == "record low rainfall"             ~ "drought",
        evtype == "record may snow"                 ~ "heavy snow",
        evtype == "record precipitation"            ~ "heavy rain",
        evtype == "record rainfall"                 ~ "heavy rain",
        evtype == "record snow"                     ~ "heavy snow",
        evtype == "record snow/cold"                ~ "winter weather",
        evtype == "record snowfall"                 ~ "heavy snow",
        evtype == "record temperature"              ~ "other",
        evtype == "record temperatures"             ~ "other",
        evtype == "record warm"                     ~ "heat",
        evtype == "record warm temps."              ~ "heat",
        evtype == "record warmth"                   ~ "heat",
        evtype == "record winter snow"              ~ "heavy snow",
        evtype == "red flag criteria"               ~ "other",
        evtype == "remnants of floyd"               ~ "other",
        evtype == "rip current"                     ~ "rip current",
        evtype == "rip currents"                    ~ "rip current",
        evtype == "rip currents heavy surf"         ~ "rip current",
        evtype == "river and stream flood"          ~ "flood",
        evtype == "river flood"                     ~ "flood",
        evtype == "river flooding"                  ~ "flood",
        evtype == "rock slide"                      ~ "other",
        evtype == "rogue wave"                      ~ "high surf",
        evtype == "rough seas"                      ~ "high surf",
        evtype == "rough surf"                      ~ "high surf",
        evtype == "rural flood"                     ~ "flood",
        evtype == "saharan dust"                    ~ "other",
        evtype == "seasonal snowfall"               ~ "winter weather",
        evtype == "severe cold"                     ~ "extreme cold/wind chill",
        evtype == "severe thunderstorm"             ~ "thunderstorm wind",
        evtype == "severe thunderstorm winds"       ~ "thunderstorm wind",
        evtype == "severe thunderstorms"            ~ "thunderstorm wind",
        evtype == "severe turbulence"               ~ "other",
        evtype == "sleet storm"                     ~ "sleet",
        evtype == "sleet/ice storm"                 ~ "sleet",
        evtype == "small stream flood"              ~ "flood",
        evtype == "sml stream fld"                  ~ "flood",
        evtype == "smoke"                           ~ "dense smoke",
        evtype == "snow"                            ~ "heavy snow",
        evtype == "snow accumulation"               ~ "heavy snow",
        evtype == "snow advisory"                   ~ "winter weather",
        evtype == "snow and cold"                   ~ "winter weather",
        evtype == "snow and heavy snow"             ~ "heavy snow",
        evtype == "snow and ice"                    ~ "winter weather",
        evtype == "snow and ice storm"              ~ "winter weather",
        evtype == "snow and sleet"                  ~ "sleet",
        evtype == "snow and wind"                   ~ "winter weather",
        evtype == "snow drought"                    ~ "drought",
        evtype == "snow freezing rain"              ~ "sleet",
        evtype == "snow showers"                    ~ "winter weather",
        evtype == "snow sleet"                      ~ "sleet",
        evtype == "snow squall"                     ~ "heavy snow",
        evtype == "snow squalls"                    ~ "heavy snow",
        evtype == "snow/ bitter cold"               ~ "winter weather",
        evtype == "snow/ ice"                       ~ "winter weather",
        evtype == "snow/blowing snow"               ~ "heavy snow",
        evtype == "snow/cold"                       ~ "winter weather",
        evtype == "snow/freezing rain"              ~ "sleet",
        evtype == "snow/heavy snow"                 ~ "heavy snow",
        evtype == "snow/high winds"                 ~ "winter weather",
        evtype == "snow/ice"                        ~ "winter weather",
        evtype == "snow/ice storm"                  ~ "winter weather",
        evtype == "snow/rain"                       ~ "winter weather",
        evtype == "snow/rain/sleet"                 ~ "sleet",
        evtype == "snow/sleet"                      ~ "sleet",
        evtype == "snow/sleet/freezing rain"        ~ "sleet",
        evtype == "snow/sleet/rain"                 ~ "sleet",
        evtype == "snow\\cold"                      ~ "winter weather",
        evtype == "snowfall record"                 ~ "heavy snow",
        evtype == "snowmelt flooding"               ~ "flood",
        evtype == "snowstorm"                       ~ "winter storm",
        evtype == "southeast"                       ~ "other",
        evtype == "storm force winds"               ~ "heavy wind",
        evtype == "storm surge"                     ~ "storm surge/tide",
        evtype == "stream flooding"                 ~ "flood",
        evtype == "street flood"                    ~ "flood",
        evtype == "street flooding"                 ~ "flood",
        evtype == "strong wind"                     ~ "heavy wind",
        evtype == "strong wind gust"                ~ "heavy wind",
        evtype == "strong winds"                    ~ "heavy wind",
        evtype == "summary august 10"               ~ "other",
        evtype == "summary august 11"               ~ "other",
        evtype == "summary august 17"               ~ "other",
        evtype == "summary august 2-3"              ~ "other",
        evtype == "summary august 21"               ~ "other",
        evtype == "summary august 28"               ~ "other",
        evtype == "summary august 4"                ~ "other",
        evtype == "summary august 7"                ~ "other",
        evtype == "summary august 9"                ~ "other",
        evtype == "summary jan 17"                  ~ "other",
        evtype == "summary july 23-24"              ~ "other",
        evtype == "summary june 18-19"              ~ "other",
        evtype == "summary june 5-6"                ~ "other",
        evtype == "summary june 6"                  ~ "other",
        evtype == "summary of april 12"             ~ "other",
        evtype == "summary of april 13"             ~ "other",
        evtype == "summary of april 21"             ~ "other",
        evtype == "summary of april 27"             ~ "other",
        evtype == "summary of april 3rd"            ~ "other",
        evtype == "summary of august 1"             ~ "other",
        evtype == "summary of july 11"              ~ "other",
        evtype == "summary of july 2"               ~ "other",
        evtype == "summary of july 22"              ~ "other",
        evtype == "summary of july 26"              ~ "other",
        evtype == "summary of july 29"              ~ "other",
        evtype == "summary of july 3"               ~ "other",
        evtype == "summary of june 13"              ~ "other",
        evtype == "summary of june 15"              ~ "other",
        evtype == "summary of june 16"              ~ "other",
        evtype == "summary of june 18"              ~ "other",
        evtype == "summary of june 23"              ~ "other",
        evtype == "summary of june 24"              ~ "other",
        evtype == "summary of june 3"               ~ "other",
        evtype == "summary of june 30"              ~ "other",
        evtype == "summary of june 4"               ~ "other",
        evtype == "summary of june 6"               ~ "other",
        evtype == "summary of march 14"             ~ "other",
        evtype == "summary of march 23"             ~ "other",
        evtype == "summary of march 24"             ~ "other",
        evtype == "summary of march 24-25"          ~ "other",
        evtype == "summary of march 27"             ~ "other",
        evtype == "summary of march 29"             ~ "other",
        evtype == "summary of may 10"               ~ "other",
        evtype == "summary of may 13"               ~ "other",
        evtype == "summary of may 14"               ~ "other",
        evtype == "summary of may 22"               ~ "other",
        evtype == "summary of may 22 am"            ~ "other",
        evtype == "summary of may 22 pm"            ~ "other",
        evtype == "summary of may 26 am"            ~ "other",
        evtype == "summary of may 26 pm"            ~ "other",
        evtype == "summary of may 31 am"            ~ "other",
        evtype == "summary of may 31 pm"            ~ "other",
        evtype == "summary of may 9-10"             ~ "other",
        evtype == "summary sept. 25-26"             ~ "other",
        evtype == "summary september 20"            ~ "other",
        evtype == "summary september 23"            ~ "other",
        evtype == "summary september 3"             ~ "other",
        evtype == "summary september 4"             ~ "other",
        evtype == "summary: nov. 16"                ~ "other",
        evtype == "summary: nov. 6-7"               ~ "other",
        evtype == "summary: oct. 20-21"             ~ "other",
        evtype == "summary: october 31"             ~ "other",
        evtype == "summary: sept. 18"               ~ "other",
        evtype == "temperature record"              ~ "other",
        evtype == "thundersnow shower"              ~ "winter weather",
        evtype == "thunderstorm"                    ~ "heavy rain",
        evtype == "thunderstorm wind (g40)"         ~ "thunderstorm wind",
        evtype == "thunderstorms"                   ~ "heavy rain",
        evtype == "tidal flooding"                  ~ "flood",
        evtype == "tornado debris"                  ~ "tornado",
        evtype == "torrential rainfall"             ~ "heavy rain",
        evtype == "tstm"                            ~ "thunderstorm wind",
        evtype == "tstm heavy rain"                 ~ "thunderstorm wind",
        evtype == "tstm wind"                       ~ "thunderstorm wind",
        evtype == "tstm wind  (g45)"                ~ "thunderstorm wind",
        evtype == "tstm wind (41)"                  ~ "thunderstorm wind",
        evtype == "tstm wind (g35)"                 ~ "thunderstorm wind",
        evtype == "tstm wind (g40)"                 ~ "thunderstorm wind",
        evtype == "tstm wind (g45)"                 ~ "thunderstorm wind",
        evtype == "tstm wind 40"                    ~ "thunderstorm wind",
        evtype == "tstm wind 45"                    ~ "thunderstorm wind",
        evtype == "tstm wind and lightning"         ~ "thunderstorm wind",
        evtype == "tstm wind g45"                   ~ "thunderstorm wind",
        evtype == "tstm wind/hail"                  ~ "thunderstorm wind",
        evtype == "tstm winds"                      ~ "thunderstorm wind",
        evtype == "tstm wnd"                        ~ "thunderstorm wind",
        evtype == "typhoon"                         ~ "hurricane/typhoon",
        evtype == "unseasonable cold"               ~ "cold/wind chill",
        evtype == "unseasonably cold"               ~ "cold/wind chill",
        evtype == "unseasonably cool"               ~ "cold/wind chill",
        evtype == "unseasonably cool & wet"         ~ "cold/wind chill",
        evtype == "unseasonably dry"                ~ "drought",
        evtype == "unseasonably hot"                ~ "heat",
        evtype == "unseasonably warm"               ~ "heat",
        evtype == "unseasonably warm & wet"         ~ "heat",
        evtype == "unseasonably warm and dry"       ~ "heat",
        evtype == "unseasonably warm year"          ~ "heat",
        evtype == "unseasonably warm/wet"           ~ "heat",
        evtype == "unseasonably wet"                ~ "other",
        evtype == "unseasonal low temp"             ~ "cold/wind chill",
        evtype == "unseasonal rain"                 ~ "other",
        evtype == "unusual warmth"                  ~ "heat",
        evtype == "unusual/record warmth"           ~ "heat",
        evtype == "unusually cold"                  ~ "cold/wind chill",
        evtype == "unusually late snow"             ~ "winter weather",
        evtype == "unusually warm"                  ~ "heat",
        evtype == "urban flood"                     ~ "flood",
        evtype == "urban flooding"                  ~ "flood",
        evtype == "urban/small strm fldg"           ~ "flood",
        evtype == "urban/sml stream fld"            ~ "flood",
        evtype == "urban/sml stream fldg"           ~ "flood",
        evtype == "urban/street flooding"           ~ "flood",
        evtype == "very dry"                        ~ "drought",
        evtype == "very warm"                       ~ "heat",
        evtype == "vog"                             ~ "other",
        evtype == "volcanic ash plume"              ~ "volcanic ash",
        evtype == "volcanic ashfall"                ~ "volcanic ash",
        evtype == "volcanic eruption"               ~ "volcanic ash",
        evtype == "wake low wind"                   ~ "other",
        evtype == "wall cloud"                      ~ "other",
        evtype == "warm weather"                    ~ "heat",
        evtype == "waterspouts"                     ~ "waterspout",
        evtype == "wet micoburst"                   ~ "heat",
        evtype == "wet microburst"                  ~ "heat",
        evtype == "wet month"                       ~ "other",
        evtype == "wet year"                        ~ "other",
        evtype == "whirlwind"                       ~ "other",
        evtype == "wild/forest fire"                ~ "wildfire",
        evtype == "wind"                            ~ "heavy wind",
        evtype == "wind advisory"                   ~ "heavy wind",
        evtype == "wind and wave"                   ~ "marine high wind",
        evtype == "wind chill"                      ~ "cold/wind chill",
        evtype == "wind damage"                     ~ "heavy wind",
        evtype == "wind gusts"                      ~ "heavy wind",
        evtype == "winds"                           ~ "heavy wind",
        evtype == "winter mix"                      ~ "winter weather",
        evtype == "winter weather mix"              ~ "winter weather",
        evtype == "winter weather/mix"              ~ "winter weather",
        evtype == "wintery mix"                     ~ "winter weather",
        evtype == "wintry mix"                      ~ "winter weather",
        evtype == "wnd"                             ~ "heavy wind",
        .default                                    = "other"
    ))

# Non-standard values are mapped to the closest official type; indeterminate # entries are classified as "other".
unique(storm$evtype) #Final unique count: 42.


######################## PLOT -- PUBLIC HEALTH IMPACT

storm_fat = aggregate(fatalities ~ evtype, storm, sum)
storm_inj = aggregate(injuries   ~ evtype, storm, sum)

fatalities = storm_fat |> filter(evtype != "other") |> arrange(desc(fatalities)) |> slice(1:5)
injuries   = storm_inj |> filter(evtype != "other") |> arrange(desc(injuries))   |> slice(1:5)

fatalities$evtype = factor(fatalities$evtype, levels = fatalities$evtype)
injuries$evtype   = factor(injuries$evtype,   levels = injuries$evtype)

p1 = ggplot(fatalities, aes(x = reorder(evtype, fatalities),
                            y = fatalities,
                            fill = fatalities)) +
    scale_fill_viridis_c() +
    geom_bar(stat = "identity") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    ylab("Fatalities") + xlab("Weather Events") +
    coord_flip()

p2 = ggplot(injuries, aes(x = reorder(evtype, injuries),
                          y = injuries,
                          fill = injuries)) +
    scale_fill_viridis_c() +
    geom_bar(stat = "identity") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    ylab("Injuries") + xlab("") +
    coord_flip()

health_plot = grid.arrange(p1, p2, nrow = 1,
                           top = textGrob("Weather Events (1996-2011) with the Greatest Public Health Impact"))

ggsave("storm_health_impact.png", plot = health_plot,
       width = 10, height = 6, dpi = 300)


######################## PLOT -- ECONOMIC IMPACT

storm_prp = aggregate(damage_prop ~ evtype, storm, sum)
storm_crp = aggregate(damage_crop ~ evtype, storm, sum)

property = storm_prp |> filter(evtype != "other") |> arrange(desc(damage_prop)) |> slice(1:5)
crops    = storm_crp |> filter(evtype != "other") |> arrange(desc(damage_crop))  |> slice(1:5)

property$evtype = factor(property$evtype, levels = property$evtype)
crops$evtype    = factor(crops$evtype,    levels = crops$evtype)

p3 = ggplot(property, aes(x = reorder(evtype, damage_prop),
                          y = (damage_prop / 10^6),
                          fill = damage_prop)) +
    scale_fill_viridis_c() +
    geom_bar(stat = "identity") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    ylab("Property Damage (in millions USD)") + xlab("Weather Events") +
    coord_flip()

p4 = ggplot(crops, aes(x = reorder(evtype, damage_crop),
                       y = (damage_crop / 10^6),
                       fill = damage_crop)) +
    scale_fill_viridis_c() +
    geom_bar(stat = "identity") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    ylab("Crop Damage (in millions USD)") + xlab("") +
    coord_flip()

damage_plot = grid.arrange(p3, p4, nrow = 1,
                           top = textGrob("Weather Events (1996-2011) with Greatest Economic Impact"))

ggsave("storm_economic_impact.png", plot = damage_plot,
       width = 10, height = 6, dpi = 300)


######################## VSL COMBINED IMPACT INDEX

# Value of a Statistical Life (VSL) approach converts all impacts to a common dollar scale, making the combined index interpretable rather than arbitrary.
# VSL source: U.S. EPA (2023), ~$11.6M per fatality (2023 dollars).
# Injury value: 1/100 of VSL per EPA guidelines = ~$116,000 per injury.
# Combined index = (fatalities * VSL) + (injuries * injury_value) + property_damage + crop_damage

vsl = 11600000   # EPA VSL in dollars
injury_value = vsl / 100  # EPA injury-to-fatality ratio

storm_vsl = storm |>
    group_by(evtype) |>
    summarise(
        total_fatalities  = sum(fatalities,   na.rm = TRUE),
        total_injuries    = sum(injuries,     na.rm = TRUE),
        total_prop        = sum(damage_prop,  na.rm = TRUE),
        total_crop        = sum(damage_crop,  na.rm = TRUE)
    ) |>
    mutate(
        vsl_index = (total_fatalities * vsl) +
            (total_injuries   * injury_value) +
            total_prop + total_crop
    ) |>
    arrange(desc(vsl_index))

# Top 10 by VSL index -- exclude "other" (catch-all for unmatched evtypes)
top10_vsl = storm_vsl |> filter(!is.na(evtype), evtype != "other") |> slice(1:10)
top10_vsl$evtype = factor(top10_vsl$evtype, levels = rev(top10_vsl$evtype))

vsl_plot = ggplot(top10_vsl,
                  aes(x = evtype,
                      y = vsl_index / 10^9,
                      fill = vsl_index)) +
    scale_fill_viridis_c() +
    geom_bar(stat = "identity") +
    theme_classic() +
    theme(legend.position = "none") +
    ylab("Combined Impact (billions USD, VSL-weighted)") +
    xlab("Weather Event") +
    ggtitle("Top 10 Weather Events by Combined Human and Economic Impact (1996-2011)") +
    coord_flip()

ggsave("storm_vsl_index.png", plot = vsl_plot,
       width = 10, height = 6, dpi = 300)


######################## TEMPORAL TREND -- ANIMATED LINE PLOT

# Animate annual VSL index for the top 10 events, building year by year.
# Top 10 determined by total VSL index over the full 1996-2011 period.
# NA excluded -- catch-all "other" category absorbs any unmatched evtypes.

top10_names = as.character(top10_vsl$evtype[top10_vsl$evtype != "other"])
top10_names = top10_names[1:10]

storm_annual = storm |>
    filter(evtype %in% top10_names) |>
    group_by(evtype, year) |>
    summarise(
        annual_vsl = (sum(fatalities,  na.rm = TRUE) * vsl) +
            (sum(injuries,    na.rm = TRUE) * injury_value) +
            sum(damage_prop,  na.rm = TRUE) +
            sum(damage_crop,  na.rm = TRUE),
        .groups = "drop"
    )

# Ensure all event-year combinations exist (fill gaps with 0)
all_combos   = expand.grid(
    evtype = top10_names,
    year   = 1996:2011,
    stringsAsFactors = FALSE
)
storm_annual = merge(all_combos, storm_annual, by = c("evtype", "year"), all.x = TRUE)
storm_annual$annual_vsl[is.na(storm_annual$annual_vsl)] = 0
storm_annual$year   = as.integer(storm_annual$year)
storm_annual$evtype = factor(storm_annual$evtype, levels = top10_names)

temporal_plot = ggplot(storm_annual,
                       aes(x     = year,
                           y     = annual_vsl / 10^9,
                           color = evtype,
                           group = evtype)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_viridis_d() +
    scale_x_continuous(breaks = seq(1996, 2011, by = 2)) +
    theme_classic() +
    theme(legend.position  = "right",
          legend.title     = element_text(size = 9),
          legend.text      = element_text(size = 8)) +
    labs(title  = "Annual Weather Event Impact, 1996-{as.integer(frame_along)}",
         x      = "Year",
         y      = "Annual Impact (billions USD, VSL-weighted)",
         color  = "Event Type") +
    transition_reveal(year)

animate(temporal_plot,
        nframes   = 80,
        fps       = 10,
        width     = 900,
        height    = 500,
        renderer  = gifski_renderer("storm_temporal.gif"))


######################## STATE POPULATION LOOKUP (2000 CENSUS MIDPOINT)

# 2000 Census state populations used as the denominator for per-capita damage.
# Source: U.S. Census Bureau, Census 2000 Summary File 1.
# Territories (PR, VI, GU, AS, MP) excluded -- not in standard state FIPS.

state_pop = data.frame(
    state = c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA",
              "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
              "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
              "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
              "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
              "DC"),
    population = c(4447100, 626932, 5130632, 2673400, 33871648, 4301261,
                   3405565, 783600, 15982378, 8186453, 1211537, 1293953,
                   12419293, 6080485, 2926324, 2688418, 4041769, 4468976,
                   1274923, 5296486, 6349097, 9938444, 4919479, 2844658,
                   5595211, 902195, 1711263, 1998257, 1235786, 8414350,
                   1819046, 18976457, 8049313, 642200, 11353140, 3450654,
                   3421399, 12281054, 1048319, 4012012, 754844, 5689283,
                   20851820, 2233169, 608827, 7078515, 5894121, 1808344,
                   5363675, 493782, 572059)
)


######################## MAP -- PROPERTY DAMAGE PER CAPITA BY STATE

storm_state = storm |>
    group_by(state) |>
    summarise(
        total_prop = sum(damage_prop, na.rm = TRUE),
        total_crop = sum(damage_crop, na.rm = TRUE),
        .groups = "drop"
    )

storm_state = merge(storm_state, state_pop, by = "state", all.x = TRUE)
storm_state = storm_state |>
    mutate(
        prop_per_capita = total_prop / population,
        crop_per_capita = total_crop / population
    ) |>
    filter(!is.na(population))  # drop territories without population data

# Plotly uses full lowercase state names for choropleth locations
storm_state$state_name = tolower(state.name[match(storm_state$state, state.abb)])
storm_state$state_name[storm_state$state == "DC"] = "district of columbia"

prop_map = plot_ly(
    data       = storm_state,
    type       = "choropleth",
    locations  = storm_state$state,
    locationmode = "USA-states",
    z          = storm_state$prop_per_capita,
    text       = paste0(storm_state$state, "<br>",
                        "Property damage per capita: $",
                        round(storm_state$prop_per_capita, 2)),
    hoverinfo  = "text",
    colorscale = "Viridis",
    reversescale = TRUE,
    height     = 500,
    colorbar   = list(
        title      = list(text = "USD per capita", font = list(size = 12)),
        thickness  = 15,
        len        = 0.6,
        x          = 1.0,
        y          = 0.5
    )
) |>
    layout(
        title = list(text = "Property Damage per Capita by State (1996-2011)",
                     font = list(size = 14)),
        geo   = list(
            scope        = "usa",
            showlakes    = TRUE,
            lakecolor    = "rgb(255,255,255)",
            projection   = list(type = "albers usa")
        ),
        margin = list(l = 0, r = 80, t = 50, b = 20)
    )

saveWidget(prop_map, "storm_property_damage_map.html", selfcontained = TRUE)


######################## MAP -- CROP DAMAGE PER CAPITA BY STATE

crop_map = plot_ly(
    data       = storm_state,
    type       = "choropleth",
    locations  = storm_state$state,
    locationmode = "USA-states",
    z          = storm_state$crop_per_capita,
    text       = paste0(storm_state$state, "<br>",
                        "Crop damage per capita: $",
                        round(storm_state$crop_per_capita, 2)),
    hoverinfo  = "text",
    colorscale = "Viridis",
    reversescale = TRUE,
    height     = 500,
    colorbar   = list(
        title      = list(text = "USD per capita", font = list(size = 12)),
        thickness  = 15,
        len        = 0.6,
        x          = 1.0,
        y          = 0.5
    )
) |>
    layout(
        title = list(text = "Crop Damage per Capita by State (1996-2011)",
                     font = list(size = 14)),
        geo   = list(
            scope        = "usa",
            showlakes    = TRUE,
            lakecolor    = "rgb(255,255,255)",
            projection   = list(type = "albers usa")
        ),
        margin = list(l = 0, r = 80, t = 50, b = 20)
    )

saveWidget(crop_map, "storm_crop_damage_map.html", selfcontained = TRUE)