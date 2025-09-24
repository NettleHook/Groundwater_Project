library(dplyr)
library(ggplot2)

water_raw <- read.csv("./data/total_water_use.csv")
summary(water_raw)

#add one for WUS_URBAN too
ggplot(water_raw, aes(x = TOTAL_WATER_USE, y = WUS_AGRICULTURAL)) + 
  geom_point() + 
  labs(x = "Total Water Use(acre-feet)", y = "Agricultural Water Usage(acre-feet)")
ggplot(water_raw, aes(x = TOTAL_WATER_USE, y = WUS_URBAN)) + 
  geom_point() + 
  labs(x = "Total Water Use(acre-feet)", y = "Agricultural Water Usage(acre-feet)")

#no missing values, but there is at least one entry where total water use == 0
water_cleaning <- water_raw %>%
  filter(TOTAL_WATER_USE != 0)
summary(water_cleaning)

#also noted from graph that it appears may be some values where it looks like TOTAL_WATER_USE is less than WUS_AGRICULTURAL or WUS_URBAN
filter(water_cleaning, WUS_AGRICULTURAL > TOTAL_WATER_USE)
#two entries:
#5-022.09 is explained by small water systems and domestic wells (amount stored in WST_OTHER)
#9-010 is explained by imported water (amount stored in WST_OTHER)
filter(water_cleaning, WUS_URBAN > TOTAL_WATER_USE)
#turns out there's none for WUS_URBAN


#checking sum of values:
water_cleaning_with_check <- water_cleaning %>%
  mutate(
    #sometimes WUS_OTHER is additional usage
    sum_used_up = WUS_AGRICULTURAL + WUS_INDUSTRIAL + WUS_URBAN + WUS_MANAGED_WETLANDS + WUS_NATIVE_VEGETATION + WUS_OTHER,
    total_water_and_other = WST_GROUNDWATER + WST_RECYCLED_WATER + WST_REUSED_WATER + WST_SURFACE_WATER + WST_OTHER,
    total = total_water_and_other - sum_used_up
  )
no_usage <- water_cleaning_with_check %>%
  filter(sum_used_up == 0)
#repeat offenders-- need to dig in. Compare with groundwater extraction database, as that sometimes has entries

groundwater <- read.csv("./data/groundwater_extraction.csv") %>%
  filter(SUBBASIN_NUMBER %in% no_usage$SUBBASIN_NUMBER)

#BIG VALLEY is only one that also has empty entries in both datasets--likely unfinished report

#however, the groundwater database only reports groundwater used in each sector, not all water

#We have some strong outliers in the urban sections. Limit decided by examining scatter plot
urban_outliers <- filter(water_cleaning_with_check, WUS_URBAN >= 200000 & WUS_AGRICULTURAL < 150000)

water_cleaning_no_urban_outliers <- filter(water_cleaning_with_check, SUBBASIN_NUMBER != "8-001") 

water_clean <- anti_join(water_cleaning_no_urban_outliers, no_usage, by = c("SUBBASIN_NUMBER", "REPORT_YEAR"))

write.csv(water_clean, './data/total_water_use_clean.csv')
          