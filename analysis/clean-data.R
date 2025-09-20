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
  filter(TOTAL_WATER_USE != 0) %>%
  droplevels()
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
  filter(sum_used_up == 0) %>%
  droplevels()
#repeat offenders-- need to dig in

water_clean <- anti_join(water_cleaning_with_check, no_usage, by = c("SUBBASIN_NUMBER", "REPORT_YEAR"))

write.csv(water_clean, './data/total_water_use_clean.csv')
          