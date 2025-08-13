library(tidyverse)
library(sf)
library(bcdata)
library(stringr)
library(viridis)

base_dir = stringr::str_extract(getwd(),"C:\\/Users\\/[a-zA-Z]+")
onedrive_wd = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/LargeDataFiles/"
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"

#new data
paynterdata<-sf::read_sf(paste0("output/fishing_days_by_waterbody.gpkg"))
#old dfo data
if(!file.exists('data/anger_survey.rds')){
  dfo_data = sf::read_sf("//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/shared_data_sets/freshwater_fisheries_society_angler_survey_2022_2023.gpkg")
  saveRDS(dfo_data, file = 'data/anger_survey.rds')
} else {
  dfo_data = readRDS('data/anger_survey.rds')
}

#now we join these data where the watershed and waterbody match
dfo_data <- dfo_data |>
  dplyr::rename(
    watershed = WATERSHED_GROUP_ID,
    waterbody = Waterbody
  ) |>
  st_transform(4326)

comparison_data <- st_join(
  paynterdata |> select(geom, days_fished_paynter = fishing_days),
  dfo_data |> select(dfo_waterbody = waterbody, days_fished_dfo = days_fished),
  join = st_intersects,
  left = FALSE  # Only rows with an intersection
)


summed_fishing <-comparison_data |>
  group_by(geom, dfo_waterbody) |>
  summarise(
    total_days_fished_paynter = sum(days_fished_paynter, na.rm = TRUE),
    total_days_fished_dfo = sum(days_fished_dfo, na.rm = TRUE),
    .groups = "drop"
  ) |>
  st_as_sf()

# drop columns where there is an NA in hte dfo_days_fished of in days fished_paynter
comparison_data <- summed_fishing |>
  dplyr::filter(!is.na(total_days_fished_dfo)) |>
  dplyr::filter(!is.na(total_days_fished_paynter))

comparison_data <- comparison_data |>
  mutate(diff = total_days_fished_dfo - total_days_fished_paynter)

summary(comparison_data$diff)



comparison_data_complete <- comparison_data |>
  filter(!is.na(total_days_fished_paynter), !is.na(total_days_fished_dfo))

# Perform paired t-test
t_test_result <- t.test(
  comparison_data_complete$total_days_fished_paynter,
  comparison_data_complete$total_days_fished_dfo,
  paired = TRUE
)

print(t_test_result)
# Create a scatter plot to visualize the relationship
ggplot(comparison_data_complete, aes(x = total_days_fished_paynter, y = total_days_fished_dfo)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Comparison of Fishing Days: Paynter vs DFO",
    x = "Fishing Days (Paynter)",
    y = "Fishing Days (DFO)"
  ) +
  theme_minimal()
