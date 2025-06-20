library(tidyverse)
library(sf)
library(readxl)
library(bcdata)
library(stringr)
library(viridis)

base_dir = stringr::str_extract(getwd(),"C:\\/Users\\/[a-zA-Z]+")
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"

dat<-read_excel("data/2023-24 iSEA waterbody level fishing days and expenditures.xlsx", sheet = 1, col_names = T)
#remove the other stuff on the right of the sheet
dat<-dat[,1:5]
#snakecase column names
colnames(dat)<-snakecase::to_snake_case(colnames(dat))
#remove the first row
dat<-dat[-1,]

#just in cased
bc = bcmaps::bc_bound() |>
  sf::st_transform(4326)


# get the wildlife management units?
wmus <- bcdc_query_geodata("wildlife-management-units") |>
  collect() |>
  select(WILDLIFE_MGMT_UNIT_ID, REGION_RESPONSIBLE,REGION_RESPONSIBLE_ID,
         REGION_RESPONSIBLE_NAME, geometry) |>
  st_transform(4326)



if(!file.exists(paste0(onedrive_wd,"named_lakes_and_rivers.rds"))){
  named_lakes = bcdc_query_geodata('freshwater-atlas-lakes') |>
    filter(!is.na(GNIS_NAME_1)) |>
    collect() |>
    dplyr::select(waterbody = GNIS_NAME_1, watershed = WATERSHED_GROUP_ID, BLUE_LINE_KEY, FWA_WATERSHED_CODE) |>
    dplyr::group_by(waterbody, watershed, BLUE_LINE_KEY, FWA_WATERSHED_CODE) |>
    dplyr::summarise(.groups = 'drop') |>
    dplyr::mutate(wb_type = "lake")

  named_rivers = bcdc_query_geodata('freshwater-atlas-rivers') |>
    filter(!is.na(GNIS_NAME_1)) |>
    collect() |>
    dplyr::select(waterbody = GNIS_NAME_1, watershed = WATERSHED_GROUP_ID, BLUE_LINE_KEY, FWA_WATERSHED_CODE) |>
    dplyr::group_by(waterbody, watershed, BLUE_LINE_KEY, FWA_WATERSHED_CODE) |>
    dplyr::summarise(.groups = 'drop') |>
    dplyr::mutate(wb_type = "river")

  named_wbs = dplyr::bind_rows(
    named_lakes,
    named_rivers
  ) |>
    sf::st_transform(4326)

  saveRDS(named_wbs, paste0(onedrive_wd,"named_lakes_and_rivers.rds"))
} else {
  named_wbs = readRDS(paste0(onedrive_wd,"named_lakes_and_rivers.rds"))
}




#combine the wmus and the named waterbodies by geometry - we need to keep the wildlife management units
named_wbs_units<- named_wbs |>
  sf::st_join(wmus, join = st_intersects) |>
  dplyr::select(waterbody, watershed, BLUE_LINE_KEY, FWA_WATERSHED_CODE, WILDLIFE_MGMT_UNIT_ID, REGION_RESPONSIBLE, REGION_RESPONSIBLE_ID,
                REGION_RESPONSIBLE_NAME, wb_type) |>
  dplyr::mutate(wb_type = ifelse(is.na(wb_type), "wmus", wb_type)) |>
  dplyr::mutate(waterbody = str_to_title(waterbody)) |>
  dplyr::distinct()

named_wbs_units$WILDLIFE_MGMT_UNIT_ID<-gsub("-", "_", named_wbs_units$WILDLIFE_MGMT_UNIT_ID)

#names
named_wbs_units<- named_wbs_units |>
  dplyr::rename(management_unit = WILDLIFE_MGMT_UNIT_ID)
dat<- dat |>
  dplyr::rename(waterbody = water_body)

#now we attempt to join the dat to this named_wbs_units by matching the wildlife management unit id

dat_combined<- full_join(
  named_wbs_units,
  dat,
  by = c("management_unit", "waterbody")
)

complete_rows <- dat_combined |>
  filter(!is.na(watershed), !is.na(BLUE_LINE_KEY))
incomplete_rows <- dat_combined |>
  filter(is.na(watershed) | is.na(BLUE_LINE_KEY))
incomplete_nogeo <- st_drop_geometry(incomplete_rows)

# Create combined keys in both datasets
incomplete_rows <- incomplete_rows |>
  mutate(wb_mu_key = paste0(waterbody, "_", management_unit))

complete_rows <- complete_rows |>
  mutate(wb_mu_key = paste0(waterbody, "_", management_unit))

matching_keys <- intersect(incomplete_rows$wb_mu_key, complete_rows$wb_mu_key)

# We aren't getting matches for these - so I don't think we can do much with them


dat_combined <- dat_combined |>
  dplyr::group_by(waterbody, watershed) |>
  dplyr::summarise(
    fishing_days = sum(fishing_days, na.rm = TRUE),
    expenditures = sum(expenditures, na.rm = TRUE),
    do_union = TRUE,
    .groups = "drop"
  ) |>
  sf::st_as_sf()

empty_geoms <- dat_combined[st_is_empty(dat_combined), ]
#remove those rows with no geometry
dat_combined <- dat_combined[!st_is_empty(dat_combined), ]

#summarise to remove duplicate rows and add the fishing days and expenditures
# Assuming your data is in a variable called dat_combined
dat_final <- dat_combined |>
  group_by(waterbody, BLUE_LINE_KEY, FWA_WATERSHED_CODE, watershed) |>
  summarise(
    fishing_days = sum(fishing_days, na.rm = TRUE),
    expenditures = sum(expenditures, na.rm = TRUE),
    .groups = "drop"
  )




# plot the new data, with color and fill being fishing_days
p1<-ggplot() +
  geom_sf(data = bc, fill = "white", color = "black") +
  geom_sf(
    data = dat_final[!is.na(dat_final$fishing_days), ],
    aes(fill = fishing_days, color = fishing_days),
    alpha = 0.5
  ) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(
    title = "Fishing Days by Waterbody",
    fill = "Fishing Days",
    color = "Fishing Days"
  )
# drop data with na in fishing_days
dat_final<- dat_final[!is.na(dat_final$fishing_days), ]



# save the new data into an output folder
sf::st_write(dat_final, paste0("output/fishing_days_by_waterbody.gpkg"), append = F)

