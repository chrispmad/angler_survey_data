library(tidyverse)
library(sf)
library(readxl)
library(bcdata)
library(stringr)
library(viridis)
library(leaflet)

base_dir = stringr::str_extract(getwd(),"C:\\/Users\\/[a-zA-Z]+")
onedrive_wd = paste0(str_extract(getwd(),"C:/Users/[A-Z]+/"),"OneDrive - Government of BC/data/")
lan_root = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/"

dat<-read_excel("data/2023-24 iSEA waterbody level fishing days and expenditures (002).xlsx", sheet = 1, col_names = T)
#remove the other stuff on the right of the sheet
dat<-dat[,1:5]
#snakecase column names
colnames(dat)<-snakecase::to_snake_case(colnames(dat))

# Let's correct 'Ã¨' to just 'è'
dat = dat |>
  dplyr::mutate(water_body = str_replace_all(water_body, 'Ã¨', 'è'))

#just in case
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

# Merge named waterbodies by waterbody and watershed - this does tempt the risk
# of joining together identically named waterbodies if they exist in the
# same watershed... but it also helps us join together waterbodies
# that we can effectively consider to be the same waterbody,
# e.g., Trevlac Pond:
leaflet() |> addTiles() |> addPolygons(data = named_wbs |> dplyr::filter(waterbody == 'Trevlac Pond'))

# Let's merge the named_wbs based on name, watershed number, wb_type, but
# NOT on blue_line_key or FWA code, since different polygons that are part of
# the same river / clump of ponds will have different BLKs and FWA codes... gah!
if(!file.exists(paste0(onedrive_wd,"named_lakes_and_rivers_merged.rds"))){
  # Initially, there are 9215 rows in 'named_wbs'
  named_wbs = named_wbs |>
    dplyr::group_by(waterbody, watershed, wb_type) |>
    dplyr::summarise() |>
    dplyr::ungroup()
  # Now there are 8,738 rows.
  saveRDS(named_wbs, paste0(onedrive_wd,"named_lakes_and_rivers_merged.rds"))
} else {
  named_wbs = readRDS(paste0(onedrive_wd,"named_lakes_and_rivers_merged.rds"))
}

# We need to append wildlife management unit IDs to each waterbody.

if(!file.exists("data/named_waterbodies_with_management_units.rds")){
  #combine the wmus and the named waterbodies by geometry - we need to keep the wildlife management units
  named_wbs_units <- named_wbs |>
    sf::st_join(wmus, join = st_intersects) |>
    dplyr::select(waterbody, watershed,
                  WILDLIFE_MGMT_UNIT_ID, REGION_RESPONSIBLE, REGION_RESPONSIBLE_ID,
                  REGION_RESPONSIBLE_NAME, wb_type) |>
    dplyr::mutate(wb_type = ifelse(is.na(wb_type), "wmus", wb_type)) |>
    dplyr::mutate(waterbody = str_to_title(waterbody)) |>
    dplyr::distinct()

  # A little check
  ws = bcdc_query_geodata('freshwater-atlas-watershed-groups') |>
    filter(WATERSHED_GROUP_ID %in% c(165,36)) |>
    collect()

  ggplot() +
    geom_sf(data = wmus[wmus$WILDLIFE_MGMT_UNIT_ID == '1-5',], fill = 'darkred') +
    geom_sf(data = ws, fill = 'lightblue', alpha = 0.7) +
    geom_sf(data = named_wbs |>
              dplyr::filter(waterbody == 'Long Lake') |>
              sf::st_filter(wmus[wmus$WILDLIFE_MGMT_UNIT_ID == '1-5',]),
            col = 'pink', fill = 'pink')
  # Ok, for cases of waterbodies that overlapped with multiple WMUs,
  # let's combine the columns we got from WMU layer using paste0().

  # Though we will lose data granularity, let's merge named_wbs_units
  # here by waterbody name and wildlife management unit ID.
  named_wbs_units_m = named_wbs_units |>
    dplyr::group_by(waterbody, WILDLIFE_MGMT_UNIT_ID, REGION_RESPONSIBLE) |>
    dplyr::summarise() |>
    dplyr::ungroup()

  saveRDS(named_wbs_units_m, "data/named_waterbodies_with_management_units.rds")
  # saveRDS(named_wbs_by_watershed, "data/named_waterbodies_with_watershed_keys.rds")
} else {
  named_wbs_units_m = readRDS("data/named_waterbodies_with_management_units.rds")
}

named_wbs_units_m$WILDLIFE_MGMT_UNIT_ID<-gsub("-", "_", named_wbs_units_m$WILDLIFE_MGMT_UNIT_ID)

#names
named_wbs_units_m <- named_wbs_units_m |>
  dplyr::rename(management_unit = WILDLIFE_MGMT_UNIT_ID)

dat <- dat |>
  dplyr::rename(waterbody = water_body)

dat_combined = named_wbs_units_m |>
  # Duplicate waterbody rows (and their geometries) to match
  # multiple rows in angling survey data when large waterbodies
  # have been split based on their management unit.
  # tidyr::separate_longer_delim(cols = management_unit, delim = ', ') |>
  # filter(waterbody == 'Fraser River') |>
  dplyr::inner_join(
    dat |>
      # filter(waterbody == 'Fraser River') |>
      dplyr::mutate(row_id = dplyr::row_number()),
    by = join_by(waterbody, management_unit)
  )

dat_combined = dat_combined |>
  dplyr::filter(!st_is_empty(geometry))

# What proportion of fishing days are we keeping in this spatialization
# step?
100*nrow(dat_combined) / nrow(dat)
# 87.9% of the rows in 'dat' were successfully mapped to some geometry.
100*sum(dat_combined$fishing_days) / sum(dat$fishing_days)
# 95.8% of the fishing days were successfully mapped to some geometry.

# Merging on watershed key!!
ws = bcdc_query_geodata('freshwater-atlas-watershed-groups') |>
  collect()

ws = sf::st_transform(ws, 4326)

dat_combined_w_watershed = dat_combined |>
  sf::st_join(ws)

## Now resummarise to waterbody name and to watershed key number.
# Summarise to waterbody name and watershed number.
dat_combined_w_watershed_m = dat_combined_w_watershed |>
  dplyr::group_by(waterbody, watershed = WATERSHED_GROUP_ID) |>
  dplyr::summarise(fishing_days = sum(fishing_days, na.rm=T),
                   geometry = sf::st_union(geometry)) |>
  dplyr::ungroup()

dat_combined_w_watershed_m

max(dat_combined_w_watershed_m$fishing_days)

dat_combined_w_watershed_m |>
  dplyr::filter(waterbody %in% c("Cultus Lake","Shuswap Lake","Kootenay Lake"))

# plot the new data, with color and fill being fishing_days
p1<-ggplot() +
  geom_sf(data = bc, fill = "white", color = "black") +
  geom_sf(
    data = dat_combined_w_watershed_m,
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

p1

ggsave(filename = "output/angler_days_by_waterbody.jpg", plot = p1,
       width = 8, height = 6, units = "in")

# drop data with na in fishing_days
# dat_combined <- dat_combined[!is.na(dat_combined$fishing_days), ]

# save the new data into an output folder
saveRDS(dat_combined_w_watershed_m, paste0("output/fishing_days_by_waterbody_and_watershed.rds"))
saveRDS(dat_combined_w_watershed_m, paste0(onedrive_wd,"fishing_days_by_waterbody_and_watershed.rds"))

