# Beta diversity on dike grasslands
# Prepare species, sites, and traits data ####
# Markus Bauer
# 2022-01-11



### Packages ###
library(here)
library(tidyverse)
library(naniar) # are_na
library(lubridate) # modify dates

### Start ###
installr::updateR(browse_news = FALSE, install_R = TRUE, copy_packages = TRUE, copy_Rprofile.site = TRUE, keep_old_packages = TRUE, update_packages = TRUE, start_new_R = FALSE, quit_R = TRUE, print_R_versions = TRUE, GUI = TRUE)
#sessionInfo()



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Load data ##########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### 1 Sites ############################################################


sites <- read_csv(
  here("data", "raw", "data_raw_sites.csv"),
  col_names = TRUE,
  na = c("", "NA", "na"),
  col_types =
    cols(
      .default = "?",
      id = "f",
      block = "f",
      location = "f",
      side = "f",
      exposition = "f",
      surveyDate_2017 = col_date(format = "%Y-%m-%d"),
      surveyDate_2018 = col_date(format = "%Y-%m-%d"),
      surveyDate_2019 = col_date(format = "%Y-%m-%d"),
      humusLevel = "f"
    )
) %>%
  mutate(across(
    starts_with("vegetationCov") |
      starts_with("vegetationHeight_") |
      starts_with("opensoilCov_") |
      starts_with("mossCov_") |
      starts_with("litterCov_") |
      starts_with("botanist_"),
    ~ as.character(.x)
  )) %>%
  pivot_longer(starts_with("vegetationCov") |
    starts_with("vegetationHeight_") |
    starts_with("opensoilCov_") |
    starts_with("mossCov_") |
    starts_with("litterCov_") |
    starts_with("botanist_"),
  names_to = c("x", "surveyYear"),
  names_sep = "_",
  values_to = "n"
  ) %>%
  pivot_wider(names_from = x, values_from = n) %>%
  mutate(across(
    c(
      surveyYear, vegetationCov, vegetationHeight,
      opensoilCov, mossCov, litterCov
    ),
    ~ as.numeric(.x)
  )) %>%
  mutate(
    surveyYearF = factor(surveyYear),
    surveyYearFminus = factor(surveyYear - 1),
    constructionYearF = factor(constructionYear),
    constructionYearFplus = factor(constructionYear + 1)
  ) %>%
  mutate(
    id = str_c(id, surveyYear, sep = "_"),
    .keep = "all",
    id = paste0("X", id),
    plot = str_sub(id, start = 2, end = 3),
    position = str_sub(id, start = 5, end = 5),
    locationAbb = str_sub(location, 1, 3),
    locationAbb = str_to_upper(locationAbb),
    locationAbb = factor(locationAbb,
      levels = unique(locationAbb[order(constructionYear)])
    ),
    locationYear = str_c(locationAbb, constructionYear, sep = "-")
  ) %>%
  select(
    -position, -starts_with("surveyDate_"), -starts_with("topsoilDepth_"),
    -cnLevel, -ends_with("Class")
  ) %>%
  relocate(plot, .after = id) %>%
  relocate(c("locationAbb", "locationYear"), .after = location) %>%
  relocate(c("surveyYear", "surveyYearF", "surveyYearFminus"),
    .after = riverkm
  ) %>%
  relocate(c("constructionYearF", "constructionYearFplus"),
    .after = constructionYear
  )


### 2 Species #########################################################

species <- data.table::fread(
  here("data", "raw", "data_raw_species.csv"),
  sep = ",",
  dec = ".",
  skip = 0,
  header = TRUE,
  na.strings = c("", "NA", "na"),
  colClasses = list(
    character = "name"
  )
) %>%
  ### Check that each species occurs at least one time ###
  group_by(name) %>%
  arrange(name) %>%
  select(name, all_of(sites$id)) %>%
  mutate(
    total = sum(c_across(starts_with("X")), na.rm = TRUE),
    presence = if_else(total > 0, 1, 0),
    name = factor(name)
  ) %>%
  filter(presence == 1) %>%
  ungroup() %>%
  select(name, sort(tidyselect::peek_vars()), -total, -presence) %>%
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0)))

### Create list with species names and their frequency ###
specieslist <- species %>%
  mutate_if(is.numeric, ~ 1 * (. != 0)) %>%
  mutate(
    sum = rowSums(across(where(is.numeric)), na.rm = TRUE),
    .keep = "unused"
  ) %>%
  group_by(name) %>%
  summarise(sum = sum(sum))
# write_csv(specieslist, "specieslist_2022xxxx.csv")


### 3 Traits ##########################################################

traits <- read_csv(
  here("data", "raw", "data_raw_traits.csv"),
  col_names = TRUE, na = c("", "NA", "na"),
  col_types =
    cols(
      .default = "f",
      name = "c",
      sociology = "d",
      l = "d",
      t = "d",
      k = "d",
      f = "d",
      r = "d",
      n = "d"
    )
) %>%
  separate(name, c("genus", "species", "ssp", "subspecies"), "_",
    remove = FALSE, extra = "drop", fill = "right"
  ) %>%
  mutate(
    genus = str_sub(genus, 1, 4),
    species = str_sub(species, 1, 4),
    subspecies = str_sub(subspecies, 1, 4),
    name = factor(name)
  ) %>%
  unite(abb, genus, species, subspecies, sep = "") %>%
  mutate(
    abb = str_replace(abb, "NA", ""),
    abb = as_factor(abb)
  ) %>%
  select(-ssp) %>%
  arrange(name)
### Check congruency of traits and species table ###
traits[duplicated(traits$abb), ]
# traits$name[which(!(traits$name %in% species$name))]
species$name[which(!(species$name %in% traits$name))]
traits <- semi_join(traits, species, by = "name")

rm(list = ls(pattern = "[^species|traits|sites]"))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ###########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 1 Create simple variables ###################################################

traits <- traits %>%
  mutate(
    leanIndicator = if_else(
      !(is.na(table30)) | !(is.na(table33)) | !(is.na(table34)), "yes", "no"
    ),
    target = if_else(
      targetHerb == "yes" | targetGrass == "yes", "yes", "no"
    ),
    ruderal = if_else(
      sociology >= 3300 & sociology < 3700, "yes", "no"
    ),
    targetEllenberg = if_else(
      sociology >= 5300 & sociology < 5400, "dry_grassland", if_else(
        sociology >= 5400 & sociology < 6000, "hay_meadow", if_else(
          sociology >= 5100 & sociology < 5200, "nardus_grassland", if_else(
            sociology >= 5200 & sociology < 5300, "sand_grassland", "no"
          )
        )
      )
    )
  )

sites <- sites %>%
  mutate(
    conf.low = seq_along(id),
    conf.high = seq_along(id),
    NtotalConc = finematerialDepth * finematerialDensity * 10 *
      NtotalPerc / 100,
    plotAge = surveyYear - constructionYear
  )


## 2 Coverages #################################################################

cover <- left_join(species, traits, by = "name") %>%
  select(
    name, family, target, targetHerb, targetArrhenatherion, leanIndicator,
    nitrogenIndicator, ruderalIndicator, table33, starts_with("X")
  ) %>%
  pivot_longer(names_to = "id", values_to = "n", cols = starts_with("X")) %>%
  group_by(id)

### * graminoid, herb, and total coverage) ####
cover_total_and_graminoid <- cover %>%
  group_by(id, family) %>%
  summarise(total = sum(n, na.rm = TRUE), .groups = "keep") %>%
  mutate(type = if_else(family == "Poaceae" |
                          family == "Cyperaceae" |
                          family == "Juncaceae",
    "graminoidCov", "herbCov"
  )) %>%
  group_by(id, type) %>%
  summarise(total = sum(total, na.rm = TRUE), .groups = "keep") %>%
  spread(type, total) %>%
  mutate(accumulatedCov = graminoidCov + herbCov) %>%
  ungroup()

### * Target species' coverage ####
cover_target <- cover %>%
  filter(target == "yes") %>%
  summarise(targetCov = sum(n, na.rm = TRUE)) %>%
  mutate(targetCov = round(targetCov, 1)) %>%
  ungroup()

### * Target herb species' coverage ####
cover_targetHerb <- cover %>%
  filter(targetHerb == "yes") %>%
  summarise(targetHerbCov = sum(n, na.rm = TRUE)) %>%
  mutate(targetHerbCov = round(targetHerbCov, 1)) %>%
  ungroup()

### * Arrhenatherum species' cover ratio ####
cover_targetArrhenatherion <- cover %>%
  filter(targetArrhenatherion == "yes") %>%
  summarise(arrhCov = sum(n, na.rm = TRUE)) %>%
  mutate(arrhCov = round(arrhCov, 1)) %>%
  ungroup()

### * Lean indicator's coverage ####
cover_leanIndicator <- cover %>%
  filter(leanIndicator == "yes") %>%
  summarise(leanCov = sum(n, na.rm = TRUE)) %>%
  mutate(leanCov = round(leanCov, 1)) %>%
  ungroup()

### * Nitrogen indicator's coverage ####
cover_nitrogenIndicator <- cover %>%
  filter(nitrogenIndicator == "yes") %>%
  summarise(nitrogenCov = sum(n, na.rm = TRUE)) %>%
  mutate(nitrogenCov = round(nitrogenCov, 1)) %>%
  ungroup()

### * Ruderal indicator's coverage ####
cover_ruderalIndicator <- cover %>%
  filter(ruderalIndicator == "yes") %>%
  summarise(ruderalCov = sum(n, na.rm = TRUE)) %>%
  mutate(ruderalCov = round(ruderalCov, 1)) %>%
  ungroup()

### * Table 33 species' coverage ####
cover_table33 <- cover %>%
  mutate(table33 = if_else(table33 == "4" |
    table33 == "3" |
    table33 == "2",
  "table33Cov", "other"
  )) %>%
  filter(table33 == "table33Cov") %>%
  summarise(table33Cov = sum(n, na.rm = TRUE)) %>%
  mutate(table33Cov = round(table33Cov, 1)) %>%
  ungroup()

### * implement in sites data set ####
sites <- sites %>%
  right_join(cover_total_and_graminoid, by = "id") %>%
  right_join(cover_target, by = "id") %>%
  right_join(cover_targetHerb, by = "id") %>%
  right_join(cover_targetArrhenatherion, by = "id") %>%
  right_join(cover_leanIndicator, by = "id") %>%
  right_join(cover_nitrogenIndicator, by = "id") %>%
  right_join(cover_ruderalIndicator, by = "id") %>%
  right_join(cover_table33, by = "id") %>%
  ### Calcute the ratio of target species richness of total species richness
  mutate(
    targetCovratio = targetCov / accumulatedCov,
    graminoidCovratio = graminoidCov / accumulatedCov
  )

rm(list = ls(pattern = "[^species|traits|sites]"))


## 3 Alpha diversity ###################################################

### a Species richness -------------------------------------------------

speciesRichness <- left_join(species, traits, by = "name") %>%
  select(
    name, rlg, rlb, target, targetHerb, targetArrhenatherion,
    ffh6510, ffh6210, nitrogenIndicator, leanIndicator, table33, table34,
    starts_with("X")
  ) %>%
  pivot_longer(names_to = "id", values_to = "n", cols = starts_with("X")) %>%
  mutate(n = if_else(n > 0, 1, 0)) %>%
  group_by(id)

### * total species richness ####
speciesRichness_all <- speciesRichness %>%
  summarise(speciesRichness = sum(n, na.rm = TRUE)) %>%
  ungroup()

### * red list Germany (species richness) ####
speciesRichness_rlg <- speciesRichness %>%
  filter(rlg == "1" | rlg == "2" | rlg == "3" | rlg == "V") %>%
  summarise(rlgRichness = sum(n, na.rm = TRUE)) %>%
  ungroup()

### * red list Bavaria (species richness) ####
speciesRichness_rlb <- speciesRichness %>%
  filter(rlb == "1" | rlb == "2" | rlb == "3" | rlb == "V") %>%
  summarise(rlbRichness = sum(n, na.rm = TRUE)) %>%
  ungroup()

### * target species (species richness) ####
speciesRichness_target <- speciesRichness %>%
  filter(target == "yes") %>%
  summarise(targetRichness = sum(n, na.rm = TRUE)) %>%
  ungroup()

### * target herb species (species richness) ####
speciesRichness_targetHerb <- speciesRichness %>%
  filter(targetHerb == "yes") %>%
  summarise(targetHerbRichness = sum(n, na.rm = TRUE)) %>%
  ungroup()

### * Arrhenatherion species (species richness) ####
speciesRichness_arrh <- speciesRichness %>%
  filter(targetArrhenatherion == "yes") %>%
  summarise(arrhRichness = sum(n, na.rm = TRUE)) %>%
  ungroup()

### * ffh6510 species (species richness) ####
speciesRichness_ffh6510 <- speciesRichness %>%
  filter(ffh6510 == "yes") %>%
  summarise(ffh6510Richness = sum(n, na.rm = TRUE)) %>%
  ungroup()

### * ffh6210 species (species richness) ####
speciesRichness_ffh6210 <- speciesRichness %>%
  filter(ffh6210 == "yes") %>%
  summarise(ffh6210Richness = sum(n, na.rm = TRUE)) %>%
  ungroup()

### * leanIndicator species (species richness) ####
speciesRichness_leanIndicator <- speciesRichness %>%
  filter(leanIndicator == "yes") %>%
  summarise(leanIndicatorRichness = sum(n, na.rm = TRUE)) %>%
  ungroup()

### * table33 (species richness) ####
speciesRichness_table33_2 <- speciesRichness %>%
  filter(table33 == "2") %>%
  summarise(table33_2Richness = sum(n, na.rm = TRUE)) %>%
  ungroup()
speciesRichness_table33_3 <- speciesRichness %>%
  filter(table33 == "3") %>%
  summarise(table33_3Richness = sum(n, na.rm = TRUE)) %>%
  ungroup()
speciesRichness_table33_4 <- speciesRichness %>%
  filter(table33 == "4") %>%
  summarise(table33_4Richness = sum(n, na.rm = TRUE)) %>%
  ungroup()

### * table34 (species richness) ####
speciesRichness_table34_2 <- speciesRichness %>%
  filter(table34 == "2") %>%
  summarise(table34_2Richness = sum(n, na.rm = TRUE)) %>%
  ungroup()
speciesRichness_table34_3 <- speciesRichness %>%
  filter(table34 == "3") %>%
  summarise(table34_3Richness = sum(n, na.rm = TRUE)) %>%
  ungroup()

### * implement in sites data set (species richness) ####
sites <- sites %>%
  right_join(speciesRichness_all, by = "id") %>%
  right_join(speciesRichness_rlg, by = "id") %>%
  right_join(speciesRichness_rlb, by = "id") %>%
  right_join(speciesRichness_target, by = "id") %>%
  right_join(speciesRichness_targetHerb, by = "id") %>%
  right_join(speciesRichness_arrh, by = "id") %>%
  right_join(speciesRichness_ffh6510, by = "id") %>%
  right_join(speciesRichness_ffh6210, by = "id") %>%
  right_join(speciesRichness_leanIndicator, by = "id") %>%
  right_join(speciesRichness_table33_2, by = "id") %>%
  right_join(speciesRichness_table33_3, by = "id") %>%
  right_join(speciesRichness_table33_4, by = "id") %>%
  right_join(speciesRichness_table34_2, by = "id") %>%
  right_join(speciesRichness_table34_3, by = "id") %>%
  ### Calcute the ratio of target species richness of total species richness
  mutate(targetRichratio = targetRichness / speciesRichness)

### b Species eveness and shannon --------------------------------------

data <- species %>%
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0))) %>%
  pivot_longer(-name, names_to = "id", values_to = "value") %>%
  pivot_wider(names_from = "name", values_from = "value") %>%
  column_to_rownames("id") %>%
  vegan::diversity(index = "shannon") %>%
  as_tibble(rownames = NA) %>%
  rownames_to_column(var = "id") %>%
  mutate(id = factor(id)) %>%
  rename(shannon = value)
sites <- sites %>%
  left_join(data, by = "id") %>%
  mutate(eveness = shannon / log(speciesRichness))

rm(list = ls(pattern = "[^species|traits|sites]"))


## 4 Biotope types #####################################################

### a Calculate types --------------------------------------------------

biotopetypes <- sites %>%
  select(
    id, table33_2Richness, table33_3Richness, table33_4Richness,
    table33Cov, table34_2Richness, table34_3Richness, targetRichness,
    targetHerbRichness, arrhRichness, targetCov, leanCov, arrhCov,
    targetHerbCov, nitrogenCov
  ) %>%
  mutate(
    table33Rich_proof = if_else(
      table33_2Richness >= 2 | table33_2Richness + table33_3Richness >= 3 |
        table33_2Richness + table33_3Richness + table33_4Richness >= 4,
      "yes", "no"
    ),
    table33Cov_proof = if_else(
      table33Cov >= 25,
      "yes", "no"
    ),
    table33_proof = if_else(
      table33Rich_proof == "yes" & table33Cov_proof == "yes",
      "yes", "no"
    ),
    table34Rich_proof = if_else(
      table34_2Richness >= 2 | table34_2Richness + table34_3Richness >= 3,
      "yes", "no"
    ),
    G312_GT6210_type = if_else(
      table33_proof == "yes" & table34Rich_proof == "yes",
      "yes", "no"
    ),
    GE_proof = if_else(
      targetHerbCov >= 12.5 & targetRichness >= 20 & nitrogenCov < 25,
      "yes", "no"
    ),
    G214_GE6510_type = if_else(
      GE_proof == "yes" &
        arrhRichness >= 1 &
        arrhCov > 0.5 &
        leanCov >= 25 &
        targetHerbCov >= 12.5,
      "yes", "no"
    ),
    G212_LR6510_type = if_else(
      GE_proof == "yes" &
        arrhRichness >= 1 &
        arrhCov > 0.5 &
        leanCov < 25,
      "yes", "no"
    ),
    G214_GE00BK_type = if_else(
      GE_proof == "yes" &
        arrhRichness == 0 &
        leanCov >= 25 &
        targetHerbCov >= 12.5,
      "yes", if_else(
        GE_proof == "yes" &
          arrhCov >= 0.5 &
          leanCov >= 25 &
          targetHerbCov >= 12.5,
        "yes", "no"
      )
    ),
    G213_GE00BK_type = if_else(
      GE_proof == "yes" &
        arrhRichness == 0 &
        leanCov >= 25 &
        targetHerbCov < 12.5,
      "yes", if_else(
        GE_proof == "yes" &
          arrhCov >= 0.5 &
          leanCov >= 25 &
          targetHerbCov < 12.5,
        "yes", "no"
      )
    ),
    G213_type = if_else(
      leanCov >= 25,
      "yes", "no"
    ),
    G212_type = if_else(
      leanCov >= 1 &
        leanCov < 25 &
        targetHerbCov >= 12.5 &
        targetHerbRichness >= 10,
      "yes", "no"
    ),
    G211_type = if_else(
      leanCov >= 1 &
        leanCov < 25 &
        targetHerbCov >= 1 &
        targetHerbRichness >= 5,
      "yes", "no"
    ),
    biotopeType = if_else(
      G312_GT6210_type == "yes", "G312-GT6210", if_else(
        G214_GE6510_type == "yes", "G214-GE6510", if_else(
          G212_LR6510_type == "yes", "G212-LR6510", if_else(
            G214_GE00BK_type == "yes", "G214-GE00BK", if_else(
              G213_GE00BK_type == "yes", "G213-GE00BK", if_else(
                G213_type == "yes", "G213", if_else(
                  G212_type == "yes", "G212", if_else(
                    G211_type == "yes",
                    "G211", "other"
                  )
                )
              )
            )
          )
        )
      )
    ),
    biotopeType = as_factor(biotopeType)
  ) %>%
  select(id, biotopeType, -ends_with("proof"), -starts_with("table")) %>%
  mutate(
    ffh6510 = str_match(biotopeType, "6510"),
    ffh6210 = str_match(biotopeType, "6210"),
    baykompv = as_factor(str_sub(biotopeType, start = 1, end = 4))
  ) %>%
  unite(ffh, ffh6510, ffh6210, sep = "") %>%
  mutate(
    ffh = str_replace(ffh, "NA", ""),
    ffh = as_factor(str_replace(ffh, "NA", "non-FFH")),
    biotopeType = as_factor(biotopeType),
    biotopePoints = if_else(
      biotopeType == "G312-GT6210", 13, if_else(
        biotopeType == "G214-GE6510", 12, if_else(
          biotopeType == "G212-LR6510", 9, if_else(
            biotopeType == "G214-GE00BK", 12, if_else(
              biotopeType == "G213-GE00BK", 9, if_else(
                biotopeType == "G213", 8, if_else(
                  biotopeType == "G212", 8, if_else(
                    biotopeType == "G211",
                    6, 0
                  )
                )
              )
            )
          )
        )
      )
    ),
    min8 = as_factor(if_else(biotopePoints >= 8, "yes", "no")),
    min9 = as_factor(if_else(biotopePoints >= 9, "yes", "no"))
  )
sites <- left_join(sites, biotopetypes, by = "id") %>%
  select(
    -targetHerbCov, -arrhCov, -leanCov, -nitrogenCov, -table33Cov,
    -targetHerbRichness, -arrhRichness, -leanIndicatorRichness,
    -ffh6510Richness, -ffh6210Richness, -table33_2Richness,
    -table33_3Richness, -table33_4Richness, -table34_2Richness,
    -table34_3Richness
  )
traits <- traits %>%
  select(
    -targetArrhenatherion, -table30, -table33, -table34,
    -nitrogenIndicator, -nitrogenIndicator2, -leanIndicator,
    -grazingIndicator, -ruderalIndicator
  )

### b Calculate constancy ---------------------------------------------

data <- sites %>%
  select(id, plot, surveyYear, ffh) %>%
  group_by(plot) %>%
  mutate(count = n()) %>%
  filter(count == max(count)) %>%
  pivot_wider(id_cols = -id,
              names_from = "surveyYear",
              values_from = "ffh") %>% # group_by(plot) %>%
  rename(x17 = "2017", x18 = "2018", x19 = "2019") %>%
  mutate(changeType = ifelse(
    (x17 == "non-FFH" & x18 != "non-FFH" & x19 != "non-FFH"),
    "better", ifelse(
      (x17 != "non-FFH" & x18 == "non-FFH" & x19 != "non-FFH") |
        (x17 == "non-FFH" & x18 != "non-FFH" & x19 == "non-FFH") |
        (x17 == "non-FFH" & x18 == "non-FFH" & x19 != "non-FFH") |
        (x17 != "non-FFH" & x18 != "non-FFH" & x19 == "non-FFH"),
      "change", ifelse(
        (x17 == "non-FFH" & x18 == "non-FFH" & x19 == "non-FFH"),
        "non-FFH", ifelse(
          x17 == "6510" & x18 == "6510" & x19 == "6510",
          "FFH6510", ifelse(
            x17 == "6210" & x18 == "6210" & x19 == "6210",
            "FFH6210", ifelse(
              x17 != "non-FFH" & x18 != "non-FFH" & x19 != "non-FFH",
              "any-FFH", "worse"
            )
          )
        )
      )
    )
  )) %>%
  select(plot, changeType)
sites <- left_join(sites, data, by = "plot")

rm(list = ls(pattern = "[^species|traits|sites]"))


## 5 Finalization ######################################################

### a Rounding ---------------------------------------------------------

sites <- sites %>%
  mutate(across(
    c(
      NtotalPerc, targetCovratio, graminoidCovratio, targetRichratio,
      shannon, eveness
    ),
    ~ round(.x, digits = 3)
  )) %>%
  mutate(across(
    c(distanceRiver, accumulatedCov),
    ~ round(.x, digits = 1)
  ))

### b Final selection of variables -------------------------------------

sites <- sites %>%
  filter(accumulatedCov > 0) %>%
  select(
    id, plot, block,
    # space
    location, locationYear, riverkm, distanceRiver,
    # time
    surveyYear,
    # local site characteristics
    exposition, side,
    # response variables
    accumulatedCov, speciesRichness,
    # conservation
    rlgRichness,
    # legal evaluation
    biotopeType, ffh, biotopePoints,
    botanist
    # not included
    # response variables:  shannon, eveness, graminoidCovratio, ruderalCov
    # conservation: targetRichness, targetRichratio, targetCovratio
    # legal evaluation: changeType, baykompv, min8, min9
    )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save processed data ###############################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Data ###
write_csv(
  sites,
  here("data", "processed", "data_processed_sites.csv")
)
