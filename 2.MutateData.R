# ============================================================
# Groups: eg (evergreen), sg (summergreen), ol (openland)
# Gets wide and long versions with both and then also extra for the age bins with no species observation


library(dplyr)
library(tidyr)
library(stringr)
rm()
north_america_pollen <- readRDS("data/north_america_pollen.rds")

# ====================================================================
# STEP 1: Load and clean the translation table=
# ==================================================================

trans <- read.csv(
  "taxaNames_classified.csv",
  header      = FALSE,
  col.names   = c("taxon", "group", "group_full"),
  quote       = "\"",
  strip.white = TRUE
)

trans <- trans %>%
  # Remove blank rows
 filter(!is.na(taxon) & taxon != "") %>%
  mutate(
    # Lowercase + trim for case-insensitive matching
    taxon = str_trim(str_to_lower(taxon)),
    group = str_trim(str_to_lower(group))
  ) %>%
  # Drop "uk" (unknown/undifferentiated) and any true NAs
filter(!is.na(group) & group != "uk" & group != "na") %>%
  # Keep only the three target short codes
  filter(group %in% c("eg", "sg", "ol")) %>%
  # Drop duplicates — keep first occurrence per taxon
  distinct(taxon, .keep_all = TRUE)


message("Translation table loaded: ", nrow(trans), " classified taxa")

# =============================================================================
# STEP 2: Pivot  wide  table to long, join translation table, drop unclassified taxa
# =================================================================================

meta_cols <- c("siteid", "depth", "age", "lat", "long")

pollen_long <- north_america_pollen %>%
  tidyr::pivot_longer(
    cols      = -all_of(meta_cols),
    names_to  = "taxon",
    values_to = "count"
  ) %>%
  mutate(
    # Lowercase + trim to match translation table keys
    taxon = str_trim(str_to_lower(taxon))
  ) %>%
  # Join group (eg  sg ol)
  left_join(trans %>% dplyr::select(taxon, group), by = "taxon") %>%
  # Drop taxa not in the translation table or classified as "uk"
 filter(!is.na(group)) %>%
  # Drop zero / NA counts
  filter(!is.na(count) & count > 0)

rm(north_america_pollen)

message("Pollen long table: ", nrow(pollen_long), " rows after filtering")


# =================================================================================
# step 3: Add age bins=
# Breaks: -100, 0, 5000, 10000, 15000, 20000, 25000=
# =============================================================================
#labels_period = c('0.05 - 0.5 ka', '0.5 - 2 ka', '2 - 4 ka', '4 - 6 ka', '6 - 8 ka', '8 - 10 ka', '10 - 12 ka')
#breaks_LGM = c(-74, 0.1, 0.35, 0.7, seq(1.5, 21.5, by=1))
age_breaks <- c(-74, 100, 1000, 3000, 5000,7000, 9000,11000,13000, 15000, 17000, 19000, 21000)
age_labels <- c("50","500","2000","4000","6000","8000","10000","12000","14000","16000","18000", "20000") 
#c(-100, 50, 500, 2000, 4000, 6000, 8000, 10000, 12000, 14000, 16000, 18000, 20000)

pollen_long <- pollen_long %>%
  mutate(
    age_bin = cut(
      age,
      breaks = age_breaks,
      labels = age_labels,
      include.lowest = TRUE,
      right = TRUE
    )
  ) %>%
  # Drop samples outside the defined age range
  filter(!is.na(age_bin))

# ================================================================================
# STEP 4: Convert counts to percent within each sample
# ==========================================================================

pollen_pct <- pollen_long %>%
group_by(siteid, depth, age, age_bin, lat, long) %>%
  dplyr::mutate(
    sample_total = sum(count, na.rm = TRUE),
    pct= (count / sample_total)
  ) %>%
  ungroup()

# ==================================================================================================
# STEP 5: Aggregate percent by functional group per sample
# ==================================================================================

pollen_by_group <- pollen_pct %>%
  group_by(siteid, depth, age, age_bin, lat, long, group) %>%
  summarise(
    group_pct = sum(pct, na.rm = TRUE),
    .groups = "drop"
  )

# ===========================================================================
# STEP 6: Pivot wide
# siteid  depth  age  age_bin  lat  long  eg  sg  ol
# =======================================================================================

pollen_wide_groups <- pollen_by_group %>%
  tidyr::pivot_wider(
    names_from= group,
    values_from = group_pct,
    values_fill = 0
  ) %>%
  # Guarantee all three columns exist even if a group has no data
  {
    df <- .
    for (col in c("eg", "sg", "ol")) {
      if (!col %in% names(df)) df[[col]] <- 0
    }
    df
  } %>%
  # Reorder columns 
  dplyr::select(siteid, depth, age, age_bin, lat, long, eg, sg, ol) %>%
  arrange(siteid, depth, age)

# ================================================================================
# STEP 3b: Build extra dataframes —
# ====================================================================================

# all siteid x age_bin combinations that should exis
all_siteids  <- unique(pollen_wide_groups$siteid)
all_age_bins <- factor(age_labels, levels = age_labels)

complete_grid <- tidyr::expand_grid(
  siteid  = all_siteids,
  age_bin = all_age_bins
)

# Site-level coordinates (use first occurrence per site)
site_coords <- pollen_wide_groups %>%
  dplyr::distinct(siteid, lat, long)

# Aggregate wide table to siteid x age_bin 
pollen_wide_extra <- pollen_wide_groups %>%
 group_by(siteid, age_bin) %>%
  summarise(
    ol = mean(ol, na.rm = TRUE),
    eg = mean(eg, na.rm = TRUE),
    sg = mean(sg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Replace NaN  with  NA
  mutate(
    ol = dplyr::na_if(ol, NaN),
    eg = dplyr::na_if(eg, NaN),
    sg = dplyr::na_if(sg, NaN)
  ) %>%
  # Join onto the complete grid so every bin is present per site
  right_join(complete_grid, by = c("siteid", "age_bin")) %>%
  # Re-attach site coordinates
 left_join(site_coords, by = "siteid") %>%
  # Enforce age_bin factor order then sort
 mutate(age_bin = factor(age_bin, levels = age_labels)) %>%
 dplyr::select(siteid, age_bin, lat, long, ol, eg, sg) %>%
 arrange(siteid, age_bin)

#Long version of the extra table 

pollen_long_extra <- pollen_wide_extra %>%
  tidyr::pivot_longer(
    cols      = c(ol, eg, sg),
    names_to  = "group",
    values_to = "group_pct"
  ) %>%
  arrange(siteid, age_bin, group)

message("Extra wide table: ", nrow(pollen_wide_extra), " rows (",
        length(all_siteids), " sites x ", length(age_labels), " bins)")
message("Extra long table: ", nrow(pollen_long_extra), " rows")

# ========================================================================
# STEP 7: Save trhem
# =========================================================================

# RDS preserves the ordered factor levels of age_bin
saveRDS(pollen_wide_groups,   "Data/pollenWide.rds")

# RDS preserves the ordered factor levels of age_bin
saveRDS(pollen_by_group,   "Data/pollenLong.rds")

# RDS preserves factor order for age_bin
saveRDS(pollen_wide_extra,    "Data/pollenWideExtra.rds")

# RDS preserves factor order for age_bin
saveRDS(pollen_long_extra,    "Data/pollenLongExtra.rds")

# =======================================================================
#sanity checks
# =======================================================================

# Original: eg + sg + ol should sum to ~1 per sample
message("\n--- Original wide sanity check ---")
pollenWide %>%
  mutate(total = eg + sg + ol) %>%
  summarise(
    mean_total = round(mean(total), 3),
    min_total  = round(min(total),  3),
    max_total  = round(max(total),  3)
  ) %>%
  print()

# Extra: confirm every site has exactly n_bins rows
message("\n--- Extra wide row count per site (should all equal ",
        length(age_bin), ") ---")
pollenWideExtra %>%
  count(siteid) %>%
  summarise(
    min_bins = min(n),
    max_bins = max(n),
    all_complete = all(n == length(age_labels))
  ) %>%
  print()


