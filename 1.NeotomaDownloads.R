# ============================================================
# Neotoma2 Pollen Data Extraction - North America


library(neotoma2)
library(dplyr)
library(tidyr)
library(sf)
library(geojsonsf)

# ===================================================================
# Define bounding boxes per country in lat long
# ===================================================================

country_bbox <- list(
  Canada  = c(-141, 42,  -52, 83),
  USA_W   = c(-125, 24,  -95, 50),   # split US to manage load
  USA_E   = c( -95, 24,  -66, 50),
  Mexico  = c(-118, 14,  -86, 33)
)

# =============================================================
#Download pollen datasets per region
# ==================================================================

download_pollen <- function(bbox, label) {
  message("\n>>> Fetching datasets: ", label)
  
  datasets <- tryCatch(
    get_datasets(
      loc = bbox,
      datasettype = "pollen",
      all_data = TRUE
    ),
    error = function(e) {
      message("  ERROR fetching datasets for ", label, ": ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(datasets) || length(datasets) == 0) {
    message("  No datasets found for ", label)
    return(NULL)
  }
  
  message("  Found ", length(datasets), " datasets. Downloading samples...")
  
  downloads <- tryCatch(
    get_downloads(datasets, verbose = FALSE),
    error = function(e) {
      message("  ERROR downloading samples for ", label, ": ", e$message)
      return(NULL)
    }
  )
  
  return(downloads)
}

raw_downloads <- lapply(names(country_bbox), function(name) {
  download_pollen(country_bbox[[name]], name)
})
names(raw_downloads) <- names(country_bbox)

# Save raw downloads as a checkpoint (large objects - saves time if R crashes)
saveRDS(raw_downloads, "neotoma_raw_downloads.rds")
# raw_downloads <- readRDS("neotoma_raw_downloads.rds")


# =================================================================
#Extract and reshape samples into wide format
# ============================================================

extract_wide <- function(downloads, label) {
  if (is.null(downloads)) return(NULL)
  
  message("\n>>> Extracting samples: ", label)
  samp <- tryCatch(
    samples(downloads),
    error = function(e) {
      message("  ERROR in samples(): ", e$message)
      return(NULL)
    }
  )
  if (is.null(samp) || nrow(samp) == 0) {
    message("  No samples extracted for ", label)
    return(NULL)
  }
  samp_pollen <- as.data.frame(samp) %>%
    dplyr::filter(elementtype == "pollen") %>%# Keep only pollen 
    dplyr::select(siteid, depth, age, lat, long, variablename, value) %>%
    dplyr::group_by(siteid, depth, age, lat, long, variablename) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  
  # Pivot taxa to wideeee format
  samp_wide <- samp_pollen %>%
    tidyr::pivot_wider(
      id_cols     = c(siteid, depth, age, lat, long),
      names_from  = variablename,
      values_from = value,
      values_fill = 0
    )
  message("  Rows: ", nrow(samp_wide), " | Taxa columns: ", ncol(samp_wide) - 5)
  return(samp_wide)
}

canada_pollen  <- extract_wide(raw_downloads[["Canada"]],  "Canada")
usa_w_pollen   <- extract_wide(raw_downloads[["USA_W"]],   "USA_W")
usa_e_pollen   <- extract_wide(raw_downloads[["USA_E"]],   "USA_E")
mexico_pollen  <- extract_wide(raw_downloads[["Mexico"]],  "Mexico")


# ======================================================================
#sTEP 4: combine it all
# ================================================================

usa_pollen <- bind_rows(usa_w_pollen, usa_e_pollen) %>%
  group_by(siteid, depth, age, lat, long) %>%
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
harmonise_cols <- function(df_list) {
  all_cols <- unique(unlist(lapply(df_list, colnames)))
  lapply(df_list, function(df) {
    missing <- setdiff(all_cols, colnames(df))
    df[missing] <- 0
    df[, all_cols]  # reorder to same sequence
  })
}
#big combinewd NA dataset
na_list <- harmonise_cols(list(canada_pollen, usa_pollen, mexico_pollen))
north_america_pollen <- bind_rows(na_list)


# ============================================================================
# STEP 5: save it all please
# ===========================================================================================

write.csv(canada_pollen,       "canada_pollen.csv",        row.names = FALSE)
write.csv(usa_pollen,          "usa_pollen.csv",            row.names = FALSE)
write.csv(mexico_pollen,       "mexico_pollen.csv",         row.names = FALSE)
write.csv(north_america_pollen,"north_america_pollen.csv",  row.names = FALSE)

taxaheads = colnames(north_america_pollen)
taxanames <- taxaheads[-c(1,2,3,4,5)]
write.csv(taxanames, "taxaheads") # gets only the taxa names to translate later

saveRDS(north_america_pollen, "north_america_pollen.rds")

#sanityt check
#message("\nDone! Final table dimensions: ", nrow(north_america_pollen),
#       " rows x ", ncol(north_america_pollen), " columns")
