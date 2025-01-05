#' Title
#'
#' @param path 
#' @param negative_for_nondetect 
#' @param codes_path 
#' @param has_provid 
#' @param na_values 
#' @param remove_na 
#' @param max_dist 
#'
#' @return
#' @export
#'
#' @examples
moc_join_lab <- function(
    .x,
    negative_for_nondetect = TRUE, 
    codes_path = system.file("extdata", "codelist_wtse.xlsx", package = "MoCiS2.wtse"), 
    has_provid = TRUE,
    na_values = c("-99.99", "N/A", "-99.9"),
    remove_na = TRUE,
    fuzzyjoin = TRUE,
    max_dist = 2,
    remove_na_measurement = TRUE,
    remove_na_parameter_name = TRUE
){
  
  # NOTE! The arguments has_provid, na_values and remove_na are
  # not implemented in the code below, but are
  # rather "remnants" from moc_read_lab().
  
  # Code lists
  koder_substans <- get_codes_substances(codes_path)
  
  koder_prc <- get_codes_prc(codes_path)
  
  # Perhaps allow for different max_dist - options for stations and species?
  koder_stationer <- .x$results |> 
    dplyr::select(PROVPLATS_ANALYSMALL) |> 
    dplyr::distinct() |> 
    get_codes_sites(
      codes_path, fuzzyjoin = fuzzyjoin, max_dist = max_dist
    )
  
  koder_art <- .x$results |> 
    dplyr::select(LATIN = GENUS) |> 
    dplyr::distinct() |> 
    get_codes_species(
      codes_path, fuzzyjoin = fuzzyjoin, max_dist = max_dist
    )
  
  # Join data
  data <- .x$results |> 
    dplyr::left_join(
      .x$uncertainty, 
      dplyr::join_by(
        PROV_KOD_ORIGINAL, PROV_KOD_LABB, NRM_PARAMETERKOD
      )
    ) |> 
    dplyr::left_join(
      .x$LOD, 
      dplyr::join_by(
        PROV_KOD_ORIGINAL, PROV_KOD_LABB, NRM_PARAMETERKOD
      )
    ) |> 
    dplyr::left_join(
      .x$LOQ, 
      dplyr::join_by(
        PROV_KOD_ORIGINAL, PROV_KOD_LABB, NRM_PARAMETERKOD
      )
    ) |> 
    dplyr::left_join(
      .x$dates, 
      dplyr::join_by(
        PROV_KOD_ORIGINAL, PROV_KOD_LABB, NRM_PARAMETERKOD
      )
    ) |> 
    dplyr::left_join(
      .x$weight, 
      dplyr::join_by(
        PROV_KOD_LABB
      )
    ) |> 
    # This is different from what moc_read_lab() does.
    # We join methods variables in "general" on the name of the source file.
    # (so a source field must be available in the data).
    # Another approach would be to join on for instance NRM_PARAMETERKOD
    # and LABB (if joining on LABB, we would need to create this variable
    # before joining the general data frame.)
    dplyr::left_join(
      .x$general,
      dplyr::join_by(source)
    ) |> 
    dplyr::left_join(
      koder_substans, 
      dplyr::join_by(NRM_PARAMETERKOD)
    ) |> 
    dplyr::left_join(
      .x$ackr, 
      dplyr::join_by(NRM_PARAMETERKOD)
    )
  
  # Add species and station variables
  data <- data |> 
    dplyr::rename(LATIN = GENUS) |> 
    dplyr::left_join(
      koder_art, 
      dplyr::join_by(LATIN)
    ) |> 
    dplyr::left_join(
      koder_stationer, 
      dplyr::join_by(PROVPLATS_ANALYSMALL)
    )
  
  # Deal with herring (sill/strömming)
  data <- moc_fix_herring(data)
  
  # Deal with measurement variables
  data <- data |> 
    dplyr::mutate(
      MATVARDETAL_ANM = dplyr::if_else(
        (abs(MATVARDETAL) <= RAPPORTERINGSGRANS_LOQ) |
          ((MATVARDETAL < 0) & (negative_for_nondetect)), "<", NA_character_, missing = NA_character_
      ),
      MATVARDETAL = dplyr::if_else(
        rep(negative_for_nondetect, n()), 
        abs(MATVARDETAL), 
        MATVARDETAL
      ),
      MATV_STD = dplyr::case_when(
        (dplyr::near(MATVARDETAL, DETEKTIONSGRANS_LOD) | (MATVARDETAL < DETEKTIONSGRANS_LOD)) & 
          (MATVARDETAL_ANM == "<") ~ "b",
        ((MATVARDETAL < RAPPORTERINGSGRANS_LOQ) | dplyr::near(MATVARDETAL, RAPPORTERINGSGRANS_LOQ)) & 
          (MATVARDETAL_ANM == "<") ~ "q",
        MATVARDETAL > RAPPORTERINGSGRANS_LOQ ~ NA_character_,
        ((MATVARDETAL > DETEKTIONSGRANS_LOD) | dplyr::near(MATVARDETAL, DETEKTIONSGRANS_LOD)) & 
          (is.na(RAPPORTERINGSGRANS_LOQ)) ~ NA_character_, 
        is.na(DETEKTIONSGRANS_LOD) & is.na(RAPPORTERINGSGRANS_LOQ) ~ NA_character_
      ),
      # MATVARDESPAR = ifelse(((MATVARDETAL < RAPPORTERINGSGRANS_LOQ) | near(MATVARDETAL, RAPPORTERINGSGRANS_LOQ)) & (MATVARDETAL > DETEKTIONSGRANS_LOD) & !(near(MATVARDETAL, DETEKTIONSGRANS_LOD)), "Ja", NA),
      MATVARDESPAR = dplyr::if_else(
        ((MATVARDETAL < RAPPORTERINGSGRANS_LOQ)) & 
          !dplyr::near(MATVARDETAL, RAPPORTERINGSGRANS_LOQ) & 
          (MATVARDETAL > DETEKTIONSGRANS_LOD) & 
          !(near(MATVARDETAL, DETEKTIONSGRANS_LOD)), 
        "Ja", 
        NA
      ),
      MATOSAKERHET = dplyr::if_else(
        !is.na(MATV_STD), NA, MATOSAKERHET
      ),
      MATOSAKERHET_ENHET = dplyr::if_else(
        is.na(MATOSAKERHET), NA, MATOSAKERHET_ENHET
      )
    ) |> 
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character), 
        \(x) dplyr::if_else(stringr::str_detect(x, "Click to choose"), NA, x)
      )
    )
  
  # Remove NA measurements, but we haven't specified
  # NA values other than "pure" NAs, e.g. 0 and -99.99 is ignored!
  # i.e. argument na_values is ignored.
  if (remove_na_measurement) {
    data <- data |> 
      dplyr::filter(
        !is.na(MATVARDETAL)
      )
  }
  
  # Need to grab "NRM_PARAMETERKOD", "PROV_BERED", "PROVKARL", "ANALYS_MET", "ANALYS_INSTR" 
  # from codelist for prc-type measurements.
  # Use an UPDATE query:
  data <- dplyr::rows_update(
    x = data, 
    y = koder_prc, 
    by = "NRM_PARAMETERKOD", 
    unmatched = "ignore"
  )
  
  moc_read_warnings(data)
  
  if (remove_na_parameter_name) {
    data <- data |> 
      dplyr::filter(!is.na(PARAMETERNAMN))
  }
  
  data

}
