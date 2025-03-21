# No info on LOD? Never reported in earlier templates!
# Is -99.99 < LOD?
# Create an empty data frame for now...
#' Title
#'
#' @param .data
#'
#' @returns
#' @export
#'
#' @examples
create_LOD_df <- function(.data, .empty = TRUE) {

  out <- .data |>
    dplyr::select(
      PROV_KOD_ORIGINAL,
      PROV_KOD_LABB,
      NRM_PARAMETERKOD
    )

  if (.empty) {
    out <- out |>
      dplyr::mutate(
        DETEKTIONSGRANS_LOD = NA_real_
      )
  } else {
    out <- out |>
      dplyr::mutate(
        # unit: ng.g-1.lv-1
        DETEKTIONSGRANS_LOD = case_when(
          stringr::str_detect(NRM_PARAMETERKOD, "HCB") ~ 0.2,
          stringr::str_detect(NRM_PARAMETERKOD, "HCH$|LINDAN") ~ 0.2,
          stringr::str_detect(NRM_PARAMETERKOD, "DDE|DDD") ~ 0.2,
          stringr::str_detect(NRM_PARAMETERKOD, "DDT") ~ 0.4,
          stringr::str_detect(NRM_PARAMETERKOD, "^CB") ~ 0.2,
          stringr::str_detect(NRM_PARAMETERKOD, "^BDE") ~ 0.02,
          stringr::str_detect(NRM_PARAMETERKOD, "HBCD") ~ 0.2,
          TRUE ~ NA_real_
        )
      )
  }

  out |>
    dplyr::filter(
      stringr::str_detect(NRM_PARAMETERKOD, "FPRC|TPRC", negate = TRUE)
    ) |>
    dplyr::mutate(
      DETEKTIONSGRANS_LOD = abs(DETEKTIONSGRANS_LOD)
    )
}

# No info on LOQ? Or can we find it in some Description sheet?
# Or is a negative value in fact LOQ? Test that approach here!
# Otherwise just create a blank column filled with NAs.
# Hint in older ITM templates: negative value is "< lowest standard dilution"
#' Title
#'
#' @param .data
#'
#' @returns
#' @export
#'
#' @examples
create_LOQ_df <- function(
    .data,
    negative_for_nondetect = TRUE,
    remove_prc = TRUE,
    na_values = -99.99
) {

  .data <- .data |>
    dplyr::select(
      PROV_KOD_ORIGINAL,
      PROV_KOD_LABB,
      NRM_PARAMETERKOD,
      MATVARDETAL
    )

  if (negative_for_nondetect) {
    .data <- .data |>
      dplyr::mutate(
        RAPPORTERINGSGRANS_LOQ = dplyr::case_when(
          MATVARDETAL %in% na_values ~ NA_real_,
          MATVARDETAL < 0 ~ MATVARDETAL,
          TRUE ~ NA_real_
        )
      ) |>
      dplyr::mutate(
        RAPPORTERINGSGRANS_LOQ = abs(RAPPORTERINGSGRANS_LOQ)
      )
  } else {
    .data <- .data |>
      dplyr::mutate(
        RAPPORTERINGSGRANS_LOQ = dplyr::case_when(
          MATVARDETAL %in% na_values ~ NA_real_,
          TRUE ~ NA_real_
        )
      )
  }

  if (remove_prc) {
    .data <- .data |>
      dplyr::filter(
        stringr::str_detect(NRM_PARAMETERKOD, "FPRC|TPRC", negate = TRUE)
      )
  }

  .data |>
    dplyr::select(
      -MATVARDETAL
    )
}

#' Title
#'
#' @param .data
#'
#' @returns
#' @export
#'
#' @examples
create_header_df <- function(.data) {
  .data |>
    dplyr::select(
      source,
      LABB = analysed_at,
      PROV_BERED = method_code_for_clean_up,
      ANALYS_MET = method_code_for_analysis
    ) |>
    dplyr::mutate(
      PROVKARL = NA_character_,
      UTFOR_LABB = "EJ_REL",
      ANALYS_INSTR = NA_character_
    ) |>
    dplyr::select(
      source, LABB, PROV_BERED, PROVKARL, ANALYS_MET, UTFOR_LABB, ANALYS_INSTR
    )
}

# This function could be developed:
# 1) Add more substances
# 2) Filter for substances only occurring in input data
# 3) (Optionally) allow for different output depending on another
#    variable, like time/year (before/after accreditation).
#' Title
#'
#' @param .data
#'
#' @returns
#' @export
#'
#' @examples
create_ackr_df <- function(.data) {
  .data |>
    dplyr::select(
      NRM_PARAMETERKOD
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      ACKREDITERAD_MET = dplyr::case_when(
        NRM_PARAMETERKOD == "FPRC" ~ "Nej",
        stringr::str_detect(NRM_PARAMETERKOD, "DDTKSUM") ~ "Ja",
        stringr::str_detect(NRM_PARAMETERKOD, "HCHSUM") ~ "Ja",
        stringr::str_detect(NRM_PARAMETERKOD, "PCB7SUM") ~ "Ja",
        stringr::str_detect(NRM_PARAMETERKOD, "^DD") ~ "Ja",
        stringr::str_detect(NRM_PARAMETERKOD, "^CB") ~ "Ja",
        stringr::str_detect(NRM_PARAMETERKOD, "HCH$") ~ "Ja",
        stringr::str_detect(NRM_PARAMETERKOD, "^HCB|^LINDAN") ~ "Ja",
        TRUE ~ "Nej"
      )
    )
}
