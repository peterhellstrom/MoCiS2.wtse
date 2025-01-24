#' Title
#'
#' @param url
#'
#' @returns
#' @export
#'
#' @examples
sgu_quick_import <- function(url) {
  x <- readr::read_delim(
    url,
    delim = ";",
    col_select = -c(HUMANT_PROV:RENINGSSTEG_FRITEXT, INT_RAPPORT, URSPRUNG),
    show_col_types = FALSE
  )

  # Convert measurement from text to numeric
  x <- x |>
    dplyr::mutate(
      MATVARDETAL = readr::parse_number(MATVARDE),
      .after = MATVARDE
    ) |>
    # Use "SMNH-style", i.e. values below LOQ reported as negative values
    # Add case for <LOD as well
    dplyr::mutate(
      MATVARDETAL = dplyr::case_when(
        MATV_STD %in% c("q", "b") ~ -MATVARDETAL,
        TRUE ~ MATVARDETAL
      )
    )

  x
}

#' Title
#'
#' @param .data
#' @param .kembiofys
#'
#' @returns
#' @export
#'
#' @examples
sgu_data_fix <- function(
    .data,
    .kembiofys = MoCiS2.wtse::get_code_list("kembiofys")
) {

  out <- .data |>
    dplyr::filter(!is.na(PROVTAG_SYFTE))

  out <- out |>
    dplyr::select(
      -c(HUMANT_PROV:RENINGSSTEG_FRITEXT, INT_RAPPORT)
    ) |>
    dplyr::rename(
      PROV_KOD_ORIGINAL_IVL = PROV_KOD_ORIGINAL
    ) |>
    dplyr::mutate(
      # import IVL data has several suffixes that has been added
      # during import to SGU, bu we only want the original SMNH
      # accnr:
      PROV_KOD_ORIGINAL = stringr::str_split_i(PROV_KOD_ORIGINAL_IVL, "_", 1),
      URSPRUNG = dplyr::case_when(
        URSPRUNG == "ivl-biota" ~ "IVL",
        TRUE ~ "NRM"
      ),
      PROV_KOD_ORIGINAL_IVL = dplyr::case_when(
        URSPRUNG == "IVL" ~ PROV_KOD_ORIGINAL_IVL,
        TRUE ~ NA_character_
      ),
      # variable MATVARDE is text, we create a new
      # numeric variable:
      MATVARDETAL = stringr::str_replace(MATVARDE, "^<", "-") |>
        readr::parse_number()
    ) |>
    dplyr::relocate(
      PROV_KOD_ORIGINAL,
      .before = PROV_KOD_ORIGINAL_IVL
    ) |>
    dplyr::mutate(
      # Change codes for CB138+163 and FPRC,
      # consistently use the ones in new/re-reported data
      UNIK_PARAMETERKOD = dplyr::case_when(
        UNIK_PARAMETERKOD == "CH07/118" ~ "CH07/509",
        UNIK_PARAMETERKOD == "CH12/65" ~ "CH12/68",
        TRUE ~ UNIK_PARAMETERKOD
      ),
      # IVL data was mostly reported as ppm, rather
      # than ppb, we change that here:
      MATVARDETAL = dplyr::case_when(
        ENHET == "mg.kg-1.lv-1" ~ MATVARDETAL * 1000,
        TRUE ~ MATVARDETAL
      ),
      ENHET = dplyr::case_when(
        ENHET == "mg.kg-1.lv-1" ~ "ng.g-1.lv-1",
        TRUE ~ ENHET
      )
    ) |>
    dplyr::select(-PARAMETERNAMN) |>
    # Join main groups for compounds,
    # and add parameter names (as we changed some codes above):
    dplyr::left_join(
      .kembiofys |>
        dplyr::select(
          UNIK_PARAMETERKOD = ID,
          PARAMETERNAMN = `Rekommenderat svenskt namn`,
          HUVUDGRUPPER = Huvudgrupper,
          SUBGRUPPER = Sub_grupper
        ),
      dplyr::join_by(UNIK_PARAMETERKOD)
    ) |>
    dplyr::relocate(
      PARAMETERNAMN, .after = UNIK_PARAMETERKOD
    )

  out
}

#' Title
#'
#' @param .data
#' @param codes_path
#'
#' @returns
#' @export
#'
#' @examples
add_nrm_code <- function(
    .data,
    codes_path = system.file("extdata", "codelist_wtse.xlsx", package = "MoCiS2.wtse"),
    join_by_instr = TRUE
) {

  # Note that variable ANALYS_INSTR must be registered and filled in codelist.
  # This is necessary, as the only way to separate DDT, DDD, DDE from
  # DDTK, DDDK, DDEK is by including info on analytical instrument

  koder_substans <- readxl::read_xlsx(
    codes_path,
    "PARAMETRAR"
  )

  koder_join <- koder_substans |>
    # Temporary solution, as I don't have any FPRCU
    dplyr::filter(
      NRM_PARAMETERKOD != "FPRCU"
    ) |>
    dplyr::select(
      NRM_PARAMETERKOD,
      UNIK_PARAMETERKOD,
      PARAMETERNAMN,
      ANALYS_INSTR
    )

  if (!join_by_instr) {
    .by <- dplyr::join_by(UNIK_PARAMETERKOD, PARAMETERNAMN)
  } else{
    .by <- dplyr::join_by(UNIK_PARAMETERKOD, PARAMETERNAMN, ANALYS_INSTR)
  }

  out <- .data |>
    dplyr::left_join(
      koder_join,
      .by
    )

}

# add_nrm_code() must be updated. Find other ways to identify duplicated
# entries. See duplicates:
# readxl::read_xlsx(
#   system.file("extdata", "codelist_wtse.xlsx", package = "MoCiS2.wtse"),
#   "PARAMETRAR"
# ) |>
#   filter(
#     n() > 1,
#     .by = c(UNIK_PARAMETERKOD)
#   ) |>
#   arrange(UNIK_PARAMETERKOD)

#' Title
#'
#' @param .data
#' @param .vars
#' @param codes_path
#'
#' @returns
#' @export
#'
#' @examples
sgu_data_to_wide <- function(
    .data,
    .vars,
    codes_path = system.file("extdata", "codelist_wtse.xlsx", package = "MoCiS2.wtse")
) {

  codes_substances <- get_codes_substances(path = codes_path)

  codes_substances <- codes_substances |>
    dplyr::filter(
      NRM_PARAMETERKOD %in% .vars
    ) |>
    dplyr::mutate(
      UNIK_PARAMETERKOD = rlang::set_names(
        UNIK_PARAMETERKOD,
        NRM_PARAMETERKOD
      )
    )

  .data |>
    dplyr::filter(
      NRM_PARAMETERKOD %in% .vars
    ) |>
    dplyr::select(
      ART, ORGAN, PROVPLATS_ID,
      PROV_KOD_ORIGINAL, RAPPORT_KOD_LABB,
      UNIK_PARAMETERKOD, MATVARDETAL
    ) |>
    tidyr::pivot_wider(
      values_from = MATVARDETAL,
      names_from = UNIK_PARAMETERKOD
    ) |>
    dplyr::rename(
      tidyselect::any_of(codes_substances$UNIK_PARAMETERKOD)
    ) |>
    dplyr::select(
      ART, ORGAN, PROVPLATS_ID,
      PROV_KOD_ORIGINAL,
      RAPPORT_KOD_LABB,
      tidyselect::matches(.vars)
    ) |>
    dplyr::arrange(
      PROV_KOD_ORIGINAL,
      RAPPORT_KOD_LABB
    )
}
