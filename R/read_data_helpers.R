#' Title
#'
#' @param data
#'
#' @returns
#' @export
#'
#' @examples
wtse_substance_group <- function(data) {

  data |>
    dplyr::mutate(
      substance_group = dplyr::case_when(
        stringr::str_detect(NRM_PARAMETERKOD, "HCH$|^DD|LINDAN") ~ "Pesticides",
        stringr::str_detect(NRM_PARAMETERKOD, "HCB") ~ "Solvents",
        stringr::str_detect(NRM_PARAMETERKOD, "^PCB|^CB|PCBSUM") ~ "PCBs",
        stringr::str_detect(NRM_PARAMETERKOD, "^BDE|HBCD") ~ "BFRs",
        stringr::str_detect(NRM_PARAMETERKOD, "^PF|^LPF|^BPF|FOSA") ~ "PFAS",
        stringr::str_detect(NRM_PARAMETERKOD, "TOTL|TOTV|BRED|FOSL|SKLV|KON") ~ "Biodata"
      ),
      .after = NRM_PARAMETERKOD
    )
}

#' Title
#'
#' @param data
#'
#' @returns
#' @export
#'
#' @examples
wtse_substance_group_sgu <- function(data) {

  data |>
    dplyr::mutate(
      substance_group = dplyr::case_when(
        stringr::str_detect(PARAMETERNAMN, "HCH|^DD") ~ "Pesticides",
        stringr::str_detect(PARAMETERNAMN, "Hexaklorbensen") ~ "Solvents",
        stringr::str_detect(PARAMETERNAMN, "PCB") ~ "PCBs",
        stringr::str_detect(PARAMETERNAMN, "PBDE|HBCD") ~ "BFRs",
        stringr::str_detect(PARAMETERNAMN, "^PF|^LPF|^BPF|FOSA") ~ "PFAS",
        stringr::str_detect(UNIK_PARAMETERKOD, "^CH12") ~ "Biodata"
      ),
      .after = UNIK_PARAMETERKOD
    )
}

# Used for CLC ITM template version 5 + BFR template!
#' Title
#'
#' @param .data
#' @param .cols
#'
#' @returns
#' @export
#'
#' @examples
add_smnh_variable_names <- function(.data, .cols = everything()) {

  .data |>
    dplyr::rename_with(
      \(x) stringr::str_replace_all(
        x,
        c(
          "pp\\-(.*)" = stringr::str_c("\\1","K"),
          "a-HCH" = "AHCH",
          "b-HCH" = "BHCH",
          "\\(|\\)|\\+" = "",
          "163" = "",
          "fat\\_percentage" = "FPRC"
        )
      ),
      .cols = dplyr::any_of(.cols)
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
fix_bfr_cols <- function(.data) {
  .data |>
    dplyr::rename_with(
      \(x) stringr::str_remove(x, "\\s+") |>
        stringr::str_replace_all(c(BDE = "BDE-")),
      tidyselect::starts_with("BDE")
    )
}

# Import from "new" SMNH templates, simple version
#' Title
#'
#' @param path
#' @param .has_provid
#'
#' @returns
#' @export
#'
#' @examples
read_lab_file_simple <- function(path, .has_provid = FALSE) {
  dplyr::left_join(
    read_lab_file(path, .has_provid = .has_provid, pivot_longer = FALSE),
    read_lab_file_weight(path, .has_provid = .has_provid),
    dplyr::join_by(PROV_KOD_ORIGINAL)
  ) |>
    dplyr::select(-DWEIGHT) |>
    dplyr::mutate(source = basename(path)) |>
    dplyr::relocate(source)
}
