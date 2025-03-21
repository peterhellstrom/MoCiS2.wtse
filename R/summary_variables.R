#' Title
#'
#' @param .cols
#' @param .na_values
#' @param .na_numeric
#' @param na.rm
#'
#' @returns
#' @export
#'
#' @examples
add_sum_contaminants <- function(
    .cols,
    .na_values = c(-9, -99, -99.99),
    .na_numeric = -99.99,
    .bound = "medium",
    na.rm = TRUE
) {
  dplyr::case_when(
    dplyr::if_all( {{ .cols }} , is.na) ~ NA,
    dplyr::if_all( {{ .cols }} , \(x) x %in% .na_values) ~ .na_numeric,
    .default = base::rowSums(
      dplyr::across(
        {{ .cols }} ,
        \(x) fix_below_loq(
          base::replace(x, x %in% .na_values, NA),
          .bound = .bound
        )
      ), na.rm = na.rm
    )
  )
}

#' Title
#'
#' @param .data
#' @param .bound
#' @param .add_PCBSUM
#'
#' @returns
#' @export
#'
#' @examples
add_clc_sums_capillary <- function(
    .data,
    .bound = "medium",
    .add_PCBTOT = TRUE
) {
  out <- .data |>
    dplyr::mutate(
      HCHSUM = add_sum_contaminants(matches("HCH$|LINDAN"), .bound = .bound),
      DDTKSUM = add_sum_contaminants(matches("^DD"), .bound = .bound),
      PCB7SUM = add_sum_contaminants(matches("^CB"), .bound = .bound),
      DDTKSUM_PCB7SUM = DDTKSUM / PCB7SUM
    )
  if (.add_PCBTOT) {
    out <- out |>
      mutate(
        PCBTOT = dplyr::case_when(
          stringr::str_detect(PROVPLATS_ANALYSMALL, "^Q") ~ `CB-138` * 9.06,
          TRUE ~ `CB-138` * 8.22
        ),
        .after = PCB7SUM
      )
  }
  out
}


#' Title
#'
#' @param .data
#' @param .bound
#'
#' @returns
#' @export
#'
#' @examples
add_bfr_sums_capillary <- function(
    .data,
    .bound = "medium"
) {

  # if (any(names(.data) == "BDE-28")) {
  if (rlang::has_name(.data, "BDE-28")) {
    out <- .data |>
      dplyr::mutate(
        PBDE5SUM = add_sum_contaminants(matches("^(?!.*28).*BDE.*$", perl = TRUE), .bound = .bound),
        PBDE6SUM = case_when(
          is.na(`BDE-28`) ~ NA_real_,
          TRUE ~ add_sum_contaminants(matches("^BDE"), .bound = .bound)
        )
      )
  } else {
    out <- .data |>
      dplyr::mutate(
        PBDE5SUM = add_sum_contaminants(matches("^(?!.*28).*BDE.*$", perl = TRUE), .bound = .bound)
      )
  }
  out
}

#' Title
#'
#' @param .data
#' @param .bound
#' @param .add_PCBSUM
#'
#' @returns
#' @export
#'
#' @examples
add_clc_bfr_sums_capillary <- function(
    .data,
    .bound = "medium",
    .add_PCBTOT = TRUE
) {
  out <- .data |>
    dplyr::mutate(
      HCHSUM = add_sum_contaminants(matches("HCH$|LINDAN"), .bound = .bound),
      DDTKSUM = add_sum_contaminants(matches("^DD"), .bound = .bound),
      PCB7SUM = add_sum_contaminants(matches("^CB"), .bound = .bound),
      DDTKSUM_PCB7SUM = DDTKSUM / PCB7SUM
    )
  if (.add_PCBTOT) {
    out <- out |>
      mutate(
        PCBTOT = dplyr::case_when(
          stringr::str_detect(PROVPLATS_ANALYSMALL, "^Q") ~ `CB-138` * 9.06,
          TRUE ~ `CB-138` * 8.22
        ),
        .after = PCB7SUM
      )
  }

  # if (any(names(.data) == "BDE-28")) {
  if (rlang::has_name(.data, "BDE-28")) {
    out <- out |>
      dplyr::mutate(
        PBDE5SUM = add_sum_contaminants(matches("^(?!.*28).*BDE.*$", perl = TRUE), .bound = .bound),
        PBDE6SUM = add_sum_contaminants(matches("^BDE"), .bound = .bound)
      )
  } else {
    out <- out |>
      dplyr::mutate(
        PBDE5SUM = add_sum_contaminants(matches("^(?!.*28).*BDE.*$", perl = TRUE), .bound = .bound)
      )
  }

  out
}

#' Title
#'
#' @param path
#' @param summary_fn
#' @param ackr_sum
#' @param format
#'
#' @returns
#' @export
#'
#' @examples
import_clc_bfr_new_template <- function(
    path,
    summary_fn = NULL,
    ackr_sum = NULL,
    format = c("long", "wide"),
    ...
){
  format <- match.arg(format)

  if (format == "long") {

    out <- moc_read_lab(
      path,
      has_provid = FALSE,
      max_dist = 0,
      remove_na = FALSE,
      na_values = "",
      dates_as_character = FALSE,
      summary_fn = summary_fn,
      ...
    ) |>
      dplyr::mutate(
        source = basename(path),
        .before = 1L
      ) |>
      dplyr::mutate(
        ANALYS_INSTR = dplyr::case_when(
          stringr::str_detect(NRM_PARAMETERKOD, "SUM|TOT") ~ "BERAKNAT",
          TRUE ~ ANALYS_INSTR
        ),
        ACKREDITERAD_MET = dplyr::case_when(
          is.na(ACKREDITERAD_MET) & stringr::str_detect(NRM_PARAMETERKOD, "SUM$|TOT$") ~ ackr_sum,
          TRUE ~ ACKREDITERAD_MET
        ),
        MATOSAKERHET_TYP = dplyr::if_else(
          is.na(MATOSAKERHET), NA, "U2"
        )
      ) |>
      dplyr::select(-MATOSAKERHET_TYP)
  }
  if (format == "wide") {
    out <- read_lab_file(
      path,
      .has_provid = FALSE,
      pivot_longer = FALSE,
      remove_na = FALSE,
      na_values = "",
    ) |>
      dplyr::mutate(
        source = basename(clc_path),
        .before = 1
      )
    if (!is.null(summary_fn)) {
      out <- summary_fn(out, ...)
    }
  }

  out
}
