#' Title
#'
#' @param .data
#' @param round_numeric
#' @param digits
#'
#' @return
#' @export
#'
#' @examples
convert_cols_to_character <- function(.data, round_numeric = TRUE, digits = 5) {

  if (round_numeric) {
    .data <-
      .data |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::where(is.numeric),
          \(x) round(x, digits = 5)
        )
      )
  }

  .data |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric),
        \(x) x |>
          as.character() |>
          stringr::str_replace("[.]", ",")
      ),
      dplyr::across(
        tidyselect::everything(),
        as.character
      )
    )
}


#' Title
#'
#' @param .data
#' @param .template
#' @param .select_template_columns
#'
#' @return
#' @export
#'
#' @examples
add_missing_columns <- function(
    .data, .template,
    .select_template_columns = TRUE
) {

  not_available <- setdiff(
    names(.template),
    names(.data)
  )

  if (length(not_available) > 0)
    message(
      paste0(
        "Unavailable columns in data: ",
        paste(not_available, collapse = ", ")
      )
    )

  # Add missing columns filled with NA
  # (data type becomes logical)
  .data[not_available] <- NA

  if (.select_template_columns) {
    .data <- .data |>
      dplyr::select(
        tidyselect::all_of(names(.template))
      )
  }
  .data
}

# x <- tibble::tibble(
#   x = c(5.23234, 4, 1.23, -1.888756, NA),
#   y = c(1:5),
#   z = c("A", "b", "c", "D", "f")
# )
#
# y <- tibble::tibble(
#   b = c(4, 2),
#   x = c(3, 3)
# )
#
# convert_cols_to_character(x)
# convert_cols_to_character(x, round = FALSE)
#
# add_missing_columns(y, x)
# add_missing_columns(y, x, .select_template_columns = FALSE)
# x |>
#   add_missing_columns(y) |>
#   convert_cols_to_character()

#' Title
#'
#' @param .data
#' @param template_path
#' @param export_path
#' @param overwrite
#' @param start_col
#' @param start_row
#' @param col_names
#'
#' @return
#' @export
#'
#' @examples
export_to_template <- function(
    .data,
    template_path = system.file(
      "extdata",
      "miljogifter-leveransmall_2024.xlsx",
      package = "MoCiS2.wtse"
    ),
    export_path = "miljogifter-leveransmall-ifylld.xlsx",
    overwrite = FALSE,
    start_col = 2,
    start_row = 6,
    col_names = FALSE

) {

  # Copy empty template (from package installation) file to export directory
  base::file.copy(
    from = template_path,
    to = export_path,
    overwrite = overwrite,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  if (base::file.exists(export_path)) {

    # Connect to workbook
    wb <- openxlsx2::wb_load(
      file = export_path
    )

    purrr::walk(
      names(.data),
      \(x) wb$add_data(
        sheet = x,
        x = .data[[x]],
        start_col = start_col,
        start_row = start_row,
        col_names = col_names,
        na.strings = NULL
      )
    )

    openxlsx2::wb_save(
      wb = wb,
      file = export_path,
      overwrite = TRUE
    )
  }
}

#' Title
#'
#' @param path
#' @param sheet
#' @param ul_data
#' @param ul_names
#' @param lr_names
#'
#' @return
#' @export
#'
#' @examples
read_xlsx_sgu_template <- function(
    path,
    sheet,
    ul_data = c(6, 2),
    ul_names = c(1, 2),
    lr_names = c(1, NA),
    offset = 1,
    col_types = sgu_template_col_types(sheet),
    ...
) {

  # skip columns 1, rows 2:5
  col_names <- readxl::read_xlsx(
    path,
    range = cellranger::cell_limits(
      ul = ul_names, lr = lr_names, sheet = sheet
    )
  ) |>
    names()

  x <- readxl::read_xlsx(
    path,
    range = cellranger::cell_limits(
      ul = ul_data,
      lr = c(NA, length(col_names) + offset),
      sheet = sheet
    ),
    col_names = col_names,
    col_types = col_types,
    ...
  )

  x |>
    dplyr::mutate(
      dplyr::across(tidyselect::where(is.POSIXct), \(x) as.Date(x))
    )

}

#' Title
#'
#' @param .sheet
#'
#' @return
#' @export
#'
#' @examples
sgu_template_col_types <- function(.sheet) {
  x <- dplyr::case_when(
    .sheet == "BESTALLARE" ~ list(rep("text", 7)),
    .sheet == "PROVMETADATA" ~ list(c(rep("text", 2), rep("numeric", 2), rep("text", 6), "date", "numeric", rep("text", 6))),
    .sheet == "PROVDATA_BIOTA" ~ list(c("text", "text", "numeric", "text", "numeric", "text")),
    .sheet == "DATA_MATVARDE" ~ list(c(rep("text", 8), "numeric", rep("text", 3), rep("numeric", 3), rep("text", 3), "date", rep("text", 3), "date", rep("text", 4))),
    .sheet == "MEDDELANDE" ~ list(rep("text", 2))
  )
  unlist(x)
}
