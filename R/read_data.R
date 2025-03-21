
#' Given PROV_KOD_ORIGINAL, splits into a table of individual PROV_KOD_ORIGINAL and corresponding pooled PROV_KOD_ORIGINAL
#'
#' @param PROV_KOD_ORIGINAL An NRM ACCNR
#'
#' @returns A tibble
#' @export
#'
#' @examples
#' unpool("C2016/00937-00939")
unpool <- function(PROV_KOD_ORIGINAL){
  if (str_detect(PROV_KOD_ORIGINAL, "-")){ #homogenat
    head <- str_extract(PROV_KOD_ORIGINAL, ".*/")
    first <- str_extract(PROV_KOD_ORIGINAL, "(?<=/)([^-]*)") |> as.integer()
    last <- str_extract(PROV_KOD_ORIGINAL, "(?<=-)(.*)") |> as.integer()
    all <- first:last |> as.character()
    PROV_KOD_ORIGINAL_IND <- paste0(head, str_pad(all, width = 5, pad = "0"))
    tibble(
      PROV_KOD_ORIGINAL_POOL = PROV_KOD_ORIGINAL,
      PROV_KOD_ORIGINAL = PROV_KOD_ORIGINAL_IND,
      ANTAL = last - first + 1
    )
  }
  else
  {
    tibble(
      PROV_KOD_ORIGINAL_POOL = PROV_KOD_ORIGINAL,
      PROV_KOD_ORIGINAL = PROV_KOD_ORIGINAL,
      ANTAL = 1
    )
  }
}


#' Title
#'
#' @param .data
#'
#' @returns
#' @export
#'
#' @examples
results_longer <- function(.data) {
  .data |>
    tidyr::pivot_longer(
      -(PROV_KOD_ORIGINAL:ORGAN),
      names_to = "NRM_PARAMETERKOD",
      values_to = "MATVARDETAL"
    )
}

#' Reads the results sheet from a lab-protocol
#'
#' @param path
#' @param sheet
#' @param .has_provid
#' @param na_values
#' @param remove_na
#' @param pivot_longer
#'
#' @returns
#' @export
#'
#' @examples
read_lab_file <- function(
    path,
    sheet = "results",
    .has_provid = TRUE,
    na_values = c("-99.99", "N/A", "-99.9"),
    remove_na = TRUE,
    pivot_longer = TRUE
){

  input_data <- readxl::read_excel(
    path, sheet = sheet,
    skip = 1,
    na = na_values,
    .name_repair = "unique_quiet"
  )

  new_labels <- c(
    "PROV_KOD_ORIGINAL", "RAPPORT_KOD_LABB",  "PROV_KOD_LABB",
    "GENUS", "PROVPLATS_ANALYSMALL", "ORGAN"
  )

  if (.has_provid) {
    names(input_data)[1:6] <- new_labels
  } else {
    names(input_data)[1:5] <- new_labels[-2]
  }

  out <- input_data |>
    dplyr::filter(
      !is.na(PROV_KOD_ORIGINAL)
    ) |>
    dplyr::mutate(
      PROV_KOD_LABB = as.character(PROV_KOD_LABB),
      # Check if "<" rather than "-" is used for LOQ
      dplyr::across(
        tidyselect::where(is.character),
        \(x) stringr::str_replace(x, "<", "-")
      ),
      dplyr::across(
        !tidyselect::matches(new_labels), as.numeric
      )
    ) |>
    dplyr::select(-tidyselect::contains("..."))

  if (pivot_longer) {
    out <- results_longer(out)

    if (remove_na) {
      out <- out|>
        dplyr::filter(!is.na(MATVARDETAL))
    }
  }

  out
}

#' Title
#'
#' @param path
#' @param variable
#' @param sheet
#' @param .has_provid
#' @param na_values
#' @param pivot_longer
#'
#' @returns
#' @export
#'
#' @examples
read_lab_file2 <- function(
    path,
    variable,
    sheet,
    .has_provid = TRUE,
    na_values = c("-99.99", "N/A"),
    pivot_longer = TRUE
) {

  # No remove_na option in this function!

  input_data <- readxl::read_excel(
    path = path, sheet = sheet,
    skip = 1,
    na = na_values,
    .name_repair = "unique_quiet"
  )

  new_labels <- c(
    "PROV_KOD_ORIGINAL", "RAPPORT_KOD_LABB", "PROV_KOD_LABB",
    "GENUS", "PROVPLATS_ANALYSMALL"
  )

  if (.has_provid) {
    names(input_data)[1:5] <- new_labels
  } else {
    names(input_data)[1:4] <- new_labels[-2]
  }

  out <- input_data |>
    dplyr::filter(
      !is.na(PROV_KOD_ORIGINAL), PROV_KOD_ORIGINAL != "0"
    ) |>
    dplyr::mutate(
      PROV_KOD_LABB = as.character(PROV_KOD_LABB),
      # Check if "<" rather than "-" is used for LOQ
      dplyr::across(
        tidyselect::where(is.character),
        \(x) stringr::str_replace(x, "<", "-")
      ),
      dplyr::across(
        !tidyselect::matches(new_labels),
        as.numeric
      )
    ) |>
    dplyr::select(
      # Could we make dropping of columns optional?
      # Negative effects on pivot_longer()-command below?
      -tidyselect::any_of(new_labels[2:5]),
      -tidyselect::contains("...")
    )

  if (pivot_longer) {
    out <- out |>
      tidyr::pivot_longer(
        -PROV_KOD_ORIGINAL,
        # Could we use only numeric columns here instead?
        # where(is.numeric),
        names_to = "NRM_PARAMETERKOD",
        values_to = variable
      )
  }
  out
}

#' Title
#'
#' @param path
#' @param sheet
#'
#' @returns
#' @export
#'
#' @examples
fix_date <- Vectorize(function(x){
  x <- as.numeric(x)

  if (is.na(x))
    return(NA)
  if (x < 100000)
    as.Date(x, origin = "1899-12-30") |>
    as.character() # Excel-date
  else
    as.Date(as.character(x), "%Y%m%d") |>
    as.character()
})

#' Title
#'
#' @param path
#' @param sheet
#'
#' @returns
#' @export
#'
#' @examples
fix_date_tmp <- function(x) {

  x <- as.numeric(x)

  dplyr::case_when(
    is.na(x) ~ NA,
    # Excel date
    x < 100000 ~ as.Date(x, origin = "1899-12-30") |>
      as.character(),
    # UNIX time epoch
    x > 100000 ~ anytime::anydate(x) |> as.character(),
    # Default
    .default = as.Date(as.character(x), "%Y%m%d") |>
      as.character()
  )
}


#' Title
#'
#' @param path
#' @param sheet
#'
#' @returns
#' @export
#'
#' @examples
read_lab_file_date <- function(
    path,
    sheet = "date of analysis",
    .has_provid = TRUE,
    na_values = c("-99.99", "N/A"),
    dates_as_character = TRUE,
    pivot_longer = TRUE
) {

  input_data <- readxl::read_excel(
    path, sheet = sheet,
    na = na_values,
    .name_repair = "unique_quiet"
  )

  new_labels <- c(
    "PROV_KOD_ORIGINAL", "RAPPORT_KOD_LABB", "PROV_KOD_LABB",
    "GENUS", "PROVPLATS_ANALYSMALL"
  )

  if (.has_provid) {
    names(input_data)[1:5] <- new_labels
  } else {
    names(input_data)[1:4] <- new_labels[-2]
  }

  out <- input_data |>
    dplyr::filter(
      !is.na(PROV_KOD_ORIGINAL), !(PROV_KOD_ORIGINAL == 0)
    ) |>
    dplyr::mutate(
      PROV_KOD_LABB = as.character(PROV_KOD_LABB)
    ) |>
    dplyr::select(
      -tidyselect::any_of(new_labels[2:5]),
      -tidyselect::contains("...")
    ) |>
    dplyr::filter(
      PROV_KOD_ORIGINAL != "0"
    ) |>
    dplyr::mutate(
      dplyr::across(-1 & !where(is.logical), \(x) janitor::convert_to_date(x))
      # dplyr::across(-1, fix_date)
      # dplyr::across(where(is.POSIXct), \(x) as.numeric(x) |> anytime::anydate())
    )
  # mutate(ANALYS_DAT = fix_date(ANALYS_DAT))

  if (dates_as_character) {
    out <- out |>
      mutate(
        across(where(is.Date), as.character)
      )
  } else {
    out <- out |>
      mutate(
        across(where(is.Date), as.Date)
      )
  }

  if (pivot_longer) {
    out <- out |>
      tidyr::pivot_longer(
        -PROV_KOD_ORIGINAL,
        names_to = "NRM_PARAMETERKOD",
        values_to = "ANALYS_DAT"
      )
  }

  out

}

# If class is logical, janitor::convert_to_date() fails
# and code interrupts with this error message:
# no applicable method for 'convert_to_datetime_helper' applied to an object of class "logical"
# One potential solution incorporated in read_lab_file_date()
# tibble(
#   x = c("20180412", "2011-01-12"),
#   d = c(42097, 43015),
#   y = c(TRUE, FALSE)
# ) |>
#   mutate(
#     across(!where(is.logical), janitor::convert_to_date)
#   )

#' Title
#'
#' @param path
#' @param sheet
#'
#' @returns
#' @export
#'
#' @examples
read_lab_file_weight <- function(
    path,
    sheet = 8,
    .has_provid = TRUE,
    remove_na = TRUE
){

  input_data <- readxl::read_excel(
    path = path,
    sheet = sheet,
    .name_repair = "unique_quiet"
  )

  new_labels <- c(
    "PROV_KOD_ORIGINAL", "RAPPORT_KOD_LABB", "PROV_KOD_LABB",
    "GENUS", "PROVPLATS_ANALYSMALL", "DWEIGHT", "WWEIGHT"
  )

  if (.has_provid) {
    names(input_data)[1:7] <- new_labels
  } else {
    names(input_data)[1:6] <- new_labels[-2]
  }

  out <- input_data |>
    dplyr::select(
      -tidyselect::any_of(new_labels[2:5]),
      -tidyselect::contains("...")
    ) |>
    dplyr::mutate(
      DWEIGHT = as.numeric(DWEIGHT),
      WWEIGHT = as.numeric(WWEIGHT)
    )

  if (remove_na) {
    out <- out |>
      dplyr::filter(!(is.na(DWEIGHT) & is.na(WWEIGHT)))
  }

  out
}

#' Title
#'
#' @param path
#' @param sheet
#'
#' @returns
#' @export
#'
#' @examples
read_lab_file_general <- function(path, sheet = "general info"){

  info <- readxl::read_excel(
    path = path,
    sheet = sheet,
    .name_repair = "unique_quiet"
  )

  tibble::tibble(
    LABB = as.character(info[5, 2]),
    PROV_BERED = as.character(info[8, 2]),
    PROVKARL = as.character(info[9, 2]),
    ANALYS_MET = as.character(info[10, 2]),
    UTFOR_LABB = if_else(str_detect(info[6, 2], "Click to choose"), "EJ_REL", as.character(info[6, 2])),
    ANALYS_INSTR = as.character(info[11, 2])
  ) |>
    dplyr::mutate(
      # ACES subdepartment should not be reported
      LABB = dplyr::if_else(stringr::str_detect(LABB, "ACES"), "ACES", LABB),
      LABB = dplyr::if_else(stringr::str_detect(LABB, "Ume"), "UMU_KEM_INST", LABB)
    )
}

#' Title
#'
#' @param path
#' @param sheet
#'
#' @returns
#' @export
#'
#' @examples
read_lab_file_ackr <- function(path, sheet = "general info"){

  readxl::read_excel(
    path = path,
    sheet = sheet,
    skip = 27,
    col_names = c("NRM_PARAMETERKOD", "ACKREDITERAD_MET"),
    .name_repair = "unique_quiet"
  ) |>
    dplyr::filter(
      !is.na(NRM_PARAMETERKOD),
      !(ACKREDITERAD_MET == "…Click to choose…")
    )
}

#' Extracts information from lab protocol
#'
#' @param path
#' @param negative_for_nondetect
#' @param codes_path
#' @param has_provid This should be FALSE for old sheets without a column for NRM provid
#'
#' @returns
#' @export
#'
#' @examples
moc_read_lab <- function(
    path,
    negative_for_nondetect = TRUE,
    codes_path = system.file("extdata", "codelist_wtse.xlsx", package = "MoCiS2.wtse"),
    has_provid = TRUE,
    na_values = c("-99.99", "N/A", "-99.9"),
    remove_na = TRUE,
    fuzzyjoin = 2,
    max_dist = 2,
    join = TRUE,
    dates_as_character = TRUE,
    summary_fn = NULL,
    ...
){
  suppressMessages({

    # Read input data
    results_wide <- read_lab_file(
      path,
      .has_provid = has_provid,
      na_values = na_values,
      remove_na = remove_na,
      pivot_longer = FALSE
    )

    if(is.function(summary_fn)) {
      results_wide <- summary_fn(results_wide, ...)
    }

    # Add optional calculated variables here?
    # Much easier to calculate in wide format,
    # otherwise we need to use summarize()
    # results_wide <- results_wide |>
    #   add_clc_sums_capillary(.bound = "medium")

    # Why do we read the data again?
    # Would it be possible to just pivot
    # results_wide?
    # results <- read_lab_file(
    #   path,
    #   .has_provid = has_provid,
    #   na_values = na_values,
    #   remove_na = remove_na,
    #   pivot_longer = TRUE
    # )

    results <- results_longer(results_wide)

    uncertainty <- read_lab_file2(
      path,
      "MATOSAKERHET",
      "uncertainty",
      .has_provid = has_provid
    )

    LOD <- read_lab_file2(
      path,
      "DETEKTIONSGRANS_LOD",
      "LOD",
      .has_provid = has_provid
    ) |>
      dplyr::mutate(
        DETEKTIONSGRANS_LOD = abs(DETEKTIONSGRANS_LOD)
      )

    LOQ <- read_lab_file2(
      path,
      "RAPPORTERINGSGRANS_LOQ",
      "LOQ",
      .has_provid = has_provid
    ) |>
      dplyr::mutate(
        RAPPORTERINGSGRANS_LOQ = abs(RAPPORTERINGSGRANS_LOQ)
      )

    general <- read_lab_file_general(path)

    ackr <- read_lab_file_ackr(path)

    weight <- read_lab_file_weight(
      path,
      .has_provid = has_provid
    )

    dates <- read_lab_file_date(
      path,
      .has_provid = has_provid,
      dates_as_character = dates_as_character
    )

    # Code lists
    koder_substans <- get_codes_substances(codes_path)

    koder_prc <- get_codes_prc(codes_path)

    # Allow for different max_dist - options for stations and species?
    koder_stationer <- results |>
      dplyr::select(PROVPLATS_ANALYSMALL) |>
      dplyr::distinct() |>
      get_codes_sites(
        codes_path,
        fuzzyjoin = fuzzyjoin,
        max_dist = max_dist
      )

    koder_art <- results |>
      dplyr::select(LATIN = GENUS) |>
      dplyr::distinct() |>
      get_codes_species(
        codes_path,
        fuzzyjoin = fuzzyjoin,
        max_dist = max_dist
      )

    if (join) {

      # Join data
      data <- results |>
        dplyr::left_join(
          uncertainty,
          dplyr::join_by(PROV_KOD_ORIGINAL, NRM_PARAMETERKOD)
        ) |>
        dplyr::left_join(
          LOD,
          dplyr::join_by(PROV_KOD_ORIGINAL, NRM_PARAMETERKOD)
        ) |>
        dplyr::left_join(
          LOQ,
          dplyr::join_by(PROV_KOD_ORIGINAL, NRM_PARAMETERKOD)
        ) |>
        dplyr::left_join(
          dates,
          dplyr::join_by(PROV_KOD_ORIGINAL, NRM_PARAMETERKOD)
        ) |>
        dplyr::left_join(
          weight,
          dplyr::join_by(PROV_KOD_ORIGINAL)
        ) |>
        dplyr::bind_cols(general) |>
        dplyr::left_join(
          koder_substans,
          dplyr::join_by(NRM_PARAMETERKOD)
        ) |>
        dplyr::left_join(
          ackr,
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

      # Deal with sill/strömming
      data <- moc_fix_herring(data)

      # Deal with measurement variables.
      # Note that NAs are filtered out in the final steps!
      data <- data |>
        dplyr::mutate(
          MATVARDETAL_ANM = dplyr::if_else(
            (abs(MATVARDETAL) <= RAPPORTERINGSGRANS_LOQ) |
              ((MATVARDETAL < 0) & (negative_for_nondetect)), "<", "", missing = ""
          ),
          MATVARDETAL = dplyr::if_else(
            rep(negative_for_nondetect, n()), abs(MATVARDETAL), MATVARDETAL
          ),
          MATV_STD = dplyr::case_when(
            (near(MATVARDETAL, DETEKTIONSGRANS_LOD) | (MATVARDETAL < DETEKTIONSGRANS_LOD)) & (MATVARDETAL_ANM == "<") ~ "b",
            ((MATVARDETAL < RAPPORTERINGSGRANS_LOQ) | near(MATVARDETAL, RAPPORTERINGSGRANS_LOQ)) & (MATVARDETAL_ANM == "<") ~ "q",
            MATVARDETAL > RAPPORTERINGSGRANS_LOQ ~ "",
            ((MATVARDETAL > DETEKTIONSGRANS_LOD) | near(MATVARDETAL, DETEKTIONSGRANS_LOD)) & (is.na(RAPPORTERINGSGRANS_LOQ)) ~ "",
            is.na(DETEKTIONSGRANS_LOD) & is.na(RAPPORTERINGSGRANS_LOQ) ~ ""
          ),
          #MATVARDESPAR = ifelse(((MATVARDETAL < RAPPORTERINGSGRANS_LOQ) | near(MATVARDETAL, RAPPORTERINGSGRANS_LOQ)) & (MATVARDETAL > DETEKTIONSGRANS_LOD) & !(near(MATVARDETAL, DETEKTIONSGRANS_LOD)), "Ja", NA),
          MATVARDESPAR = dplyr::if_else(
            ((MATVARDETAL < RAPPORTERINGSGRANS_LOQ)) & !near(MATVARDETAL, RAPPORTERINGSGRANS_LOQ) & (MATVARDETAL > DETEKTIONSGRANS_LOD) & !(near(MATVARDETAL, DETEKTIONSGRANS_LOD)), "Ja", NA),
          MATOSAKERHET = dplyr::if_else(MATV_STD != "", NA, MATOSAKERHET),
          MATOSAKERHET_ENHET = dplyr::if_else(is.na(MATOSAKERHET), NA, MATOSAKERHET_ENHET)
        ) |>
        dplyr::filter(!is.na(MATVARDETAL)) |>
        dplyr::mutate_if(
          is.character, \(x) dplyr::if_else(stringr::str_detect(x, "Click to choose"), NA, x)
        )

      # Need to grab "NRM_PARAMETERKOD", "PROV_BERED", "PROVKARL", "ANALYS_MET", "ANALYS_INSTR"
      # from codelist for prc-type measurements.
      # Use an UPDATE query - this is not in MoCiS2 on github
      data <- dplyr::rows_update(
        x = data,
        y = koder_prc,
        by = "NRM_PARAMETERKOD",
        unmatched = "ignore"
      )

      moc_read_warnings(data)

      data |>
        filter(!is.na(PARAMETERNAMN))
    }
    else {
      list(
        results_wide = results_wide,
        results = results,
        uncertainty = uncertainty,
        LOD = LOD,
        LOQ = LOQ,
        general = general,
        ackr = ackr,
        weight = weight,
        dates = dates,
        koder_art = koder_art,
        koder_stationer = koder_stationer
      )
    }

  })
}
#' Title
#'
#' @param record
#'
#' @returns
#' @export
#'
#' @examples
add_header_info <- function(record){
  header <- record$raw[1]
  # See code list for explanations
  dplyr::mutate(
    record,
    YEAR = as.numeric(str_sub(header, 1, 4)),
    WEEK = as.numeric(str_sub(header, 5, 6)),
    DAY= as.numeric(str_sub(header, 7, 7)),
    GENUS = stringr::str_sub(header, 12, 15),
    SEX2 = stringr::str_sub(header, 45, 45),
    LOC = stringr::str_sub(header, 46, 49),
    MYEAR = as.numeric(stringr::str_sub(header, 25, 26)),
    MYEAR = dplyr::if_else(MYEAR > 50, MYEAR + 1900, MYEAR + 2000),
    PROVTAG_DAT = dplyr::case_when(
      (WEEK < 53) & (DAY %in% 1:7) ~ as.character(as.Date(stringr::str_sub(header, 1, 7), "%Y%W%u")),
      (WEEK > 79) ~ as.character(YEAR), # Week unknown
      (DAY %in% c(0, 8, 9)) ~ as.Date(paste0(stringr::str_sub(header, 1, 6), "3"), "%Y%W%u") |>
        str_sub(1, 7) # Day unknown, week uncertain
    )
  )
}

#' Title
#'
#' @param record
#'
#' @returns
#' @export
#'
#' @examples
add_id_cols <- function(record){
  accnr <- dplyr::filter(record, stringr::str_detect(NRM_CODE, "ACCNR")) |> dplyr::pull(VALUE)
  if (length(accnr) == 0)
    warning(paste("No ACCNR for record:", record$row[1]))
  if (length(accnr) > 1)
    warning(paste("Multiple ACCNR for record:", record$row[1], "picking first."))
  dplyr::mutate(
    record,
    ACCNR = accnr[1],
    LAB_KOD = dplyr::if_else(Group == "lab", VALUE, NA)
  ) |>
    tidyr::fill(LAB_KOD) |>
    dplyr::filter(!stringr::str_detect(NRM_CODE, "ACCNR"))
}

#' Title
#'
#' @param record
#'
#' @returns
#' @export
#'
#' @examples
add_bio_cols <- function(record){
  dplyr::mutate(
    record,
    NRM_CODE = dplyr::if_else(NRM_CODE == "KPRL", "KRPL", NRM_CODE), # Common misspelling
    # UNIT = ifelse(NRM_CODE %in% c("TOTV", "TOTL", "KRPL"), str_extract(VALUE, "[^,]+$") |>  tolower(), UNIT),
    ORGAN = dplyr::if_else(
      NRM_CODE == "ALDR", dplyr::case_when(
        str_extract(VALUE, "[^,]+$") == "7" ~ "FJALL",
        str_extract(VALUE, "[^,]+$") == "9" ~ "OTOLIT",
        str_extract(VALUE, "[^,]+$") == "10" ~ "OPERCULUM"
      ),
      ORGAN
    ),
    VALUE = ifelse(NRM_CODE %in% c("TOTV", "TOTL", "KRPL", "ALDR"), str_extract(VALUE, "[^,]*"), VALUE),
    TOTL = ifelse(NRM_CODE %in% c("TOTL", "TOTLH"), as.numeric(VALUE), NA),
    KRPL = ifelse(NRM_CODE == "KRPL", as.numeric(VALUE), NA),
    TOTV = ifelse(NRM_CODE %in% c("TOTV", "TOTVH"), as.numeric(VALUE), NA),
    ALDR = ifelse(NRM_CODE %in% c("ALDR", "ALDRH"), as.numeric(VALUE), NA),
    ANM = ifelse(NRM_CODE %in% c("ANM", "NOTE"), VALUE, NA),
    SEX = ifelse(NRM_CODE == "SEX", as.numeric(VALUE), NA),
    NHOM = ifelse(str_length(ACCNR) == 17, as.numeric(str_sub(ACCNR, 13, 17)) - as.numeric(str_sub(ACCNR, 7, 11)) + 1, 1),
    NHOM = ifelse(NRM_CODE == "NHOM", str_extract(VALUE, "[0-9]+") |> as.numeric(), NHOM),
    NHOM = max(NHOM),
    NRM_CODE = ifelse((NRM_CODE %in% c("TOTV", "TOTL", "KRPL", "ALDR")) & str_detect(ACCNR, "-"), paste0(NRM_CODE, "H"), NRM_CODE)
  ) |>
    fill(TOTL, KRPL, TOTV, ALDR, ANM, SEX, .direction = "downup")
}
#' Title
#'
#' @param record
#'
#' @returns
#' @export
#'
#' @examples
add_prc_cols <- function(record){
  dplyr::mutate(
    record,
    FPRC = dplyr::if_else(stringr::str_detect(NRM_CODE, "^FPRC"), as.numeric(VALUE), NA),
    LTPRC = dplyr::if_else(stringr::str_detect(NRM_CODE, "^LTPRC"), as.numeric(VALUE), NA),
    MTPRC = dplyr::if_else(stringr::str_detect(NRM_CODE, "^MTPRC"), as.numeric(VALUE), NA)
  ) |>
    dplyr::group_by(LAB_KOD) |>
    tidyr::fill(FPRC, LTPRC, MTPRC, .direction = "downup") |>
    dplyr::ungroup()
}

#' Title
#'
#' @param record
#'
#' @returns
#' @export
#'
#' @examples
frame_record <- function(record){
  record |>
    tidyr::fill(ORGAN, LAB) |>
    add_header_info() |>
    add_id_cols() |>
    add_bio_cols() |>
    add_prc_cols() |>
    dplyr::filter((label == "no")|is.na(label)) |>
    dplyr::select(-label)
}


#' Title
#'
#' @param prc_path
#' @param codes_path
#'
#' @returns
#' @export
#'
#' @examples
moc_read_prc <- function(prc_path, codes_path = system.file("extdata", "codelist_prc.xls", package = "MoCiS2.wtse"), enc = "latin1"){

  codes <- readxl::read_excel(codes_path) |>
    dplyr::select(Group, NRM_CODE, LAB, ORGAN, PARAMETER, label)

  tibble::tibble(
    raw = readLines(prc_path, encoding = enc)
  ) |>
    dplyr::mutate(
      NRM_CODE = stringr::str_extract(raw, "[^+]([^=]*)") |> trimws() |> toupper(),
      NRM_CODE = dplyr::if_else(stringr::str_sub(raw, 1, 1) %in% c("1", "2"), "HEADER", NRM_CODE),
      VALUE = stringr::str_extract(raw, "[^=]+$") |> trimws()
    ) |>
    dplyr::left_join(codes, dplyr::join_by(NRM_CODE)) |>
    dplyr::mutate(
      PARAMETER = dplyr::if_else(is.na(PARAMETER), NRM_CODE, PARAMETER),
      rec_id = cumsum(str_sub(raw, 1, 1) %in% c("1", "2"))
    ) |>
    dplyr::group_by(rec_id) |>
    tidyr::nest() |>
    dplyr::pull(data) |>
    purrr::map_df(frame_record) |>
    dplyr::mutate(
      VALUE = as.numeric(VALUE),
      VALUE = dplyr::if_else(VALUE %in% c(-9, -.9), NA, VALUE)
    )
}
