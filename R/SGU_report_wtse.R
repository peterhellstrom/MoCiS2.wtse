# Lägg till behandling på NRM av provet (homogenisering)?
# HOMOGEN~ACHEXDEE
# Ackrediterad met: ska sättas till Nej för biodata, inte 'Vet_ej'
#' Title
#'
#' @param .data 
#'
#' @return
#' @export
#'
#' @examples
fix_analysdata <- function(.data) {
  
  .data |> 
    dplyr::rename_with(
      \(x) stringr::str_replace_all(
        x, c("PROV_KOD_LABB" = "RAPPORT_KOD_LABB")
      )
    ) |> 
    mutate(
      ANALYS_MET = dplyr::case_when(
        ANALYS_MET == "Bromanalys5" ~ "ACES_BFR",
        # 1990s methods for CLC: UKG & UKO, possibly more.
        stringr::str_detect(ANALYS_MET, "^UK") ~ "ACES_CLC",
        stringr::str_detect(ANALYS_MET, "GC/ECD-PCB/OCP") ~ "ACES_CLC",
        TRUE ~ ANALYS_MET
      ),
      ANALYS_DAT = as.Date(ANALYS_DAT),
      LABB = stringr::str_remove(LABB, "o$") |> 
        toupper(),
      PROV_BERED = dplyr::case_when(
        stringr::str_detect(PROV_BERED, "PCB/OCP-BIOTA") ~ "ACHEXDEE",
        TRUE ~ PROV_BERED
      ),
      # ANALYS_INSTR = case_when(
      #   ANALYS_MET == "ACES_CLC" ~ "GC-ECD",
      #   ANALYS_MET == "ACES_BFR" ~ "GC-MS",
      #   TRUE ~ ANALYS_INSTR
      # ),
      ACKREDITERAD_MET = dplyr::case_when(
        ANALYS_MET == "ACES_CLC" & ANALYS_DAT >= as.Date("1999-05-06") ~ "Ja",
        ANALYS_MET == "ACES_CLC" & ANALYS_DAT < as.Date("1999-05-06") ~ "Nej",
        ANALYS_MET == "ACES_BFR" ~ "Nej",
        TRUE ~ ACKREDITERAD_MET
      ),
      MATOSAKERHET_TYP = dplyr::if_else(
        is.na(MATOSAKERHET), NA, "U2"
      )
    )
}

#' Title
#'
#' @param .data 
#'
#' @return
#' @export
#'
#' @examples
add_brood_comment <- function(.data) {
  
  # Assume no-relaying within site and year (although this HAS occurred)
  .data_sum <- .data |> 
    dplyr::select(
      PROV_KOD_ORIGINAL, PROVTAG_DAT, PROVPLATS_ANALYSMALL,
    ) |> 
    dplyr::mutate(
      PROVTAG_AR = year(PROVTAG_DAT)
    ) |> 
    dplyr::distinct() |> 
    dplyr::arrange(
      PROVTAG_AR, PROVTAG_DAT, PROVPLATS_ANALYSMALL, PROV_KOD_ORIGINAL
    ) |> 
    dplyr::summarize(
      n = dplyr::n(),
      KOMMENTAR_PROV = dplyr::case_when(
        n == 1 ~ NA_character_,
        n > 1 ~ glue::glue("{stringr::str_flatten(PROV_KOD_ORIGINAL, collapse = ', ', last = ' och ')} tillhör samma kull"),
        TRUE ~ NA_character_
      ),
      # We could join on PROVTAG_DAT as well, 
      # but consider case where complete brood was collected during two different days
      .by = c(PROVTAG_AR, PROVPLATS_ANALYSMALL)
    )
  
  .data |> 
    dplyr::mutate(
      PROVTAG_AR = year(PROVTAG_DAT)
    ) |> 
    dplyr::left_join(
      .data_sum |> dplyr::select(-n),
      dplyr::join_by(PROVPLATS_ANALYSMALL, PROVTAG_AR)
    ) |> 
    dplyr::select(-PROVTAG_AR)
  
}

#' Title
#'
#' @param biodata
#' @param analysdata
#' @param add_bio_pars
#'
#' @return
#' @export
#'
#' @examples
wtse_join_SGU <- function(
    biodata, 
    analysdata, 
    has_pools = FALSE,
    add_bio_pars = TRUE,
    codes_path = system.file("extdata", "codelist_wtse.xlsx", package = "MoCiS2.wtse"),
    measurement_cols = c(
      "TOTV", "TOTL", "BRED", "FOSL", "FOSV", 
      "SKTJ", "SKLV", "SKTJ", "SKVO", 
      "SKLI", "UTTO"
    ),
    remove_na_biodata = FALSE
){
  
  # Development: NOT READY
  # as many input columns (PROVMETADATA) are removed during the
  # summarize step! Must be joined back to summarized data somehow.
  if (has_pools) {
    bio_data <- purrr::map_df(
      unique(analysdata$PROV_KOD_ORIGINAL), 
      unpool
    ) |> 
      dplyr::left_join(
        biodata, 
        dplyr::join_by(PROV_KOD_ORIGINAL)
      ) |> 
      dplyr::mutate(
        KON = dplyr::if_else(
          stringr::str_detect(PROV_KOD_ORIGINAL_POOL, "-") & (KON %in% c("F", "M")),
          "X",
          KON
        )
      ) |>
      dplyr::group_by(
        PROV_KOD_ORIGINAL_POOL
      )
    
    bio_data <- bio_data |> 
      dplyr::summarize(
        PROVTAG_DAT = min(PROVTAG_DAT),
        ANTAL_DAGAR = max(ANTAL_DAGAR),
        KON = pool_sex(KON),
        ANTAL = unique(ANTAL),
        # ALDR = mean_or_na(ALDR),
        dplyr::across(
          tidyselect::any_of(measurement_cols), 
          mean_or_na
        )
      ) |> 
      dplyr::rename(
        PROV_KOD_ORIGINAL = PROV_KOD_ORIGINAL_POOL
      )
  } else (
    if (!is.null(analysdata)) {
      bio_data <- biodata |> 
        dplyr::semi_join(
          analysdata, 
          dplyr::join_by(PROV_KOD_ORIGINAL)
        )
    } else {
      bio_data <- biodata
    }
  )
  
  if (add_bio_pars){
    
    bio_measurements <- bio_data |> 
      dplyr::select(
        tidyselect::any_of(
          c(
            "PROV_KOD_ORIGINAL", 
            "ORGAN",
            # "PROVPLATS_ANALYSMALL", 
            measurement_cols
          )
        )
      ) |> 
      tidyr::pivot_longer(
        tidyselect::any_of(measurement_cols), 
        names_to = "NRM_PARAMETERKOD", 
        values_to = "MATVARDETAL"
      ) |> 
      dplyr::mutate(
        ORGAN = dplyr::case_when(
          ORGAN == "AGG" & NRM_PARAMETERKOD == "FOSL" ~ "EMBRYO",
          ORGAN == "AGG" & NRM_PARAMETERKOD == "FOSV" ~ "EMBRYO",
          ORGAN == "AGG" & NRM_PARAMETERKOD == "SKLV" ~ "SKAL",
          ORGAN == "AGG" & NRM_PARAMETERKOD == "SKTJ" ~ "SKAL",
          ORGAN == "AGG" & NRM_PARAMETERKOD == "SKLI" ~ "SKAL",
          ORGAN == "AGG" & NRM_PARAMETERKOD == "SKVO" ~ "SKAL",
          TRUE ~ ORGAN
        ),
        MATVARDETAL = dplyr::if_else(
          is.nan(MATVARDETAL), 
          NA, 
          MATVARDETAL
        ),
        NRM_PARAMETERKOD = dplyr::if_else(
          stringr::str_detect(PROV_KOD_ORIGINAL, "-"), 
          stringr::str_c(NRM_PARAMETERKOD, "H"), 
          NRM_PARAMETERKOD
        )
      )
    
    # Variables excluded from the code list:
    # vv/TV & and 4 variables ending with "_ENHET".
    # Full codelist has 17 columns, we select 12 here,
    # NOT in order of appearance in the sheet.
    kodlista <- readxl::read_excel(
      codes_path, 
      sheet = "PARAMETRAR"
    ) |> 
      dplyr::select(
        NRM_PARAMETERKOD, PARAMETERNAMN, UNIK_PARAMETERKOD, 
        LABB, UTFOR_LABB, ENHET, PROV_BERED, PROVKARL, 
        ANALYS_INSTR, ANALYS_MET, ACKREDITERAD_MET, PROV_LAGR
      )
    
    bio_measurements <- bio_measurements |> 
      dplyr::left_join(
        kodlista, 
        dplyr::join_by(NRM_PARAMETERKOD)
      )
    
    # Handle missing biodata, either remove missing values or
    # keep, setting MATV_STD to m.
    # Develop further? For instance if no embryo is present
    # should we set embryo length to 0, and add MATV_STD = n (not detected)?
    if (remove_na_biodata) {
      bio_measurements <- bio_measurements |> 
        dplyr::filter(
          !is.na(MATVARDETAL)
        )
    } else {
      bio_measurements <- bio_measurements |> 
        dplyr::mutate(
          MATV_STD = dplyr::case_when(
            is.na(MATVARDETAL) ~ "m", 
            TRUE ~ NA_character_
          )
        )
    }
    
    # bio_measurements does not contain all variables in analysdata
    # so necessary variables need to be filled after grouping on PROV_KOD_ORIGINAL.
    # An alternative approach would be to join missing variables
    # in bio_measurements from analysdata BEFORE binding rows.
    data <- dplyr::bind_rows(
      analysdata, 
      bio_measurements
    ) |> 
      dplyr::group_by(
        PROV_KOD_ORIGINAL
      ) |> 
      tidyr::fill(
        # Sampling location variables
        PROVPLATS_ID, PROVPLATS_ANALYSMALL, LOC, NAMN_PROVPLATS, LOKDIST,
        # Taxon info
        GENUS, ART, LATIN, DYNTAXA_TAXON_ID, ARTDIST,
        .direction = "downup"
      ) |>  
      dplyr::ungroup()
    
  } else {
    data <- analysdata
  }
  
  # Should more columns be removed (ORGAN?)
  data |> 
    dplyr::left_join(
      bio_data |> 
        dplyr::select(
          -tidyselect::any_of(c("PROVPLATS_ANALYSMALL", measurement_cols))
        ), 
      dplyr::join_by(PROV_KOD_ORIGINAL)
    )
}

#' Title
#'
#' @param data
#' @param sheet
#' @param file
#' @param template_path
#' @param program
#'
#' @return
#' @export
#'
#' @examples
wtse_format_SGU <- function(
    data, 
    sheet,
    template_path = system.file(
      "extdata", 
      "miljogifter_leveransmall.xlsx", 
      package = "MoCiS2.wtse"
    ),
    convert_to_character = TRUE,
    round_numeric = TRUE,
    digits = 5
){
  
  # Avoid scientific notation
  options(scipen = 999)
  
  template <- readxl::read_excel(
    template_path, sheet, n_max = 1
  ) |> 
    select(-1L)
  
  data <- data |> 
    dplyr::arrange(
      PARAMETERNAMN, 
      PROV_KOD_ORIGINAL
    ) |> 
    dplyr::select(
      dplyr::intersect(
        names(template), 
        names(data)
      )
    ) |> 
    dplyr::distinct()
  
  if (sheet %in% c("PROVMETADATA", "PROVDATA_BIOTA")) {
    data <- data |> 
      dplyr::arrange(PROV_KOD_ORIGINAL)
  }
  
  data <- add_missing_columns(data, template)
  
  if (convert_to_character) {
    data <- convert_cols_to_character(
      data, 
      round_numeric = round_numeric, 
      digits = digits
    )
  } else {
    if (round_numeric)
      data <- data |> 
        dplyr::mutate(
          dplyr::across(
            tidyselect::where(is.numeric), 
            \(x) round(x, digits)
          )
        )
  }
  
  data
  
}

#' Title
#'
#' @param .data 
#'
#' @returns
#' @export
#'
#' @examples
sgu_create <- function(.data) {
  purrr::map(
    c("PROVMETADATA", "PROVDATA_BIOTA", "DATA_MATVARDE") |> 
      rlang::set_names(), 
    \(x) wtse_format_SGU(
      .data, 
      x, 
      convert_to_character = FALSE, 
      round_numeric = TRUE,
      digits = 5
    )
  )
}
