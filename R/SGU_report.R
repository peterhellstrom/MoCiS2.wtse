#' Title
#'
#' @param sex 
#'
#' @return
#' @export
#'
#' @examples
pool_sex <- function(sex){
  if (all(is.na(sex)))
    return(NA)
  sexes <- na.omit(sex) |> unique()
  dplyr::if_else(length(sexes) == 1, sexes, "X")
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
moc_join_SGU <- function(
    biodata, 
    analysdata, 
    add_bio_pars = TRUE,
    codes_path = system.file("extdata", "codelist_wtse.xlsx", package = "MoCiS2.wtse")
){
  
  bio_pool_data <- purrr::map_df(
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
    dplyr::group_by(PROV_KOD_ORIGINAL_POOL) |> 
    dplyr::summarize(
      PROVTAG_DAT = min(PROVTAG_DAT),
      ANTAL_DAGAR = max(ANTAL_DAGAR),
      KON = pool_sex(KON),
      ANTAL = unique(ANTAL),
      ALDR = mean_or_na(ALDR),
      TOTV = mean_or_na(TOTV),
      TOTL = mean_or_na(TOTL)
    ) |> 
    dplyr::rename(
      PROV_KOD_ORIGINAL = PROV_KOD_ORIGINAL_POOL
    )
  
  if (add_bio_pars){
    
    kodlista <- readxl::read_excel(
      codes_path, 
      sheet = "PARAMETRAR"
    ) |> 
      dplyr::select(
        NRM_PARAMETERKOD, PARAMETERNAMN, UNIK_PARAMETERKOD, 
        LABB, UTFOR_LABB, 
        ENHET, 
        PROV_BERED, PROVKARL, 
        ANALYS_INSTR, ANALYS_MET, ACKREDITERAD_MET, 
        PROV_LAGR
      ) |> 
      # Note this step: ORGAN is hard-coded and assumed
      # to be "HELKROPP"
      dplyr::mutate(
        ORGAN = "HELKROPP"
      )
    
    bio_measurements <- bio_pool_data |> 
      dplyr::select(
        PROV_KOD_ORIGINAL, ALDR, TOTV, TOTL
      ) |> 
      tidyr::pivot_longer(
        c("ALDR", "TOTV", "TOTL"), 
        names_to = "NRM_PARAMETERKOD", 
        values_to = "MATVARDETAL"
      ) |> 
      dplyr::mutate(
        MATVARDETAL = dplyr::if_else(
          is.nan(MATVARDETAL), 
          NA, 
          MATVARDETAL
        ),
        NRM_PARAMETERKOD = dplyr::if_else(
          stringr::str_detect(PROV_KOD_ORIGINAL, "-"), 
          paste0(NRM_PARAMETERKOD, "H"), 
          NRM_PARAMETERKOD
        )
      ) |> 
      dplyr::filter(!is.na(MATVARDETAL)) |> 
      dplyr::left_join(
        kodlista, 
        dplyr::join_by(NRM_PARAMETERKOD)
      )
    
    data <- dplyr::bind_rows(
      analysdata, 
      bio_measurements
    ) |> 
      dplyr::group_by(PROV_KOD_ORIGINAL) |> 
      tidyr::fill(
        PROVPLATS_ID, 
        NAMN_PROVPLATS, 
        ART, 
        DYNTAXA_TAXON_ID, 
        .direction = "downup"
      ) |>  
      dplyr::ungroup()
    
    # Add species specific ORGAN for age-determination
    data <- data |> 
      dplyr::mutate(
        ORGAN = dplyr::if_else(
          stringr::str_sub(NRM_PARAMETERKOD, 1, 4) == "ALDR",
          dplyr::case_when(
            ART == "Gadda" ~ "CLEITRUM",
            ART %in% c("Stromming", "Sill") ~ "FJALL",
            ART %in% c("Tanglake", "Torsk", "Abborre", "Roding") ~ "OTOLIT"
          ),
          ORGAN
        ),
        ORGAN = dplyr::if_else(
          (stringr::str_sub(NRM_PARAMETERKOD, 1, 4) == "TOTL") & (ART == "Blamussla"), 
          "SKAL",
          ORGAN
        )
      )
  } else {
    data <- analysdata
  }
  
  data |> 
    dplyr::left_join(
      bio_pool_data, 
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
moc_write_SGU <- function(
    data, sheet, file = NULL, 
    template_path = system.file("extdata", "miljogifter-leveransmall_2024.xlsx", package = "MoCiS2.wtse"), 
    program = "none",
    convert_to_character = TRUE,
    ...
){
  
  # Avoid scientific notation
  options(scipen = 999)
  
  template <- readxl::read_excel(template_path, sheet, n_max = 1) |> 
    select(-1L)
  
  if (program %in% c("hav", "limn"))
    data <- data |> 
    dplyr::mutate(
      PROVTAG_SYFTE = "NMO",
      PROVPLATS_TYP = "Bakgrund",
      PROVTAG_ORG = "NRM",
      ACKR_PROV = "Nej",
      DIREKT_BEHA = "FRYST",
      PROVDATA_TYP = "BIOTA",
      PROVPLATS_MILJO = if_else(
        program == "hav", 
        "HAV-BRACKV", 
        "SJO-SOTV-RINN"
      ),
      PLATTFORM = dplyr::if_else(
        program == "hav", 
        "FISKEBAT", 
        "SMABAT"
      ),
      PLATTFORM = dplyr::if_else(
        ART == "Blamussla", 
        "SAKNAS", 
        PLATTFORM
      ),
      PLATTFORM = dplyr::if_else(
        ART %in% c("Fisktarna", "Sillgrissla", "Strandskata"), 
        "SAKNAS", 
        PLATTFORM
      ),
      PROVTAG_MET = case_when(
        ART == "Blamussla" & NAMN_PROVPLATS =='Kvädöfjärden' ~ "Bottenskrapa",
        ART == "Blamussla" & NAMN_PROVPLATS =='Nidingen' ~ "Dykning",
        ART == "Blamussla" & NAMN_PROVPLATS =='Fjällbacka' ~ "Havskrap",
        TRUE ~ "Natfiske"
      ),
      PROVTAG_MET = dplyr::if_else(
        ART %in% c("Fisktarna", "Sillgrissla", "Strandskata"), 
        "Aggplockning", 
        PROVTAG_MET
      ),
      MATOSAKERHET_TYP = dplyr::if_else(
        is.na(MATOSAKERHET), 
        NA, 
        "U2"
      )
    )
  
  data <- data |> 
    dplyr::arrange(PARAMETERNAMN) |> 
    dplyr::select(
      intersect(names(template), names(data))
    ) |> 
    dplyr::distinct()
  
  data <- add_missing_columns(data, template)
  
  if (convert_to_character) {
    data <- convert_cols_to_character(data)
  }
  
  if (is.null(file)) {
    data
  } else {
    openxlsx2::write_xlsx(data, file = file, ...)
  }
  
}
