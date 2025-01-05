#' Title
#'
#' @param path 
#' @param sheet 
#'
#' @return
#' @export
#'
#' @examples
get_codes_prc <- function(
    path = system.file("extdata", "codelist_wtse.xlsx", package = "MoCiS2.wtse"), 
    sheet = "PARAMETRAR",
    columns = c("NRM_PARAMETERKOD", "PROV_BERED", "PROVKARL", "ANALYS_MET", "ANALYS_INSTR")
) {
  readxl::read_excel(path, sheet) |> 
    dplyr::select(tidyselect::all_of(columns)) |> 
    dplyr::filter(
      stringr::str_detect(NRM_PARAMETERKOD, "FPRC|TPRC")
    )
}

#' Title
#'
#' @param path 
#' @param sheet 
#'
#' @return
#' @export
#'
#' @examples
get_codes_substances <- function(
    path = system.file("extdata", "codelist_wtse.xlsx", package = "MoCiS2.wtse"), 
    sheet = "PARAMETRAR",
    columns = c(
      "NRM_PARAMETERKOD", "PARAMETERNAMN", "UNIK_PARAMETERKOD", 
      "ENHET", "MATOSAKERHET_ENHET", "PROV_LAGR"
    )
) {
  if (is.null(columns)) {
    readxl::read_excel(path, sheet)
  } else {
    readxl::read_excel(path, sheet) |> 
      dplyr::select(tidyselect::all_of(columns))
  }
}

#' Title
#'
#' @param path 
#' @param sheet 
#' @param columns 
#'
#' @return
#' @export
#'
#' @examples
get_codes_substances_methods <- function(
    path = system.file("extdata", "codelist_wtse.xlsx", package = "MoCiS2.wtse"), 
    sheet = "PARAMETRAR",
    columns = c(
      "NRM_PARAMETERKOD", "LABB", "PROV_BERED", "PROVKARL", 
      "ANALYS_MET", "UTFOR_LABB", "ANALYS_INSTR"
    )
) {
  readxl::read_excel(path, sheet) |> 
    dplyr::select(tidyselect::all_of(columns))
}

#' Title
#'
#' @param .data 
#' @param path 
#' @param sheet 
#' @param max_dist 
#'
#' @return
#' @export
#'
#' @examples
get_codes_sites <- function(
    .data = NULL, 
    path = system.file("extdata", "codelist_wtse.xlsx", package = "MoCiS2.wtse"), 
    sheet = "STATIONER", 
    fuzzyjoin = TRUE,
    max_dist = 2
) {
  
  stations_list <- readxl::read_excel(path, sheet) |> 
    dplyr::select(-tidyselect::contains("...")) |> 
    dplyr::distinct()
  
  if (is.null(.data)) {
    stations_list
  } else {
    if (fuzzyjoin) {
      .data |> 
        fuzzyjoin::stringdist_left_join(
          stations_list,
          by = "PROVPLATS_ANALYSMALL", 
          max_dist = max_dist,
          distance_col = "LOKDIST"
        ) |> 
        dplyr::select(
          -PROVPLATS_ANALYSMALL.y, 
          PROVPLATS_ANALYSMALL = PROVPLATS_ANALYSMALL.x
        ) |> 
        dplyr::distinct()
    } else{
      .data |> 
        dplyr::left_join(
          stations_list,
          dplyr::join_by("PROVPLATS_ANALYSMALL")
        ) |> 
        dplyr::mutate(
          LOKDIST = if_else(
            PROVPLATS_ANALYSMALL %in% stations_list$PROVPLATS_ANALYSMALL, 0, NA
          )
        )
    }
  }
}

#' Title
#'
#' @param .data 
#' @param path 
#' @param sheet 
#' @param max_dist 
#'
#' @return
#' @export
#'
#' @examples
get_codes_species <- function(
    .data = NULL, 
    path = system.file("extdata", "codelist_wtse.xlsx", package = "MoCiS2.wtse"), 
    sheet = "ARTER", 
    fuzzyjoin = TRUE,
    max_dist = 2
) {
  
  species_list <- readxl::read_excel(path, sheet) |> 
    dplyr::select(
      -tidyselect::contains("...")
    ) |> 
    dplyr::distinct()
  
  if (is.null(.data)) {
    species_list
  } else {
    # This function could be made more general
    # if we used input column as an argument
    if (fuzzyjoin) {
      .data |> 
        fuzzyjoin::stringdist_left_join(
          species_list,
          by = "LATIN", 
          max_dist = max_dist,
          distance_col = "ARTDIST"
        ) |> 
        dplyr::select(
          -LATIN.y, 
          LATIN = LATIN.x
        ) |> 
        dplyr::distinct()
    } else {
      .data |> 
        dplyr::left_join(
          species_list, 
          dplyr::join_by("LATIN")
        ) |> 
        dplyr::mutate(
          ARTDIST = if_else(
            LATIN %in% species_list$LATIN, 0, NA
          )
        )
    }
  }
}

#' Title
#'
#' @param list_name 
#' @param exclude_sheets 
#' @param clean_names 
#' @param current 
#'
#' @return
#' @export
#'
#' @examples
get_code_list <- function(
    list_name = c("kembiofys", "provmetadata", "data_matvarde", "bestallare", "provdata", "inspire", "prc", "current"),
    exclude_sheets = c("version"),
    clean_names = FALSE,
    current = "codelist_wtse.xlsx"
) {
  
  list_name <- match.arg(list_name)
  
  list_file <- dplyr::case_when(
    list_name == "kembiofys" ~ "kodlistor_pq_nv_KemBioFys_PARAMETERNAMN.xlsx",
    list_name == "provmetadata" ~ "kodlistor_pq_nv_PROVMETADATA.xlsx",
    list_name == "data_matvarde" ~ "kodlistor_pq_nv_DATA_MATVARDE.xlsx",
    list_name == "bestallare" ~ "kodlistor_pq_nv_BESTALLARE.xlsx",
    list_name == "provdata" ~ "kodlistor_pq_nv_PROVDATA_ALLA_KODER.xlsx",
    list_name == "inspire" ~ "kodlistor_pq_nv_Inspire_2023-04-25.xlsx",
    list_name == "prc" ~ "codelist_prc.xlsx",
    list_name == "current" ~ current
  )
  
  codes_path = system.file(
    "extdata", list_file, package = "MoCiS2.wtse"
  )
  
  codes_sheets <- readxl::excel_sheets(codes_path)
  if (class(exclude_sheets)[[1]] == "character") {
    codes_sheets <- codes_sheets[!codes_sheets %in% exclude_sheets]
  } else if (class(exclude_sheets)[[1]] %in% c("numeric", "integer")) {
    codes_sheets <- codes_sheets[-c(exclude_sheets)]
  } else {
    stop("Argument exclude_sheets must be character, integer or numeric.")
  }
  
  out <- purrr::map(
    codes_sheets |> rlang::set_names(), 
    \(x) {
      readxl::read_excel(codes_path, x) |> 
        dplyr::rename(
          tidyselect::any_of(
            c(
              `Rekommenderat svenskt namn` = "Rekommenterat svenskt namn",
              `Rekommenderat engelskt namn` = "Rekommenterat engelskt namn"
            )
          )
        )
    }
  ) |> 
    purrr::list_rbind(names_to = "ID_Lista")
  
  if(clean_names) {
    out <- out |> janitor::clean_names()
  }
  
  out
}

#' Title
#'
#' @param list_name 
#' @param clean_names 
#' @param list_file 
#'
#' @return
#' @export
#'
#' @examples
get_code_list_rmdk <- function(
    list_name,
    clean_names = FALSE,
    list_file = "rmdk_kodlistor_lankar.xlsx"
) {
  
  codes_path = system.file(
    "extdata", list_file, package = "MoCiS2.wtse"
  )
  
  codelists <- readxl::read_excel(codes_path, "kodlistor")
  
  code_groups <- codelists |> 
    dplyr::filter(Huvudgrupp == list_name) |> 
    mutate(
      URL_csv = glue::glue("https://kodlistor.miljodatasamverkan.se/def/vocabulary/{Huvudgrupp}/{Grupp}/csv") |> 
        as.character() |> 
        set_names(Grupp)
    )
  
  out <- purrr::map(
    code_groups$URL_csv, 
    \(x) {
      readr::read_csv(x, show_col_types = FALSE) |> 
        dplyr::rename(
          tidyselect::any_of(
            c(
              `Rekommenderat svenskt namn` = "Rekommenterat svenskt namn",
              `Rekommenderat engelskt namn` = "Rekommenterat engelskt namn"
            )
          )
        )
    }
  ) |> 
    purrr::list_rbind(names_to = "ID_Lista")
  
  if(clean_names) {
    out <- out |> 
      janitor::clean_names()
  }
  
  out
}

# get_code_list_rmdk("BESTALLARE")
# get_code_list_rmdk("PROVDATA_BIOTA")
# get_code_list_rmdk("PROVMETADATA") |> view()
# get_code_list_rmdk("styrdok")
# get_code_list("provmetadata")

#' Title
#'
#' @param search_string 
#' @param .data 
#'
#' @return
#' @export
#'
#' @examples
code_list_search <- function(search_string, .data) {
  .data |> 
    dplyr::filter(
      dplyr::if_any(
        tidyselect::everything(), 
        \(x) stringr::str_detect(x, search_string)
      )
    )
}
