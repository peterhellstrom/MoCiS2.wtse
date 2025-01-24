#' Title
#'
#' @param path 
#'
#' @returns
#' @export
#'
#' @examples
get_mocis_stations <- function(
    path = system.file("extdata", "codelist.xlsx", package = "MoCiS2")
) {
  
  stations_mocis <- MoCiS::stations() |> 
    rlang::set_names(c("Marine", "Limnic"))
  
  # Korrigera för en bugg i funktionen MoCiS::stations(),
  # Brännträsket har ingen kolumn EKOO, NKOO är dubblerad
  names(stations_mocis$Limnic$BRAN) <- c("name", "NKOO", "EKOO")
  
  stations_mocis <- purrr::map(
    stations_mocis, 
    \(x) dplyr::bind_rows(x, .id = "LOC")
  ) |> 
    purrr::list_rbind(names_to = "program") |> 
    dplyr::rename_with(toupper) |> 
    dplyr::select(NAME, LOC, PROGRAM, NKOO, EKOO)
  
  stations_mocis2 <- readxl::read_xlsx(path, "STATIONER") |> 
    dplyr::mutate(
      PROVPLATS_ID = as.integer(PROVPLATS_ID)
    ) |> 
    dplyr::summarize(
      PROVPLATS_ANALYSMALL = stringr::str_flatten(
        PROVPLATS_ANALYSMALL |> stringr::str_unique(), 
        collapse = "; ", 
        na.rm = TRUE
      ),
      .by = c(LOC, PROVPLATS_ID, NAMN_PROVPLATS)
    )
  
  stations_mocis <- stations_mocis |> 
    dplyr::left_join(
      stations_mocis2 |> 
        dplyr::select(-PROVPLATS_ANALYSMALL),
      dplyr::join_by(LOC)
    )
  
  stations_mocis_sf <- stations_mocis |> 
    sf::st_as_sf(
      coords = c("EKOO", "NKOO"), 
      crs = 3847,
      remove = FALSE
    )
  
  stations_mocis_sf
}
