# For internal use
moc_fix_herring <- function(data) {
  data |> 
    dplyr::mutate(
      ART = dplyr::if_else(
        (ART == "Stromming") & 
          (LOC %in% c("VADO", "FLAD", "KULL", "ABBE", "HABU", "40G7", "UTLV", "UTLA")), 
        "Sill", 
        ART
      )
    )
}

moc_read_warnings_base_message <- function(
    data, 
    .filter_col, 
    .pull_col
) {
  
  data |> 
    dplyr::filter(is.na( {{ .filter_col }} )) |> 
    dplyr::pull( {{ .pull_col }} ) |> 
    base::unique() |> 
    paste(collapse = ", ") |> 
    message()
}

moc_read_warnings <- function(data) {
  
  if (max(data$ARTDIST, na.rm = TRUE) > 0) {
    
    message("WARNING! The following species were fuzzy matched:")
    
    data |> 
      dplyr::filter(ARTDIST > 0) |> 
      dplyr::select(ART, LATIN) |> 
      dplyr::mutate(
        match = paste("*", paste(LATIN, ART, sep = " -> "))
      ) |> 
      dplyr::pull(match) |> 
      base::unique() |> 
      paste(collapse = "\n") |> 
      message()
  }
  
  if (max(data$LOKDIST, na.rm = TRUE) > 0) {
    # if (any(data$PROVPLATS_ANALYSMALL != data$NAMN_PROVPLATS)) {
    
    message("WARNING! The following locations were fuzzy matched:")
    
    data |> 
      # dplyr::filter(PROVPLATS_ANALYSMALL != NAMN_PROVPLATS) |> 
      dplyr::filter(LOKDIST > 0) |> 
      dplyr::select(PROVPLATS_ANALYSMALL, NAMN_PROVPLATS) |> 
      dplyr::mutate(
        match = paste("*", paste(PROVPLATS_ANALYSMALL, NAMN_PROVPLATS, sep = " -> "))
      ) |> 
      dplyr::pull(match) |> 
      base::unique() |> 
      paste(collapse = "\n") |> 
      message()
  }
  
  if (anyNA(data$ART)) {
    message("WARNING! Unable to match the following species:")
    moc_read_warnings_base_message(data, ART, LATIN)
  }
  
  if (anyNA(data$NAMN_PROVPLATS)) {
    message("WARNING! Unable to match the following locations:")
    moc_read_warnings_base_message(data, NAMN_PROVPLATS, PROVPLATS_ANALYSMALL)
  }
  
  if (anyNA(data$PARAMETERNAMN)) {
    message("WARNING! Unable to match the following substance codes:")
    moc_read_warnings_base_message(data, PARAMETERNAMN, NRM_PARAMETERKOD)
  }
}
