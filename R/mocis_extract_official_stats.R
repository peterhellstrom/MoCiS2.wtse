#' Title
#'
#' @param contaminants 
#' @param sites 
#' @param species 
#' @param run_date 
#' @param dir 
#'
#' @return
#' @export
#'
#' @examples
mocis_extract_official_stats <- function(
    contaminants,
    sites,
    species,
    run_date,
    dir
) {
  
  p <- tidyr::expand_grid(
    contaminants,
    sites,
    species = species,
    run_date = run_date
  ) |> 
    dplyr::mutate(
      relative_path = glue::glue("{programme}/{site_code}/{species}/{substance}/data_{site_code}_{species}_{substance}_{run_date}.xlsx"),
      full_path = file.path(dir, relative_path),
      chk_exists = file.exists(full_path)
    )
  
  p_read <- p |> 
    dplyr::filter(chk_exists) |> 
    dplyr::select(full_path, site_name, substance)
  
  x <- purrr::pmap(
    p_read,
    \(full_path, site_name, substance) {
      readxl::read_xlsx(full_path, "Mean values") |> 
        dplyr::mutate(
          SITE = site_name, 
          Name = substance
        ) |> 
        dplyr::rename(Mean = 2L)
    }
  ) |> 
    purrr::list_rbind()
  
  x_wide <- purrr::map(
    unique(p_read$substance) |> 
      sort() |> 
      rlang::set_names(),
    \(substance) x |> 
      dplyr::filter(Name == substance) |> 
      dplyr::select(YEAR, Mean, SITE) |> 
      tidyr::complete(YEAR, SITE) |> 
      dplyr::arrange(YEAR, SITE) |> 
      tidyr::pivot_wider(
        names_from = YEAR, 
        values_from = Mean
      ) |> 
      dplyr::mutate(
        Name = substance,
        .before = 1L
      )
  )
  
  c(
    list(
      input_files = p |> 
        dplyr::filter(chk_exists) |> 
        dplyr::select(-full_path, -chk_exists)
    ),
    list(data = x),
    x_wide
  )
  
}
