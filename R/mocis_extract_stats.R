#' Title
#'
#' @param obj
#'
#' @returns
#' @export
#'
#' @examples
extract_linmod_stats <- function(obj) {
  with(
    obj, {
      data.frame(
        slope = slope,
        ci.lower = lower,
        ci.upper = upper,
        r2 = r2,
        pval = p,
        cv = cv[1],
        ldt = cv[2],
        yrq = cv[3],
        pow1 = power[1],
        pow2 = power[2],
        pow3 = power[3]
      )
    }
  )
}

#' Title
#'
#' @param obj
#'
#' @returns
#' @export
#'
#' @examples
extract_smooth_stats <- function(obj) {
  with(
    obj, {
      data.frame(
        tau = tau,
        p.tau = p.tau,
        cv.sm = cv[1],
        p.smooth = p.smooth,
        min.trend = cv[2],
        yhat.last = yhat.last,
        yhat.last.lower = yhat.last.lower,
        yhat.last.upper = yhat.last.upper
      )
    }
  )
}

#' Title
#'
#' @param obj
#'
#' @returns
#' @export
#'
#' @examples
extract_changep_stats <- function(obj) {
  with(
    obj, {
      data.frame(
        changepoint = ifelse(!is.null(obj), changepoint, NA)
      )
    }
  )
}
