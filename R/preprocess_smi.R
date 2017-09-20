#' Preprocess SMI eye tracking data file
#'
#' @param d A data frame with eye tracking data.
#' @param x_max A number indicating the max pixel dimension in the horizontal plane.
#' @param y_max A number indicating the max pixel dimension in the vertical plane.
#' @param samp_rate A number indicating the sampling rate of the eye tracker that generated the data.
#' @param avg_eyes Logical indicating whether data from each eye should be averaged together.
#' @return A processed data frame that is ready for analysis.
#' @examples
#' preprocess_smi(d = df, x_max = 1680, y_max = 1050, samp_rate = 120, avg_eyes = TRUE)

preprocess_smi <- function(d, x_max = 1680, y_max = 1050, samp_rate = 120, avg_eyes = TRUE) {

  ## average the eyes
  if (avg_eyes == TRUE) {
    # round to the nearest pixel
    d$x <- round(rowMeans(d[,c("lx","rx")], na.rm=TRUE))
    d$y <- round(rowMeans(d[,c("ly","ry")], na.rm=TRUE))
    d <- d[, !(names(d) %in% c("lx","rx","ly","ry"))]
  }

  ## clip off out of range numbers
  d$x[d$x < 0 | d$x > x_max] <- NA
  d$y[d$y < 0 | d$y > y_max] <- NA

  ## convert the time into seconds
  d$t <- round((d$t - d$t[1])/(1000000), 3)

  ## generate time slices relative to each trial
  d <- d %>%
    split(.$stimulus) %>%
    purrr::map_df(generate_tstim_vect)

  ## round to the nearest sample
  d$t.stim <- round(d$t.stim * samp_rate) / samp_rate

  ## y flip (so origin is cartesian, not matrix (bottom left, instead of top left)
  d$y <- y_max - d$y

  d
}
