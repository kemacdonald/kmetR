#' Generate timing information within each trial for SMI eye tracking data
#'
#' @param trial_df A data frame for each trial in the SMI eye tracking data
#' @return A data frame that has a t.stim timing column

generate_tstim_vect <- function(trial_df) {
  # get the min and max time for that trial
  tvals_df <- trial_df %>%
    group_by(stimulus) %>%
    mutate(min_t = min(t),
           max_t = max(t),
           n = n())

  # extract relevant values
  min_t <- tvals_df %>% pull(min_t) %>% unique()
  max_t <- tvals_df %>% pull(max_t) %>% unique()
  n <- tvals_df %>% pull(n) %>% unique()
  diff_t <- max_t - min_t

  # create the t.stim
  t_stim_v <- seq(0, diff_t, length.out = n)

  # add the t.stim vector back to the data frame
  trial_df %>% mutate(t.stim = t_stim_v)
}
