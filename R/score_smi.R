#' Score trial for SMI eye tracking data
#'
#' @param d A data frame with eye tracking data
#' @param trial_column A string indicating the column name that tracks trial number.
#' @param response_column A string indicating the column name that tracks where participants were looking.
#' @return A data frame that has an RT for the first gaze shift in each trial.
#' @examples
#' score_smi(d = df, trial_column = trial_num, response_column = ss_looking_char)

score_smi <- function(d, trial_column = trial_num, response_column) {
  # quote and capture environment
  trial_column <- enquo(trial_column)
  response_column <- enquo(response_column)

  # check if there is a shift in the trial after the critical onset
  # and record where the shift started and ended
  crit.window.responses <- d %>%
    select(!!response_column, !!trial_column) %>%
    group_by(!!trial_column) %>%
    summarise(min_t = min(!!response_column)) %>%
    arrange(min_t)

  # store info about the shift
  shift.start <- crit.window.responses$target_looking[1]
  shift.info <-paste(crit.window.responses$target_looking[1], crit.window.responses$target_looking[2],
                     sep = "-")

  # check if there is only one "response" in the target_looking vector
  # if 1, then there was no shift (i.e., no change from response at crit.onset)
  if (nrow(crit.window.responses) == 1) {
    trial_score <- trial_df %>%
      mutate(rt = NA, shift_type = "no_shift") %>%
      select(rt, shift_type)
  } else {
    # get the earliest time point when target looking switches from the critical onset value
    trial_score <- trial_df %>%
      filter_(t.filter.type) %>%
      filter(target_looking != shift.start) %>%
      select_(t.select.type, "target_looking") %>%
      group_by(target_looking) %>%
      summarise_(rt = interp(~ min(x), x = as.name(t.select.type))) %>%
      filter(rt == min(rt)) %>%
      mutate(shift_type = ifelse(shift.info == "center-target", "C_T",
                                 ifelse(shift.info == "center-distracter", "C_D",
                                        ifelse(shift.info == "target-distracter", "T_D",
                                               ifelse(shift.info== "target-center", "T_C",
                                                      ifelse(shift.info == "distracter-target", "D_T",
                                                             ifelse(shift.info == "distracter-center", "D_C")))))),
             shift_accuracy = ifelse(shift_type == "C_T", "correct", "incorrect")) %>%
      select(rt, shift_type, shift_accuracy)
  }

  # add the rt and score to the trial data frame
  trial_df <- cbind(trial_df, trial_score)

  return(trial_df)
}
