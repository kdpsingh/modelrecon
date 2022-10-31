#' @import dplyr
NULL

#' Calculate net benefit (or realized net benefit)
#'
#' This function is used to calculate the net benefit and is intended to be used
#' after the [apply_threshold()] function. When the [apply_constraint()]
#' function is also applied, then this function returns the *realized* net
#' benefit.
#'
#' @param data Data frame returned by the [apply_threshold()] function. It must
#'   contain a logical column called `prediction` (with `TRUE`/`FALSE` values)
#'   and a logical column called `outcome` (with `TRUE`/`FALSE` values).
#' @param verbose Whether to print out the number of true positives and false
#'   positives
#'
#' @return The net benefit or realized net benefit
#' @export
#'
#' @examples
calculate_nb = function(data, verbose = FALSE) {
  data %>%
    mutate(n = n()) %>%
    filter(!is.na(met_threshold)) %>%
    group_by(met_threshold, n) %>%
    summarize(true_positives = sum(prediction & outcome),
              false_positives = sum(prediction & outcome == FALSE)) %>%
    ungroup() %>%
    mutate(nb = true_positives - met_threshold/(1-met_threshold) * false_positives) %>%
    mutate(nb = nb / n) %>%
    {
      if (verbose) print(.)
      .
    } %>%
    summarize(nb = sum(nb)) %>%
    pull(nb)
}

#' Apply threshold to predictions
#'
#' This function takes a data frame with a column named `probability`, applies a
#' threshold to that column, and returns an updated data frame containing a new
#' logical column named `prediction` and a new numeric column called
#' `met_threshold`. In the `prediction` column, probabilities >= to threshold
#' are assigned a value of `TRUE` and the probabilities < threshold are assigned
#' a value of `FALSE`. For the patients with a predicted value of `TRUE`, the
#' `met_threshold` column is contains the threshold value which a given
#' prediction met. This is useful when multiple thresholds are applied with
#' different constraints.
#'
#' @param data A data frame containing a column named `probability`
#' @param threshold A threshold probability, with a value between 0 and 1
#'
#' @return A data frame with a new column called `prediction` and
#'   `met_threshold`, and a new attribute called `thresholds` that contains the
#'   newly added threshold and any previously applied thresholds.
#' @export
#'
#' @examples
apply_threshold = function(data, threshold) {
  if (!('met_threshold' %in% names(data))) {
    data =
      data %>%
      arrange(desc(probability)) %>%
      mutate(prediction = probability >= threshold)

    data =
      data %>%
      mutate(met_threshold = if_else(prediction, threshold, NA_real_))
  } else {
    data =
      data %>%
      arrange(desc(probability)) %>%
      mutate(prediction =
               case_when(
                 prediction == TRUE ~ TRUE,
                 is.na(met_threshold) & probability >= threshold ~ TRUE,
                 TRUE ~ FALSE # if neither of above criteria met, then FALSE
                       )
             )

    data =
      data %>%
      mutate(met_threshold =
               if_else(prediction & is.na(met_threshold),
                       threshold,
                       met_threshold))

    if (threshold < max(attributes(data)$thresholds)) {
     stop(paste0('New threshold must be greater than previous maximum ',
                'threshold of ', max(attributes(data)$thresholds)))
    } else if (threshold == max(attributes(data)$thresholds)) {
     warning(paste0('New threshold must be greater than previous maximum ',
                   'threshold of ', max(attributes(data)$thresholds),
                   '. Because the current threshold is equal set threshold, ',
                   'previously added constraints at this threshold have been ',
                   'overwritten.'))
    }
  }
  attributes(data)$thresholds = c(attributes(data)$thresholds, threshold)
  data
}


#' Set all predictions to TRUE
#'
#' This functions takes a data frame and returns a new column called
#' `prediction` where all values are set to `TRUE` as per a treat-all strategy,
#' a new column called `met_threshold` containing the value provided in the
#' threshold, which is only used for the purposes of calculating the net
#' benefit.
#'
#' @param data
#'
#' @return A data frame with a new column called `prediction` and
#'   `met_threshold`, and a new attribute called `thresholds` that contains the
#'   newly added threshold and any previously applied thresholds.
#' @export
#'
#' @examples
apply_all = function(data, threshold) {
  data = data %>%
    mutate(prediction = TRUE) %>%
    mutate(met_threshold = threshold)

  attributes(data)$thresholds = c(attributes(data)$thresholds, threshold)
  data
}

#' Apply a constraint to the previously added threshold
#'
#' This function should only be used after [apply_threshold()]. Once a threshold
#' has been applied, this function adjusts the `TRUE` predictions such that only
#' the top predictions (up to the number specified in `capacity`) will remain
#' `TRUE`. If `capacity` is set to `Inf`, then this function does not alter any
#' predictions. If multiple thresholds have been applied, this function only
#' applies to the most recently set threshold. This allows [apply_threshold()]
#' and `apply_constraint()` to be chained together multiple times using pipes.
#'
#' @param data
#' @param capacity
#'
#' @return A data frame with an altered set of predictions based on the
#'   specified capacity. For patients whose predictions were converted from
#'   `TRUE` to `FALSE`, the `met_threshold` column value changes to `NA`.
#' @export
#'
#' @examples
apply_constraint = function (data, capacity) {
  data =
    data %>%
    mutate(cumulative_true = cumsum(prediction & met_threshold == tail(attributes(data)$thresholds, 1))) %>%
    mutate(prediction = prediction & cumulative_true <= capacity) %>%
    select(-cumulative_true)

  data =
    data %>%
    mutate(met_threshold = if_else(prediction, met_threshold, NA_real_))

  data
}
