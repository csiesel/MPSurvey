#' Export Results to Excel
#'
#' This function takes the results from a statistical analysis, processes the
#' data to extract relevant statistics, and exports the processed data to an
#' Excel file.
#'
#' @param results A list containing the results of a statistical analysis.
#'   The list should have a component named `table_body` which is a data frame
#'   containing the raw data.
#' @param file A string specifying the path to the output Excel file.
#'
#' @return None. The function writes the processed data to an Excel file.
#' @export
export_results <- function(results, file) {

  start_data <- results$table_body

  parse_stat_col <- function(stat_col, var_type) {
    stat_col <- as.character(stat_col)
    var_type <- as.character(var_type)

    out <- data.frame(
      n = rep(NA_real_, length(stat_col)),
      N = rep(NA_real_, length(stat_col)),
      prop = rep(NA_real_, length(stat_col)),
      mean = rep(NA_real_, length(stat_col)),
      stringsAsFactors = FALSE
    )

    is_cont <- !is.na(var_type) & grepl("continuous", var_type, ignore.case = TRUE)
    is_cat  <- !is.na(var_type) & grepl("categorical|dichotomous", var_type, ignore.case = TRUE)

    if (any(is_cont)) {
      x <- stat_col[is_cont]

      cont_n <- suppressWarnings(
        as.numeric(sub("^\\s*([0-9.]+).*", "\\1", x))
      )

      out$n[is_cont] <- cont_n
      out$N[is_cont] <- cont_n

      out$mean[is_cont] <- suppressWarnings(
        as.numeric(sub("^.*?[[:space:]\n]+([-0-9.]+)\\s*\\(.*$", "\\1", x))
      )
    }

    if (any(is_cat)) {
      x <- stat_col[is_cat]

      out$n[is_cat] <- suppressWarnings(
        as.numeric(sub("^\\s*([0-9.]+)\\s*/.*", "\\1", x))
      )

      out$N[is_cat] <- suppressWarnings(
        as.numeric(sub("^.*?/\\s*([0-9.]+).*", "\\1", x))
      )

      out$prop[is_cat] <- suppressWarnings(
        as.numeric(sub("^.*?[[:space:]\n]+<?\\s*([-0-9.]+)%.*$", "\\1", x))
      )
    }

    out
  }

  parse_ci_col <- function(ci_col) {
    ci_col <- as.character(ci_col)

    out <- data.frame(
      low = rep(NA_real_, length(ci_col)),
      upp = rep(NA_real_, length(ci_col)),
      stringsAsFactors = FALSE
    )

    has_ci <- !is.na(ci_col) & ci_col != ""

    if (any(has_ci)) {
      x <- gsub("%", "", ci_col[has_ci])
      x <- gsub("<0.001", "0.001", x, fixed = TRUE)

      out$low[has_ci] <- suppressWarnings(
        as.numeric(sub("^\\s*([-0-9.]+),.*$", "\\1", x))
      )

      out$upp[has_ci] <- suppressWarnings(
        as.numeric(sub("^.*?,\\s*([-0-9.]+)\\s*$", "\\1", x))
      )
    }

    out
  }

  convert_data <- function(data) {

    overall_stat <- parse_stat_col(data$stat_0, data$var_type)
    overall_ci   <- parse_ci_col(data$ci_stat_0)

    female_stat  <- parse_stat_col(data$stat_1, data$var_type)
    female_ci    <- parse_ci_col(data$ci_stat_1)

    male_stat    <- parse_stat_col(data$stat_2, data$var_type)
    male_ci      <- parse_ci_col(data$ci_stat_2)

    data %>%
      dplyr::mutate(
        Overall_prop = ifelse(
          grepl("continuous", var_type, ignore.case = TRUE),
          overall_stat$mean,
          overall_stat$prop
        ),
        Female_prop = ifelse(
          grepl("continuous", var_type, ignore.case = TRUE),
          female_stat$mean,
          female_stat$prop
        ),
        Male_prop = ifelse(
          grepl("continuous", var_type, ignore.case = TRUE),
          male_stat$mean,
          male_stat$prop
        ),

        Overall_n = overall_stat$n,
        Overall_N = overall_stat$N,
        Overall_95_low = overall_ci$low,
        Overall_95_upp = overall_ci$upp,

        Female_n = female_stat$n,
        Female_N = female_stat$N,
        Female_95_low = female_ci$low,
        Female_95_upp = female_ci$upp,

        Male_n = male_stat$n,
        Male_N = male_stat$N,
        Male_95_low = male_ci$low,
        Male_95_upp = male_ci$upp
      ) %>%
      dplyr::select(
        variable, var_type, var_label, row_type, label,
        Overall_n, Overall_N, Overall_prop, Overall_95_low, Overall_95_upp,
        Male_n, Male_N, Male_prop, Male_95_low, Male_95_upp,
        Female_n, Female_N, Female_prop, Female_95_low, Female_95_upp
      )
  }

  end_data <- convert_data(start_data)

  xlsx::write.xlsx(end_data %>% as.data.frame(), file = file, row.names = FALSE)
}
