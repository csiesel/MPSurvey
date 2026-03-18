#' Carry Forward Selected Responses Into a New Variable
#'
#' Creates a new variable by carrying forward selected responses from a base
#' variable into a new variable, while preserving specified responses from an
#' original variable.
#'
#' @param x A data frame or tibble.
#' @param parent_var A character string naming the variable whose selected
#'   responses should be carried forward.
#' @param child_var A character string naming the original variable that
#'   corresponds to the new variable.
#' @param new_var A character string naming the new variable to create.
#' @param orig_resp A vector of responses in `parent_var` that should be carried
#'   forward into `new_var`.
#' @param keep_resp A vector of responses in `child_var` that should be kept
#'   and not overwritten.
#' @param verbose
#'
#' @return The input data frame with a new variable added.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   q1 = c("Yes", "No", "DK", "Yes", NA),
#'   q2 = c(NA, "Refused", "No", "Maybe", "Yes")
#' )
#'
#' carry_forward(
#'   x = df,
#'   parent_var = "q1",
#'   child_var = "q2",
#'   new_var = "q2_cf",
#'   orig_resp = c("Yes", "No"),
#'   keep_resp = c("Refused", "Maybe")
#' )
carry_forward <- function(x, parent_var, child_var, new_var,
                          orig_resp, keep_resp,
                          verbose = TRUE) {

  if (!is.data.frame(x)) stop("`x` must be a data frame or tibble.")

  if (!all(c(parent_var, child_var) %in% names(x))) {
    stop("`parent_var` and/or `child_var` not found in `x`.")
  }

  # Store originals for audit
  base_chr <- as.character(x[[parent_var]])
  orig_chr <- as.character(x[[child_var]])

  # Create new variable
  out <- x |>
    dplyr::mutate(
      !!new_var := dplyr::case_when(
        # 1. Keep specified original responses
        !is.na(.data[[child_var]]) & .data[[child_var]] %in% keep_resp ~ .data[[child_var]],

        # 2. Carry forward selected base responses
        !is.na(.data[[parent_var]]) & .data[[parent_var]] %in% orig_resp ~ .data[[parent_var]],

        # 3. Otherwise keep original
        TRUE ~ .data[[child_var]]
      )
    )

  out <- dplyr::relocate(out, !!new_var, .after = dplyr::all_of(child_var))

  new_chr <- as.character(out[[new_var]])

  # --- Audit calculations ---
  carried_idx <- is.na(orig_chr) &
    !is.na(base_chr) &
    base_chr %in% as.character(orig_resp)

  kept_idx <- !is.na(orig_chr) &
    orig_chr %in% as.character(keep_resp)

  changed_idx <- new_chr != orig_chr & !(is.na(new_chr) & is.na(orig_chr))

  n_carried <- sum(carried_idx, na.rm = TRUE)
  n_kept <- sum(kept_idx, na.rm = TRUE)
  n_changed <- sum(changed_idx, na.rm = TRUE)

  # --- Message output ---
  if (verbose) {
    message("carry_forward() ", new_var, ":")
    message("  • Values carried forward: ", n_carried)
    message("  • Values preserved (keep_resp): ", n_kept)
    message("  • Total values changed: ", n_changed)

    # Optional preview of changes
    if (n_changed > 0) {
      preview_df <- data.frame(
        parent = base_chr,
        child = orig_chr,
        new = new_chr
      )[changed_idx, , drop = FALSE]

      preview_df <- preview_df |>
        dplyr::distinct()
      message("  • Preview of changes:")
      print(preview_df, row.names = FALSE)
    }

    if (n_changed == 0) {
      message("  • No changes made.")
    }
  }

  return(out)
}
