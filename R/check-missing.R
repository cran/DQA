#' @title Check Missing Data Item-wise with Dependency Logic
#' @description
#' Analyzes missing data (`NA` values) for each variable (item-wise) by
#' considering dependencies between variables. This function goes beyond simple
#' NA counts by classifying missingness into different categories based on rules
#' defined in metadata.
#'
#' @details
#' This function classifies each row for a given variable into one of four states:
#' \itemize{
#'   \item **Completed:** The value is present where it is expected.
#'   \item **Missing:** The value is `NA` where it was expected (based on a parent condition).
#'   \item **Jump:** The value is `NA` because the parent condition was not met (i.e., the question was correctly skipped).
#'   \item **Unexpected:** The value is present where it was *not* expected (a data quality issue).
#' }
#' @details
#' The metadata (`M_data`) must contain the following columns to define the rules:
#' \itemize{
#'   \item **VARIABLE:** The name of the variable in the source data (`S_data`) to be checked for missingness.
#'   \item **VARIABLE_Code:** A unique numeric or character code assigned to each variable for identification and dependency mapping.
#'   \item **Dependency:** Specifies the dependency of the variable on another variable. A value of `0` indicates no dependency, while other values indicate the `VARIABLE_Code` of the parent variable.
#'   \item **Dep_Value:** The specific value or condition of the parent variable (as referenced in `Dependency`) that must be met for the current variable to be applicable. Use `"ANY"` if the value of the parent variable can be any non-missing value.
#' }
#'
#' @param S_data A data frame containing the source data to be checked.
#' @param M_data A metadata data frame containing the validation rules.
#' @param var_select A numeric or character vector specifying which variables to process.
#'   Can be indices or names from the `VARIABLE` column of `M_data`. Defaults to all variables.
#' @param Show_Plot A logical value. If `TRUE`, a ggplot bar chart showing the
#'   missingness percentage for each variable is displayed.
#'
#' @return
#' A `data.table` summarizing the missing data analysis for each variable, with
#' columns such as `VARIABLE`, `Missing_Count`, `Jump_Count`, `Unexpected_Count`,
#' `Total_Applicable`(the variable's value was expected to be completed based on metadata rules.),
#' `Percent_Complete`, and `Percent_Missing`.
#' @family missing data checks
#' @importFrom data.table as.data.table rbindlist
#' @importFrom ggplot2 ggplot aes geom_col theme_minimal theme element_text labs
#' @importFrom stats reorder setNames
#' @export
#'
#' @examples
#' # 1. Define comprehensive sample data and metadata
#' Meta_data <- data.frame(
#'   stringsAsFactors = FALSE,
#'   VARIABLE = c(
#'     "ID", "Gender", "Age", "Has_Job", "Job_Title",
#'     "Job_Satisfaction", "Last_Promotion_Year", "Has_Insurance",
#'     "Insurance_Provider", "Annual_Checkup"
#'   ),
#'   VARIABLE_Code = 1:10,
#'   Var_order = 1:10,
#'   Segment_Names = c(
#'     "Demographic", "Demographic", "Demographic", "Employment", "Employment",
#'     "Employment", "Employment", "Health", "Health", "Health"
#'   ),
#'   Dependency = c(0, 0, 0, 0, 4, 5, 5, 0, 8, 8),
#'   Dep_Value = c(
#'     "0", "0", "0", "0", "Yes", "ANY", "ANY", "0", "Yes", "Yes"
#'   )
#' )
#'
#' Source_data <- data.frame(
#'   ID = 1:10,
#'   Gender = c("Male", "Female", "Male", "Female", "Male",
#'              "Female", "Male", "Female", "Male", "Female"),
#'   Age = c(25, 42, 31, 55, 29, 38, 45, 22, 60, 33),
#'   Has_Job = c("Yes", "Yes", "No", "Yes", "Yes", "No", "Yes", "Yes", "No", "Yes"),
#'   Job_Title = c(NA, "Manager", NA, "Analyst", NA, "Student",
#'                 "Director", "Engineer", NA, "Designer"),
#'   Job_Satisfaction = c(5, 9, NA, 8, 7, NA, 10, 9, NA, 6),
#'   Last_Promotion_Year = c(2020, 2021, NA, NA, NA, NA, 2024, 2022, NA, 2023),
#'   Has_Insurance = c("Yes", "No", "Yes", "Yes", "No", "Yes", "Yes", "No", "No", "Yes"),
#'   Insurance_Provider = c("Provider A", NA, "Provider B", "Provider C",
#'                          "Provider D", NA, "Provider E", NA, NA, "Provider F"),
#'   Annual_Checkup = c("Yes", NA, "No", "Yes", NA, "Yes", "Yes", "No", NA, "Yes")
#' )
#'
#' # 2. Run the item-wise check with plot
#' item_report <- check_missing_itemwise(
#'   S_data = Source_data, M_data = Meta_data, Show_Plot = TRUE
#' )
#'print(item_report)
check_missing_itemwise <- function(S_data, M_data, var_select = 1:nrow(M_data), Show_Plot = FALSE) {
  # --- Fix for "no visible binding for global variable" NOTE ---
  . <- VARIABLE <- Var_order <- VARIABLE_Code <- Dependency <- Dep_Value <- NULL
  Percent_Missing <- NULL # For ggplot2 plot

  # --- 1. SETUP ---
  S_data <- data.table::as.data.table(S_data)
  M_data <- data.table::as.data.table(M_data)

  var_select <- unique(var_select)
  if (!is.numeric(var_select)) {
    selected_indices <- M_data[VARIABLE %in% var_select, which = TRUE]
    if (length(selected_indices) == 0) stop("None of the selected variables were found in M_data.")
    Var_Order <- selected_indices
  } else {
    Var_Order <- var_select
  }

  if (!all(M_data$VARIABLE %in% names(S_data))) {
    missing_vars <- setdiff(M_data$VARIABLE, names(S_data))
    stop(paste("Some M_data variables are not in S_data:", paste(missing_vars, collapse = ", ")))
  }

  dep_lookup <- M_data[, .(VARIABLE, VARIABLE_Code)]
  Total_Row <- nrow(S_data)
  result_list <- list()

  # --- 2. ITERATION ---
  for (i in Var_Order) {
    if (i > nrow(M_data)) next

    current_var <- M_data[i, VARIABLE]
    dep_code    <- M_data[i, Dependency]
    dep_value   <- trimws(as.character(M_data[i, Dep_Value]))

    # --- 3. LOGIC FOR NON-DEPENDENT VARIABLES ---
    if (is.na(dep_code) || dep_code == 0) {
      na_count     <- sum(is.na(S_data[[current_var]]))
      completed    <- Total_Row - na_count
      completeness <- if (Total_Row > 0) 100 * completed / Total_Row else NA_real_

      result_list[[current_var]] <- data.table::data.table(
        VARIABLE              = current_var,
        Missing_Count         = as.integer(na_count),
        Jump_Count            = 0L,
        Unexpected_Count      = 0L,
        Total_Applicable      = as.integer(Total_Row),
        Completed_Count       = as.integer(completed),
        Percent_Complete      = round(completeness, 2),
        Percent_Missing       = round(100 - completeness, 2)
      )
      next
    }

    # --- 4. LOGIC FOR DEPENDENT VARIABLES ---
    dep_var_vec <- dep_lookup[VARIABLE_Code == dep_code, VARIABLE]
    dep_var     <- if (length(dep_var_vec) > 0) dep_var_vec[[1]] else NA_character_

    if (is.na(dep_var) || !(dep_var %in% names(S_data))) next

    parent_val <- as.character(S_data[[dep_var]])
    child_val  <- S_data[[current_var]]

    is_parent_na <- is.na(parent_val)
    is_child_na  <- is.na(child_val)

    if (toupper(dep_value) == "ANY") {
      parent_condition_met <- !is_parent_na
    } else {
      parent_condition_met <- !is_parent_na & (trimws(parent_val) == dep_value)
    }

    completed_rows   <-  parent_condition_met & !is_child_na
    missing_rows     <-  parent_condition_met &  is_child_na
    jump_rows        <- !parent_condition_met & !is_parent_na & is_child_na
    unexpected_rows  <- !parent_condition_met & !is_parent_na & !is_child_na

    completed_n  <- sum(completed_rows)
    miss_n       <- sum(missing_rows)
    jump_n       <- sum(jump_rows)
    unexpected_n <- sum(unexpected_rows)

    expected_n       <- completed_n + miss_n

    completeness_pct <- if (expected_n > 0) 100 * completed_n / expected_n else NA_real_
    missingness_pct  <- if (expected_n > 0) 100 * miss_n / expected_n else NA_real_

    result_list[[current_var]] <- data.table::data.table(
      VARIABLE              = current_var,
      Missing_Count         = as.integer(miss_n),
      Jump_Count            = as.integer(jump_n),
      Unexpected_Count      = as.integer(unexpected_n),
      Total_Applicable      = as.integer(expected_n),
      Completed_Count       = as.integer(completed_n),
      Percent_Complete      = round(completeness_pct, 2),
      Percent_Missing       = round(missingness_pct, 2)
    )
  }

  # --- 10. FINAL OUTPUT ---
  if (length(result_list) == 0) return(data.table::data.table())
  item_result <- data.table::rbindlist(result_list, fill = TRUE)

  if (isTRUE(Show_Plot) && nrow(item_result) > 0) {
    # Ensure there's data to plot after filtering NAs
    plot_data <- item_result[!is.na(Percent_Missing) & Percent_Missing >= 0]
    if (nrow(plot_data) > 0) {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = reorder(VARIABLE, -Percent_Missing), y = Percent_Missing)) +
        ggplot2::geom_col(fill = "#66cdaa") +
        ggplot2::theme_minimal(base_family = "sans") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
                       plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::labs(title = "Item Missingness Analysis", y = "Missingness Percent (%)", x = "Variable")
      print(p)
    }
  }

  return(item_result)
}


#' @title Check Missing Data by Segments
#' @description
#' Analyzes data completeness at the segment level. A segment is a group of
#' variables defined in the `Segment_Names` column of the metadata.
#'
#' @details
#' For each segment, this function evaluates every row of the source data (`S_data`)
#' and classifies it into one of three categories:
#' \itemize{
#'   \item **Complete:** The row has all values as non-missing for any variable within the segment.
#'   \item **Incomplete:** The row has at least one `NA` value for variables in the segment.
#'   \item **Fully Missing:** All variables belonging to the segment are `NA` for that row.
#' }
#' @details
#' The metadata (`M_data`) must contain the following columns to define the rules:
#' \itemize{
#'   \item **VARIABLE:** The name of the variable in the source data (`S_data`) to be checked for missingness.
#'   \item **VARIABLE_Code:** A unique numeric or character code assigned to each variable for identification and dependency mapping.
#'   \item **Dependency:** Specifies the dependency of the variable on another variable. A value of `0` indicates no dependency, while other values indicate the `VARIABLE_Code` of the parent variable.
#'   \item **Dep_Value:** The specific value or condition of the parent variable (as referenced in `Dependency`) that must be met for the current variable to be applicable.
#'    Use `"ANY"` if the value of the parent variable can be any non-missing value.
#' }
#' The function returns a summary table with counts and percentages for each category per segment.
#'
#' @param S_data A data frame containing the source data to be checked.
#' @param M_data A metadata data frame containing the validation rules, including a `Segment_Names` column.
#' @param Show_Plot A logical value. If `TRUE`, a stacked bar chart visualizing
#'   the proportions for each segment is displayed.
#'
#' @return
#' A `data.frame` summarizing the analysis for each segment, with columns:
#' `SEGMENT`, `Total_Rows`, `Complete_Count`, `Incomplete_Count`, `Missing_Count`,
#' `Percent_Complete`, `Percent_Incomplete`, and `Percent_Missing`.
#'
#' @family missing data checks
#' @importFrom ggplot2 geom_bar coord_flip scale_fill_brewer position_stack
#' @export
#'
#' @examples
#' # 1. Define comprehensive sample data and metadata
#' Meta_data <- data.frame(
#'   stringsAsFactors = FALSE,
#'   VARIABLE = c(
#'     "ID", "Gender", "Age", "Has_Job", "Job_Title",
#'     "Job_Satisfaction", "Last_Promotion_Year", "Has_Insurance",
#'     "Insurance_Provider", "Annual_Checkup"
#'   ),
#'   VARIABLE_Code = 1:10,
#'   Var_order = 1:10,
#'   Segment_Names = c(
#'     "Demographic", "Demographic", "Demographic", "Employment", "Employment",
#'     "Employment", "Employment", "Health", "Health", "Health"
#'   ),
#'   Dependency = c(0, 0, 0, 0, 4, 5, 5, 0, 8, 8),
#'   Dep_Value = c(
#'     "0", "0", "0", "0", "Yes", "ANY", "ANY", "0", "Yes", "Yes"
#'   )
#' )
#'
#' Source_data <- data.frame(
#'   ID = 1:10,
#' Gender = c("Male", NA, "Male", "Female", "Male","Female", "Male", "Female", "Male", "Female"),
#' Age = c(25, NA, 31, 55, 29, 38, 45, 22, 60, 33),
#' Has_Job = c("Yes", NA, "No", "Yes", "Yes", "No", "Yes", "Yes", "No", "Yes"),
#' Job_Title = c(NA, NA, NA, "Analyst", NA, "Student","Director", "Engineer", NA, "Designer"),
#' Job_Satisfaction = c(5, NA, NA, 8, 7, NA, 10, 9, NA, 6),
#' Last_Promotion_Year = c(2020,NA , 2021, NA, NA, NA, 2024, 2022, NA, 2023),
#' Has_Insurance = c("Yes", NA, "Yes", "Yes", "No", "Yes", "Yes", "No", "No", "Yes"),
#' Insurance_Provider = c("Provider A", NA, "Provider B", "Provider C","Provider D", NA, "Provider E",
#'  NA, NA, "Provider F"),
#' Annual_Checkup = c("Yes", NA, "No", "Yes", NA, "Yes", "Yes", "No", NA, "Yes")
#' )
#'# 3. Run the segment check with plot
#' segment_report <- check_missing_segments(
#'   S_data = Source_data, M_data = Meta_data, Show_Plot = TRUE
#' )
#'print(segment_report)
check_missing_segments <- function(S_data, M_data, Show_Plot = FALSE) {
  # --- Fix for "no visible binding for global variable" NOTE ---
  SEGMENT <- Category <- Value <- NULL

  # --- 0. Input Validation ---
  required_cols <- c("Segment_Names", "VARIABLE", "Dependency", "VARIABLE_Code", "Dep_Value")
  if (!all(required_cols %in% names(M_data))) {
    stop(paste("M_data is missing required columns:", setdiff(required_cols, names(M_data))))
  }
  if (!is.data.frame(S_data) || !is.data.frame(M_data)) {
    stop("S_data and M_data must be data.frames.")
  }
  # Check for variables listed in M_data$VARIABLE that are missing in S_data
  missing_in_data <- setdiff(unique(M_data$VARIABLE), names(S_data))
  if (length(missing_in_data) > 0) {
    stop(
      paste(
        "These variables are listed in M_data but are missing in S_data:",
        paste(missing_in_data, collapse = ", ")
      )
    )
  }

  total_rows <- nrow(S_data)
  if (total_rows == 0) return(data.frame())

  # --- 1. Pre-computation and Setup ---
  code_to_var <- stats::setNames(M_data$VARIABLE, M_data$VARIABLE_Code)
  segment_names <- unique(M_data$Segment_Names)
  results_list <- vector("list", length(segment_names))

  # --- 2. Iterate Through Each Segment ---
  for (i in seq_along(segment_names)) {
    segment <- segment_names[i]
    segment_meta <- M_data[M_data$Segment_Names == segment, ]
    seg_vars <- intersect(unique(segment_meta$VARIABLE), names(S_data))

    if (length(seg_vars) == 0) {
      warning(paste("Segment '", segment, "' has no variables present in S_data. Skipping."))
      next
    }

    # --- 3. Identify Errors for Each Variable in the Segment ---
    error_flags <- matrix(FALSE, nrow = total_rows, ncol = length(seg_vars), dimnames = list(NULL, seg_vars))

    for (var_name in seg_vars) {
      var_meta <- segment_meta[segment_meta$VARIABLE == var_name, ][1L, ]
      var_col <- S_data[[var_name]]
      dep_code <- if (is.na(var_meta$Dependency)) 0 else var_meta$Dependency

      if (dep_code == 0) {
        error_flags[, var_name] <- is.na(var_col)
      } else {
        parent_name <- code_to_var[as.character(dep_code)]
        parent_col <- if (!is.null(parent_name) && parent_name %in% names(S_data)) S_data[[parent_name]] else rep(NA, total_rows)

        parent_condition_met <- if (toupper(var_meta$Dep_Value) == "ANY") {
          !is.na(parent_col)
        } else {
          !is.na(parent_col) & (as.character(parent_col) == var_meta$Dep_Value)
        }

        error_flags[, var_name] <- (parent_condition_met & is.na(var_col)) | (!parent_condition_met & !is.na(var_col))
      }
    }

    # --- 4. Classify Each Row for the Segment ---
    row_has_any_error <- rowSums(error_flags) > 0
    na_matrix <- is.na(S_data[, seg_vars, drop = FALSE])
    row_is_fully_missing <- rowSums(na_matrix) == length(seg_vars)

    # --- 5. Calculate Final Counts ---
    missing_count <- sum(row_is_fully_missing)
    complete_count <- sum(!row_has_any_error & !row_is_fully_missing)
    incomplete_count <- total_rows - complete_count - missing_count

    # --- 6. Aggregate Results ---
    results_list[[i]] <- data.frame(
      SEGMENT = segment,
      Total_Rows = total_rows,
      Complete_Count = complete_count,
      Incomplete_Count = incomplete_count,
      Missing_Count = missing_count,
      Percent_Complete = round(100 * complete_count / total_rows, 2),
      Percent_Incomplete = round(100 * incomplete_count / total_rows, 2),
      Percent_Missing = round(100 * missing_count / total_rows, 2)
    )
  }

  # --- 7. Final Output ---
  final_df <- do.call(rbind, results_list)
  if (!is.null(final_df)) {
    final_df <- final_df[order(final_df$SEGMENT), ]
    rownames(final_df) <- NULL
  } else {
    return(data.frame()) # Return empty frame if no segments were processed
  }

  # --- 8. Plotting (if requested) ---
  if (isTRUE(Show_Plot) && nrow(final_df) > 0) {
    # Reshape data from wide to long format for ggplot
    plot_data_long <- data.frame(
      SEGMENT = rep(final_df$SEGMENT, 3),
      Category = factor(
        rep(c("Complete", "Incomplete", "Missing"), each = nrow(final_df)),
        levels = c("Complete", "Incomplete", "Missing")
      ),
      Value = c(final_df$Percent_Complete, final_df$Percent_Incomplete, final_df$Percent_Missing)
    )

    p <- ggplot2::ggplot(plot_data_long, ggplot2::aes(x = SEGMENT, y = Value, fill = Category)) +
      ggplot2::geom_bar(stat = "identity", position = "stack") +
      ggplot2::coord_flip() + # Flip coordinates to make bars horizontal
      ggplot2::theme_minimal(base_family = "sans") +
      ggplot2::labs(
        title = "Segment Completeness Analysis",
        x = "Segment Name",
        y = "Percentage (%)"
      ) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_fill_brewer(palette = "Pastel1")

    print(p)
  }

  return(final_df)
}


#' @title Check Missing Data by Record (Unit Check)
#' @description
#' Provides a high-level summary of data completeness across the entire dataset
#' by classifying each row (or "record") as complete, incomplete, or missing.
#'
#' @details
#' This function evaluates all specified variables for each row and determines
#' its overall status based on the same error logic as `check_missing_segments`.
#' A row is:
#' \itemize{
#'   \item **Complete:** The row has all values as non-missing for any variable within the each rows.
#'   \item **Incomplete:** The row has at least one `NA` value for variables in the each rows.
#'   \item **Fully Missing:** All variables in the record are `NA` for that row.
#' }
#' @details
#' The metadata (`M_data`) must contain the following columns to define the rules:
#' \itemize{
#'   \item **VARIABLE:** The name of the variable in the source data (`S_data`) to be checked for missingness.
#'   \item **VARIABLE_Code:** A unique numeric or character code assigned to each variable for identification and dependency mapping.
#'   \item **Dependency:** Specifies the dependency of the variable on another variable. A value of `0` indicates no dependency, while other values indicate the `VARIABLE_Code` of the parent variable.
#'   \item **Dep_Value:** The specific value or condition of the parent variable (as referenced in `Dependency`) that must be met for the current variable to be applicable.
#'    Use `"ANY"` if the value of the parent variable can be any non-missing value.
#' }
#' The function returns a single-row data frame summarizing the counts and
#' percentages for the entire dataset.
#'
#' @param S_data A data frame containing the source data to be checked.
#' @param M_data A metadata data frame containing the validation rules.
#' @param Show_Plot A logical value. If `TRUE`, a pie chart visualizing the
#'   proportions of complete, incomplete, and missing rows is displayed.
#' @param start_var A numeric value indicating the starting variable index (from `M_data`)
#'   to include in the analysis. Defaults to 1.
#' @param skip_vars A character or numeric vector of variables to exclude from the analysis.
#'   Can be variable names or column indices.
#'
#' @return
#' A single-row `data.frame` with summary counts and percentages:
#' `Total_Rows`, `Complete_Count`, `Incomplete_Count`, `Missing_Count`,
#' `Percent_Complete`, `Percent_Incomplete`, and `Percent_Missing`.
#'
#' @family missing data checks
#' @importFrom ggplot2 coord_polar theme_void geom_text
#' @export
#'
#' @examples
#' # 1. Define comprehensive sample data and metadata
#' Meta_data <- data.frame(
#'   stringsAsFactors = FALSE,
#'   VARIABLE = c(
#'     "ID", "Gender", "Age", "Has_Job", "Job_Title",
#'     "Job_Satisfaction", "Last_Promotion_Year", "Has_Insurance",
#'     "Insurance_Provider", "Annual_Checkup"
#'   ),
#'   VARIABLE_Code = 1:10,
#'   Var_order = 1:10,
#'   Segment_Names = c(
#'     "Demographic", "Demographic", "Demographic", "Employment", "Employment",
#'     "Employment", "Employment", "Health", "Health", "Health"
#'   ),
#'   Dependency = c(0, 0, 0, 0, 4, 5, 5, 0, 8, 8),
#'   Dep_Value = c(
#'     "0", "0", "0", "0", "Yes", "ANY", "ANY", "0", "Yes", "Yes"
#'   )
#' )
#'
#' Source_data <- data.frame(
#'   ID = 1:10,
#'   Gender = c("Male", NA, "Male", "Female", "Male","Female", "Male", "Female", "Male", "Female"),
#' Age = c(25, NA, 31, 55, 29, 38, 45, 22, 60, 33),
#' Has_Job = c("Yes", NA, "No", "Yes", "Yes", "No", "Yes", "Yes", "No", "Yes"),
#' Job_Title = c(NA, NA, NA, "Analyst", NA, "Student","Director", "Engineer", NA, "Designer"),
#' Job_Satisfaction = c(5, NA, NA, 8, 7, NA, 10, 9, NA, 6),
#' Last_Promotion_Year = c(2020,NA , 2021, NA, NA, NA, 2024, 2022, NA, 2023),
#' Has_Insurance = c("Yes", NA, "Yes", "Yes", "No", "Yes", "Yes", "No", "No", "Yes"),
#' Insurance_Provider = c("Provider A", NA, "Provider B", "Provider C","Provider D", NA, "Provider E",
#'  NA, NA, "Provider F"),
#' Annual_Checkup = c("Yes", NA, "No", "Yes", NA, "Yes", "Yes", "No", NA, "Yes")
#' )
#' # 4. Run the row-wise check with plot
#' row_report <- check_missing_record(
#'   S_data = Source_data, M_data = Meta_data, skip_vars = "ID", Show_Plot = TRUE
#' )
#'print(row_report)
check_missing_record <- function(S_data, M_data, Show_Plot = FALSE, start_var = 1, skip_vars = NULL) {
  # --- Fix for "no visible binding for global variable" NOTE ---
  Category <- Value <- NULL

  # --- 0. Input Validation ---
  required_cols <- c("VARIABLE", "Dependency", "VARIABLE_Code", "Dep_Value")
  if (!all(required_cols %in% names(M_data))) {
    stop(paste("M_data is missing required columns:", setdiff(required_cols, names(M_data))))
  }
  if (!is.data.frame(S_data) || !is.data.frame(M_data)) {
    stop("S_data and M_data must be data.frames.")
  }
  # Check for variables listed in M_data$VARIABLE that are missing in S_data
  missing_in_data <- setdiff(unique(M_data$VARIABLE), names(S_data))
  if (length(missing_in_data) > 0) {
    stop(
      paste(
        "These variables are listed in M_data but are missing in S_data:",
        paste(missing_in_data, collapse = ", ")
      )
    )
  }

  total_rows <- nrow(S_data)
  if (total_rows == 0) return(data.frame())

  # --- 1. Variable Selection ---
  all_vars <- intersect(unique(M_data$VARIABLE), names(S_data))

  if (start_var > 0 && start_var <= length(all_vars)) {
    all_vars <- all_vars[start_var:length(all_vars)]
  }

  if (!is.null(skip_vars)) {
    if (is.numeric(skip_vars)) {
      skip_indices <- skip_vars[skip_vars > 0 & skip_vars <= length(all_vars)]
      if (length(skip_indices) > 0) all_vars <- all_vars[-skip_indices]
    } else if (is.character(skip_vars)) {
      all_vars <- setdiff(all_vars, skip_vars)
    }
  }

  if (length(all_vars) == 0) {
    warning("No variables left to check after applying filters.")
    return(data.frame(
      Total_Rows = total_rows, Complete_Count = total_rows, Incomplete_Count = 0, Missing_Count = 0,
      Percent_Complete = 100.00, Percent_Incomplete = 0.00, Percent_Missing = 0.00
    ))
  }

  # --- 2. Error Identification ---
  code_to_var <- stats::setNames(M_data$VARIABLE, M_data$VARIABLE_Code)
  error_flags <- matrix(FALSE, nrow = total_rows, ncol = length(all_vars), dimnames = list(NULL, all_vars))

  for (var_name in all_vars) {
    var_meta <- M_data[M_data$VARIABLE == var_name, ][1L, ]
    var_col <- S_data[[var_name]]
    dep_code <- if (is.na(var_meta$Dependency)) 0 else var_meta$Dependency

    if (dep_code == 0) {
      error_flags[, var_name] <- is.na(var_col)
    } else {
      parent_name <- code_to_var[as.character(dep_code)]
      parent_col <- if (!is.null(parent_name) && parent_name %in% names(S_data)) S_data[[parent_name]] else rep(NA, total_rows)

      parent_condition_met <- if (toupper(var_meta$Dep_Value) == "ANY") {
        !is.na(parent_col)
      } else {
        !is.na(parent_col) & (as.character(parent_col) == var_meta$Dep_Value)
      }

      error_flags[, var_name] <- (parent_condition_met & is.na(var_col)) | (!parent_condition_met & !is.na(var_col))
    }
  }

  # --- 3. Row Classification ---
  row_has_any_error <- rowSums(error_flags) > 0
  na_matrix <- is.na(S_data[, all_vars, drop = FALSE])
  row_is_fully_missing <- rowSums(na_matrix) == length(all_vars)

  # --- 4. Final Counts ---
  missing_count <- sum(row_is_fully_missing)
  complete_count <- sum(!row_has_any_error & !row_is_fully_missing)
  incomplete_count <- total_rows - complete_count - missing_count

  # --- 5. Prepare Results Data Frame ---
  results <- data.frame(
    Total_Rows = total_rows,
    Complete_Count = complete_count,
    Incomplete_Count = incomplete_count,
    Missing_Count = missing_count,
    Percent_Complete = round(100 * complete_count / total_rows, 2),
    Percent_Incomplete = round(100 * incomplete_count / total_rows, 2),
    Percent_Missing = round(100 * missing_count / total_rows, 2)
  )

  # --- 6. Plotting (if requested) ---
  if (Show_Plot) {
    plot_data <- data.frame(
      Category = c("Complete", "Incomplete", "Missing"),
      Value = c(results$Percent_Complete, results$Percent_Incomplete, results$Percent_Missing)
    )
    plot_data$Category <- factor(plot_data$Category, levels = c("Complete", "Incomplete", "Missing"))

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = "", y = Value, fill = Category)) +
      ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::theme_void(base_family = "sans") +
      ggplot2::labs(title = "Overall Record Completeness") +
      ggplot2::geom_text(
        aes(label = ifelse(Value > 2, paste0(round(Value), "%"), "")),
        position = ggplot2::position_stack(vjust = 0.5)
      ) +
      ggplot2::scale_fill_brewer(palette = "Pastel1")

    print(p)
  }

  return(results)
}
