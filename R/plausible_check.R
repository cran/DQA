#' @title Perform plausibility Check for Data Frame Columns
#' @description This function evaluates Plausibility Rule Checking for Data Frame Columns
#' It Checks logical and clinical rules on columns of a data frame, based on metadata specifications.
#' Supports flexible rule definition, check logical range of variables, and customizable output.
#'
#' @param S_data data.frame. The source data in which rules will be evaluated. Each column may be referenced by the rules.
#' @param M_data data.frame. Metadata describing variables and their plausibility rules. Must include at least columns \code{VARIABLE} , \code{Plausible_Rule}, \code{TYPE} and \code{Plausible_Error_Type}.
#' @param Result logical (default: \code{FALSE}). If \code{TRUE}, returns row-by-row evaluation results for each rule. If \code{FALSE}, returns a summary table for each rule.
#' @param show_column character vector (default: \code{NULL}). Names of columns from \code{S_data} to include in the result when \code{Result = TRUE}. Ignored otherwise.
#' @param date_parser_fun function (default: \code{smart_to_gregorian_vec}). Converting Persian dates to English,Function to convert date values or date literals to \code{Date} class. Must accept character vectors and return \code{Date} objects.
#' @param var_select character, numeric, or \code{"all"} (default: \code{"all"}). Subset of variables (rules) to check. Can be a character vector of variable names, numeric vector of row indices in \code{M_data}, or \code{"all"} to run all rules.
#' @param verbose logical (default: \code{FALSE}). If \code{TRUE}, prints diagnostic messages during rule processing and evaluation.
#'
#' @details
#' The metadata data.frame (\code{M_data}) must contain at least the following columns:
#' \itemize{
#'   \item \strong{VARIABLE}: The name of the variable in \code{S_data} to which the rule applies.
#'   \item \strong{Plausible_Rule}: The logical rule (as a string) to be evaluated for each row.
#'   \item \strong{TYPE}: The expected type of the variable (e.g., "numeric", "date", "character").
#'   \item \strong{Plausible_Error_Type}: The error type for each rule will be reported in the summary output.Based on the importance and severity of the rule, it can include two options: "Warning" or "Error".
#' }
#'
#' For each variable described in \code{M_data}, the function:
#' \itemize{
#'   \item Replaces any instance of the string "val" in the rule with the actual column name of the variable.
#'   \item Parses and detects any date literals in the rule and substitutes them with placeholders; these placeholders are converted to Date class using the provided \code{date_parser_fun}.
#'   \item Automatically converts any referenced data columns to the appropriate type (numeric, date, or character) based on the \code{TYPE} column in the metadata.
#'   \item Detects which columns from \code{S_data} are referenced in each rule and ensures they are available and correctly typed before evaluation.
#'   \item Evaluates the rule for each row of \code{S_data}, using vectorized evaluation for performance where possible, and falling back to row-wise evaluation if necessary (e.g., for rules that are not vectorizable, such as those using \code{ifelse} with NA logic).
#' }
#'
#' The function supports flexible rule definitions, including conditions involving multiple columns, and custom logic using R expressions.
#'
#' If \code{Result = FALSE}, the function returns a summary table for each rule, including counts and percentages of rows that meet or do not meet the condition, as well as the error type from the metadata if present.
#'
#' If \code{Result = TRUE}, the function returns a data.frame with one column per rule/variable, each containing logical values (\code{TRUE}, \code{FALSE}, or \code{NA}) for every row, plus any extra columns from \code{S_data} listed in \code{show_column}.
#' @return
#' If \code{Result = FALSE}: a data.frame summary with columns:
#' \itemize{
#'   \item VARIABLE: Name of the variable/rule.
#'   \item Condition_Met: Number of rows where the rule is TRUE.
#'   \item Condition_Not_Met: Number of rows where the rule is FALSE.
#'   \item NA_Count: Number of rows with missing/indeterminate result.
#'   \item Total_Applicable: Number of non-NA rows.
#'   \item Total_Rows: Number of total rows.
#'   \item Percent_Met: Percentage of applicable rows meeting the condition.
#'   \item Percent_Not_Met: Percentage of applicable rows not meeting the condition.
#'   \item Plausible_Error_Type: Error type from metadata (if available).
#' }
#'
#' @examples
#' # Source data
#' S_data <- data.frame(
#'   National_code = c("123", "1456", "789","545","4454","554"),
#'   LastName = c("Aliyar","Johnson","Williams","Brown","Jones","Garcia"),
#'   VisitDate = c("2025-09-23", "2021-01-10", "2021-01-03","1404-06-28","1404-07-28",NA),
#'   Test_date = c("1404-07-01", "2021-01-09", "2021-01-14","1404-06-29","2025-09-19",NA),
#'   Certificate_validity = c("2025-09-21", "2025-01-12", "2025-02-11","1403-06-28","2025-09-19",NA),
#'   DiastolicBP = c(110, NA, 145, 125,114,NA),
#'   SystolicBP = c(125, 150, NA, 110,100,NA),
#'   Prescription_drug= c("Atorvastatin", "Metformin", "Amlodipine",
#'   "Omeprazole", "Aspirin","Metoprolol"),
#'   Blood_type = c("A-","B+","AB","A+","O-","O+"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # META DATA
#' M_data <- data.frame(
#'   VARIABLE = c("National_code", "Certificate_validity", "VisitDate",
#'                "Test_date","LastName","DiastolicBP","SystolicBP",
#'                "Prescription_drug","Blood_type"),
#'   Plausible_Rule = c(
#'     "val<=123",
#'     "",
#'     "",
#'     "",
#'     "",
#'     "val < 40 | val > 145",
#'     "val < 50 | val > 230",
#'     "",
#'     ""),
#'   TYPE=c("numeric","date","date","date","character",
#'          "numeric","numeric","character","character"),
#'   Plausible_Error_Type = c("warning",NA,"Error","warning",NA,"warning","warning",NA,"Error"),
#'   stringsAsFactors = FALSE
#' )
#'
#' result <- plausible_check(
#'   S_data = S_data,
#'   M_data = M_data,
#'   Result = TRUE,
#'   show_column = c("National_code")
#' )
#'
#' print(result)
#'
#' result <- plausible_check(
#'   S_data = S_data,
#'   M_data = M_data,
#'   Result = FALSE,
#'   var_select = c("DiastolicBP","DiastolicBP")
#' )
#'
#' print(result)
#'
#' @export
plausible_check <- function(
    S_data,
    M_data,
    Result = FALSE,
    show_column = NULL,
    date_parser_fun = smart_to_gregorian_vec,
    var_select = "all",
    verbose = FALSE
) {
  # Check required input
  if (missing(S_data) || missing(M_data)) stop("S_data and M_data are required.")
  if (nrow(S_data) == 0) stop("S_data is empty.")
  if (nrow(M_data) == 0) stop("M_data is empty.")

  # Keep a full copy of metadata (for TYPE lookups)
  M_all <- M_data[!duplicated(M_data$VARIABLE, fromLast = TRUE), , drop = FALSE]
  # Subset of rules to run (based on var_select)
  M_sub <- M_all
  if (!identical(var_select, "all")) {
    if (is.numeric(var_select)) {
      M_sub <- M_sub[var_select, , drop = FALSE]
    } else {
      M_sub <- M_sub[M_sub$VARIABLE %in% as.character(var_select), , drop = FALSE]
    }
  }
  if (nrow(M_sub) == 0) stop("No rules remain to run after var_select.")

  n <- nrow(S_data)
  date_literal_pattern <- "'1[34][0-9]{2}[-./][0-9]{1,2}[-./][0-9]{1,2}'"

  # Prepare rules: replace val, extract date literals, parse, detect referenced columns
  rule_list <- list()
  for (i in seq_len(nrow(M_sub))) {
    var_name <- as.character(M_sub$VARIABLE[i])
    rule_raw <- as.character(M_sub$Plausible_Rule[i])
    if (is.na(rule_raw) || trimws(rule_raw) == "") next

    # Replace 'val' with actual column name
    rule_text <- gsub("\\bval\\b", var_name, rule_raw, perl = TRUE)

    # Find date literals and replace with placeholders
    placeholders <- list()
    dm <- gregexpr(date_literal_pattern, rule_text, perl = TRUE)
    if (dm[[1]][1] != -1) {
      lits <- unique(regmatches(rule_text, dm)[[1]])
      for (k in seq_along(lits)) {
        lit <- lits[k]
        date_str <- gsub("'", "", lit)
        greg_date <- tryCatch(date_parser_fun(date_str), error = function(e) NA)
        if (!is.na(greg_date)) {
          ph <- paste0(".DATE_", gsub("[^A-Za-z0-9_]", "_", var_name), "_", k)
          rule_text <- gsub(lit, ph, rule_text, fixed = TRUE)
          placeholders[[ph]] <- as.Date(greg_date)
        }
      }
    }

    # Parse rule expression
    parsed <- tryCatch(parse(text = rule_text), error = function(e) NULL)
    expr <- if (!is.null(parsed)) parsed[[1]] else NULL

    # Detect referenced columns in the rule
    vars_in_rule <- character(0)
    if (!is.null(expr)) {
      vars_in_rule <- intersect(all.vars(expr), names(S_data))
    } else {
      tmp <- tryCatch(parse(text = gsub("\\bval\\b", var_name, rule_raw)), error = function(e) NULL)
      if (!is.null(tmp)) vars_in_rule <- intersect(all.vars(tmp[[1]]), names(S_data))
    }

    rule_list[[length(rule_list) + 1]] <- list(
      var = var_name,
      raw = rule_raw,
      text = rule_text,
      expr = expr,
      placeholders = placeholders,
      vars_in_rule = unique(vars_in_rule),
      error_type = if ("Plausible_Error_Type" %in% names(M_sub)) M_sub$Plausible_Error_Type[i] else NA
    )
  }

  if (length(rule_list) == 0) {
    warning("No valid rule found for evaluation.")
    return(data.frame())
  }

  # Determine columns needed from S_data
  referenced_cols <- unique(unlist(lapply(rule_list, function(rd) rd$vars_in_rule)))
  vars_to_run <- unique(sapply(rule_list, function(rd) rd$var))
  needed_cols <- intersect(union(referenced_cols, vars_to_run), names(S_data))

  # Determine TYPEs from metadata
  type_map <- setNames(as.character(M_all$TYPE), M_all$VARIABLE)
  date_cols <- intersect(names(type_map[type_map == "date"]), needed_cols)
  numeric_cols <- intersect(names(type_map[type_map == "numeric"]), needed_cols)

  # Subset processed_data to only needed columns
  processed_data <- S_data[, needed_cols, drop = FALSE]

  # Apply type conversions
  if (length(date_cols) > 0) {
    for (cname in date_cols) {
      processed_data[[cname]] <- tryCatch(date_parser_fun(processed_data[[cname]]), error = function(e) processed_data[[cname]])
    }
  }
  if (length(numeric_cols) > 0) {
    for (cname in numeric_cols) {
      x <- processed_data[[cname]]
      if (is.numeric(x) || inherits(x, "Date")) next
      y <- if (is.factor(x)) as.character(x) else x
      if (is.character(y)) y <- trimws(gsub(",", "", y))
      processed_data[[cname]] <- suppressWarnings(as.numeric(y))
    }
  }

  # Evaluate each rule: try vectorized, else fall back to row-wise
  res_list <- list()
  for (rd in rule_list) {
    vname <- rd$var
    if (verbose) message("Evaluating rule for: ", vname)
    if (is.null(rd$expr)) {
      res_list[[vname]] <- rep(NA, n); next
    }

    # Minimal set of columns for this rule
    cols_needed_for_rule <- intersect(unique(c(rd$vars_in_rule, vname)), names(processed_data))
    cols_list <- as.list(processed_data[, cols_needed_for_rule, drop = FALSE])
    # Attach placeholders as needed
    if (length(rd$placeholders) > 0) {
      for (ph in names(rd$placeholders)) cols_list[[ph]] <- rd$placeholders[[ph]]
    }

    # Vectorized evaluation environment
    eval_env <- list2env(cols_list, parent = baseenv())

    vec_result <- tryCatch(eval(rd$expr, envir = eval_env), error = function(e) e)

    if (inherits(vec_result, "error")) {
      if (verbose) message("Vectorized eval failed for ", vname, " -> falling back to row-wise.")
      single_result <- sapply(seq_len(n), function(i) {
        row_env <- new.env(parent = baseenv())
        for (nm in names(cols_list)) {
          v <- cols_list[[nm]]
          val <- if (length(v) >= i) v[i] else v
          assign(nm, val, envir = row_env)
        }
        out <- tryCatch(eval(rd$expr, envir = row_env), error = function(e) NA)
        if (length(out) != 1) NA else as.logical(out)
      })
      res_list[[vname]] <- as.logical(single_result)
    } else {
      if (is.atomic(vec_result) && length(vec_result) == n) {
        res_list[[vname]] <- as.logical(vec_result)
      } else if (length(vec_result) == 1) {
        res_list[[vname]] <- rep(as.logical(vec_result), n)
      } else {
        if (verbose) message("Unexpected result shape for ", vname, " -> falling back to row-wise.")
        single_result <- sapply(seq_len(n), function(i) {
          row_env <- new.env(parent = baseenv())
          for (nm in names(cols_list)) {
            v <- cols_list[[nm]]; val <- if (length(v) >= i) v[i] else v
            assign(nm, val, envir = row_env)
          }
          out <- tryCatch(eval(rd$expr, envir = row_env), error = function(e) NA)
          if (length(out) != 1) NA else as.logical(out)
        })
        res_list[[vname]] <- as.logical(single_result)
      }
    }
  } # end rules loop

  # Format output
  res_dt <- as.data.frame(res_list, stringsAsFactors = FALSE)
  if (isTRUE(Result)) {
    if (!is.null(show_column)) {
      valid_cols <- intersect(show_column, names(S_data))
      if (length(valid_cols) > 0) {
        res_dt <- cbind(res_dt, S_data[, valid_cols, drop = FALSE])
      } else {
        warning("Selected show_column(s) do not exist in the data.")
      }
    }
    rownames(res_dt) <- NULL
    return(res_dt)
  } else {
    # When Result = FALSE: summary table, include Plausible_Error_Type if present
    summary_list <- lapply(names(res_list), function(vn) {
      vec <- res_list[[vn]]
      na_count <- sum(is.na(vec))
      met <- sum(vec, na.rm = TRUE)
      total <- length(vec) - na_count
      error_type <- NA
      if ("Plausible_Error_Type" %in% names(M_sub)) {
        row_match <- which(M_sub$VARIABLE == vn)
        if (length(row_match) > 0) error_type <- M_sub$Plausible_Error_Type[row_match[1]]
      }
      data.frame(
        VARIABLE = vn,
        Condition_Met = met,
        Condition_Not_Met = total - met,
        NA_Count = na_count,
        Total_Applicable = total,
        Total_Rows = length(vec),
        Percent_Met = if (total > 0) round(100 * met / total, 2) else NA,
        Percent_Not_Met = if (total > 0) round(100 * (total - met) / total, 2) else NA,
        Plausible_Error_Type = error_type,
        stringsAsFactors = FALSE
      )
    })
    return(do.call(rbind, summary_list))
  }
}
