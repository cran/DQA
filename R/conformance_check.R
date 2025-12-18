#' @title Perform Conformance Check on Data Based on Defined Rules
#' @description This function evaluates a source dataframe (`S_data`) against a set
#' of rules defined in a metadata dataframe (`M_data`). It uses a set of
#' default rule functions but can also use a user-provided file.
#'
#' @param S_data A dataframe containing the source data to be checked.
#' @param M_data A metadata dataframe that specifies the rules. It must
#' contain the columns `VARIABLE`, `Conformance_Rule`, and `Value`.
#' @details
#' The metadata (`M_data`) for conformance_check must include:
#' \itemize{
#'   \item **VARIABLE:** The name of the column in `S_data` to which the rule applies.
#'   \item **Conformance_Rule:** The name of the rule function to execute for the VARIABLE (must be defined in the rule file).
#'   \item **Value:** Rule parameters such as Allowed length of values,, allowed category values,
#'    or column names required for computational checks.
#' }
#' @param rule_file The path to a custom R file where rule functions are defined.
#' If `NULL` (default), the standard rule definitions file included with the`DQA` package will be used.
#' Instructions for using this file are available under the name `conformance_rules`.
#' @param na_as_error A logical value. If `TRUE`, `NA` values in the source
#' data are treated as errors (non-conformant). If `FALSE` (default), they
#' are ignored.
#' @param var_select Character or integer vector of variables to check. Accepts
#' variable names, column numbers, or a mix. Default is "all" (check all variables in M_data).
#'
#' @return A dataframe containing the results of the conformance check for each rule.
#'
#' @importFrom stats median
#' @export
#'
#' @examples
#' # 1. Create sample source data (S_data)
#' S_data <- data.frame(
#'   id = 1:10,
#'   national_id = c("1234567890", "0987654321", "123", NA, "1112223334",
#'                   "1234567890", "5556667778", "9998887770", "12345", "4445556667"),
#'   gender = c(1, 2, 1, 3, 2, 1, NA, 2, 1, 2), # 1=Male, 2=Female, 3=Error
#'   age = c(25, 40, 150, 33, -5, 65, 45, 29, 70, 55),
#'   part_a = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55),
#'   part_b = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
#'   total_parts = c(15, 25, 35, 45, 55, 65, 75, 85, 94, 105), # one error in row 9
#'   stringsAsFactors = FALSE
#' )
#'
#' # 2. Create sample metadata (M_data)
#' M_data <- data.frame(
#'   VARIABLE = c(
#'     "national_id",
#'     "national_id",
#'     "gender",
#'     "total_parts"
#'   ),
#'   Conformance_Rule = c(
#'     "length_check",
#'     "unique_check",
#'     "category_check",
#'     "arithmetic_check"
#'   ),
#'   Value = c(
#'     "10",                  # national_id length must be 10
#'     "",                    # unique
#'     "1 | 2",               # Allowed values for gender
#'     "part_a + part_b"      # Computational rule for total_parts
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # 3. Run the conformance check using the package's default rules
#' # Ensure the 'DQA' package is loaded before running
#'  conformance_results <- conformance_check(S_data = S_data, M_data = M_data)
#'  print(conformance_results)
#'
conformance_check <- function(
    S_data,
    M_data,
    rule_file = NULL,
    na_as_error = FALSE,
    var_select = "all"
) {
  stopifnot(is.data.frame(S_data), is.data.frame(M_data))

  # --- Filter changes based on var_select (ability to get column number and name) ---
  if (!identical(var_select, "all")) {
    # Convert all items to character (column names)
    # If it is a number, find the column name
    var_select <- unlist(var_select)

    # Fix: only once as.integer with suppressWarnings
    tmp_int <- suppressWarnings(as.integer(var_select))
    num_vars  <- tmp_int[!is.na(tmp_int)]
    name_vars <- as.character(var_select[is.na(tmp_int)])

    # Convert all valid column numbers to names
    if (length(num_vars) > 0) {
      valid_num_vars <- num_vars[num_vars > 0 & num_vars <= ncol(S_data)]
      name_vars <- c(name_vars, names(S_data)[valid_num_vars])
    }
    # Remove duplicates and keep only those that exist in S_data
    name_vars <- unique(name_vars)
    name_vars <- name_vars[name_vars %in% names(S_data)]

    # Filter M_data only on these variables
    M_data <- M_data[M_data$VARIABLE %in% name_vars, , drop = FALSE]
    if (nrow(M_data) == 0) {
      stop("None of the selected variables are present in both S_data and M_data.")
    }
  }

  if (is.null(rule_file)) {
    rule_file <- system.file("extdata", "rule_definitions.R", package = "DQA", mustWork = TRUE)
  }
  if (!file.exists(rule_file)) {
    stop("Rule file not found: ", normalizePath(rule_file, mustWork = FALSE))
  }

  required_cols <- c("VARIABLE", "Conformance_Rule", "Value")
  missing_cols <- setdiff(required_cols, names(M_data))
  if (length(missing_cols)) stop("M_data is missing required columns: ", paste(missing_cols, collapse = ", "))

  # 2) Create rule environment, inject data, and load the rule file
  rule_env <- new.env(parent = baseenv())
  assign("S_data", S_data, envir = rule_env)
  assign("M_data", M_data, envir = rule_env)
  sys.source(rule_file, envir = rule_env)

  # 3) Local helper functions
  is_binary01 <- function(x) {
    if (!is.numeric(x)) return(FALSE)
    ux <- unique(x[!is.na(x)])
    if (!length(ux)) return(FALSE)
    all(ux %in% c(0, 1))
  }

  parse_value_all <- function(value_str) {
    if (is.na(value_str) || !nzchar(value_str)) {
      return(list(vals = numeric(0), ops = character(0), lits = character(0)))
    }
    parts <- trimws(unlist(strsplit(value_str, "\\|", perl = TRUE), use.names = FALSE))
    vals <- numeric(0); ops <- character(0); lits <- character(0)
    for (p in parts) {
      if (!nzchar(p)) next
      lhs <- trimws(sub("=.*$", "", p))
      m_op <- regexpr("^(<=|>=|==|!=|<|>)", lhs, perl = TRUE)
      op <- NA_character_
      num_str <- lhs
      if (m_op[1] != -1L) {
        op <- regmatches(lhs, m_op)
        num_str <- sub("^(<=|>=|==|!=|<|>)\\s*", "", lhs, perl = TRUE)
      }
      num_str_norm <- gsub(",", ".", trimws(num_str), fixed = TRUE)
      if (grepl("^[+-]?(?:\\d+[\\.]\\d+|\\d+|[\\.]\\d+)$", num_str_norm, perl = TRUE)) {
        vt <- suppressWarnings(as.numeric(num_str_norm))
        if (!is.na(vt)) {
          vals <- c(vals, vt); ops <- c(ops, ifelse(is.na(op), NA_character_, op))
          next
        }
      }
      lits <- c(lits, lhs)
    }
    if (length(ops) < length(vals)) length(ops) <- length(vals)
    list(vals = vals, ops = ops, lits = lits)
  }

  call_rule <- function(Func, x, val_num, val_ops, val_lit) {
    out <- tryCatch(Func(x, val_num, val_ops, val_lit), error = function(e) e)
    if (!inherits(out, "error")) return(out)
    if (!is.null(val_num) || !is.null(val_ops)) {
      out <- tryCatch(Func(x, val_num, val_ops), error = function(e) e)
      if (!inherits(out, "error")) return(out)
    }
    if (!is.null(val_num)) {
      out <- tryCatch(Func(x, val_num), error = function(e) e)
      if (!inherits(out, "error")) return(out)
    }
    if (!is.null(val_lit)) {
      out <- tryCatch(Func(x, val_lit), error = function(e) e)
      if (!inherits(out, "error")) return(out)
    }
    out <- tryCatch(Func(x), error = function(e) e)
    if (!inherits(out, "error")) return(out)
    stop("Rule execution failed: ", conditionMessage(out))
  }

  # 4) Execute rules
  results <- vector("list", nrow(M_data))
  k <- 0L
  for (i in seq_len(nrow(M_data))) {
    rule_name <- trimws(as.character(M_data[i, "Conformance_Rule", drop = TRUE]))
    var_name  <- as.character(M_data[i, "VARIABLE", drop = TRUE])
    value_str <- as.character(M_data[i, "Value", drop = TRUE])

    if (!nzchar(rule_name)) next
    if (!nzchar(var_name) || !(var_name %in% names(S_data))) {
      warning("Column '", var_name, "' not found in S_data; skipping row ", i, "."); next
    }
    if (!exists(rule_name, envir = rule_env, mode = "function")) {
      warning("Rule function '", rule_name, "' not found in the rule file; skipping row ", i, "."); next
    }

    pv <- parse_value_all(value_str)
    val_num <- if (length(pv$vals)) pv$vals else NULL
    val_ops <- if (length(pv$ops))  pv$ops  else NULL
    val_lit <- if (length(pv$lits)) pv$lits else NULL

    Func <- get(rule_name, envir = rule_env, mode = "function")
    xcol <- S_data[[var_name]]

    F_raw <- tryCatch(
      call_rule(Func, xcol, val_num, val_ops, val_lit),
      error = function(e) {
        warning("Error executing Rule '", rule_name, "' for variable '", var_name, "': ", conditionMessage(e))
        NA
      }
    )

    # ==== NA count، Condition_Met و Condition_Not_Met ====
    denom <- NA_integer_
    cond_met <- NA_integer_
    cond_not_met <- NA_integer_
    percent <- NA_real_
    not_percent <- NA_real_
    na_count <- NA_integer_

    if (!is.null(F_raw) && length(F_raw) == length(xcol)) {
      # When the output is the size of all rows
      na_count <- sum(is.na(F_raw))

      if (isTRUE(na_as_error)) {
        # Count all NAs as errors
        cond_met <- sum(F_raw == TRUE, na.rm = TRUE)
        cond_not_met <- sum(F_raw == FALSE | is.na(F_raw))
        denom <- length(F_raw)
      } else {
        # Ignore NAs
        cond_met <- sum(F_raw == TRUE, na.rm = TRUE)
        cond_not_met <- sum(F_raw == FALSE, na.rm = TRUE)
        denom <- sum(!is.na(F_raw))
      }
      percent <- if (denom > 0L) round(cond_met / denom * 100, 2) else 0
      not_percent <- if (denom > 0L) round(cond_not_met / denom * 100, 2) else 0
    } else {
      # When the output is in summary/single form
      base_vec <- xcol
      na_count <- sum(is.na(base_vec))

      if (isTRUE(na_as_error)) {
        denom <- length(base_vec)
        cond_met <- if (length(F_raw) == 1L && !is.na(F_raw)) {
          if (is.numeric(F_raw)) {
            if (F_raw >= 0 && F_raw <= 1) as.integer(round(F_raw * denom)) else as.integer(round(F_raw))
          } else if (is.logical(F_raw)) {
            if (isTRUE(F_raw)) denom else 0L
          } else NA_integer_
        } else if (is.numeric(F_raw)) {
          as.integer(round(suppressWarnings(sum(F_raw, na.rm = TRUE))))
        } else NA_integer_
        cond_not_met <- denom - cond_met
      } else {
        denom <- sum(!is.na(base_vec))
        cond_met <- if (length(F_raw) == 1L && !is.na(F_raw)) {
          if (is.numeric(F_raw)) {
            if (F_raw >= 0 && F_raw <= 1) as.integer(round(F_raw * denom)) else as.integer(round(F_raw))
          } else if (is.logical(F_raw)) {
            if (isTRUE(F_raw)) denom else 0L
          } else NA_integer_
        } else if (is.numeric(F_raw)) {
          as.integer(round(suppressWarnings(sum(F_raw, na.rm = TRUE))))
        } else NA_integer_
        cond_not_met <- denom - cond_met
      }
      percent <- if (denom > 0L) round(cond_met / denom * 100, 2) else 0
      not_percent <- if (denom > 0L) round(cond_not_met / denom * 100, 2) else 0
    }

    if (!is.na(denom) && denom == 0L) {
      cond_met <- 0L; cond_not_met <- 0L; percent <- 0; not_percent <- 0
    }

    k <- k + 1L
    results[[k]] <- data.frame(
      Variable = var_name,
      Condition_Met = cond_met,
      Condition_Not_Met = cond_not_met,
      Condition_Met_Percent = percent,
      Condition_Not_Met_Percent = not_percent,
      NA_Count = na_count,
      conformance_rule = rule_name,
      stringsAsFactors = FALSE
    )
  }

  if (k == 0L) {
    return(data.frame(
      Variable = character(),
      Condition_Met = integer(),
      Condition_Not_Met = integer(),
      Condition_Met_Percent = double(),
      Condition_Not_Met_Percent = double(),
      NA_Count = integer(),
      conformance_rule = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, results[seq_len(k)])
}
