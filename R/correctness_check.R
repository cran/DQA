#' @title Validate Data Against Correctness Rules
#' @description This function validates a data frame against a set of correctness rules specified in another data frame.
#' It allows for complex validation operations, comparison with reference data, and detailed reporting.
#'
#' @param S_data A data frame containing the data to be validated.
#' @param M_data A data frame containing the validation rules. Must have at least the following columns:
#'   \itemize{
#'     \item \code{VARIABLE}: The name of the variable to validate (must match column names in S_data)
#'     \item \code{Correctness_Rule}: The validation rule as an R expression (string)
#'     \item \code{TYPE}: The data type of the variable ("date", "numeric", or other)
#'     \item \code{Correctness_Error_Type}: (Optional) Classification of the error type
#'   }
#' @param Result Logical. If \code{TRUE}, returns the detailed results for each row in S_data.
#'   If \code{FALSE} (default), returns a summary of validation results.
#' @param show_column Character vector. When \code{Result=TRUE}, specifies additional columns from
#'   S_data to include in the output.
#' @param date_parser_fun Function to convert date strings to Date objects. Default is \code{smart_to_gregorian_vec},
#'   which should handle various date formats including Jalali dates.
#' @param golden_data Optional data frame or list containing reference data for validation.
#'   Accessible within rules via the \code{GOLDEN} variable.
#' @param key_column Character string specifying the column name that links rows in S_data to
#'   corresponding rows in golden_data. Required when comparing individual rows with golden_data.
#' @param external_data Optional list or data frame containing additional data for validation rules.
#' @param var_select Character vector or numeric indices specifying which variables from M_data to validate.
#'  By default, it validates all variables.
#' @param batch_size integer. Number of rows to process in each batch (for efficiency).
#' @param verbose logical. If TRUE, prints progress messages.
#'
#' @details
#' The function evaluates each rule specified in M_data against the corresponding data in S_data.
#' Rules are R expressions written as strings, evaluated in an environment where:
#'
#' \itemize{
#'   \item Variables from S_data are available directly by name
#'   \item \code{val} refers to the current variable being validated
#'   \item \code{GOLDEN} provides access to reference data (when golden_data is provided)
#' }
#'
#' Type conversion is applied to variables in S_data based on the TYPE column in M_data:
#' \itemize{
#'   \item "date": Values are converted using the date_parser_fun
#'   \item "numeric": Values are converted to numeric
#'   \item Other types: No conversion is applied
#' }
#'
#' Special handling for date comparisons is provided, including automatic wrapping
#' of GOLDEN references when comparing dates.
#'
#' @return
#' If \code{Result=FALSE} (default): A data frame with one row per validated variable containing:
#' \itemize{
#'   \item VARIABLE: Variable name
#'   \item Condition_Met: Count of rows meeting the condition
#'   \item Condition_Not_Met: Count of rows not meeting the condition
#'   \item NA_Count: Count of rows where validation produced NA
#'   \item Total_Applicable: Count of non-NA validation results
#'   \item Total_Rows: Total number of rows in S_data
#'   \item Percent_Met: Percentage of applicable rows meeting the condition
#'   \item Percent_Not_Met: Percentage of applicable rows not meeting the condition
#'   \item Error_Type: Value from Correctness_Error_Type column in M_data
#' }
#'
#' If \code{Result=TRUE}: A data frame with one row per row in S_data, containing:
#' \itemize{
#'   \item One column per validated variable with logical values (TRUE/FALSE/NA)
#'   \item Any additional columns specified in show_column
#' }
#'
#' @examples
#' Authorized_drug<-data.frame(
#'   Drug_ID = 1:10,
#'   Drug_Name = c("Atorvastatin", "Metformin", "Amlodipine", "Omeprazole", "Aspirin",
#'                 "Levothyroxine", "Sertraline", "Pantoprazole", "Losartan", "ASA"),
#'   stringsAsFactors = FALSE
#' )
#'
#' golde<-data.frame(
#'   National_code = c("123", "456", "789","545","4454","554","665"),
#'   LastName = c("Bahman","Johnson","Williams","Brown","Jones","Garcia","Miller"),
#'   Certificate_Expiry = c("1404-07-01", "2030-01-12", "2025-01-11",
#'   "1404-06-28","2025-09-19",NA,NA),
#'   Blood_type = c("A-","B+","AB","A+","O-","O+","AB-"),
#'   stringsAsFactors = FALSE
#' )
#'
#' S_data <- data.frame(
#'   National_code = c("123", "1456", "789","545","4454","554"),
#'   LastName = c("Aliyar","Johnson","Williams","Brown","Jones","Garcia"),
#'   VisitDate = c("2025-09-23", "2021-01-10", "2021-01-03","1404-06-28","1404-07-28",NA),
#'   Test_date = c("1404-07-01", "2021-01-09", "2021-01-14","1404-06-29","2025-09-19",NA),
#'   Certificate_validity = c("2025-09-23", "2025-01-12", "2025-02-11","1403-06-28","2025-09-19",NA),
#'   Systolic_Reading1 = c(110, NA, 145, 125,114,NA),
#'   Systolic_Reading2 = c(125, 150, NA, 110,100,NA),
#'   Prescription_drug= c("Atorvastatin", "Metformin", "Amlodipine",
#'    "Omeprazole", "Aspirin","Metoprolol"),
#'   Blood_type = c("A-","B+","AB","A+","O-","O+"),
#'   Height = c(178,195,165,NA,155,1.80),
#'   stringsAsFactors = FALSE
#' )
#'
#' M_data <- data.frame(
#'   VARIABLE = c("National_code", "Certificate_validity", "VisitDate","Test_date",
#'                "LastName","Systolic_Reading1","Systolic_Reading2",
#'                "Prescription_drug","Blood_type","Height"),
#'   Correctness_Rule = c(
#'     "National_code %in% GOLDEN$National_code",
#'     "val <= GOLDEN$Certificate_Expiry",
#'     "((val >= '1404-06-01' & val <= '1404-06-31') | val == as.Date('2021-01-02'))",
#'     "val != VisitDate",
#'     "val %in% GOLDEN$LastName",
#'     "",
#'     "",
#'     "val %in% Authorized_drug$Drug_Name",
#'     "val %in% GOLDEN$Blood_type",
#'     ""),
#'   TYPE=c("numeric","date","date","date","character","numeric",
#'   "numeric","character","character","numeric"),
#'   Correctness_Error_Type=c("Error",NA,"Warning","Error",NA,NA,NA,NA,"Error","Warning"),
#'   stringsAsFactors = FALSE
#' )
#'
#' result <- correctness_check(
#'   S_data = S_data,
#'   M_data = M_data,
#'   golden_data = golde,
#'   key_column = c("National_code"),
#'   Result =FALSE,
#'   external_data = Authorized_drug
#' )
#'
#' print(result)
#' #
#' result <- correctness_check(
#'   S_data = S_data,
#'   M_data = M_data,
#'   golden_data = golde,
#'   #key_column = c("National_code"),#If you do not select a key, you can use Gold Data as a
#'   #list and your logical rules will be NA.
#'   Result =TRUE,
#'   external_data = Authorized_drug
#' )
#' print(result)
#'
#' @export
correctness_check <- function(
    S_data,
    M_data,
    Result = FALSE,
    show_column = NULL,
    date_parser_fun = smart_to_gregorian_vec,
    golden_data = NULL,
    key_column = NULL,
    external_data = NULL,
    var_select = "all",
    batch_size = 1000,
    verbose = FALSE
) {
  message(paste("Settings: batch_size =", batch_size))
  if (nrow(S_data) == 0) stop("S_data is empty.")
  if (nrow(M_data) == 0) stop("M_data is empty.")

  ## --- Keep full metadata and remove duplicate rules (keep last) ---
  M_all <- M_data[!duplicated(M_data$VARIABLE, fromLast = TRUE), , drop = FALSE]

  ## --- Determine initial selected variables from var_select ---
  if (identical(var_select, "all")) {
    selected_vars_initial <- as.character(M_all$VARIABLE)
  } else {
    selected_vars_initial <- character()
    for (item in var_select) {
      if (is.numeric(item)) {
        valid_idx <- item[item > 0 & item <= nrow(M_all)]
        if (length(valid_idx)) selected_vars_initial <- c(selected_vars_initial, as.character(M_all$VARIABLE[valid_idx]))
      } else {
        selected_vars_initial <- c(selected_vars_initial, as.character(item))
      }
    }
    selected_vars_initial <- unique(selected_vars_initial)
  }

  ## --- Detect dependencies across all rules in M_all (safe parse, fallback tokens) ---
  deps <- character()
  for (i in seq_len(nrow(M_all))) {
    rule_txt <- as.character(M_all$Correctness_Rule[i])
    if (is.na(rule_txt) || trimws(rule_txt) == "") next
    # Replace 'val' with dummy variable for parsing
    rule_try <- gsub("\\bval\\b", "DUMMY_VAR_FOR_PARSE", rule_txt, perl = TRUE)
    parsed <- tryCatch(parse(text = rule_try), error = function(e) NULL)
    if (!is.null(parsed)) {
      deps <- c(deps, all.vars(parsed[[1]]))
    } else {
      # Fallback: extract token names via regex
      toks <- unique(regmatches(rule_try, gregexpr("\\b[A-Za-z_][A-Za-z0-9_\\.]*\\b", rule_try, perl = TRUE))[[1]])
      deps <- c(deps, toks)
    }
  }
  deps <- unique(deps)

  # Allow external_data's name as dependency if it's a data.frame
  ext_name <- NULL
  if (!is.null(external_data) && is.data.frame(external_data)) ext_name <- deparse(substitute(external_data))

  possible_names <- unique(c(M_all$VARIABLE, names(S_data), "GOLDEN", ext_name))
  deps_in_scope <- intersect(deps, possible_names)
  deps_vars_in_meta <- intersect(deps_in_scope, M_all$VARIABLE)

  ## Final selected vars for rule collection = initial selection + dependencies in metadata
  selected_vars <- unique(c(selected_vars_initial, deps_vars_in_meta))

  ## Subset M_sub (rules to run) based on selected_vars (if var_select != "all")
  M_sub <- M_all
  if (!identical(var_select, "all")) {
    if (length(selected_vars) == 0) {
      warning("No valid variables selected after var_select + dependency detection. No checks will be performed.")
      return(if (isTRUE(Result)) data.frame() else data.frame())
    }
    M_sub <- M_all[M_all$VARIABLE %in% selected_vars, , drop = FALSE]
  }
  if (nrow(M_sub) == 0) {
    warning("No rules selected to run.")
    return(if (isTRUE(Result)) data.frame() else data.frame())
  }

  ## --- Helper: Safe numeric conversion ---
  safe_as_numeric_vec <- function(x) {
    if (is.numeric(x) || inherits(x, "Date")) return(x)
    y <- if (is.factor(x)) as.character(x) else x
    if (is.character(y)) y <- trimws(gsub(",", "", y))
    suppressWarnings(as.numeric(y))
  }

  ## --- Setup base environment for rule evaluation (date_parser_fun, external_data) ---
  base_env <- new.env(parent = baseenv())
  assign("date_parser_fun", date_parser_fun, envir = base_env)
  if (!is.null(external_data) && is.data.frame(external_data)) {
    ext_nm <- deparse(substitute(external_data))
    assign(ext_nm, external_data, envir = base_env)
  } else if (!is.null(external_data) && is.list(external_data)) {
    list2env(external_data, envir = base_env)
  }

  ## --- Determine columns needed for rule evaluation and apply conversions ---
  type_map <- setNames(as.character(M_all$TYPE), M_all$VARIABLE)
  referenced_cols <- intersect(deps, names(S_data))
  vars_to_run <- as.character(M_sub$VARIABLE)
  needed_cols <- unique(intersect(union(vars_to_run, referenced_cols), names(S_data)))
  processed_data <- S_data[, needed_cols, drop = FALSE]

  # Convert date columns
  date_cols <- intersect(names(type_map[type_map == "date"]), names(processed_data))
  numeric_cols <- intersect(names(type_map[type_map == "numeric"]), names(processed_data))
  if (length(date_cols) > 0) {
    for (cname in date_cols) {
      processed_data[[cname]] <- tryCatch(date_parser_fun(processed_data[[cname]]), error = function(e) processed_data[[cname]])
    }
  }
  # Convert numeric columns
  if (length(numeric_cols) > 0) {
    for (cname in numeric_cols) {
      processed_data[[cname]] <- safe_as_numeric_vec(processed_data[[cname]])
    }
  }

  ## --- Prepare GOLDEN mapping by key if requested ---
  golden_env_type <- NULL
  golden_by_key <- list()
  if (!is.null(golden_data)) {
    if (is.data.frame(golden_data) && !is.null(key_column) && key_column %in% names(golden_data) && key_column %in% names(S_data)) {
      # Convert golden columns to date/numeric if applicable
      if (length(date_cols) > 0) {
        g_date_cols <- intersect(date_cols, names(golden_data))
        for (c in g_date_cols) golden_data[[c]] <- tryCatch(date_parser_fun(golden_data[[c]]), error = function(e) golden_data[[c]])
      }
      if (length(numeric_cols) > 0) {
        g_num_cols <- intersect(numeric_cols, names(golden_data))
        for (c in g_num_cols) golden_data[[c]] <- safe_as_numeric_vec(golden_data[[c]])
      }
      golden_data[[key_column]] <- as.character(golden_data[[key_column]])
      for (i in seq_len(nrow(golden_data))) {
        k <- golden_data[[key_column]][i]
        if (!is.na(k) && nzchar(k)) golden_by_key[[k]] <- as.list(golden_data[i, , drop = TRUE])
      }
      golden_env_type <- "by_key"
    } else {
      golden_env_type <- "global"
    }
  }

  ## --- Parse rules from M_sub (replace val, handle date literals, wrap GOLDEN comparisons) ---
  rule_data <- list()
  date_literal_pattern <- "'[0-9]{4}[-./][0-9]{1,2}[-./][0-9]{1,2}'"
  for (i in seq_len(nrow(M_sub))) {
    var_name <- as.character(M_sub$VARIABLE[i])
    rule_raw <- as.character(M_sub$Correctness_Rule[i])
    var_type <- as.character(M_sub$TYPE[i])
    if (is.na(rule_raw) || trimws(rule_raw) == "") next
    # Replace 'val' with variable name
    rule_text <- gsub("\\bval\\b", var_name, rule_raw, perl = TRUE)
    placeholders <- list()
    # Replace date literals with placeholder variables
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
    # Wrap GOLDEN$col with date_parser_fun if rule compares dates and GOLDEN$ present
    if (!is.null(var_type) && var_type == "date" && grepl("GOLDEN\\$", rule_text)) {
      rule_text <- gsub("\\b(GOLDEN\\$[A-Za-z0-9_\\.]+)\\b", "date_parser_fun(\\1)", rule_text, perl = TRUE)
    }
    parsed <- tryCatch(parse(text = rule_text), error = function(e) NULL)
    rule_data[[var_name]] <- list(expr = parsed, placeholders = placeholders, raw = rule_raw)
  }

  if (length(rule_data) == 0) {
    warning("No valid rules to evaluate.")
    return(if (isTRUE(Result)) data.frame() else data.frame())
  }

  ## --- Function to evaluate one rule (batch first, fallback to row-by-row; includes GOLDEN and external) ---
  n <- nrow(S_data)
  process_variable <- function(var_name, rd) {
    if (is.null(rd$expr)) return(rep(NA, n))
    result_vec <- logical(n)
    batches <- ceiling(n / batch_size)
    for (b in seq_len(batches)) {
      start_idx <- (b - 1) * batch_size + 1
      end_idx <- min(b * batch_size, n)
      idx <- start_idx:end_idx
      batch_rows <- processed_data[idx, , drop = FALSE]
      # Try vectorized evaluation
      eval_env_vec <- new.env(parent = base_env)
      for (ph in names(rd$placeholders)) assign(ph, rd$placeholders[[ph]], envir = eval_env_vec)
      if (!is.null(golden_data) && golden_env_type == "global") assign("GOLDEN", golden_data, envir = eval_env_vec)
      if (!is.null(external_data) && is.data.frame(external_data)) {
        ext_nm <- deparse(substitute(external_data))
        assign(ext_nm, external_data, envir = eval_env_vec)
      } else if (!is.null(external_data) && is.list(external_data)) {
        list2env(external_data, envir = eval_env_vec)
      }
      for (col in names(batch_rows)) assign(col, batch_rows[[col]], envir = eval_env_vec)
      vec_res <- tryCatch(eval(rd$expr, envir = eval_env_vec), error = function(e) e)
      if (!inherits(vec_res, "error") && (is.atomic(vec_res) && (length(vec_res) == length(idx) || length(vec_res) == 1))) {
        if (length(vec_res) == 1) result_vec[idx] <- rep(as.logical(vec_res), length(idx)) else result_vec[idx] <- as.logical(vec_res)
      } else {
        # Fallback to per-row evaluation (also handles GOLDEN by-key)
        for (i in idx) {
          row_env <- new.env(parent = base_env)
          for (ph in names(rd$placeholders)) assign(ph, rd$placeholders[[ph]], envir = row_env)
          for (col in names(processed_data)) assign(col, processed_data[[col]][i], envir = row_env)
          # If key not present, return FALSE
          if (!is.null(golden_data)) {
            if (golden_env_type == "by_key" && !is.null(key_column) && key_column %in% names(S_data)) {
              key <- as.character(S_data[[key_column]][i])
              if (!is.na(key) && key %in% names(golden_by_key)) {
                assign("GOLDEN", golden_by_key[[key]], envir = row_env)
              } else {
                result_vec[i] <- FALSE
                next
              }
            } else {
              assign("GOLDEN", golden_data, envir = row_env)
            }
          }
          if (!is.null(external_data) && is.data.frame(external_data)) {
            ext_nm <- deparse(substitute(external_data))
            assign(ext_nm, external_data, envir = row_env)
          } else if (!is.null(external_data) && is.list(external_data)) {
            list2env(external_data, envir = row_env)
          }
          out <- tryCatch(eval(rd$expr, envir = row_env), error = function(e) NA)
          result_vec[i] <- if (length(out) != 1) NA else as.logical(out)
        }
      }
    }
    result_vec
  } # process_variable

  ## --- Evaluate all rules (sequentially) ---
  res_list <- list()
  for (v in names(rule_data)) {
    if (verbose) message("Processing: ", v)
    res_list[[v]] <- process_variable(v, rule_data[[v]])
  }

  ## --- Decide which variables to return (only user-selected ones if var_select != "all") ---
  if (identical(var_select, "all")) {
    cols_to_return <- names(res_list)
  } else {
    cols_to_return <- intersect(selected_vars_initial, names(res_list))
    if (length(cols_to_return) == 0) {
      warning("None of the requested var_select variables had rules to evaluate.")
      if (isTRUE(Result)) return(data.frame()) else return(data.frame())
    }
  }

  ## --- Format output ---
  if (isTRUE(Result)) {
    res_df <- as.data.frame(res_list[cols_to_return], stringsAsFactors = FALSE)
    if (!is.null(show_column)) {
      valid_cols <- show_column[show_column %in% names(S_data)]
      if (length(valid_cols) > 0) res_df <- cbind(res_df, S_data[, valid_cols, drop = FALSE])
    }
    return(res_df)
  }

  ## --- Summary statistics for selected columns only ---
  # Add Correctness_Error_Type to output
  error_type_map <- if ("Correctness_Error_Type" %in% colnames(M_all)) {
    setNames(as.character(M_all$Correctness_Error_Type), M_all$VARIABLE)
  } else {
    setNames(rep(NA_character_, nrow(M_all)), M_all$VARIABLE)
  }

  summary_list <- lapply(cols_to_return, function(vn) {
    vec <- res_list[[vn]]
    na_count <- sum(is.na(vec))
    met <- sum(vec, na.rm = TRUE)
    total <- length(vec) - na_count
    not_met <- total - met
    data.frame(
      VARIABLE = vn,
      Condition_Met = met,
      Condition_Not_Met = not_met,
      NA_Count = na_count,
      Total_Applicable = total,
      Total_Rows = length(vec),
      Percent_Met = if (total > 0) round(100 * met / total, 2) else NA,
      Percent_Not_Met = if (total > 0) round(100 * not_met / total, 2) else NA,
      Correctness_Error_Type = error_type_map[[vn]],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, summary_list)
}
