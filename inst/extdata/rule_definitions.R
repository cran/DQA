#============ Default Rule Functions for the DQA Package ============

#' Checks for unique (non-duplicate) values (ignores NA)
unique_check <- function(x) {
  res <- !duplicated(x)
  res[is.na(x)] <- NA
  res
}

#' Checks for the presence of alphabetic characters (at least one letter)
character_check <- function(x) {
  grepl("[A-Za-z]", as.character(x))
}

#' Checks if value is a valid email
email_check <- function(x) {
  grepl("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}$", as.character(x), ignore.case = TRUE)
}

#' Checks if value can be interpreted as numeric
numeric_check <- function(x) {
  val <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), NA, !is.na(val))
}

#' Checks if value is integer (whole number, no decimals)
integer_check <- function(x) {
  val <- suppressWarnings(as.numeric(x))
  res <- !is.na(val) & (val %% 1 == 0)
  res[is.na(x)] <- NA
  res
}

#' Checks if string length matches the given value (default 10)
length_check <- function(x, val_num = NULL, ...) {
  len <- if (!is.null(val_num) && length(val_num) >= 1 && !is.na(val_num[1])) as.integer(val_num[1]) else 10L
  out <- nchar(as.character(x)) == len
  out[is.na(x)] <- NA
  out
}

#' Checks membership in allowed categories (numbers and/or literals)
category_check <- function(x, val_num = NULL, val_ops = NULL, val_lit = NULL) {
  n <- length(x)
  x_num <- suppressWarnings(as.numeric(x))
  x_chr <- as.character(x)
  ok <- rep(FALSE, n)
  # Numeric equality
  if (!is.null(val_num) && length(val_num) > 0) {
    ok <- ok | (!is.na(x_num) & (x_num %in% val_num))
  }
  # Numeric comparisons
  if (!is.null(val_ops) && !is.null(val_num) && length(val_ops) == length(val_num)) {
    for (j in seq_along(val_ops)) {
      v <- val_num[j]; op <- val_ops[j]
      if (!is.na(v) && !is.na(op)) {
        expr <- paste("!is.na(x_num) & (x_num", op, "v)")
        ok <- ok | eval(parse(text = expr))
      }
    }
  }
  # Literal matches
  if (!is.null(val_lit) && length(val_lit) > 0) {
    ok <- ok | (!is.na(x_chr) & (tolower(x_chr) %in% tolower(val_lit)))
  }
  ok[is.na(x)] <- NA
  ok
}

#' Checks if value is in a user-supplied set (from Value)
set_check <- function(x, val_lit = NULL) {
  if (is.null(val_lit) || length(val_lit) == 0) return(rep(NA, length(x)))
  x_chr <- as.character(x)
  res <- x_chr %in% val_lit
  res[is.na(x)] <- NA
  res
}

#' Checks for not missing values (not NA)
not_null_check <- function(x) {
  !is.na(x)
}

#' Checks if numeric value is in a given inclusive range (Value="min|max")
range_check <- function(x, val_num = NULL, ...) {
  val <- suppressWarnings(as.numeric(x))
  if (is.null(val_num) || length(val_num) < 2) return(rep(NA, length(x)))
  !is.na(val) & val >= min(val_num, na.rm = TRUE) & val <= max(val_num, na.rm = TRUE)
}

#' Checks if value matches a date format (default: "YYYY-MM-DD" or custom via val_lit)
date_check <- function(x, val_lit = NULL) {
  fmt <- if(!is.null(val_lit) && length(val_lit) > 0) val_lit[1] else "%Y-%m-%d"
  res <- logical(length(x))
  for (i in seq_along(x)) {
    # استخراج اجزای اصلی
    splits <- strsplit(fmt, "[^%a-zA-Z]")[[1]]
    nums <- regmatches(x[i], gregexpr("\\d+", x[i]))[[1]]
    d <- suppressWarnings(tryCatch(as.Date(x[i], format = fmt), error = function(e) NA))
    valid <- !is.na(d)

    if(valid) {
      input_parts <- as.integer(nums)
      date_parts <- as.POSIXlt(d)
      is_same <- TRUE
      if("%Y" %in% splits) is_same <- is_same & (date_parts$year + 1900 == input_parts[which(splits == "%Y")])
      if("%m" %in% splits) is_same <- is_same & (date_parts$mon + 1 == input_parts[which(splits == "%m")])
      if("%d" %in% splits) is_same <- is_same & (date_parts$mday == input_parts[which(splits == "%d")])
      res[i] <- is_same
    } else {
      res[i] <- FALSE
    }
    if(is.na(x[i])) res[i] <- NA
  }
  res
}

#' Checks value against a regular expression provided in val_lit[1]
regex_check <- function(x, val_lit = NULL) {
  if (is.null(val_lit) || length(val_lit) == 0) return(rep(NA, length(x)))
  grepl(val_lit[1], as.character(x))
}

#' Flexible cross-column arithmetic validation (sum, equality, multiplication, etc.)
arithmetic_check <- function(x, val_num = NULL, val_ops = NULL, val_lit = NULL) {
  if (!exists("S_data", inherits = TRUE)) stop("S_data not found in the rule environment.")
  n <- length(x); out <- rep(NA, n)
  expr <- if (!is.null(val_lit) && length(val_lit) > 0) paste(val_lit, collapse = " ") else ""
  to_num <- function(v) suppressWarnings(as.numeric(v))
  tol <- 1e-9

  # col * k | col / k | col */ k
  m <- regexec("^\\s*([A-Za-z][A-Za-z0-9_.]*)\\s*(\\*|/|\\*/)\\s*([+-]?(?:\\d*\\.?\\d+))\\s*$", expr, perl = TRUE)
  r <- regmatches(expr, m)[[1]]
  if (length(r)) {
    col <- r[2]; op <- r[3]; k <- as.numeric(gsub(",", ".", r[4]))
    if (!(col %in% names(S_data)) || is.na(k)) return(rep(NA, n))
    x_num <- to_num(x); y_num <- to_num(S_data[[col]]); ok <- !is.na(x_num) & !is.na(y_num)
    if (op == "*") out[ok] <- abs(x_num[ok] - y_num[ok] * k) <= tol
    else if (op == "/") out[ok] <- abs(x_num[ok] - y_num[ok] / k) <= tol
    else {
      a <- b <- rep(NA, n)
      a[ok] <- abs(x_num[ok] - y_num[ok] * k) <= tol
      b[ok] <- abs(x_num[ok] - y_num[ok] / k) <= tol
      out <- ifelse(is.na(a) & is.na(b), NA, (a | b))
    }
    return(out)
  }
  # sum pattern: col_a + col_b | col_c
  if (grepl("[+|]", expr, perl = TRUE)) {
    cols <- unique(trimws(unlist(strsplit(expr, "[+|]", perl = TRUE))))
    cols <- cols[nzchar(cols)]
    miss <- setdiff(cols, names(S_data))
    if (length(miss)) return(rep(NA, n))
    mat <- sapply(cols, function(nn) to_num(S_data[[nn]])); if (is.null(dim(mat))) mat <- cbind(mat)
    sum_vec <- rowSums(mat, na.rm = FALSE)
    x_num <- to_num(x); ok <- !is.na(x_num) & !is.na(sum_vec)
    out[ok] <- abs(x_num[ok] - sum_vec[ok]) <= tol
    return(out)
  }
  # equality pattern: col_a == col_b or just col_b
  rhs_expr <- trimws(sub("^.*={1,2}\\s*", "", expr))
  if (grepl("^[A-Za-z][A-Za-z0-9_.]*$", rhs_expr)) {
    if (!(rhs_expr %in% names(S_data))) return(rep(NA, n))
    x_num <- to_num(x); y_num <- to_num(S_data[[rhs_expr]]); ok <- !is.na(x_num) & !is.na(y_num)
    out[ok] <- abs(x_num[ok] - y_num[ok]) <= tol
    return(out)
  }
  rep(NA, n)
}

#' BMI column check based on weight and height columns (auto-detects height unit)
#' @param x The column to validate as BMI (e.g., S_data$BMI)
#' @param val_lit Should be a string like "Weight,Height" (from M_data$Value)
#' @param val Optional parameter (for rounding digits and tolerance)
bmi_check <- function(x, val_lit = NULL, val = NULL) {
  if (!exists("S_data", inherits = TRUE)) {
    stop("S_data is not available in the rule environment.")
  }
  # Split val_lit if passed as a single string (e.g., "Weight,Height")
  if (is.null(val_lit)) {
    stop("val_lit must contain column names for weight and height.")
  }
  if (length(val_lit) == 1) {
    val_lit <- unlist(strsplit(val_lit, "[,|;]", perl = TRUE))
    val_lit <- trimws(val_lit)
  }
  if (length(val_lit) < 2) {
    stop("You must provide both weight and height column names in val_lit or Value.")
  }
  wt_col <- val_lit[1]
  ht_col <- val_lit[2]
  if (!(wt_col %in% names(S_data)) || !(ht_col %in% names(S_data))) {
    stop("Provided column names are not found in S_data.")
  }

  wt <- suppressWarnings(as.numeric(gsub(",", ".", S_data[[wt_col]])))
  ht <- suppressWarnings(as.numeric(gsub(",", ".", S_data[[ht_col]])))
  bmi_obs <- suppressWarnings(as.numeric(gsub(",", ".", x)))

  digits <- if (!is.null(val) && length(val) >= 1 && !is.na(val[1])) as.integer(val[1]) else 1L
  tol    <- if (!is.null(val) && length(val) >= 2 && !is.na(val[2])) as.numeric(val[2]) else 0.01

  n <- length(bmi_obs)
  out <- rep(NA, n)

  ht_unit_cm <- FALSE
  if (any(!is.na(ht))) {
    q50 <- stats::median(ht, na.rm = TRUE)
    ht_unit_cm <- is.finite(q50) && q50 > 10
  }

  ok <- !is.na(wt) & !is.na(ht) & ht > 0 & !is.na(bmi_obs)
  if (any(ok)) {
    if (ht_unit_cm) {
      h_m <- ht[ok] / 100
      bmi_calc <- wt[ok] / (h_m^2)
    } else {
      bmi_calc <- wt[ok] / (ht[ok]^2)
    }
    bmi_calc <- round(bmi_calc, digits)
    bmi_ref  <- round(bmi_obs[ok], digits)
    out[ok]  <- abs(bmi_calc - bmi_ref) <= tol
  }
  out
}
