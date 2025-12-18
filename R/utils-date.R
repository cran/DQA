# تابع اصلی تبدیل تاریخ (همه کاره)
smart_to_gregorian_vec <- local({
  cache <- new.env(parent = emptyenv(),hash = TRUE)

  jalali_to_gregorian_scalar <- function(jy, jm, jd) {
    if (is.na(jy) | is.na(jm) | is.na(jd) | jy == "" | jm == "" | jd == "") return(NA_character_)
    jy <- suppressWarnings(as.integer(jy))
    jm <- suppressWarnings(as.integer(jm))
    jd <- suppressWarnings(as.integer(jd))
    if (is.na(jy) | is.na(jm) | is.na(jd) | jm < 1L | jm > 12L | jd < 1L | jd > 31L) return(NA_character_)
    g_days_in_month <- c(31L,28L,31L,30L,31L,30L,31L,31L,30L,31L,30L,31L)
    j_days_in_month <- c(31L,31L,31L,31L,31L,31L,30L,30L,30L,30L,30L,29L)
    jy_tmp <- jy - ifelse(jy > 979L, 979L, 0L)
    days <- 365L * jy_tmp + ((jy_tmp %/% 33L) * 8L) + (((jy_tmp %% 33L) + 3L) %/% 4L)
    if (jm > 1L) days <- days + sum(j_days_in_month[1:(jm - 1L)])
    days <- days + jd - 1L
    g_days <- days + 79L
    if (is.na(g_days)) return(NA_character_)
    gy <- 1600L + 400L * (g_days %/% 146097L)
    g_days <- g_days %% 146097L
    leap <- TRUE
    if (!is.na(g_days) && g_days >= 36525L) {
      g_days <- g_days - 1L
      gy <- gy + 100L * (g_days %/% 36524L)
      g_days <- g_days %% 36524L
      if (g_days >= 365L) g_days <- g_days + 1L else leap <- FALSE
    }
    if (is.na(g_days)) return(NA_character_)
    gy <- gy + 4L * (g_days %/% 1461L)
    g_days <- g_days %% 1461L
    if (!is.na(g_days) && g_days >= 366L) {
      leap <- FALSE
      g_days <- g_days - 1L
      gy <- gy + (g_days %/% 365L)
      g_days <- g_days %% 365L
    }
    i <- 1L
    while (!is.na(g_days) && i <= 12L && g_days >= (g_days_in_month[i] + if (i == 2L && leap) 1L else 0L)) {
      g_days <- g_days - (g_days_in_month[i] + if (i == 2L && leap) 1L else 0L)
      i <- i + 1L
    }
    gm <- i
    gd <- g_days + 1L
    if (is.na(gy) | is.na(gm) | is.na(gd) | gm < 1L | gm > 12L | gd < 1L | gd > 31L) return(NA_character_)
    sprintf("%04d-%02d-%02d", gy, gm, gd)
  }

  smart_to_gregorian_scalar <- function(date_str) {
    if (is.na(date_str) || identical(date_str, "")) return(as.Date(NA))
    ds <- trimws(as.character(date_str))
    ds_std <- gsub("[./]", "-", ds)
    # 8-digit yyyymmdd (Gregorian or Jalali if starts with '1')
    if (grepl("^\\d{8}$", ds)) {
      if (grepl("^1\\d{7}$", ds)) {
        jy <- substr(ds, 1, 4); jm <- substr(ds, 5, 6); jd <- substr(ds, 7, 8)
        out <- jalali_to_gregorian_scalar(jy, jm, jd)
        return(as.Date(out))
      } else {
        gy <- substr(ds, 1, 4); gm <- substr(ds, 5, 6); gd <- substr(ds, 7, 8)
        out <- sprintf("%04d-%02d-%02d", as.integer(gy), as.integer(gm), as.integer(gd))
        return(as.Date(out))
      }
    }
    # Jalali with separators 1xxx-x-x
    if (grepl("^1\\d{3}-\\d{1,2}-\\d{1,2}$", ds_std)) {
      parts <- unlist(strsplit(ds_std, "-"))
      out <- jalali_to_gregorian_scalar(parts[1], parts[2], parts[3])
      return(as.Date(out))
    }
    # Gregorian yyyy-m-d
    if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", ds_std)) {
      return(as.Date(ds_std))
    }
    # d-m-yyyy or m-d-yyyy
    if (grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", ds_std)) {
      parts <- unlist(strsplit(ds_std, "-"))
      if (as.integer(parts[1]) > 12L) {
        out <- sprintf("%04d-%02d-%02d", as.integer(parts[3]), as.integer(parts[2]), as.integer(parts[1]))
      } else {
        out <- sprintf("%04d-%02d-%02d", as.integer(parts[3]), as.integer(parts[1]), as.integer(parts[2]))
      }
      return(as.Date(out))
    }
    # d/m/yyyy or m/d/yyyy or with dots
    if (grepl("^\\d{1,2}[./-]\\d{1,2}[./-]\\d{4}$", ds)) {
      parts <- unlist(strsplit(gsub("[./]", "-", ds), "-"))
      if (as.integer(parts[1]) > 12L) {
        out <- sprintf("%04d-%02d-%02d", as.integer(parts[3]), as.integer(parts[2]), as.integer(parts[1]))
      } else {
        out <- sprintf("%04d-%02d-%02d", as.integer(parts[3]), as.integer(parts[1]), as.integer(parts[2]))
      }
      return(as.Date(out))
    }
    # ناشناخته
    return(as.Date(NA))
  }

  function(x) {
    x <- as.character(x)
    n <- length(x)
    if (n == 0) return(as.Date(character(0)))
    out <- rep(as.Date(NA), n)
    keys <- ifelse(is.na(x) | x == "", NA_character_, x)
    non_empty <- which(!is.na(keys) & keys != "")
    if (length(non_empty)) {
      unseen <- non_empty[!vapply(keys[non_empty], exists, logical(1), envir = cache, inherits = FALSE)]
      if (length(unseen)) {
        conv <- vapply(keys[unseen], function(s) smart_to_gregorian_scalar(s), as.Date(NA))
        for (i in seq_along(unseen)) assign(keys[unseen[i]], conv[i], envir = cache)
      }
    }
    for (i in seq_len(n)) {
      k <- keys[i]
      if (is.na(k) || k == "") {
        out[i] <- as.Date(NA)
      } else {
        if (!exists(k, envir = cache, inherits = FALSE)) {
          assign(k, smart_to_gregorian_scalar(k), envir = cache)
        }
        out[i] <- get(k, envir = cache, inherits = FALSE)
      }
    }
    out
  }
})
