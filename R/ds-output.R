print_stats <- function(data) {
  n <- nchar(format(data$uss, nsmall = 2))
  width1 <- 52 + (2 * n)
  width2 <- as.integer(width1 / 2)
  width3 <- width2 - 5
  width4 <- width2 - 2

  col1 <- max(nchar(as.character(data$lowobs)))
  col2 <- max(nchar(as.character(data$highobs)))
  col3 <- max(nchar(as.character(data$lowobsi)))
  col4 <- max(nchar(as.character(data$highobsi)))
  v <- nchar("Value")
  ol <- max(col1, col2, col3, col4, v)
  gap <- width4 - (2 * ol)

  cat(
    formatc("Univariate Analysis", width1), "\n\n",
    formatl("N"), formatr(data$obs, n), formats(),
    formatl("Variance"), formatr(data$variance, n), "\n",
    formatl("Missing"), formatr(data$missing, n), formats(),
    formatl("Std Deviation"), formatr(data$stdev, n), "\n",
    formatl("Mean"), formatr(data$avg, n), formats(),
    formatl("Range"), formatr(data$range, n), "\n",
    formatl("Median"), formatr(data$median, n), formats(),
    formatl("Interquartile Range"), formatr(data$iqrange, n), "\n",
    formatl("Mode"), formatr(data$mode, n), formats(),
    formatl("Uncorrected SS"), formatr(data$uss, n), "\n",
    formatl("Trimmed Mean"), formatr(data$tavg, n), formats(),
    formatl("Corrected SS"), formatr(data$css, n), "\n",
    formatl("Skewness"), formatr(data$skew, n), formats(),
    formatl("Coeff Variation"), formatr(data$cvar, n), "\n",
    formatl("Kurtosis"), formatr(data$kurtosis, n), formats(),
    formatl("Std Error Mean"), formatr(data$sem, n), "\n\n",
    formatc("Quantiles", width1), "\n\n",
    formatc("Quantile", width2), formatc("Value", width2), "\n\n",
    formatc("Max       ", width2), formatnc(data$Max, width2), "\n",
    formatc("99%       ", width2), formatnc(data$per99, width2), "\n",
    formatc("95%       ", width2), formatnc(data$per95, width2), "\n",
    formatc("90%       ", width2), formatnc(data$per90, width2), "\n",
    formatc("Q3        ", width2), formatnc(data$per75, width2), "\n",
    formatc("Median    ", width2), formatnc(data$median, width2), "\n",
    formatc("Q1        ", width2), formatnc(data$per25, width2), "\n",
    formatc("10%       ", width2), formatnc(data$per10, width2), "\n",
    formatc("5%        ", width2), formatnc(data$per5, width2), "\n",
    formatc("1%        ", width2), formatnc(data$per1, width2), "\n",
    formatc("Min       ", width2), formatnc(data$min, width2), "\n\n",
    formatc("Extreme Values", width1), "\n\n",
    formatc("Low", width2), formatc("High", width2), "\n\n",
    formatol("Obs", ol), format_gap(gap), formatol("Value", ol), formats(),
    formatol("Obs", ol), format_gap(gap), formatol("Value", ol), "\n"
  )
  for (i in seq_len(5)) {
    cat(
      "", formatol(data$lowobsi[i], ol), format_gap(gap), formatol(data$lowobs[i], ol), formats(),
      formatol(data$highobsi[i], ol), format_gap(gap), formatol(data$highobs[i], ol), "\n"
    )
  }
}

print_cross <- function(data) {
  p <- length(data$utility$var2_levels)
  q <- p + 2
  h <- p + 1
  r <- (h * 15) - 3
  f <- length(data$utility$var1_levels)
  g <- f + 2
  h <- p + 1

  col_names <- c(data$utility$varnames[1], data$utility$var2_levels, "Row Total")
  col_totals <- c("Column Total", data$utility$column_totals, data$utility$obs)

  cat(
    formatter("    Cell Contents\n"), "|---------------|\n", "|", formatter("Frequency"),
    "|\n", "|", formatter("Percent"), "|\n", "|", formatter("Row Pct"), "|\n",
    "|", formatter("Col Pct"), "|\n", "|---------------|\n\n", "Total Observations: ",
    data$utility$obs, "\n\n"
  )
  cat("-", rep("---------------", q), sep = "")
  cat("\n")
  cat(
    "|              |", format(data$utility$varnames[2], width = r, justify = "centre"),
    "|"
  )
  cat("\n")
  cat("-", rep("---------------", q), sep = "")
  cat("\n|")
  for (i in seq_along(col_names)) {
    cat(formatter(col_names[i]), "|")
  }
  cat("\n-", rep("---------------", q), sep = "")
  cat("\n")

  for (i in seq_len(f)) {
    cat("|")
    for (j in seq_len(q)) {
      cat(formatter(data$utility$twowaytable[i, j]), "|")
    }
    cat("\n")
    cat("|              |")
    for (j in seq_len(p)) {
      cat(formatter(data$utility$percent_table[i, j]), "|")
    }
    cat("              |")
    cat("\n")
    cat("|              |")
    for (j in seq_len(h)) {
      cat(formatter(data$utility$row_percent[i, j]), "|")
    }
    cat("\n")
    cat("|              |")
    for (j in seq_len(p)) {
      cat(formatter(data$utility$column_percent[i, j]), "|")
    }
    cat("              |")
    cat("\n-", rep("---------------", q), sep = "")
    cat("\n")
  }
  cat("|")
  for (i in seq_along(col_totals)) {
    cat(formatter(col_totals[i]), "|")
  }
  cat("\n")
  cat("|              |")
  for (i in seq_along(data$utility$percent_column)) {
    cat(formatter(data$utility$percent_column[i]), "|")
  }
  cat("              |")
  cat("\n-", rep("---------------", q), sep = "")
  cat("\n")
}


print_cross2 <- function(data) {

  # output formatting
  p <- length(data$variable_levels)
  q <- p + 2
  h <- p + 1
  r <- (h * 15) - 3
  f <- length(data$row_name)
  g <- f + 2
  h <- p + 1
  tu <- q * 15

  cat(format(paste(data$variable_names[1], "vs", data$variable_names[2]), width = tu, justify = "centre"), "\n")
  cat("-", rep("---------------", q), sep = "")
  cat("\n")
  cat("|              |", format(data$variable_names[2], width = r, justify = "centre"), "|")
  cat("\n")
  cat("-", rep("---------------", q), sep = "")
  cat("\n|")
  for (i in seq_along(data$column_names)) {
    cat(formatter(data$column_names[i]), "|")
  }
  cat("\n-", rep("---------------", q), sep = "")
  cat("\n")

  for (i in seq_len(f)) {
    cat("|")
    for (j in seq_len(q)) {
      cat(formatter(data$twowaytable[i, j]), "|")
    }
    cat("\n")
    cat("|              |")
    for (j in seq_len(p)) {
      cat(formatter(data$percent_table[i, j]), "|")
    }
    cat("              |")
    cat("\n")
    cat("|              |")
    for (j in seq_len(h)) {
      cat(formatter(data$row_percent[i, j]), "|")
    }
    cat("\n")
    cat("|              |")
    for (j in seq_len(p)) {
      cat(formatter(data$column_percent[i, j]), "|")
    }
    cat("              |")
    cat("\n-", rep("---------------", q), sep = "")
    cat("\n")
  }
  cat("|")
  for (i in seq_along(data$column_totals)) {
    cat(formatter(data$column_totals[i]), "|")
  }
  cat("\n")
  cat("|              |")
  for (i in seq_along(data$percent_column)) {
    cat(formatter(data$percent_column[i]), "|")
  }
  cat("              |")
  cat("\n-", rep("---------------", q), sep = "")
  cat("\n\n\n")
}


print_screen <- function(x) {
  columns <- c("  Column Name  ", "  Data Type  ", "  Levels  ", "  Missing  ", "  Missing (%)  ")
  len_col <- as.vector(sapply(columns, nchar))
  xlev <- lapply(x$levels, paste, collapse = " ") %>%
    lapply(nchar) %>%
    unlist() %>%
    max()
  # If there are several classes, join them into one string:
  x$Types <- lapply(x$Types, paste, collapse = ", ")
  lengths <- list(x$Variables, x$Types, xlev, x$Missing, x$MissingPer)
  n <- length(columns)
  nlist <- list()
  for (i in seq_len(n)) {
    nlist[[i]] <- max(len_col[i], max(sapply(lengths[[i]], nchar)))
  }
  clengths <- unlist(nlist)
  clengths[3] <- max(10, xlev)
  dash <- sum(clengths) + 6
  cat(rep("-", dash), sep = "")
  cat("\n|")
  for (i in seq_len(n)) {
    cat(format(columns[i], width = clengths[i], justify = "centre"), "|", sep = "")
  }
  cat("\n", rep("-", dash), sep = "")
  cat("\n")
  for (i in seq_len(x$Columns)) {
    cat(
      "|", format(x$Variables[i], width = clengths[1], justify = "centre"), "|",
      format(x$Types[i], width = clengths[2], justify = "centre"), "|",
      format(paste(x$levels[[i]], collapse = " "), width = clengths[3], justify = "centre"), "|",
      format(as.character(x$Missing[i]), width = clengths[4], justify = "centre"), "|",
      format(as.character(x$MissingPer[i]), width = clengths[5], justify = "centre"), "|\n", sep = ""
    )
  }
  cat(rep("-", dash), sep = "")
  cat("\n\n")
  cat(
    " Overall Missing Values          ", x$MissingTotal, "\n", "Percentage of Missing Values    ", x$MissingTotPer, "%\n",
    "Rows with Missing Values        ", x$MissingRows, "\n", "Columns With Missing Values     ", x$MissingCols, "\n"
  )
}


print_fcont <- function(data) {

  blen <-
    data %>%
    use_series(utility) %>%
    use_series(breaks) %>%
    nchar() %>%
    max()

  blen2 <-
    blen %>%
    multiply_by(2) %>%
    add(4)

  flen <-
    data %>%
    use_series(utility) %>%
    use_series(frequency) %>%
    nchar() %>%
    max() %>%
    max(9)

  clen <-
    data %>%
    use_series(utility) %>%
    use_series(cumulative) %>%
    nchar() %>%
    max() %>%
    max(13)

  total <- blen2 + flen + clen + 39
  dash <- total - 2

  col_names <- c("Bins", " Frequency", " Cum Frequency", " Percent", " Cum Percent")
  col_widths <- c(blen2, flen, clen, 13, 13)
  len_names <- 5

  cat(format(paste("Variable:", data$utility$varname), width = total, justify = "centre"), "\n")
  cat("|")
  cat(rep("-", dash), sep = "")
  cat("|\n")
  cat("|")
  for (i in seq_len(len_names)) {
    cat(format(col_names[i], width = col_widths[i], justify = "centre"), "|", sep = " ")
  }
  cat("\n|")
  cat(rep("-", dash), sep = "")
  cat("|")
  for (i in seq_len(data$utility$bins)) {
    k <- i + 1
    cat(
      "\n|", formata(data$utility$breaks[i], 1, blen), "-", formata(data$utility$breaks[k], 1, blen), "|",
      formata(data$utility$frequency[i], 2, flen), "|", formata(data$utility$cumulative[i], 2, clen), "|",
      formata(data$utility$percent[i], 2, 12), "|", formata(data$utility$cum_percent[i], 2, 12), "|"
    )
    cat("\n|")
    cat(rep("-", dash), sep = "")
    cat("|")
  }
  cat("\n")
  nlen <- blen * 2 + 4
  if (data$utility$na_count > 0) {
    na_percent <- format((data$utility$na_count / data$utility$n) * 100, nsmall = 2)
    cat("|", format("Missing", width = nlen, justify = "centre"))
    cat("|", format(as.character(round(data$utility$na_count, 2)), width = flen, justify = "centre"))
    cat(" |",  format("-", width = clen, justify = "centre"))
    cat(" |", format(as.character(na_percent), width = 12, justify = "centre"))
    cat(" |", format("-", width = 12, justify = "centre"))
    cat(" |")
    cat("\n|")
    cat(rep("-", dash), sep = "")
    cat("|\n")
  }
  cat("|", format("Total", width = nlen, justify = "centre"))
  cat("|", format(as.character(data$utility$n), width = flen, justify = "centre"))
  cat(" |",  format("-", width = clen, justify = "centre"))
  cat(" |", format("100.00", width = 12, justify = "centre"))
  cat(" |", format("-", width = 12, justify = "centre"))
  cat(" |")
  cat("\n|")
  cat(rep("-", dash), sep = "")
  cat("|")
}


print_ftable <- function(x) {
  nr <- nrow(x$ftable)
  nc <- ncol(x$ftable)
  w1 <- max(nchar("Levels"), nchar(x$ftable$Levels), nchar("Missing"))
  w2 <- max(nchar("Frequency"), nchar(x$ftable$Frequency), nchar(x$utility$na_count))
  w3 <- max(nchar("Cum Frequency"), nchar(x$ftable$Cum.Frequency))
  w <- sum(w1, w2, w3, 26, 16)
  cat(format(paste("Variable:", x$utility$varname), width = w, justify = "centre"), "\n")
  cat(rep("-", w), sep = "")
  cat("\n")
  cat(format("Levels", width = w1, justify = "centre"), fs(),
      format("Frequency", width = w2, justify = "centre"), fs(),
      format("Cum Frequency", width = w3, justify = "centre"), fs(),
      format("Percent", width = 13, justify = "centre"), fs(),
      format("Cum Percent", width = 13, justify = "centre"), "\n")
  cat(rep("-", w), sep = "")
  for (i in seq_len(nr)) {
    cat("\n")
    cat(format(as.character(x$ftable$Levels[i]), width = w1, justify = "centre"), fs(),
      format(as.character(x$ftable$Frequency[i]), width = w2, justify = "centre"), fs(),
      format(as.character(x$ftable$Cum.Frequency[i]), width = w3, justify = "centre"), fs(),
      format(as.character(x$ftable$Percent[i]), width = 13, justify = "centre"), fs(),
      format(as.character(x$ftable$Cum.Percent[i]), width = 13, justify = "centre")
    )
    cat("\n")
    cat(rep("-", w), sep = "")
  }
  cat("\n")
  if (x$utility$na_count > 0) {
    na_percent <- format((x$utility$na_count / x$n) * 100, nsmall = 2)
    cat(format("Missing", width = w1, justify = "centre"), fs(),
        format(as.character(x$utility$na_count), width = w2, justify = "centre"), fs(),
        format("-", width = w3, justify = "centre"), fs(),
        format(as.character(na_percent), width = 13, justify = "centre"), fs(),
        format("-", width = 13, justify = "centre"))
    cat("\n")
    cat(rep("-", w), sep = "")
    cat("\n")
  }
  cat(format("Total", width = w1, justify = "centre"), fs(),
      format(as.character(x$utility$n), width = w2, justify = "centre"), fs(),
      format("-", width = w3, justify = "centre"), fs(),
      format("100.00", width = 13, justify = "centre"), fs(),
      format("-", width = 13, justify = "centre"))
  cat("\n")
  cat(rep("-", w), sep = "")
  cat("\n\n")
}


print_ftable2 <- function(x) {
  nr <- nrow(x$ftable)
  nc <- ncol(x$ftable)
  w1 <- max(nchar("Levels"), nchar(x$ftable$Levels), nchar("Missing"))
  w2 <- max(nchar("Frequency"), nchar(x$ftable$Frequency), nchar(x$na_count))
  w3 <- max(nchar("Cum Frequency"), nchar(x$ftable$`Cum Frequency`))
  w <- sum(w1, w2, w3, 26, 16)
  cat(format(paste("Variable:", x$varname), width = w, justify = "centre"), "\n")
  cat(rep("-", w), sep = "")
  cat("\n")
  cat(format("Levels", width = w1, justify = "centre"), fs(),
      format("Frequency", width = w2, justify = "centre"), fs(),
      format("Cum Frequency", width = w3, justify = "centre"), fs(),
      format("Percent", width = 13, justify = "centre"), fs(),
      format("Cum Percent", width = 13, justify = "centre"), "\n")
  cat(rep("-", w), sep = "")
  for (i in seq_len(nr)) {
    cat("\n")
    cat(format(as.character(x$ftable$Levels[i]), width = w1, justify = "centre"), fs(),
        format(as.character(x$ftable$Frequency[i]), width = w2, justify = "centre"), fs(),
        format(as.character(x$ftable$`Cum Frequency`[i]), width = w3, justify = "centre"), fs(),
        format(as.character(x$ftable$Percent[i]), width = 13, justify = "centre"), fs(),
        format(as.character(x$ftable$`Cum Percent`[i]), width = 13, justify = "centre")
    )
    cat("\n")
    cat(rep("-", w), sep = "")
  }
  cat("\n")
  if (x$na_count > 0) {
    na_percent <- format((x$na_count / x$n) * 100, nsmall = 2)
    cat(format("Missing", width = w1, justify = "centre"), fs(),
        format(as.character(x$na_count), width = w2, justify = "centre"), fs(),
        format("-", width = w3, justify = "centre"), fs(),
        format(as.character(na_percent), width = 13, justify = "centre"), fs(),
        format("-", width = 13, justify = "centre"))
    cat("\n")
    cat(rep("-", w), sep = "")
    cat("\n")
  }
  cat(format("Total", width = w1, justify = "centre"), fs(),
      format(as.character(x$n), width = w2, justify = "centre"), fs(),
      format("-", width = w3, justify = "centre"), fs(),
      format("100.00", width = 13, justify = "centre"), fs(),
      format("-", width = 13, justify = "centre"))
  cat("\n")
  cat(rep("-", w), sep = "")
  cat("\n\n")
}


print_group <- function(data) {
  line <- 23
  n <- 21
  n_names <- max(nchar(data$stats[2, c(-1)]))
  n_uss <- max(nchar(data$stats[12, c(-1)]))
  w <- max(n_names, n_uss) + 2
  cola <- ncol(data$stats)
  col <- cola - 1
  ow <- 23 * cola - col
  row <- nrow(data$stats)

  cat(format(paste(data$yvar, "by", data$xvar), width = ow, justify = "centre"), "\n")
  cat(rep("-", ow), sep = "", "\n")
  cat("|")
  for (i in seq_len(cola)) {
    cat(format(colnames(data$stats)[i], width = n, justify = "right"), "|", sep = "")
  }
  cat("\n")
  cat(rep("-", ow), sep = "", "\n")
  for (i in seq_len(row)) {
    cat("|")
    for (j in seq_len(cola)) {
      cat(format(data$stats[i, j], width = n, justify = "right"), "|", sep = "")
    }
    cat("\n")
  }
  cat(rep("-", ow), sep = "", "\n")
}
