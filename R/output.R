print_cross <- function(data) {

    p <- length(data$var2_levels)
    q <- p + 2
    h <- p + 1
    r <- (h * 15) - 3
    f <- length(data$var1_levels)
    g <- f + 2
    h <- p + 1

    col_names <- c(data$varnames[1], data$var2_levels, "Row Total")
    col_totals <- c("Column Total", data$column_totals, data$obs)

    cat(formatter("    Cell Contents\n"), "|---------------|\n", "|", formatter("Frequency"),
        "|\n", "|", formatter("Percent"), "|\n", "|", formatter("Row Pct"), "|\n",
        "|", formatter("Col Pct"), "|\n", "|---------------|\n\n", "Total Observations: ",
        data$obs, "\n\n")
    cat("-", rep("---------------", q), sep = "")
    cat("\n")
    cat("|              |", format(data$varnames[2], width = r, justify = "centre"),
        "|")
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
    for (i in seq_along(col_totals)) {
        cat(formatter(col_totals[i]), "|")
    }
    cat("\n")
    cat("|              |")
    for (i in seq_along(data$percent_column)) {
        cat(formatter(data$percent_column[i]), "|")
    }
    cat("              |")
    cat("\n-", rep("---------------", q), sep = "")
    cat("\n")

}


print_screen <- function(x) {

    columns <- c('  Column Name  ', '  Data Type  ', '  Levels  ', '  Missing  ', '  Missing (%)  ')
    len_col <- as.vector(sapply(columns, nchar))
    xlev <- lapply(k$levels, paste, collapse = " ") %>%
        lapply(nchar) %>%
        unlist %>%
        max
    lengths <- list(x$Variables, x$Types, xlev, x$Missing, x$MissingPer)
    n <- length(columns)
    nlist <- list()
    for (i in seq_len(n)) {
        nlist[[i]] <- max(len_col[i], max(sapply(lengths[[i]], nchar)))
    }
    clengths <- unlist(nlist)
    clengths[3] <- max(10, xlev)
    dash <- sum(clengths) + 6
    cat(rep("-",dash), sep = "")
    cat("\n|")
    for(i in seq_len(n)) {
        cat(columns[i], "|", sep = "")
    }
    cat("\n", rep("-",dash), sep = "")
    cat("\n")
    for (i in seq_len(x$Columns)) {
        cat("|", format(x$Variables[i], width = clengths[1], justify = 'centre'), "|",
            format(x$Types[i], width = clengths[2], justify = 'centre'), "|",
            format(paste(x$levels[[i]], collapse = " "), width = clengths[3], justify = 'centre'), "|",
            format(as.character(x$Missing[i]), width = clengths[4], justify = 'centre'), "|",
            format(as.character(x$MissingPer[i]), width = clengths[5], justify = 'centre'), "|\n", sep = ""
        )
    }
    cat(rep("-",dash), sep = "")
    cat("\n\n")
    cat(' Overall Missing Values          ', x$MissingTotal, "\n", 'Percentage of Missing Values    ', x$MissingTotPer, "%\n",
        'Rows with Missing Values        ', x$MissingRows, "\n", "Columns With Missing Values     ", x$MissingCols, "\n")

}


print_fcont <- function(data) {

    cat(format(paste('Variable:', data$varname), width = 77, justify = 'centre'), '\n')
    cat("|---------------------------------------------------------------------------|
|                                 Cumulative                    Cumulative  |
|     Bins      |  Frequency   |   Frequency  |   Percent    |    Percent   |
|---------------------------------------------------------------------------|")
    for (i in seq_len(data$bins)) {
        k <- i + 1
        cat("\n|", formata(data$breaks[i], 1, 5), "-", formata(data$breaks[k], 1, 5), "|",
            formata(data$frequency[i], 2, 12), "|", formata(data$cumulative[i], 2, 12), "|",
            formatas(data$percent[i], 2, 12), "|", formatas(data$cum_percent[i], 2, 12), "|")
        cat("\n|---------------------------------------------------------------------------|")
    }

}


print_ftable <- function(data) {

  nr <- nrow(data$ftable)
  nc <- ncol(data$ftable)
  cat(format(paste('Variable:', data$varname), width = 76, justify = 'centre'), '\n')
  cat("|--------------------------------------------------------------------------|
|                                Cumulative                    Cumulative  |
|    Levels    |  Frequency   |   Frequency  |   Percent    |    Percent   |
|--------------------------------------------------------------------------|\n")
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      cat("|", formatter_freq(data$ftable[i, j]))
    }
    cat("|")
    cat("\n|--------------------------------------------------------------------------|\n")
  }
  cat('\n\n')

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

    cat(format(paste(data$yvar, 'by', data$xvar), width = ow, justify = 'centre'), '\n')
    cat(rep('-', ow), sep = '', '\n')
    cat('|')
    for (i in seq_len(cola)) {
        cat(format(data$stats[1, i], width = n, justify = 'right'), '|', sep = '')
    }
    cat('\n')
    cat(rep('-', ow), sep = '', '\n')
    for (i in 2:row) {
        cat('|')
        for (j in seq_len(cola)) {
            cat(format(data$stats[i, j], width = n, justify = 'right'), '|', sep = '')
        }
        cat('\n')
    }
    cat(rep('-', ow), sep = '', '\n')

}
