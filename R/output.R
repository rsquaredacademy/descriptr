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
