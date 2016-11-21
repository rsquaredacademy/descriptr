formatter_freq <- function(x) {
    if (missing(x))
        stop("argument x is missing.")
    x <- as.character(x)
    return(format(x, width = 13, justify = "centre"))
}

formatter <- function(x) {
    x <- as.character(x)
    ret <- format(x, width = 13, justify = "right")
    return(ret)
}

percent <- function(x, y) {
    out <- round((x / y) * 100, 2)
    return(out)
}

formata <- function(x, round, width, justify = "centre") {
    x <- round(x, round)
    x <- as.character(x)
    return(format(x, width = width, justify = justify))
}

formatas <- function(x, round, width, justify = "centre") {
    return(format(x, width = width, justify = justify))
}

bin_size <- function(data, bins) {
    return((max(data, na.rm = TRUE) - min(data, na.rm = TRUE)) / bins)
}

intervals <- function(data, bins, na.rm = TRUE) {
    binsize <- bin_size(data, bins)
    bin <- bins - 1
    interval <- min(data)
    for (i in seq_len(bin)) {
        out <- interval[i] + binsize
        interval <- c(interval, out)
    }
    interval <- c(interval, max(data))
    return(interval)
}

freq <- function(data, bins, inta) {
    result <- c()
    for (i in seq_len(bins)) {
        k <- i + 1
        freq <- data >= inta[i] & data <= inta[k]
        out <- length(data[freq])
        result <- c(result, out)
    }
    return(result)
}

divide_by <- function(x) {
    1 / x
}

standardize <- function(x, avg, stdev, p) {
    result <- ((x - avg) / stdev) ^ p
    return(result)
}

sums <- function(x, q) {
    avg <- mean(x)
    stdev <- sd(x)
    result <- sum(sapply(x, standardize, avg, stdev, q))
    return(result)
}

md_helper <- function(x, y) {
    result <- abs(x - y)
    return(result)
}

std_error <- function(x) {
    result <- sd(x) / (length(x) ^ 0.5)
    return(result)
}

ss <- function(x) {
    return(x ^ 2)
}

uss <- function(x, y) {
    return((x - y) ^ 2)
}

stat_uss <- function(x) {

    if(!is.numeric(x)) {
      stop('x must be numeric')
    }
    result <- sum(x ^ 2)
    return(result)
}


formatl <- function(x) {
    x <- format(x, nsmall = 2)
    ret <- format(x, width = 20, justify = "left")
    return(ret)
}

formatol <- function(x, w) {
    x <- as.character(x)
    ret <- format(x, width = w, justify = "centre")
    return(ret)
}

fl <- function(x, w) {
    x <- as.character(x)
    ret <- format(x, width = w, justify = "left")
    return(ret)
}

fc <- function(x, w) {
    x <- as.character(x)
    ret <- format(x, width = w, justify = "centre")
    return(ret)
}


formatr <- function(x, w) {
    x <- rounda(x)
    x <- format(x, nsmall = 2)
    ret <- format(x, width = w, justify = "right")
    return(ret)
}

formatrc <- function(x, w) {
    x <- as.character(x)
    ret <- format(x, width = w, justify = "right")
    return(ret)
}



formatc <- function(x, w) {
    if (is.numeric(x)) {
        x <- round(x, 2)
        y <- as.character(x)
        ret <- format(y, width = w, justify = "centre")
    } else {
        y <- as.character(x)
        ret <- format(y, width = w, justify = "centre")
    }
    return(ret)
}


formatnc <- function(x, w) {
    x <- rounda(x)
    x <- format(x, nsmall = 2)
    ret <- format(x, width = w, justify = "centre")
    return(ret)
}


formats <- function() {
    x <- rep("    ")
    return(x)
}

format_gap <- function(w) {
    x <- rep("", w)
    return(x)
}

return_pos <- function(data, number) {
    out <- c()
    for (i in seq_len(length(data))) {
        if (data[i] == number) {
            out <- c(out, i)
        }
    }
    return(out)
}

row_pct <- function(mat, tot) {
    d <- dim(mat)
    rows <- d[1]
    l <- length(tot)
    result <- c()
    for (i in seq_len(rows)) {
        diva <- mat[i, ] / tot[i]
        result <- rbind(result, diva)
    }
    rownames(result) <- NULL
    return(result)
}

col_pct <- function(mat, tot) {
    d <- dim(mat)
    cols <- d[2]
    l <- length(tot)
    result <- c()
    for (i in seq_len(cols)) {
        diva <- mat[, i] / tot[i]
        result <- cbind(result, diva)
    }
    colnames(result) <- NULL
    return(result)
}

rounda <- function(x) {
    round(x, 2)
}

l <- function(x) {
    x <- as.character(x)
    k <- grep("\\$", x)
    if (length(k) == 1) {
        temp <- strsplit(x, "\\$")
        out <- temp[[1]][2]
    } else {
        out <- x
    }
    return(out)
}

fround <- function(x) {
    format(round(x, 2), nsmall = 2)
}

pol_chi <- function(l1, l2, df, col) {

    x <- c(l1, seq(l1, l2, 0.01), l2)
    y <- c(0, dchisq(seq(l1, l2, 0.01), df), 0)
    polygon(x, y, col = col)

}

pol_f <- function(l1, l2, num_df, den_df, col) {

    x <- c(l1, seq(l1, l2, 0.01), l2)
    y <- c(0, df(seq(l1, l2, 0.01), num_df, den_df), 0)
    polygon(x, y, col = col)

}


pol_cord <- function(l1, l2, mean, sd, col) {

    x <- c(l1, seq(l1, l2, 0.01), l2)
    y <- c(0, dnorm(seq(l1, l2, 0.01), mean, sd), 0)
    polygon(x, y, col = col)

}


xax <- function(mean) {

    xl <- mean - 3
    xu <- mean + 3
    x <- seq(xl, xu, 0.01)
    return(x)
}


seql <- function(mean, sd) {

    lmin <- mean - 3 * sd
    lmax <- mean + 3 * sd
    l    <- seq(lmin, lmax, sd)
    return(l)

}


xmm <- function(mean, sd) {
    xmin <- mean - 4 * sd
    xmax <- mean + 4 * sd
    out  <- c(xmin, xmax)
    return(out)
}


pol_t <- function(l1, l2, df, col) {

    x <- c(l1, seq(l1, l2, 0.01), l2)
    y <- c(0, dt(seq(l1, l2, 0.01), df), 0)
    polygon(x, y, col = col)

}


# # paired t test
# extract <- function(x, y) {
#   z <- x - y
#   dat <- as.data.frame(cbind(x, y, z))
#   return(dat)
# }

# samp_err <- function(sigma, n) {
#   result <- sigma / (n ^ 0.5)
#   return(result)
# }

# # case 2: sigma unknown
# conf_int_t <- function(u, s, n, alpha = 0.05) {
#   a <- alpha / 2
#   df <- n - 1
#   error <- round(qt(a, df), 3) * -1
#   lower <- u - (error * samp_err(s, n))
#   upper <- u + (error * samp_err(s, n))
#   result <- c(lower, upper)
#   return(result)
# }

# stat <- function(x) {
#   n <- length(x)
#   Mean <- mean(x)
#   stdev <- sd(x)
#   serror <- samp_err(stdev, n)
#   out <- c(Mean, stdev, serror)
#   # out <- list(mean = Mean, sd = stdev)
#   return(out)
# }

# cor_sig <- function(corr, n) {
#   t <- corr / ((1 - (corr ^ 2)) / (n - 2)) ^ 0.5
#   df <- n - 2
#   sig <- (1 - pt(t, df)) * 2
#   return(round(sig, 4))
# }

# formatter_pair <- function(x, w) {
#   x1 <- format(x, nsmall = 4)
#   x2 <- as.character(x1)
#   ret <- format(x2, width = w, justify = "centre")
#   return(ret)
# }


# format_cor <- function(x) {
#   x1 <- format(x, nsmall = 4)
#   x2 <- as.character(x1)
#   ret <- format(x2, width = 13, justify = "centre")
#   return(ret)
# }

# format_cil <- function(x, w) {
#   x <- as.character(x)
#   ret <- format(x, width = w, justify = "centre")
#   return(ret)
# }

# format_ciu <- function(x, w) {
#   x <- as.character(x)
#   ret <- format(x, width = w, justify = "centre")
#   return(ret)
# }

# r <- function(x) {
#   return(round(x, 4))
# }

# fs <- function() {
#     x <- rep("  ")
#     return(x)
# }

# fg <- function(x, w) {
#     z <- as.character(x)
#     y <- format(z, width = w, justify = 'centre')
#     return(y)
# }

# # basic computations
# anova_split <- function(data, x, y) {
#     by_factor <- data %>%
#         group_by_(y) %>%
#         select_(x) %>%
#         summarise_each(funs(length, mean, var, sd)) %>%
#         as.data.frame()
#     return(by_factor)
# }

# overall_stats <- function(data, x) {
#     by_stats <- data %>%
#         select_(x) %>%
#         summarise_each(funs(length, mean, sd)) %>%
#         as.data.frame()
#     return(by_stats)
# }

# anova_avg <- function(data, y) {
#     avg <- data %>%
#         select_(y) %>%
#         summarise_each(funs(mean)) %>%
#         as.data.frame()
#     return(avg)
# }

# conf_int_t <- function(u, s, n, alpha = 0.05) {
#   a <- alpha / 2
#   df <- n - 1
#   error <- round(qt(a, df), 3) * -1
#   lower <- u - (error * samp_err(s, n))
#   upper <- u + (error * samp_err(s, n))
#   result <- c(lower, upper)
#   return(result)
# }

# conf_int_p <- function(u, se, alpha = 0.05) {
#   a <- alpha / 2
#   error <- round(qnorm(a), 3) * -1
#   lower <- u - (error * se)
#   upper <- u + (error * se)
#   result <- c(lower, upper)
#   return(result)
# }

# std_err <- function(x) {
#   se <- sd(x) / sqrt(length(x))
#   return(round(se, 3))
# }

# mean_t <- function(x) {
#     return(round(mean(x), 3))
# }

# sd_t <- function(x) {
#     s <- sd(x)
#     return(round(s, 3))
# }

# data_split <- function(data, x, y) {
#   by_gender <- data %>%
#     group_by_(x) %>%
#     select_(y) %>%
#     summarise_each(funs(length, mean_t, sd_t, std_err)) %>%
#     as.data.frame()
#   return(by_gender)
# }

# da <- function(data, y) {
#   dat <- data %>%
#     select_(y) %>%
#     summarise_each(funs(length, mean_t, sd_t, std_err)) %>%
#     as.data.frame()
#   return(dat)
# }

# sd_diff <- function(n1, n2, s1, s2) {
#   n1 <- n1 - 1
#   n2 <- n2 - 1
#   n <- (n1 + n2) - 2
#   return(((n1 * s1 + n2 * s2) / n) ^ 0.5)
# }

# se_diff <- function(n1, n2, s1, s2) {
#     df <- n1 + n2 - 2
#     n_1 <- n1 - 1
#     n_2 <- n2 - 1
#     v <- (n_1 * s1 + n_2 * s2) / df
#     return(sqrt(v * (1 / n1 + 1 / n2)))
# }

# se_sw <- function(s1, s2, n1, n2) {
#   return(((s1 / n1) + (s2 / n2)) ^ 0.5)
# }


# df <- function(n1, n2, s1, s2) {
#   sn1 <- s1 / n1
#   sn2 <- s2 / n2
#   m1 <- 1 / (n1 - 1)
#   m2 <- 1 / (n2 - 1)
#   num <- (sn1 + sn2) ^ 2
#   den <- (m1 * (sn1 ^ 2)) + (m2 * (sn2 ^ 2))
#   return(round(num / den))
# }


# conf_int_pooled <- function(mean_diff, se_diff, alpha = 0.05) {
#   a <- alpha / 2
#   t <- qt(0.025, 198)
# }

# # format functions
# fw <- function(x, w) {
#     x <- as.character(x)
#     z <- format(x, width = w, justify = 'centre')
#     return(z)
# }

# fn <- function(x, w) {
#     y <- as.character(x)
#     z <- format(y, width = w, justify = 'centre')
#     return(z)
# }


# formats_t <- function() {
#     x <- '   '
#     return(x)
# }

# lns <- function(w) {
#     cat(rep('-', w), sep = "")
# }
