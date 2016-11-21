summary_stats <- function(data) UseMethod('summary_stats')

summary_stats.default <- function(data) {

    if(!is.numeric(data)) {
      stop('data must be numeric')
    }

    data <- na.omit(data)
    low <- tailobs(data, 5, 'low')
    high <- tailobs(data, 5, 'high')
    low_val <- rindex(data, low)
    high_val <- rindex(data, high)

    result <- list(
        Observations = length(data),
        Missing_Obs = sum(is.na(data)),
        Mean = mean(data),
        Trimmed_mean = mean(data, trim = 0.05),
        Standard_Deviation = sd(data),
        Variance = var(data),
        Skewness = skewness(data),
        Kurtosis = kurtosis(data),
        Uncorrect_SS = stat_uss(data),
        Correct_SS = stat_css(data),
        Coefficient_Of_Variation = stat_cvar(data),
        Std_Error_Mean = std_error(data),
        Median = median(data),
        Mode = stat_mode(data),
        Range = stat_range(data),
        Min = min(data), Max = max(data),
        Interquartile_Range = IQR(data),
        Percentile_99 = quantile(data, 0.99),
        Percentile_95 = quantile(data, 0.95),
        Percentile_90 = quantile(data, 0.90),
        Percentile_75 = quantile(data, 0.75),
        Percentile_25 = quantile(data, 0.25),
        Percentile_10 = quantile(data, 0.10),
        Percentile_05 = quantile(data, 0.05),
        Percentile_01 = quantile(data, 0.01),
        Lowest_Obs_1 = low_val[1],
        Lowest_Obs_2 = low_val[2],
        Lowest_Obs_3 = low_val[3],
        Lowest_Obs_4 = low_val[4],
        Lowest_Obs_5 = low_val[5],
        Highest_Obs_1 = high_val[1],
        Highest_Obs_2 = high_val[2],
        Highest_Obs_3 = high_val[3],
        Highest_Obs_4 = high_val[4],
        Highest_Obs_5 = high_val[5],
        Lowest_Obs = low,
        Highest_Obs = high,
        Lowest_Obs_Index = low_val,
        Highest_Obs_Index = high_val
    )

    class(result) <- 'summary_stats'
    return(result)
}


print.summary_stats <- function(data) {

  print_stats(data)

}
