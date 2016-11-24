freq_table2 <- function(data, name) UseMethod("freq_table2")

freq_table2.default <- function(data, name) {
    
  # variable name
  var_name = name

  # exclude missing data
  data <- na.omit(data)

  # levels
  if (is.factor(data)) {
    level_names <- levels(data)
  }

  # copy of data
  data1 <- data

  data <- as.numeric(data)

  # length of input
  data_len <- length(data)  
  
  # unique values in the input
  cq <- unique(sort(data))
  
  # count of unique values in the input
  result <- as.vector(table(data))
  
  # length of result
  len <- length(result)
  
  # cumulative frequency
  cum <- cumsum(result)
  
  # percent
  per <- percent(result, data_len)
  
  # cumulative percent
  cum_per <- percent(cum, data_len)
  
  # matrix
  if (is.factor(data1)) {
    ftable <- cbind(level_names, result, cum, per, cum_per)
  } else {
    ftable <- cbind(cq, result, cum, per, cum_per)
  }
  
  # modify column names of the matrix
  colnames(ftable) <- c("Levels", "Frequency", "Cum Frequency", 
                        "Percent", "Cum Percent")

  result <- list(
    ftable = ftable,
    varname = var_name
  )
  
  class(result) <- "freq_table2"
  return(result)
  
}


print.freq_table2 <- function(x, ...) {
  print_ftable2(x)
}