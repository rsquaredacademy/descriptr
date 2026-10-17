# Descriptive statistics

Range of descriptive statistics for continuous data.

## Usage

``` r
ds_summary_stats(data, ...)
```

## Arguments

- data:

  An object of type `numeric` or `data.frame`.

- ...:

  Column(s) in `data`.

## See also

[`summary`](https://rdrr.io/r/base/summary.html)
[`ds_freq_table`](https://descriptr.rsquaredacademy.com/reference/ds_freq_table.md)
[`ds_cross_table`](https://descriptr.rsquaredacademy.com/reference/ds_cross_table.md)

## Examples

``` r
# numeric data
ds_summary_stats(mtcarz$mpg)
#>                         Univariate Analysis                          
#> 
#>  N                       32.00      Variance                36.32 
#>  Missing                  0.00      Std Deviation            6.03 
#>  Mean                    20.09      Range                   23.50 
#>  Median                  19.20      Interquartile Range      7.38 
#>  Mode                    10.40      Uncorrected SS       14042.31 
#>  Trimmed Mean            19.95      Corrected SS          1126.05 
#>  Skewness                 0.67      Coeff Variation         30.00 
#>  Kurtosis                -0.02      Std Error Mean           1.07 
#> 
#>                               Quantiles                               
#> 
#>               Quantile                            Value                
#> 
#>              Max                                  33.90                
#>              99%                                  33.44                
#>              95%                                  31.30                
#>              90%                                  30.09                
#>              Q3                                   22.80                
#>              Median                               19.20                
#>              Q1                                   15.43                
#>              10%                                  14.34                
#>              5%                                   12.00                
#>              1%                                   10.40                
#>              Min                                  10.40                
#> 
#>                             Extreme Values                            
#> 
#>                 Low                                High                
#> 
#>   Obs                        Value       Obs                        Value 
#>   15                         10.4        20                         33.9  
#>   16                         10.4        18                         32.4  
#>   24                         13.3        19                         30.4  
#>    7                         14.3        28                         30.4  
#>   17                         14.7        26                         27.3  

# single variable
ds_summary_stats(mtcarz, mpg)
#> -------------------------------- Variable: mpg --------------------------------
#> 
#>                         Univariate Analysis                          
#> 
#>  N                       32.00      Variance                36.32 
#>  Missing                  0.00      Std Deviation            6.03 
#>  Mean                    20.09      Range                   23.50 
#>  Median                  19.20      Interquartile Range      7.38 
#>  Mode                    10.40      Uncorrected SS       14042.31 
#>  Trimmed Mean            19.95      Corrected SS          1126.05 
#>  Skewness                 0.67      Coeff Variation         30.00 
#>  Kurtosis                -0.02      Std Error Mean           1.07 
#> 
#>                               Quantiles                               
#> 
#>               Quantile                            Value                
#> 
#>              Max                                  33.90                
#>              99%                                  33.44                
#>              95%                                  31.30                
#>              90%                                  30.09                
#>              Q3                                   22.80                
#>              Median                               19.20                
#>              Q1                                   15.43                
#>              10%                                  14.34                
#>              5%                                   12.00                
#>              1%                                   10.40                
#>              Min                                  10.40                
#> 
#>                             Extreme Values                            
#> 
#>                 Low                                High                
#> 
#>   Obs                        Value       Obs                        Value 
#>   15                         10.4        20                         33.9  
#>   16                         10.4        18                         32.4  
#>   24                         13.3        19                         30.4  
#>    7                         14.3        28                         30.4  
#>   17                         14.7        26                         27.3  
#> 
#> 
#> 

# multiple variables
ds_summary_stats(mtcarz, mpg, disp, hp)
#> -------------------------------- Variable: mpg --------------------------------
#> 
#>                         Univariate Analysis                          
#> 
#>  N                       32.00      Variance                36.32 
#>  Missing                  0.00      Std Deviation            6.03 
#>  Mean                    20.09      Range                   23.50 
#>  Median                  19.20      Interquartile Range      7.38 
#>  Mode                    10.40      Uncorrected SS       14042.31 
#>  Trimmed Mean            19.95      Corrected SS          1126.05 
#>  Skewness                 0.67      Coeff Variation         30.00 
#>  Kurtosis                -0.02      Std Error Mean           1.07 
#> 
#>                               Quantiles                               
#> 
#>               Quantile                            Value                
#> 
#>              Max                                  33.90                
#>              99%                                  33.44                
#>              95%                                  31.30                
#>              90%                                  30.09                
#>              Q3                                   22.80                
#>              Median                               19.20                
#>              Q1                                   15.43                
#>              10%                                  14.34                
#>              5%                                   12.00                
#>              1%                                   10.40                
#>              Min                                  10.40                
#> 
#>                             Extreme Values                            
#> 
#>                 Low                                High                
#> 
#>   Obs                        Value       Obs                        Value 
#>   15                         10.4        20                         33.9  
#>   16                         10.4        18                         32.4  
#>   24                         13.3        19                         30.4  
#>    7                         14.3        28                         30.4  
#>   17                         14.7        26                         27.3  
#> 
#> 
#> 
#> -------------------------------- Variable: disp --------------------------------
#> 
#>                           Univariate Analysis                            
#> 
#>  N                         32.00      Variance               15360.80 
#>  Missing                    0.00      Std Deviation            123.94 
#>  Mean                     230.72      Range                    400.90 
#>  Median                   196.30      Interquartile Range      205.18 
#>  Mode                     275.80      Uncorrected SS       2179627.47 
#>  Trimmed Mean             228.00      Corrected SS          476184.79 
#>  Skewness                   0.42      Coeff Variation           53.72 
#>  Kurtosis                  -1.07      Std Error Mean            21.91 
#> 
#>                                 Quantiles                                 
#> 
#>                Quantile                              Value                 
#> 
#>               Max                                    472.00                
#>               99%                                    468.28                
#>               95%                                    449.00                
#>               90%                                    396.00                
#>               Q3                                     326.00                
#>               Median                                 196.30                
#>               Q1                                     120.83                
#>               10%                                    80.61                 
#>               5%                                     77.35                 
#>               1%                                     72.53                 
#>               Min                                    71.10                 
#> 
#>                               Extreme Values                              
#> 
#>                  Low                                  High                 
#> 
#>   Obs                          Value       Obs                          Value 
#>   20                           71.1        15                            472  
#>   19                           75.7        16                            460  
#>   18                           78.7        17                            440  
#>   26                            79         25                            400  
#>   28                           95.1         5                            360  
#> 
#> 
#> 
#> --------------------------------- Variable: hp ---------------------------------
#> 
#>                          Univariate Analysis                           
#> 
#>  N                        32.00      Variance               4700.87 
#>  Missing                   0.00      Std Deviation            68.56 
#>  Mean                    146.69      Range                   283.00 
#>  Median                  123.00      Interquartile Range      83.50 
#>  Mode                    110.00      Uncorrected SS       834278.00 
#>  Trimmed Mean            143.57      Corrected SS         145726.88 
#>  Skewness                  0.80      Coeff Variation          46.74 
#>  Kurtosis                  0.28      Std Error Mean           12.12 
#> 
#>                                Quantiles                                
#> 
#>               Quantile                              Value                
#> 
#>              Max                                   335.00                
#>              99%                                   312.99                
#>              95%                                   253.55                
#>              90%                                   243.50                
#>              Q3                                    180.00                
#>              Median                                123.00                
#>              Q1                                     96.50                
#>              10%                                    66.00                
#>              5%                                     63.65                
#>              1%                                     55.10                
#>              Min                                    52.00                
#> 
#>                              Extreme Values                             
#> 
#>                  Low                                High                 
#> 
#>   Obs                         Value       Obs                         Value 
#>   19                           52         31                           335  
#>    8                           62         29                           264  
#>   20                           65          7                           245  
#>   18                           66         24                           245  
#>   26                           66         17                           230  
#> 
#> 
#> 

# all variables
ds_summary_stats(mtcarz)
#> -------------------------------- Variable: mpg --------------------------------
#> 
#>                         Univariate Analysis                          
#> 
#>  N                       32.00      Variance                36.32 
#>  Missing                  0.00      Std Deviation            6.03 
#>  Mean                    20.09      Range                   23.50 
#>  Median                  19.20      Interquartile Range      7.38 
#>  Mode                    10.40      Uncorrected SS       14042.31 
#>  Trimmed Mean            19.95      Corrected SS          1126.05 
#>  Skewness                 0.67      Coeff Variation         30.00 
#>  Kurtosis                -0.02      Std Error Mean           1.07 
#> 
#>                               Quantiles                               
#> 
#>               Quantile                            Value                
#> 
#>              Max                                  33.90                
#>              99%                                  33.44                
#>              95%                                  31.30                
#>              90%                                  30.09                
#>              Q3                                   22.80                
#>              Median                               19.20                
#>              Q1                                   15.43                
#>              10%                                  14.34                
#>              5%                                   12.00                
#>              1%                                   10.40                
#>              Min                                  10.40                
#> 
#>                             Extreme Values                            
#> 
#>                 Low                                High                
#> 
#>   Obs                        Value       Obs                        Value 
#>   15                         10.4        20                         33.9  
#>   16                         10.4        18                         32.4  
#>   24                         13.3        19                         30.4  
#>    7                         14.3        28                         30.4  
#>   17                         14.7        26                         27.3  
#> 
#> 
#> 
#> -------------------------------- Variable: disp --------------------------------
#> 
#>                           Univariate Analysis                            
#> 
#>  N                         32.00      Variance               15360.80 
#>  Missing                    0.00      Std Deviation            123.94 
#>  Mean                     230.72      Range                    400.90 
#>  Median                   196.30      Interquartile Range      205.18 
#>  Mode                     275.80      Uncorrected SS       2179627.47 
#>  Trimmed Mean             228.00      Corrected SS          476184.79 
#>  Skewness                   0.42      Coeff Variation           53.72 
#>  Kurtosis                  -1.07      Std Error Mean            21.91 
#> 
#>                                 Quantiles                                 
#> 
#>                Quantile                              Value                 
#> 
#>               Max                                    472.00                
#>               99%                                    468.28                
#>               95%                                    449.00                
#>               90%                                    396.00                
#>               Q3                                     326.00                
#>               Median                                 196.30                
#>               Q1                                     120.83                
#>               10%                                    80.61                 
#>               5%                                     77.35                 
#>               1%                                     72.53                 
#>               Min                                    71.10                 
#> 
#>                               Extreme Values                              
#> 
#>                  Low                                  High                 
#> 
#>   Obs                          Value       Obs                          Value 
#>   20                           71.1        15                            472  
#>   19                           75.7        16                            460  
#>   18                           78.7        17                            440  
#>   26                            79         25                            400  
#>   28                           95.1         5                            360  
#> 
#> 
#> 
#> --------------------------------- Variable: hp ---------------------------------
#> 
#>                          Univariate Analysis                           
#> 
#>  N                        32.00      Variance               4700.87 
#>  Missing                   0.00      Std Deviation            68.56 
#>  Mean                    146.69      Range                   283.00 
#>  Median                  123.00      Interquartile Range      83.50 
#>  Mode                    110.00      Uncorrected SS       834278.00 
#>  Trimmed Mean            143.57      Corrected SS         145726.88 
#>  Skewness                  0.80      Coeff Variation          46.74 
#>  Kurtosis                  0.28      Std Error Mean           12.12 
#> 
#>                                Quantiles                                
#> 
#>               Quantile                              Value                
#> 
#>              Max                                   335.00                
#>              99%                                   312.99                
#>              95%                                   253.55                
#>              90%                                   243.50                
#>              Q3                                    180.00                
#>              Median                                123.00                
#>              Q1                                     96.50                
#>              10%                                    66.00                
#>              5%                                     63.65                
#>              1%                                     55.10                
#>              Min                                    52.00                
#> 
#>                              Extreme Values                             
#> 
#>                  Low                                High                 
#> 
#>   Obs                         Value       Obs                         Value 
#>   19                           52         31                           335  
#>    8                           62         29                           264  
#>   20                           65          7                           245  
#>   18                           66         24                           245  
#>   26                           66         17                           230  
#> 
#> 
#> 
#> -------------------------------- Variable: drat --------------------------------
#> 
#>                         Univariate Analysis                          
#> 
#>  N                       32.00      Variance                 0.29 
#>  Missing                  0.00      Std Deviation            0.53 
#>  Mean                     3.60      Range                    2.17 
#>  Median                   3.70      Interquartile Range      0.84 
#>  Mode                     3.07      Uncorrected SS         422.79 
#>  Trimmed Mean             3.58      Corrected SS             8.86 
#>  Skewness                 0.29      Coeff Variation         14.87 
#>  Kurtosis                -0.45      Std Error Mean           0.09 
#> 
#>                               Quantiles                               
#> 
#>               Quantile                            Value                
#> 
#>              Max                                   4.93                
#>              99%                                   4.78                
#>              95%                                   4.31                
#>              90%                                   4.21                
#>              Q3                                    3.92                
#>              Median                                3.70                
#>              Q1                                    3.08                
#>              10%                                   3.01                
#>              5%                                    2.85                
#>              1%                                    2.76                
#>              Min                                   2.76                
#> 
#>                             Extreme Values                            
#> 
#>                 Low                                High                
#> 
#>   Obs                        Value       Obs                        Value 
#>    6                         2.76        19                         4.93  
#>   22                         2.76        27                         4.43  
#>   15                         2.93        20                         4.22  
#>   16                           3         29                         4.22  
#>   12                         3.07        32                         4.11  
#> 
#> 
#> 
#> --------------------------------- Variable: wt ---------------------------------
#> 
#>                         Univariate Analysis                          
#> 
#>  N                       32.00      Variance                 0.96 
#>  Missing                  0.00      Std Deviation            0.98 
#>  Mean                     3.22      Range                    3.91 
#>  Median                   3.33      Interquartile Range      1.03 
#>  Mode                     3.44      Uncorrected SS         360.90 
#>  Trimmed Mean             3.20      Corrected SS            29.68 
#>  Skewness                 0.47      Coeff Variation         30.41 
#>  Kurtosis                 0.42      Std Error Mean           0.17 
#> 
#>                               Quantiles                               
#> 
#>               Quantile                            Value                
#> 
#>              Max                                   5.42                
#>              99%                                   5.40                
#>              95%                                   5.29                
#>              90%                                   4.05                
#>              Q3                                    3.61                
#>              Median                                3.33                
#>              Q1                                    2.58                
#>              10%                                   1.96                
#>              5%                                    1.74                
#>              1%                                    1.54                
#>              Min                                   1.51                
#> 
#>                             Extreme Values                            
#> 
#>                 Low                                High                
#> 
#>   Obs                        Value       Obs                        Value 
#>   27                         1.51        15                         5.42  
#>   NA                         1.61        12                         5.34  
#>   NA                         1.83        NA                         5.25  
#>   NA                         1.94        NA                         4.07  
#>   NA                         2.14        NA                         3.85  
#> 
#> 
#> 
#> -------------------------------- Variable: qsec --------------------------------
#> 
#>                         Univariate Analysis                          
#> 
#>  N                       32.00      Variance                 3.19 
#>  Missing                  0.00      Std Deviation            1.79 
#>  Mean                    17.85      Range                    8.40 
#>  Median                  17.71      Interquartile Range      2.01 
#>  Mode                    17.02      Uncorrected SS       10293.48 
#>  Trimmed Mean            17.79      Corrected SS            98.99 
#>  Skewness                 0.41      Coeff Variation         10.01 
#>  Kurtosis                 0.86      Std Error Mean           0.32 
#> 
#>                               Quantiles                               
#> 
#>               Quantile                            Value                
#> 
#>              Max                                  22.90                
#>              99%                                  22.07                
#>              95%                                  20.10                
#>              90%                                  19.99                
#>              Q3                                   18.90                
#>              Median                               17.71                
#>              Q1                                   16.89                
#>              10%                                  15.53                
#>              5%                                   15.05                
#>              1%                                   14.53                
#>              Min                                  14.50                
#> 
#>                             Extreme Values                            
#> 
#>                 Low                                High                
#> 
#>   Obs                        Value       Obs                        Value 
#>   29                         14.5         9                         22.9  
#>   31                         14.6         6                         20.22 
#>   24                         15.41       21                         20.01 
#>   30                         15.5         8                          20   
#>    7                         15.84       20                         19.9  
#> 
#> 
#> 
```
