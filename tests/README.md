Tests and Coverage
================
05 October, 2018 14:26:49

-   [Coverage](#coverage)
-   [Unit Tests](#unit-tests)

This output is created by [covrpage](https://github.com/yonicd/covrpage).

Coverage
--------

Coverage summary is created using the [covr](https://github.com/r-lib/covr) package.

| Object                                                | Coverage (%) |
|:------------------------------------------------------|:------------:|
| descriptr                                             |     72.72    |
| [R/ds-launch-shiny-app.R](../R/ds-launch-shiny-app.R) |     0.00     |
| [R/ds-summary-stats.R](../R/ds-summary-stats.R)       |     0.00     |
| [R/ds-cross-table.R](../R/ds-cross-table.R)           |     37.31    |
| [R/ds-describe.R](../R/ds-describe.R)                 |     56.36    |
| [R/ds-freq-table.R](../R/ds-freq-table.R)             |     64.10    |
| [R/ds-freq-cont.R](../R/ds-freq-cont.R)               |     66.67    |
| [R/dist-binomial.R](../R/dist-binomial.R)             |     70.69    |
| [R/ds-group-summary.R](../R/ds-group-summary.R)       |     71.08    |
| [R/ds-screener.R](../R/ds-screener.R)                 |     71.23    |
| [R/dist-f.R](../R/dist-f.R)                           |     73.33    |
| [R/dist-chisquare.R](../R/dist-chisquare.R)           |     73.68    |
| [R/dist-normal.R](../R/dist-normal.R)                 |     77.51    |
| [R/ds-output.R](../R/ds-output.R)                     |     78.28    |
| [R/ds-mult-table.R](../R/ds-mult-table.R)             |     79.49    |
| [R/ds-multistats.R](../R/ds-multistats.R)             |     84.62    |
| [R/ds-freq-mult.R](../R/ds-freq-mult.R)               |     86.79    |
| [R/dist-t.R](../R/dist-t.R)                           |     87.55    |
| [R/ds-utils.R](../R/ds-utils.R)                       |     96.84    |
| [R/ds-cross-mult.R](../R/ds-cross-mult.R)             |    100.00    |

<br>

Unit Tests
----------

Unit Test summary is created using the [testthat](https://github.com/r-lib/testthat) package.

|                      | file                                                  |    n|  time|  error|  failed|  skipped|  warning|
|----------------------|:------------------------------------------------------|----:|-----:|------:|-------:|--------:|--------:|
| test-binom-dist.R    | [test-binom-dist.R](testthat/test-binom-dist.R)       |   49|  3.29|      0|       0|        0|        0|
| test-chi-dist.R      | [test-chi-dist.R](testthat/test-chi-dist.R)           |   20|  1.37|      0|       0|        0|        0|
| test-cross-table.R   | [test-cross-table.R](testthat/test-cross-table.R)     |   22|  0.62|      0|       0|        1|        0|
| test-describe.R      | [test-describe.R](testthat/test-describe.R)           |   78|  1.48|      0|       0|        0|        0|
| test-f-dist.R        | [test-f-dist.R](testthat/test-f-dist.R)               |   18|  0.55|      0|       0|        0|        0|
| test-freq-cont.R     | [test-freq-cont.R](testthat/test-freq-cont.R)         |   11|  0.11|      0|       0|        1|        0|
| test-freq-table.R    | [test-freq-table.R](testthat/test-freq-table.R)       |    8|  0.09|      0|       0|        1|        0|
| test-group-summary.R | [test-group-summary.R](testthat/test-group-summary.R) |    9|  0.58|      0|       0|        1|        0|
| test-multistats.R    | [test-multistats.R](testthat/test-multistats.R)       |    1|  0.28|      0|       0|        0|        0|
| test-normal-dist.R   | [test-normal-dist.R](testthat/test-normal-dist.R)     |   30|  1.92|      0|       0|        0|        0|
| test-output.R        | [test-output.R](testthat/test-output.R)               |    7|  1.85|      0|       0|        0|        0|
| test-screen.R        | [test-screen.R](testthat/test-screen.R)               |   14|  0.14|      0|       0|        0|        0|
| test-t-dist.R        | [test-t-dist.R](testthat/test-t-dist.R)               |   24|  1.88|      0|       0|        0|        0|
| test-utils.R         | [test-utils.R](testthat/test-utils.R)                 |   34|  0.35|      0|       0|        0|        0|

<details open> <summary> Show Detailed Test Results </summary>

| file                                                          | context            | test                                                       | status  |    n|  time|
|:--------------------------------------------------------------|:-------------------|:-----------------------------------------------------------|:--------|----:|-----:|
| [test-binom-dist.R](testthat/test-binom-dist.R#L5)            | binom\_dist        | output from dist\_binom\_plot matches expected result      | PASS    |    8|  0.95|
| [test-binom-dist.R](testthat/test-binom-dist.R#L23)           | binom\_dist        | dist\_binom\_plot throws the appropriate errors            | PASS    |    6|  0.05|
| [test-binom-dist.R](testthat/test-binom-dist.R#L34)           | binom\_dist        | output from dist\_binom\_prob matches expected result      | PASS    |   18|  2.14|
| [test-binom-dist.R](testthat/test-binom-dist.R#L84)           | binom\_dist        | dist\_binom\_prob throws the appropriate errors            | PASS    |    7|  0.06|
| [test-binom-dist.R](testthat/test-binom-dist.R#L94)           | binom\_dist        | dist\_dist\_binom\_perc throws the appropriate errors      | PASS    |   10|  0.09|
| [test-chi-dist.R](testthat/test-chi-dist.R#L4)                | chi-dist           | dist\_chi\_plot returns appropriate error messages         | PASS    |    4|  0.03|
| [test-chi-dist.R](testthat/test-chi-dist.R#L18)               | chi-dist           | output from dist\_chi\_perc matches expected results       | PASS    |    4|  0.64|
| [test-chi-dist.R](testthat/test-chi-dist.R#L33)               | chi-dist           | dist\_chi\_perc returns appropriate error messages         | PASS    |    4|  0.03|
| [test-chi-dist.R](testthat/test-chi-dist.R#L47)               | chi-dist           | output from dist\_chi\_prob matches expected result        | PASS    |    4|  0.63|
| [test-chi-dist.R](testthat/test-chi-dist.R#L61)               | chi-dist           | dist\_chi\_prob returns appropriate error messages         | PASS    |    4|  0.04|
| [test-cross-table.R](testthat/test-cross-table.R#L6)          | ds\_cross\_table   | output from ds\_cross\_table matches expected results      | PASS    |   21|  0.61|
| [test-cross-table.R](testthat/test-cross-table.R#L51)         | ds\_cross\_table   | ouput from plot.ds\_cross\_table matches expected output   | SKIPPED |    1|  0.01|
| [test-describe.R](testthat/test-describe.R#L4)                | describe           | output from ds\_tailobs match expected result              | PASS    |    2|  0.06|
| [test-describe.R](testthat/test-describe.R#L10)               | describe           | ds\_tailobs returns the appropriate error                  | PASS    |    5|  0.05|
| [test-describe.R](testthat/test-describe.R#L19)               | describe           | output from ds\_rindex match expected result               | PASS    |    4|  0.04|
| [test-describe.R](testthat/test-describe.R#L27)               | describe           | ds\_rindex returns the appropriate error                   | PASS    |    3|  0.03|
| [test-describe.R](testthat/test-describe.R#L34)               | describe           | output from ds\_skewness matches expected result           | PASS    |    6|  0.07|
| [test-describe.R](testthat/test-describe.R#L44)               | describe           | ds\_skewness returns the appropriate error                 | PASS    |    2|  0.02|
| [test-describe.R](testthat/test-describe.R#L50)               | describe           | output from ds\_kurtosis matches expected result           | PASS    |    6|  0.06|
| [test-describe.R](testthat/test-describe.R#L60)               | describe           | ds\_kurtosis returns the appropriate error                 | PASS    |    2|  0.02|
| [test-describe.R](testthat/test-describe.R#L66)               | describe           | ds\_css matches `Sum Sq` from anova                        | PASS    |    6|  0.23|
| [test-describe.R](testthat/test-describe.R#L76)               | describe           | ds\_css returns the appropriate error                      | PASS    |    2|  0.01|
| [test-describe.R](testthat/test-describe.R#L82)               | describe           | output from ds\_cvar matches the expected result           | PASS    |    2|  0.02|
| [test-describe.R](testthat/test-describe.R#L88)               | describe           | ds\_cvar returns the appropriate error                     | PASS    |    2|  0.05|
| [test-describe.R](testthat/test-describe.R#L94)               | describe           | output from ds\_mode matches the expected result           | PASS    |    6|  0.50|
| [test-describe.R](testthat/test-describe.R#L104)              | describe           | ds\_mode returns the appropriate error                     | PASS    |    2|  0.01|
| [test-describe.R](testthat/test-describe.R#L110)              | describe           | output from ds\_range matches the expected result          | PASS    |    6|  0.11|
| [test-describe.R](testthat/test-describe.R#L120)              | describe           | ds\_range returns the appropriate error                    | PASS    |    2|  0.02|
| [test-describe.R](testthat/test-describe.R#L126)              | describe           | output from ds\_mdev matches the expected result           | PASS    |    6|  0.04|
| [test-describe.R](testthat/test-describe.R#L136)              | describe           | output from ds\_mdev matches the expected result           | PASS    |    2|  0.02|
| [test-describe.R](testthat/test-describe.R#L141)              | describe           | ds\_mdev returns the appropriate error                     | PASS    |    2|  0.02|
| [test-describe.R](testthat/test-describe.R#L147)              | describe           | output from ds\_hmean matches the expected output          | PASS    |    3|  0.01|
| [test-describe.R](testthat/test-describe.R#L153)              | describe           | ds\_hmean throws the appropriate error                     | PASS    |    2|  0.02|
| [test-describe.R](testthat/test-describe.R#L159)              | describe           | output from ds\_gmean matches the expected output          | PASS    |    3|  0.06|
| [test-describe.R](testthat/test-describe.R#L166)              | describe           | ds\_gmean throws the appropriate error                     | PASS    |    2|  0.01|
| [test-f-dist.R](testthat/test-f-dist.R#L5)                    | f-dist             | dist\_f\_plot returns appropriate error messages           | PASS    |    6|  0.05|
| [test-f-dist.R](testthat/test-f-dist.R#L24)                   | f-dist             | output from dist\_f\_perc matches the expected result      | PASS    |    2|  0.22|
| [test-f-dist.R](testthat/test-f-dist.R#L32)                   | f-dist             | dist\_f\_perc returns appropriate error messages           | PASS    |    8|  0.08|
| [test-f-dist.R](testthat/test-f-dist.R#L60)                   | f-dist             | output from dist\_f\_prob matches expected result          | PASS    |    2|  0.20|
| [test-freq-cont.R](testthat/test-freq-cont.R#L5)              | ds\_freq\_cont     | output from ds\_freq\_cont matches expected result         | PASS    |    8|  0.10|
| [test-freq-cont.R](testthat/test-freq-cont.R#L19)             | ds\_freq\_cont     | ds\_freq\_cont returns appropriate errors                  | PASS    |    2|  0.01|
| [test-freq-cont.R](testthat/test-freq-cont.R#L24)             | ds\_freq\_cont     | output from ds\_freq\_cont plot is as expected             | SKIPPED |    1|  0.00|
| [test-freq-table.R](testthat/test-freq-table.R#L5)            | freq\_table        | output from ds\_freq\_table matches expected results       | PASS    |    6|  0.09|
| [test-freq-table.R](testthat/test-freq-table.R#L14)           | freq\_table        | ds\_freq\_table returns appropriate errors                 | PASS    |    1|  0.00|
| [test-freq-table.R](testthat/test-freq-table.R#L18)           | freq\_table        | output from ds\_freq\_table plot is as expected            | SKIPPED |    1|  0.00|
| [test-group-summary.R](testthat/test-group-summary.R#L14)     | ds\_group\_summary | output from ds\_group\_summary matches the expected result | PASS    |    6|  0.56|
| [test-group-summary.R](testthat/test-group-summary.R#L40_L43) | ds\_group\_summary | ds\_group\_summary throws the appropriate error            | PASS    |    2|  0.02|
| [test-group-summary.R](testthat/test-group-summary.R#L52)     | ds\_group\_summary | output from ds\_group\_summary plot is as expected         | SKIPPED |    1|  0.00|
| [test-multistats.R](testthat/test-multistats.R#L8)            | multistats         | output from multistats is as expected                      | PASS    |    1|  0.28|
| [test-normal-dist.R](testthat/test-normal-dist.R#L5)          | normal-dist        | output from dist\_norm\_perc matches expected output       | PASS    |    6|  0.83|
| [test-normal-dist.R](testthat/test-normal-dist.R#L25)         | normal-dist        | dist\_norm\_perc throws the appropriate errors             | PASS    |    9|  0.08|
| [test-normal-dist.R](testthat/test-normal-dist.R#L39)         | normal-dist        | output from dist\_norm\_prob matches expected output       | PASS    |    6|  0.90|
| [test-normal-dist.R](testthat/test-normal-dist.R#L59)         | normal-dist        | dist\_norm\_prob throws the appropriate errors             | PASS    |    9|  0.11|
| [test-output.R](testthat/test-output.R#L41)                   | print              | output from print\_cross matches expected output           | PASS    |    1|  0.21|
| [test-output.R](testthat/test-output.R#L70)                   | print              | output from print\_screener matches expected output        | PASS    |    1|  0.11|
| [test-output.R](testthat/test-output.R#L91)                   | print              | output from print\_fcont matches the expected result       | PASS    |    1|  0.17|
| [test-output.R](testthat/test-output.R#L109)                  | print              | output from freq\_table matches the expected result        | PASS    |    1|  0.12|
| [test-output.R](testthat/test-output.R#L137)                  | print              | output from group\_summary matches the expected result     | PASS    |    1|  0.51|
| [test-output.R](testthat/test-output.R#L210)                  | print              | output from print\_ftable2 matches the expected result     | PASS    |    1|  0.41|
| [test-output.R](testthat/test-output.R#L300)                  | print              | output from print\_cross2 matches the expected result      | PASS    |    1|  0.32|
| [test-screen.R](testthat/test-screen.R#L9)                    | ds\_screener       | output from ds\_screener matches the expected result       | PASS    |   13|  0.12|
| [test-screen.R](testthat/test-screen.R#L48)                   | ds\_screener       | ds\_screener throws the appropriate error                  | PASS    |    1|  0.02|
| [test-t-dist.R](testthat/test-t-dist.R#L5)                    | t-dist             | output from dist\_t\_perc matches expected results         | PASS    |    6|  0.77|
| [test-t-dist.R](testthat/test-t-dist.R#L25)                   | t-dist             | dist\_t\_perc returns appropriate error messages           | PASS    |    6|  0.06|
| [test-t-dist.R](testthat/test-t-dist.R#L46)                   | t-dist             | output from dist\_t\_prob matches expected result          | PASS    |    8|  1.02|
| [test-t-dist.R](testthat/test-t-dist.R#L75)                   | t-dist             | dist\_t\_prob returns appropriate error messages           | PASS    |    4|  0.03|
| [test-utils.R](testthat/test-utils.R#L4)                      | utils              | output from formatc matches the expected result            | PASS    |    2|  0.01|
| [test-utils.R](testthat/test-utils.R#L10)                     | utils              | output from formatl matches the expected result            | PASS    |    2|  0.02|
| [test-utils.R](testthat/test-utils.R#L16)                     | utils              | output from formatr matches the expected result            | PASS    |    1|  0.02|
| [test-utils.R](testthat/test-utils.R#L21)                     | utils              | output from formats matches the expected result            | PASS    |    1|  0.00|
| [test-utils.R](testthat/test-utils.R#L26)                     | utils              | output from formatnc matches the expected result           | PASS    |    1|  0.01|
| [test-utils.R](testthat/test-utils.R#L31)                     | utils              | output from format\_gap matches the expected result        | PASS    |    1|  0.00|
| [test-utils.R](testthat/test-utils.R#L36)                     | utils              | output from formatol matches the expected result           | PASS    |    2|  0.02|
| [test-utils.R](testthat/test-utils.R#L42)                     | utils              | output from l matches the expected result                  | PASS    |    3|  0.04|
| [test-utils.R](testthat/test-utils.R#L49)                     | utils              | output from percent matches the expected result            | PASS    |    2|  0.02|
| [test-utils.R](testthat/test-utils.R#L55)                     | utils              | output from formata matches the expected result            | PASS    |    1|  0.01|
| [test-utils.R](testthat/test-utils.R#L60)                     | utils              | output from formatas matches the expected result           | PASS    |    1|  0.00|
| [test-utils.R](testthat/test-utils.R#L65)                     | utils              | output from formatter\_freq matches the expected result    | PASS    |    1|  0.04|
| [test-utils.R](testthat/test-utils.R#L72)                     | utils              | output from row\_pct matches the expected result           | PASS    |    1|  0.01|
| [test-utils.R](testthat/test-utils.R#L79)                     | utils              | output from col\_pct matches the expected result           | PASS    |    1|  0.00|
| [test-utils.R](testthat/test-utils.R#L84)                     | utils              | output from formatter matches the expected result          | PASS    |    1|  0.02|
| [test-utils.R](testthat/test-utils.R#L89)                     | utils              | output from fround matches the expected result             | PASS    |    1|  0.00|
| [test-utils.R](testthat/test-utils.R#L94)                     | utils              | output form xmm matches the expected result                | PASS    |    1|  0.01|
| [test-utils.R](testthat/test-utils.R#L98)                     | utils              | output from seql matches the expected result               | PASS    |    1|  0.02|
| [test-utils.R](testthat/test-utils.R#L102)                    | utils              | output form xmmp matches the expected result               | PASS    |    2|  0.02|
| [test-utils.R](testthat/test-utils.R#L107)                    | utils              | output from seqlp matches the expected result              | PASS    |    2|  0.02|
| [test-utils.R](testthat/test-utils.R#L113)                    | utils              | output form xmn matches the expected result                | PASS    |    1|  0.00|
| [test-utils.R](testthat/test-utils.R#L117)                    | utils              | output from seqln matches the expected result              | PASS    |    1|  0.00|
| [test-utils.R](testthat/test-utils.R#L123)                    | utils              | output from intervals matches expected result              | PASS    |    1|  0.01|
| [test-utils.R](testthat/test-utils.R#L128_L131)               | utils              | output from freq matches the expected result               | PASS    |    1|  0.02|
| [test-utils.R](testthat/test-utils.R#L135)                    | utils              | output from stat\_uss matches the expected result          | PASS    |    1|  0.03|
| [test-utils.R](testthat/test-utils.R#L140)                    | utils              | output from div\_by matches the expected result            | PASS    |    1|  0.00|

</details>

<details> <summary> Session Info </summary>

| Field    | Value                            |
|:---------|:---------------------------------|
| Version  | R version 3.5.1 (2018-07-02)     |
| Platform | x86\_64-w64-mingw32/x64 (64-bit) |
| Running  | Windows &gt;= 8 x64 (build 9200) |
| Language | English\_India                   |
| Timezone | Asia/Calcutta                    |

| Package  | Version |
|:---------|:--------|
| testthat | 2.0.0   |
| covr     | 3.2.0   |
| covrpage | 0.0.59  |

</details>

<!--- Final Status : skipped/warning --->
