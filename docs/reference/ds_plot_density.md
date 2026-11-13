# Generate density plots

Creates density plots if the data has continuous variables.

## Usage

``` r
ds_plot_density(data, ..., color = "blue", print_plot = TRUE)
```

## Arguments

- data:

  A `data.frame` or `tibble`.

- ...:

  Column(s) in `data`.

- color:

  Color of the plot.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Examples

``` r
# plot single variable
ds_plot_density(mtcarz, mpg)


# plot multiple variables
ds_plot_density(mtcarz, mpg, disp, hp)


# plot all variables
ds_plot_density(mtcarz)


```
