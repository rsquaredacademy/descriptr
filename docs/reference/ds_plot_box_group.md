# Compare distributions

Creates box plots if the data has both categorical & continuous
variables.

## Usage

``` r
ds_plot_box_group(data, ..., print_plot = TRUE)
```

## Arguments

- data:

  A `data.frame` or `tibble`.

- ...:

  Column(s) in `data`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Examples

``` r
# subset data
mt <- dplyr::select(mtcarz, cyl, disp, mpg)

# plot select variables
ds_plot_box_group(mtcarz, cyl, gear, mpg)


# plot all variables
ds_plot_box_group(mt)

```
