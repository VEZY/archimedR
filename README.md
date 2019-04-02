
<!-- README.md is generated from README.Rmd. Please edit that file -->

# archimedR

The package is made to read and set input parameters and meteorology of
the ARCHIMED model, to execute, import the outputs and analyse them.

## Installation

To install archimedR, simply execute the following lines of code:

``` r
# install.packages("remotes")
remotes::install_github("VEZY/archimedR")
```

## Example

### Read parameter values

You can read the parameter values from the configuration file using
`read_config`:

``` r
read_config(file = "Archimed/app_parameters/ArchimedConfiguration.properties",
            parameter = c("latit","altitude"))
```

### Read meteo

To read the meteo file:

``` r
import_meteo("output/meteo.csv")
```

### Run ARCHIMED:

To run ARCHIMED from R, you can use this command:

``` r
run_archimed(path= "Archimed", memory = 4096)
```

The memory argument allocate some space from the RAM to the JVM (Java
Virtual Machine). Make sure to have enough RAM on your computer, and to
set enough RAM for the simulation.

### Read outputs

Two models outputs are available:

  - Object-scale: the node values are all reported in the
    `nodes_values.csv` file. To read it, use this command:

<!-- end list -->

``` r
read_out_node(path= "output/nodes_values.csv")
```

  - Plot-scale: ARCHIMED also returns a report at plot scale in the
    `mirdata.xlsx` file:

<!-- end list -->

``` r
read_out_plot(path= "output/mirdata.xlsx", sheet= "Mir")
```

## Disclaimer

This package is still under active development, as for the ARCHIMED
model. Although we try to keep the functions calls and outputs
consistants between versions, the function calls may change over time.
