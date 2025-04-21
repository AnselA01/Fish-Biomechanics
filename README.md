---
editor_options: 
  markdown: 
    wrap: 72
---

# Statistical Analysis of the Variation in Material Properties of Vertebrae Regions in Yellow Perch

Takashi Maie Lab and the Center for Interdisciplinary Research (CIR),
St. Olaf College

St. Olaf faculty

-   Takashi Maie, PhD. (Maie Lab)

-   Laura Boem-Vock, PhD. (CIR)

Underagraduate CIR fellows

-   Ansel Alldredge (CIR)
-   Abigail Hahs (CIR)
-   Otto Schmidt (CIR)

## Project

This project aims to identify three tensile properties of yellow perch
vertebrae from collected stress-strain data. Data are not shared in the
repo.

## For Those Looking to Continue Working on This Project

The following README files details the code in rstudio create by the
undergraduate CIR fellows listed above during the 2024-2025 academic
year. During this time, the fellows have developed an automated
algorithm which takes in the time series data of applied force and
compression length for a tensile compression test of fish vertebral
bones and calculates the Young's Modulus of each bone. The repo below
contains all the code used throughout this process and thus there is
much more than is used in the final product. However, all the code is
included here for reference as well as to help future individuals
understand our thought process, what we have tried, and other threads of
work which we did not finish.\

A couple items to highlight:

Throughout the repo there are folders label yield stress and often are
accompanied by the label "incomplete" or "not final." We began looking
at automated ways to calculate the yield stress for each bone but did
not have enough time complete this process by the end of the year. Thus
none of the code is finalized but the ideas may be useful in the future.

Within the explanation folder there are additional documents detailing
further this project. This includes a document explaing and
demonstrating the statistical analysis conducted to determine whether
exterior vertebral bones were significantly stronger than interior
bones. We found this to be significant. There is also information for
those who simply wish to use our automated algorithm of finding the
Young's Modulus. If this is you there is no need to read this document
further. Instead refer to \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ for the needed
information.

## Tensile Property extraction

## Statistical Analysis

## Repo - Methods for tiding data and extracting tensile properties.

-   `src` - all Rmd and R source files

    -   `md` - rmd files

        -   main - ??????????

        -   `old` - outdated work exploring ways to represent time
            series data

            -   pf09segmenting - explores possible segmenting techniques
                for representing time series data

            -   polynomial_regression - explores using polynomial
                regression techniques to represent time series data

        -   `yieldStress - prelim_incomplete` - preliminary explorations
            for finding the yield stress - no work in folder is final or
            complete

            -   yieldStress - attempted generalization of a method for
                finding the yield stress

            -   yieldStress preliminary - exploratory work for finding
                yield stress

        -   `youngsModulus - exploratory work` - exploratory work for
            finding Young's Modulus

            -   investigateProblematicYMs - investigative work to
                explore cases where Young's Modulus values were
                outliers/different than expected. This exploration was
                used to improve our algorithm for finding the Young's
                Modulus.

            -   max_slope - investigative work for find Young's Modulus
                by simply the maximum slope of the spline after
                filtering and cleaning the time series data.

            -   spline - investigative work for splining time series
                data. Splining in used in all three methods of Young's
                Modulus calculation.

            -   splinesOfDerivatives - investigative work for splining
                the derivative of the time series data. This idea is
                carried out in the First Derivative Spline (FDS) method.

            -   threshold justification - explanation for use of
                filtering out any data with a stress value less than
                0.2. This filtering threshold is used in the data
                cleaning process.

            -   youngsModulus -

            -   youngsModulusDetermination -

    -   `script` - R source files.

        -   `helpers` - functions used throughout the alogrithm

            -   data.R

            -   general.R

            -   image.R

            -   plot.R

        -   `old`  - functions no longer used in the algorithm, kept for
            record

            -   changePointAnalysis.R - functions used in implementing
                change point analysis technique for finding the Young's
                Modulus value. This method was not chosen and is not
                used in the algorithm.

            -   polynomialRegression.R - functions used in implementing
                polynomial regression to represent stress/strain data.
                This method was not chosen and is not used in the
                algorithm.

        -   `yieldStress - preliminary - not complete`

            -   yieldStress.R - functions related to calculating the
                yield stress. This is not complete or functional but
                kept for the record.

        -   `youngsModulus`

            -   automatedCalculation.R

            -   youngsModulus.R

            -   youngsModulusDetermination.R

-   `img` - images of stress/strain curves for all fish bones

    -   `plain` - images of stress/strain curves with no additional
        markings

    -   `ymResults`

        -   `decisions` - images of stress/strain curves with three
            marked Young's Modulus values and the chosen value
            highlighted in green.

        -   other folder - images of stress/strain curves with three
            marked Young's Modulus values from the three different
            methods (Max, Inflection, FDS)

-   `data` - all data

    -   area.csv - csv containing the surface area of each crushed bone

    -   Perch study - MAIE Lab - Tally.csv - csv containing basic
        information about each fish in the study including the fish
        length, a metric used in the statistical analysis

    -   folders - each folder is one fish and within each folder each
        csv contains the time series data output by the tensile
        compression machine for a single bone. The name of the csv is
        first a two letter code for the fish type, a two number code for
        the fish number of that type, a two letter code (CP, MT, LT, or
        UT) for the region of the vertebrae the bone is from, and a
        final two number code for the number bone in that vertebral
        region.

-   `statistical_analysis`

-   `explanation`

-   `results`

    -   `yieldStress - preliminary work - not complete` - exploratory
        results from calculation the yield stress of each bone

    -   `youngs-modulus` - results of each time the algorithm to
        calculate Young's Modulus is run

        -   README - details the contents of each folder in
            youngs-modulus and how the results may be used

## Methodology

The overall method of determining the Young's Modulus value

### Young's Modulus

#### Calculation

Calculation of Young's modulus is performed using three methods:

1.  Max - This method takes the data below a strain value of 0.2, fits a
    spline to the data with 10 degrees of freedom, calculates the slope
    of the spline every 0.0001 units of strain and selects the highest
    slope. This value is the Young's Modulus value.

2.  Inflection - This method takes takes the data below a strain value
    of 0.2, fits a spline to the data with 10 degrees of freedom,
    calculates the second derivative fo the spline every 0.0001 units of
    strain and selects the maximum second derivative value. The first
    derivative at of the spline at the point of the maximum second
    derivative is evaluated. This value is the Young's Modulus value.

3.  FDS - This method takes the data below a strain value of 0.2, and
    takes the numerical derivatives between points with a lag of 2 (due
    to repeated strain-steps in dataset with different stress values)
    and fits a spline to the derivatives with 10 degress of freedom. The
    first local maxima of the spline of the derivatives is found to a
    precision of 0.0001 units of strain. A spline of the original data
    with 10 degrees of freedom is fit. This spline's derivative is
    evaluated at the local maxima strain value. This is teh Young's
    Modulus value.

#### Selection

To select the correct Young's modulus value from our pool of three
values, we considered a

#### Automated Young's Modulus Calculation

-   `youngs_modulus_calculation_entry.Rmd` is the entry for performing
    automated calculation of Young's Modulus for all available bone
    data.

Steps:

1.  CSV data in the form `AAXBBX.csv` where AA is two-letter fish name

-   Eeach set of results is saved in a new folder named with the current
    day's date.

-   choices.csv lists the chosen value for each bone

    choices.csv Structure: rows: bone identified with the name column
    columns: name - string; structured as AAXBBX where AA is two letter
    fish type and BB is two letter bone type - ex. PF01CP02 or
    PF999LT999 method - string; chosen method used to calculate Young's
    Modulus slope - double; Young's Modulus value), score (coefficient
    of variation strain - double; strain value at which Young's Modulus
    was calculated inconclusive - boolean; indicates confidence in
    chosen Young's Modulus

-   results.csv lists all calculated values for each bone

    results.csv Structure: rows: bone identified with the name column
    columns: name - string; same as above max.slope - double; Young's
    Modulus for "max" method max.strain - double; Strain for "max"
    method max.score - double; Score for "max" method inflection.slope -
    double; Young's Modulus for "inflection" method inflection.strain -
    double; Strain value for "inflection" method inflection.score -
    double; Score for "inflection" method fds.slope - double; Young's
    Modulus for "fds" method fds.strain - double; Strain value for "fds"
    method fds.score - double; Score for "fds" method

-   inconclusives.txt lists all bones marked as inconclusive. Their data
    is saved in the folder "inconclusive-data"
