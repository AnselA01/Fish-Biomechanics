---
editor_options: 
  markdown: 
    wrap: 72
---

# Statistical Analysis of the Variation in Material Properties of Vertebrae Regions in Yellow Perch

Updated: April 22, 2025

Takashi Maie Lab and the Center for Interdisciplinary Research (CIR),
St. Olaf College

St. Olaf faculty

-   Takashi Maie, PhD. (Maie Lab)

-   Laura Boem-Vock, PhD. (CIR)

Underagraduate CIR fellows

-   Ansel Alldredge (CIR) -
    [aalldredge001\@gmail.com](mailto:aalldredge001@gmail.com){.email}
    \| 763-670-1770
-   Abigail Hahs (CIR)
-   Otto Schmidt (CIR) -
    [ottohschmidt4\@gmail.com](mailto:ottohschmidt4@gmail.com){.email}
    \| 651-402-6313

## Project Overview

This project aims to identify the Young's Modulus of yellow perch
vertebrae from collected stress-strain time series data. Data are not
shared in the repo.

The following **README.md** file details the code in rstudio create by
the undergraduate CIR fellows listed above during the 2024-2025 academic
year. During this time, the fellows have developed an automated
algorithm which takes in the time series data of applied force and
compression length for a tensile compression test of fish vertebral
bones and calculates the Young's Modulus of each bone. Then statistical
analysis of these Young's Modulus values is conducted. The project can
be divided into these two portions:

1.  The Algorithm
2.  The Statistical Analysis.

## For Those Looking to Use the Automated Algorithm to Get Young's Modulus Values

The document **youngs_modulus_calculation_entry.Rmd** details the steps
to run the algorithm and get a Young's Modulus value for each fish bone.
The document **Automated Young's Modulus Calculation Workflow** found in
the **explanation** **folder** includes resources for ensuring the
algorithm runs properly as well as troubleshooting.

## For Those Looking to Use the Statistical Analysis of Young's Modulus Values

The document **youngs_modulus_statistical_analysis_outline.Rmd** in the
**statistical analysis folder** details the statistical analysis
conducted on the bones of 21 perch. The statistical methods are
described in detail and justification for the process in discussed.

## For Those Looking for Pre-Written Text for an Academic Paper Describing the Algorithm and/or Statistical Analysis

The document **Algorithm - Final Write Up** in the **explanation
folder** includes paragraphs written by the CIR fellows that may be used
in an academic paper or other similar product.

## For Those Looking to Continue Working on This Project

The following resources will be helpful for those picking up this
project:

1.  This **README.md** here is a useful starting point and a home base
    for all the explanatory documents associated with the project. The
    Repo section below details the code structure of the project. There
    is much more than is used in the final product. However, all the
    code is included here for reference as well as to help future
    individuals understand our thought process, what we have tried, and
    other threads of work which we did not finish.
2.  The document **Algorithm - Final Write Up** in the **explanation
    folder** includes extensive explanation about the algorithm with
    insights and justification for the process. Reading this document in
    full is the single best way to orient yourself to the details of the
    algorithm portion of the project.
3.  The document **youngs_modulus_statistical_analysis_outline.Rmd** in
    the **statistical analysis folder** details the statistical analysis
    conducted on the bones of 21 perch. The statistical methods are
    described in detail and justification for the process in discussed.
    Reading this document in full is the single best way to orient
    yourself ot the details of the statistical analysis portion of the
    project.\

## Repo - Methods for Tiding Data, Extracting Tensile Properties, Statistical Analysis.

Throughout the repo there are folders labeled yield stress and often are
accompanied by the label "incomplete" or "not final." We began looking
at automated ways to calculate the yield stress for each bone but did
not have enough time complete this process by the end of the year. Thus
none of the code is finalized but the ideas may be useful in the future.

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
                0.8 N. This filtering threshold is used in the data
                cleaning process.

            -   youngsModulus -

            -   youngsModulusDetermination -

    -   `script` - R source files.

        -   `helpers` - functions used throughout the algorithm

            -   data.R - includes functions for fetching bone data for
                use in algorithm, data cleaning, file parsing, and
                calculation of stress and strain values.

            -   general.R - ???

            -   image.R - function which saves fish ggplots as images
                files

            -   plot.R - plotting functions

        -   `old` - functions no longer used in the algorithm, kept for
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

SHOULD WE KEEP THE REST OF THIS??????

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
    and fits a spline to the derivatives with 10 degrees of freedom. The
    first local maxima of the spline of the derivatives is found to a
    precision of 0.0001 units of strain. A spline of the original data
    with 10 degrees of freedom is fit. This spline's derivative is
    evaluated at the local maxima strain value. This is the Young's
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

-   Each set of results is saved in a new folder named with the current
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
