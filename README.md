# Fish Biomechanics

## Directory Contents

-   `data` - bone .csv files here (they are tab, not comma separated)

-   `img` - all generated images of stress/strain curves for all fish and bones

        -  `plain` - just stress strain graphs for all fish
        
        -  `ymResults` - stress strain graphs with Max, Inflection, FDS methods finding Young's Modulus
        
              - `decisions` - plots which show selected Young's Modulus based on logic

-   `log` - the autogit script log file.

-   `src` - all Rmd and R source files

    -   `md` - R markdown files.

        -   `main` - General testing and function usage examples.

        -   `max_slope` - Exploratory calculations for finding Young's Modulus using inflection point.
        
        -   `pf09segmenting` - Exploration of segmenting method with pf09 bones.

        -   `polynomial_regression` - Exploration of polynomial regression modeling with pf09 bones.

        -   `spline` - Exploration of spline method with pf09 bones
        
        -   `splinesOfDerivatives` - Exploration of splining of numerical derivatives

        -   `thresholdjustification` - Justiifcaiton for chosen thresholdhold to filter noise data
        
         - `old` - old files we are no longer directly using for the project

    -   `script` - R source files.

        -   `changePointAnalysis` - Functions for use with segmenting method

        -   `data` - wrangling data from raw bone to useful stress/strain data.

            -   `data.fetch`: a helpful and user-friendly function for fetching bone data for any number of fish, segments, and trials.

                -   Syntax: `list <- data.fetch(fish_numbers, segments, trials)` or `data.fetch(subject)`

                    -   arguments:

                        -   `subject`: if you just want one fish. Argument is a string `<fish type xx><fish number yy><segment zz><trial a>`

                        -   `fish_numbers`: a list of fish numbers like `c(1, 2, 3)`

                        -   `segments`: a list of segments like `c("lt", "cp")`

                        -   `trials`: a list of trials like `c(1, 2)`

                        -   The `fish_numbers` and `segments` arguments default to everything, so, if you want all segments for fish pf01, you would use `var <- data.fetch(fish_numbers = c(1))`. If you want all fish for segment "lt", you would use `var <- data.fetch(segments = c("lt"))`. `trials` defaults to 1.

                    -   returns: a list of bones indexed by a fish number, segment, and trial number as `list[["xxyyz"]]`
            
        -   `image` - function to save ggplots to images

        -   `plot` - Functions used for plotting data

        -   `polynomialRegression` - functions for polynomial regression of data

        -   `youngsModulus` - Calculation of Young's Modulus for Stress/Strain data with three methods: global maximum slope, first inflection point, first slope local maximum. Function for calculation using all three methods is `ym.calculate(bone)`. 

        -   `youngsModulusDetermine` - Determination of the correct young's modulus calculation method. Function for detetmination is `ym.Determine`. User passes the return value from `ym.calculate`.
        
            - Calculation and determination can be performed in one step with function `ym.calculateAndDetermine(bone)`

## Methodology

The overall method of determining the Young's Modulus value 

### Young's Modulus

#### Calculation

Calculation of Young's modulus performed using three methods:

1. Max - This method takes the data below a strain value of 0.2, fits a spline to the data with 10 degrees of freedom, calculates the slope of the spline every 0.0001 units of strain and selects the highest slope. This value is the Young's Modulus value.

2. Inflection - This method takes takes the data below a strain value of 0.2, fits a spline to the data with 10 degrees of freedom, calculates the second derivative fo the spline every 0.0001 units of strain and selects the maximum second derivative value. The first derivative at of the spline at the point of the maximum second derivative is evaluated. This value is the Young's Modulus value.

3. FDS - This method takes the data below a strain value of 0.2, and takes the numerical derivatives between points with a lag of 2 (due to repeated strain-steps in dataset with different stress values) and fits a spline to the derivatives with 10 degress of freedom. The first local maxima of the spline of the derivatives is found to a precision of 0.0001 units of strain. A spline of the original data with 10 degrees of freedom is fit. This spline's derivative is evaluated at the local maxima strain value. This is teh Young's Modulus value.




#### Selection

To select the correct Young's modulus value from our pool of three values, we considered __score and 



## References
