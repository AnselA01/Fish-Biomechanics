# Fish Biomechanics

## Directory Contents

-   `data`

    bone .csv files here (they are tab, not comma separated)

-   `img`

    all generated images of stress/strain curves for all fish and bones

-   `log`

    the autogit script log file.

-   `src`

    -   `md`

        R markdown files.

        -   `main` - General testing and function usage examples.

        -   `pf09segmenting` - Exploration of segmenting method with pf09 bones.

        -   `polynomial_regression` - Exploration of polynomial regression modeling with pf09 bones.

        -   `spline` - Exploration of spline method with pf09 bones

        -   `thresholdjustification` -Justiifcaiton for chosen thresholdhold to filter noise data

    -   `script`

        R source files.

        -   `changePointAnalysis` - Functions for use with segmenting method

        -   `data` - wrangling data from raw bone to useful stress/strain data.

            -   `data.fetch` function usage:

                -   This is a helpful and user-friendly function for fetching bone data for any number of fish, segments, and trials.

                -   Syntax: `bones_list <- data.fetch(fish_numbers, segments, trials)`

                    -   args:

                        -   `fish_numbers`: a list of fish numbers like `c(1, 2, 3)`

                        -   `segments`: a list of segments like `c("lt", "cp")`

                        -   `trials`: a list of trials like `c(1, 2)`
                        
                        - The `fish_numbers` and `segments` arguments default to everything, so, if you want all segments for fish pf01, you would use `var <- data.fetch(fish_numbers = c(1))`. If you want all fish for segment "lt", you would use `var <- data.fetch(segments = c("lt"))`. `trials` defaults to 1.

                    -   returns a list of bones indexed by a fish number, segment, and trial number as `bones_list[["01lt1"]]`

        -   `image` - function to save ggplots to images

        -   `plot` - Functions used for plotting data

        -   `polynomialRegression` - functions for polynomial regression of data
        
        -   `YoungsModulus` - functions for calculating Young's Modulus of Stress/Strain data with two methods: Max Slope (`ym.MaxSlope`) and spline of first derivative (ym.FDS)
        
## References
