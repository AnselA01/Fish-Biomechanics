# Fish Biomechanics

## Directory Contents

-   `data`

    bone .csv files here (they are tab, not comma separated)

-   `img`

    all generated images of stress/strain curves for all fish and bones

-   `log`

    currently only the autogit log file.

-   `src`

    -   `md`

        R markdown files.

        -   `main` - Your entry point for testing and using any .R scripts.
        
        -   `pf09segmenting` - Exploration of segmenting method with pf09 bones.
        
        -   `polynomial_regression` - Exploration of polynomial regression modeling with pf09 bones.

        -   `spline` - Exploration of spline method with pf09 bones
        
        -   `thresholdjustification` -Justiifcaiton for chosen thresholdhold to filter noise data

    -   `script`

        R source files. 

        -   `changePointAnalysis` - Functions for use with segmenting method

        -   `data` - wrangling data from raw bone to useful stress/strain data.
        
        -   `image` - function to save ggplots to images
        
        -   `plot` - Functions used for plotting data
        
        -   `polynomialRegression' - functions for polynomial regression of data
        


