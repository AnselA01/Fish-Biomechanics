# Fish Biomechanics

## Directory Contents

-   `data`

    bone .csv files here (they are tab, not comma separated)

-   `img`

    all generated images here

-   `log`

    currently only the autogit log file.

-   `src`

    -   `md`

        R markdown files.

        -   `main` - Your entry point for testing and using any .R scripts.

        -   `spline` - Exploration of spline method with pf09 bones
        
        -   `thresholdjustification` -Justiifcaiton for chosen thresholdhold to filter noise out of data
        
        -   `pf09segmenting` - Exploration of segmenting method with pf09 bones.

    -   `script`

        R source files. 

        -   `changePointAnalysis` - Functions for use with segmenting method
        
            - `data.generator` is the function you should use for getting any bones.

        -   `data` - wrangling data from raw bone to useful stress/strain data.
        
        -   `image` - function to save ggplots to images
        
        -   `plot` - Functions used for plotting data
        


