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

            -   `data.generator()` tips:

                -   Syntax: `data.generator(data_directory, fish_number)` where data_directory is a string path to your data directory (ours is "./data") and fish_number is a number 1-21. The type of its return is a coro iterator. <https://coro.r-lib.org/articles/generator.html>

                -   Collecting bones: `coro::collect(data.generator("./data, fish_number"))` to get all bones for a `fish_number` in a list. If you just want to do something to a bunch of bones without saving the result, this is not for you. Use the iteration method below.

                -   Iterating over bones: `coro::loop(for (bone in data.generator("./data", fish_number)) { <do something with bone> })` to iterate over the bones of `fish_number`.

        -   `image` - function to save ggplots to images

        -   `plot` - Functions used for plotting data

        -   \`polynomialRegression' - functions for polynomial regression of data
