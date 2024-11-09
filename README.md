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

            -   `data.generator function` tips:
            
                - This is a helpful function for requesting bone data for specific fish and segments

                -   Syntax: `data.generator(data_directory, fish_number, segment)` where data_directory is a string path to your data directory (ours is "./data"), fish_number is a number 1-21, and segment is one of ("CP", "LT", "MT", "UT") and is not case-sensitive. `fish_number` and `segment` are optional but `segment` is usable only with `fish_number`. The return type is a coro iterator. <https://coro.r-lib.org/articles/generator.html>

                -   Collecting : `coro::collect(data.generator("./data", fish_number, segment))` to get all bones for a `fish_number` and a `segmebonesnt` in a list. If you just want to do something to a bunch of bones without saving the result, this is not for you. Use the iteration method below. The trial number can be indexed with `[[trial_number]] of the collect() call.`

                -   Iterating over bones: `coro::loop(for (bone in data.generator("./data", fish_number, segment)) { <do something with bone> })` to iterate over the `segment` bones of `fish_number`.

                -   Example: I want to collect the "LT" bones of fish 10 into a list called `pf10lt_list`

                       `pf10lt_list <- coro::collect(data.generator("./data", 10, "lt"))`

        -   `image` - function to save ggplots to images

        -   `plot` - Functions used for plotting data

        -   `polynomialRegression` - functions for polynomial regression of data
