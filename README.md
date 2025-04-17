# Statistical Analysis of the Variation in Material Properties of Vertebrae Regions in Yellow Perch

Takashi Maie Lab and the Center for Interdisciplinary Research (CIR), St. Olaf College

St. Olaf faculty

- Takashi Maie, PhD. (Maie Lab)

- Laura Boem-Vock, PhD. (CIR)

Underagraduate CIR fellows


- Ansel Alldredge (CIR)
- Abigail Hahs (CIR)
- Otto Schmidt (CIR)

## Project 

This project aims to identify three tensile properties of yellow perch vertebrae from collected stress-strain data. Data are not shared in the repo.


## Tensile prorerty extraction



## Statistical Analysis

## Repo - Methods for tiding data and extracting tensile properties.

-   `src` - all Rmd and R source files

    -   `md` - rmd files
    
        - 
        - 
        - 
        - 
        - 
        - 
        - 
        - 
        - 
        - 

    - `script` - R source files.
    
        -
        -
        -
        -
        -
        -
        -
        -
        -
        -
        -
        -

## Methodology

The overall method of determining the Young's Modulus value 

### Young's Modulus

#### Calculation

Calculation of Young's modulus is performed using three methods:

1. Max - This method takes the data below a strain value of 0.2, fits a spline to the data with 10 degrees of freedom, calculates the slope of the spline every 0.0001 units of strain and selects the highest slope. This value is the Young's Modulus value.

2. Inflection - This method takes takes the data below a strain value of 0.2, fits a spline to the data with 10 degrees of freedom, calculates the second derivative fo the spline every 0.0001 units of strain and selects the maximum second derivative value. The first derivative at of the spline at the point of the maximum second derivative is evaluated. This value is the Young's Modulus value.

3. FDS - This method takes the data below a strain value of 0.2, and takes the numerical derivatives between points with a lag of 2 (due to repeated strain-steps in dataset with different stress values) and fits a spline to the derivatives with 10 degress of freedom. The first local maxima of the spline of the derivatives is found to a precision of 0.0001 units of strain. A spline of the original data with 10 degrees of freedom is fit. This spline's derivative is evaluated at the local maxima strain value. This is teh Young's Modulus value.


#### Selection

To select the correct Young's modulus value from our pool of three values, we considered a


#### Automated Young's Modulus Calculation

- `youngs_modulus_calculation_entry.Rmd` is the entry for performing automated calculation of Young's Modulus for all available bone data.

Steps:

1. CSV data in the form `AAXBBX.csv` where AA is two-letter fish name 

- Eeach set of results is saved in a new folder named with the current day's date.

- choices.csv lists the chosen value for each bone

  choices.csv Structure:
  rows: bone identified with the name column
  columns: 
    name - string; structured as AAXBBX where AA is two letter fish type and BB is                    two letter bone type - ex. PF01CP02 or PF999LT999
    method - string; chosen method used to calculate Young's Modulus
    slope - double; Young's Modulus value), score (coefficient of variation
    strain - double; strain value at which Young's Modulus was calculated
    inconclusive - boolean; indicates confidence in chosen Young's Modulus
  
- results.csv lists all calculated values for each bone

  results.csv Structure:
  rows: bone identified with the name column
  columns:
    name - string; same as above 
    max.slope - double; Young's Modulus for "max" method
    max.strain - double; Strain for "max" method
    max.score - double; Score for "max" method
    inflection.slope - double; Young's Modulus for "inflection" method
    inflection.strain - double; Strain value for "inflection" method
    inflection.score - double; Score for "inflection" method
    fds.slope - double; Young's Modulus for "fds" method
    fds.strain - double; Strain value for "fds" method
    fds.score - double;  Score for "fds" method
    
- inconclusives.txt lists all bones marked as inconclusive. Their data is saved in the folder "inconclusive-data"
