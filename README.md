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



## References
