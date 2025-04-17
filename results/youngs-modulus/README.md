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