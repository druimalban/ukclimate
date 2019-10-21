# Ranked data format

Example:
  England_and_Wales Maximum Temperature (Degrees C)
  Areal series, starting from 1910
  Allowances have been made for topographic, coastal and urban effects where relationships are found to exist.
  Seasons: Winter=Dec-Feb, Spring=Mar-May, Summer=June-Aug, Autumn=Sept-Nov. (Winter: Year refers to Jan/Feb).
  Monthly values are ranked and displayed to 1 dp and seasonal/annual values to 2 dp. Where values are equal, rankings are based in order of year descending.
  Data are provisional from January 2018 & Winter 2018. Last updated 01/04/2019

First line: REGION MEASUREMENT (UNIT)
Second line: SERIES TYPE, starting from YYYY
Third line: Comment
Fourth line: Comment
Fifth line: Comment
Sixth line: Data are provisional from MONTH & SEASON. Last updated DD/MM/YYYY
Seventh line: Empty
Eighth line: Column headers
Ninth line onwards: Data in columns for each month, then each season, then the annual measurement.
