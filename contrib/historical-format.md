# Station format
Line 1: Full, qualified name.
Line 2: Easting and northing, latitude and longitude, elevation.
Line 3: Comment
Line 4: Comment
Line 5: Comment
Line 6: Comment (column names, always the same)
Line 7: Comment (corresponding units for column names)
Line 8 onwards: data in tab-separated columns. Each line starts with a tab.
                There is one exception: If the site is closed, then the last line should say "Site closed". Check this!


The Met Office advise to select from line 6 downwards, so obviously the stuff above it is metadata while line 6 onwards concerns data in colums.

Columns:
0. Year
1. Month
2. Max. temperature for the month (°C)
3. Mark previous column as estimated
4. Min. temperature for the month (°C)
5. Mark previous column as estimated
6. Number of days of air frost
7. Mark previous column as estimated
8. Rainfall (mm)
9. Mark previous column as estimated
10. Hours of sun
11. Mark previous column as estimated, or whether it is using a given sensor (always appears to be one or the other).
12. Mark a given entry as being provisional

BUT - some station data is not quite in the same format as others.
For instance, Whitby uses a dollar ($) symbol to mark if sun data came from Whitby Coastguard. It also says "all data from whitby" on one line.

We can work out location from the first line with a simple regular expression.
