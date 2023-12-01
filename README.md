# ukclimate - Parse raw historical Met Office weather station data

The Met Office provides a number of datasets for public benefit.
These largely relate to a number of historical measurements at various sites
around the UK.

I wanted to use this data, but because it is largely not provided in a format
which I could easily process, like CSV, this became difficult, and I had to
manually parse it.

The files are designed to be manually copied into a spreadsheet program.
These are separated by regular characters, but the format is not consistent
across years/locations, and there are additionally comments throughout.
I believe these are updated manually by Met Office staff.
	
The library and sample client is able to serialise most if not all examples of
historical station data in JSON and CSV available from the Met Office, at the
time of writing. Needless to say, this is a work in progress.

I’m mostly writing this as a learning exercise for Haskell, particularly as
regards to parser combinators, as well as using the wonderful recursion-schemes
library, and lenses.

Note: make sure `~/.ukclimate/data` is a real directory with sites.json in it.
You can make a symbolic link to the path of `contrib` in the source directory for
instance.

-----

Implemented:
  * Parse a given file into a general representation of historic data and regional time-ordered series
  * Download and cache files from the Met Office
  * Cache results from already-parsed files
  * Debug annotations provided by attoparsec, making clear where failure occurs
  
To-do:
  * Parsing regional sets that are rank-ordered rather than time-ordered (it's very similar data)
  * Improve caching of results