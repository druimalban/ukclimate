# ukclimate - Use public Met Office datasets for fun

What works [mostly]:
  * Parse a given file into a general representation of historic data and regional time-ordered series
  * Download and cache files from the Met Office
 
What doesn't work [yet]:
  * Parsing regional sets that are rank-ordered rather than time-ordered (it's very similar data)

21 October, 2019: Parser works for most of the datasets, it's clear where they fail. Please help if you can! The parser is fast and it won't parse a given file if it's already parsed. However, caching of the results doesn't really work in the same way that we've cached downloaded files. I don't think there's much point implementing it in the same way because it doesn't provide much work anyway. I don't have the time to write a better cachingmechanism. What works:

Note: make sure `~/.ukclimate/data` is a real directory with sites.json in it. You can make a symbolic link to the path of `contrib` in the source directory for instance.

-----

The Met Office provides a number of datasets for public benefit. These largely relate to a number of historical measurements at various sites around the UK.
I wanted to use this data, but because it is largely not provided in a format I could easily process, like CSV, this became difficult, and I had to manually parse it.
	
The library and sample client is able to serialise most if not all examples of historical station data in JSON and CSV available from the Met Office, at the time of writing.
Needless to say, this is a work in progress.

I’m mostly writing this as a learning exercise for Haskell, particularly with regards to parser combinators, as well as using the wonderful recursion-schemes library and lenses.
