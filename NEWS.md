# tyler 0.0.0.9000


November 1, 2024
* I narrowed the focus of the package to include only mystery caller study functions.  The mapping and isochrone functions will be kept separately.  

November 1, 2024
We started better naming conventions with named for what part of the project they are used:

* "phase0_" - Able to gather the data and clean it (genderize, standardize phone numbers, geocode)
* "clean_phase_1" - Sets up the confirmation calls.  
* "phase2_" - Sets up the REDCap database inputs, quality control.  

* "results_section_" - Creates the prose of the results section.  
* "figure_" - For plotting data in the results section.  
* "map_" - For mapping data as a dot map or a honeycomb plot or a region.  


November 2, 2024
* I continue to struggle with issues of getting the pkgdown website to post in github.  This looks like it is related to use of the `exploratory::statecode()` function that may not be public or exportable `@export`?
* I had a hard time getting the phase1 vignette to post.  

