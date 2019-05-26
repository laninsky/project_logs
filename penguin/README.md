R scripts for conducting CubSFS and SNAPP analyses for:

# Receding ice drove rapid expansions in eight Southern Ocean penguins
Theresa L. Cole, Ludovic Dutoit, Nic Dussex, Tom Hart, Alana Alexander, María José Frugone, Jane L. Younger,  Gemma V. Clucas, Yves Cherel, Richard Cuthbert, Ursula Ellenberg, Steven R. Fiddaman, David Houston, Pierre Jouventin, Thomas Mattern, Gary Miller, Colin Miskelly, Paul Nolan, Michael J. Polito, Petra Quillfeldt, Peter G. Ryan, Adrian Smith, Alan JD Tennyson, David Thompson, Barbara Wienecke, Juliana A. Vianna, Jonathan M. Waters

## Input
For CubSFS analyses, the input were folded SFS files formatted for fastsimcoal2. Although they were folded, the full number of sites were present (but the second half of the SFS file had '0' counts, reflecting this).

For the SNAPP analysis, the input was a tab delimited file with the following variables (also detailed in the R script comments):  
Replicate - 1, 2 or 3  
Delta_theta - change in theta for each replicate, inferred from comparing theta at terminal to theta at nearest internal node  
Location - name of terminal lineages used in each SNAPP replicate  
Location_code - standardized location code across all replicates   
Taxa_common_name - common name of species included in analysis   
Taxa_scientific - scientific name of species included in analysis  
Taxa_order_for_graphing - desired order (left to right) for plotting of taxa  
Location_order_for_graphing  - desired order (left to right) for plotting of locations  
Overall_order - overall order for plotting of locations (order fields were used to sort data before importing into R)  

## Description
The CubSFS scripts describe the steps taken to infer demographic history from an SFS, and to plot this comparatively across species (more details are given in the comments in the R scripts).

The SNAPP script describes the steps taken to plot the change in theta inferred from SNAPP across species.

## To cite:
Theresa L. Cole, Ludovic Dutoit, Nic Dussex, Tom Hart, Alana Alexander, María José Frugone, Jane L. Younger,  Gemma V. Clucas, Yves Cherel, Richard Cuthbert, Ursula Ellenberg, Steven R. Fiddaman, David Houston, Pierre Jouventin, Thomas Mattern, Gary Miller, Colin Miskelly, Paul Nolan, Michael J. Polito, Petra Quillfeldt, Peter G. Ryan, Adrian Smith, Alan JD Tennyson, David Thompson, Barbara Wienecke, Juliana A. Vianna, Jonathan M. Waters. Receding ice drove rapid expansions in eight Southern Ocean penguins. 

Please also cite R and these packages, without which the analyses would not be possible:  
- CubSFS
- gridExtra
- parallel
- scales
- tidyverse
