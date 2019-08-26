#################################################################
#### 1. Description of data sets that were reconciled across ####
#################################################################
# (also see tibble previews below for description of columns)

## "DOC_Massey_database.txt": IDs sent to DOC and Massey to inform their 
# databases.

## "NZCETA_archive.txt": NZCeTA Māui/Hector's tissue info, not 100% 
# reconciled

## "Pathology_data.txt": List of cases where a diagnosis of cause of death
# was possible.

## "Maui_recapture.txt": Reconciled biopsy and stranding information for
# Māui dolphins. Similar information for Hector's is more fragmentary
# so will need to narrow sample list down first.

# All data sets were delivered in excel format. I saved all of them as 
# tab delimited for importing into R, in some cases after modifying them
# to make them more appropriate as flat files e.g. adding column with
# information given as colour coding in original excel file.

# Setting wd where these files found
setwd("/Users/alanaalexander/Dropbox/hologenome/original_files")

#####################################################################
#### 2. Loading libraries, reading in files, tibble descriptions ####
#####################################################################
library(tidyverse)
DOC_Massey_database <- read_tsv("DOC_Massey_database.txt")
DOC_Massey_database
## A tibble: 217 x 26
#UoA_Code Species_subspec… Common_Name Collection_Date Location
#<chr>    <chr>            <chr>       <chr>           <chr>   
#  1 Che03NZ… Cephalorhynchus… Hector's d… 10-Nov-03       Between…
#2 Che03NZ… Cephalorhynchus… Hector's d… 11-Nov-03       Mangama…
#3 Che03NZ… Cephalorhynchus… Hector's d… 16-Dec-02       "Timaru…
#4 Che03NZ… Cephalorhynchus… Hector's d… 10-Oct-03       Barryto…
#5 Che03NZ… Cephalorhynchus… Hector's d… 24-Oct-03       Mikonui…
#6 Che03NZ… Cephalorhynchus… Hector's d… 13-Nov-03       Haast B…
#7 Che04NZ… Cephalorhynchus… Hector's d… 25-Apr-04       3nm nor…
#8 Che04NZ… Cephalorhynchus… Hector's d… 22-Nov-04       Port Le…
#9 Che04NZ… Cephalorhynchus… Hector's d… 4-Dec-04        Akaroa …
#10 Che04NZ… Cephalorhynchus… Hector's d… 11-Dec-04       Te Waew…
## … with 207 more rows, and 21 more variables: LatitudeS_Northing <chr>,
##   LongitudeE_Easting <chr>, Tissue_Type <chr>, Collected_By <chr>,
##   DoC_Agency <chr>, TL_m <chr>, Age_Class <chr>, Sex_observed <chr>,
##   `Pregnant?` <chr>, Doc_incident_no <chr>, Doc_animal_no <dbl>,
##   Franz_source_code <chr>, Massey_Code <chr>, MONZ_code <chr>,
##   CETOS_code <chr>, Necropsy_file_no_Pathology_no <chr>,
##   Genetic_Sex <chr>, mtDNA_haplotype <chr>, no_microsatellites <dbl>,
##   MHC_DQA <chr>, MHC_DQB <chr>

NZCETA_archive <- read_tsv("NZCETA_archive.txt")
NZCETA_archive
## A tibble: 300 x 21
#Checked Species_name Common_name Code  Date_stranded Date_received
#<chr>   <chr>        <chr>       <chr> <chr>         <chr>        
#  1 Yes     Cephalorhyn… Hector's d… Che0… NA            NA           
#2 Yes     Cephalorhyn… Hector's d… Che0… NA            NA           
#3 Yes     Cephalorhyn… Hector's d… Che0… NA            NA           
#4 Yes     Cephalorhyn… Hector's d… Che0… NA            NA           
#5 Yes     Cephalorhyn… Hector's d… Che0… NA            NA           
#6 Yes     Cephalorhyn… Hector's d… Che0… NA            NA           
#7 Yes     Cephalorhyn… Hector's d… Che0… NA            NA           
#8 Yes     Cephalorhyn… Hector's d… Che0… 08/11/2002    NA           
#9 Yes     Cephalorhyn… Hector's d… Che0… 10/11/2003    not sure bef…
#10 Yes     Cephalorhyn… Hector's d… Che0… 11/11/2003    unsure       
## … with 290 more rows, and 15 more variables: Location <chr>,
##   Region <chr>, Ocean <chr>, Cause_of_death <chr>, Sex <chr>, Who <chr>,
##   X13 <lgl>, Source <lgl>, What <chr>, Info_sent <chr>, Extracted <chr>,
##   Info_requested <chr>, Other_info <chr>, Archiving_info <chr>, X21 <lgl>

Pathology_data <- read_tsv("Pathology_data.txt")
Pathology_data
## A tibble: 94 x 15
#H_no. Path_No. `Maui?` Age_class Sex   COD_category Cause_of_death
#<chr> <chr>    <chr>   <chr>     <chr> <chr>        <chr>         
#  1 H153  41043    y       A         F     Disease      toxoplasma    
#2 H157  41223    NA      A         F     Predation    shark predati…
#3 H163  42764    NA      A         F     Misc         Renal cyst/ha…
#4 H166  42428    NA      S         M     Disease      toxoplasma    
#5 H168  42366    NA      A         F     Disease      fungal septic…
#6 H169  42644    NA      A         F     Disease      toxoplasma    
#7 H176  42749    NA      A         F     Disease      bacterial + f…
#8 H182  43204    NA      S         M     Known bycat… known bycatch 
#9 H188  44214    NA      A         F     Disease      toxoplasma    
#10 H189  44217    NA      S         M     Known bycat… known bycatch 
## … with 84 more rows, and 8 more variables: `Death_by_pathogen?` <chr>,
##   Notes <chr>, X10 <lgl>, X11 <lgl>, X12 <lgl>, X13 <lgl>, X14 <lgl>,
##   X15 <lgl>

Maui_recapture <- read_tsv("Maui_recapture.txt")
Maui_recapture
## A tibble: 114 x 23
#Individual_num Indiv_ID Sex   Stranded_sample `2001` `2002` `2003` `2004`
#<dbl> <chr>    <chr> <chr>           <chr>  <chr>  <chr>  <chr> 
#  1              1 NI33     F     NA              NI33   NA     NI77,… NA    
#2              2 NI34     F     NA              NI34   NA     NA     NA    
#3              3 NI35     M     NA              NI35   NA     NA     NA    
#4              4 NI49     F     NA              NI49   NA     NI85   NA    
#5              5 NI50     F     NA              NI50   NA     NA     NA    
#6              6 NI51     F     NA              NI51   NA     NA     NA    
#7              7 NI52     F     NA              NI52   NA     NA     NA    
#8              8 NI36     M     NA              NI36,… NA     NA     NA    
#9              9 NI37     M     NA              NI37   NA     NA     NA    
#10             10 NI38     F     NA              NI38,… NA     NA     NA    
## … with 104 more rows, and 15 more variables: `2006` <chr>, `2007` <chr>,
##   `2010` <chr>, `2011` <chr>, `2013` <chr>, `2015` <chr>, `2016` <chr>,
##   `2018` <chr>, Notes <chr>, `2010_match_status` <chr>,
##   `2011_match_status` <chr>, `2013_match_status` <lgl>,
##   `2015_match_status` <chr>, `2016_match_status` <chr>,
##   `2018_match_status` <lgl>

##################################################################
#### 3. Massaging data so that there are columns that line up ####
##################################################################

# Creating a pathology_code column capturing the 'H' code that is part of the 'Code' in the NZCETA_archive
NZCETA_archive <- NZCETA_archive %>% rowwise() %>% mutate(pathology_code=gsub("-.*","",gsub(").*","",gsub("/.*","",unlist(strsplit(Code,"\\(H"))[2]))))
# Standardizing the number of significant figures in the pathology_code
NZCETA_archive <- NZCETA_archive %>% mutate(pathology_code=ifelse(nchar(pathology_code)==2,
                                                paste("H0",pathology_code,sep=""),
                                                paste("H",pathology_code,sep="")))

# Doing the same thing with the DOC_Massey_database (the H code is part of the Doc_incident_no column in this file)
DOC_Massey_database <- DOC_Massey_database %>% rowwise() %>% mutate(pathology_code=gsub("-.*","",gsub("/.*","",gsub("N/A",NA,Doc_incident_no))))
# Standardizing the number of significant figures in the pathology_code
DOC_Massey_database <- DOC_Massey_database %>% rowwise() %>% mutate(pathology_code=ifelse(nchar(pathology_code)==3,
                                 gsub("H","H0",pathology_code),
                                 pathology_code)) 

# Removing trailing information in 'Code' column so we are left just with the UoA_code for both datasets
NZCETA_archive <- NZCETA_archive %>% rowwise() %>% mutate(scrubbed_uoa_code=gsub("/.*","",gsub("\\(.*","",gsub(" .*","",Code))))
DOC_Massey_database <- DOC_Massey_database %>% rowwise() %>% mutate(scrubbed_uoa_code=gsub("/.*","",gsub(" .*","",UoA_Code)))

#######################################################################
#### 4. Creating a "scrubbed_uoa_code" column for joining datasets ####
#######################################################################
# If joining goes to plan, there should be no more rows than exist in NZCETA
# (because DOC_Massey_database should mostly be a subset of NZCETA) i.e. the following should be 0:
dim(full_join(NZCETA_archive,DOC_Massey_database))[1] - dim(NZCETA_archive)[1]

# Unfortunately joining the NZCETA and DOC_Massey_databases did not go smoothly (the above came out as 163!), so
# we need to figure out what is causing these issues (presumably individual entries being)
# present more than once

# First: Looking for potential duplicate entries in the Massey database based on uoa_code:
dim(DOC_Massey_database)[1]-length(unique(DOC_Massey_database$scrubbed_uoa_code))
# No duplicate entries - the numbers match up before and after merging (i.e. ^ is 0)

# Howver, there are duplicate entries for NZCETA uoa_code (less unique entries than entries in the following table)
# which will need to be addressed before combining all of these guys into one file
dim(NZCETA_archive)[1]-length(unique(NZCETA_archive$scrubbed_uoa_code))
duplicatednames <- unique(NZCETA_archive$scrubbed_uoa_code[(duplicated(NZCETA_archive$scrubbed_uoa_code))])

# In NZCeTA the following Che Codes are present twice (within duplicatednames):
duplicatednames
#"Che04NZ15" "CheSIFP"  

# Looking at "Che04NZ15" first...
NZCETA_archive %>% filter(grepl("Che04NZ15",Code)) %>% select(Code)
# Che04NZ15 (H89/04)
# Che04NZ15 (H91/04)
# In the DOC_Massey_database, the second sample is called Che04NZ16, so modifying the scrubbed_uoa_code manually:
NZCETA_archive$scrubbed_uoa_code[(which(NZCETA_archive$Code=="Che04NZ15 (H91/04)"))] <- "CHE04NZ16"

# Adding column with reconciliation_notes information (to comment on name change, and for any further issues with other samples)
NZCETA_archive <- NZCETA_archive %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHE04NZ16","sample name changed based on DOC_Massey","NA"))

# In NZCETA, multiple samples have "CheSIFP" as their code. 
# In the "Other_Info" field, there are two individuals with H-codes that can be matched up to the DOC_Massey_database. 
# ****NOTE***** During a check of the date, it actually appears the Other_info column is not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("CheSIFP",Code)) %>% select(Code,Other_info)

# When I checked the DOC_Massey_database [using View(DOC_Massey_database)] there are individual samples that correspond to 
# these H codes: to H028 is CheTI12, and to H032 is CheTI13 (matching the TL given in the NZCeTA 'Other info' along with the H-code).
# However, in the NZCeTA record, CheTi12 and CheTi13 are *also* both present as their own samples
# Here's looking at the CheTi12/13 and CheSIFP samples in the NZCETA_archive database:
View(NZCETA_archive %>% filter(Code==duplicatednames[2] | Code=="CheTi12" | Code=="CheTi13") %>% arrange(Date_stranded))

# CheTi12 and CheTi13 each have a 'Date stranded' and 'Location' matching one of the "CheSIFP" samples, 
# but not the specific CheSIFP samples with the H codes mentioned previously. 
# The 'Collected by' category also doesn't match, but in the 'Collected by' category in the DOC/Massey database, 
# CheTI12 and CheTI13 have the two different collectors specified separated by "/". 
# I'm guessing this issue has been recognised previously, and that's why CheTI12 and CheTI13 have been 
# flagged this way in the 'DOC/Massey database'.

# I'm going to associate the 'Other_info' from the "CheSIFP" sample that match up on the H codes
# with the CheTI12/CheTI13 records (that have 'NA' in the Other_info column), but otherwise I'll use the 
# CETOS codes for the rest of the CheSIFPs as their scrubbed_uoa_code (just to give them a unique identifier). 
# However, I'll also add a column flagging them as 'uncertain record/tissue match up'. 
# To me, the most parsimonious explanation is that the 'Other info' and 'Who' fields were potentially 
# mixed up between these samples, but unlikely any of these samples will be used for this project
# anyway (just trying to solve discrepancies for the kaitiaki of the data)
# ****NOTE***** During a check of the date, it actually appears the Other_info column is not reliable (incomplete sort), so
# may need to redo these steps

# Pulling out the row and column indices for the Other_info column of CheTi12 and CheTi13
# ****NOTE***** During a check of the date, it actually appears the Other_info column is not reliable (incomplete sort), so
# may need to redo these steps
Other_info_column_index <- which(names(NZCETA_archive)=="Other_info")
CheTi13_row_index <- which(NZCETA_archive$Code=="CheTi13")
CheTi12_row_index <- which(NZCETA_archive$Code=="CheTi12")

# Replacing the 'Other_info' for these samples (which were NA) with the info from the CheSIFP samples that matches up to the DOC/Massey database
# ****NOTE***** During a check of the date, it actually appears the Other_info column is not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive[CheTi13_row_index,Other_info_column_index] <- as.character(NZCETA_archive[grep("H32",NZCETA_archive$Other_info),] %>% select(Other_info))
NZCETA_archive[CheTi12_row_index,Other_info_column_index] <- as.character(NZCETA_archive[grep("H28",NZCETA_archive$Other_info),] %>% select(Other_info))

# ****NOTE***** During a check of the date, it actually appears the Other_info column is not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive <- NZCETA_archive %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CheTi12","Other_info taken from CheSIFP with matching H-code",reconciliation_notes))
NZCETA_archive <- NZCETA_archive %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CheTi13","Other_info taken from CheSIFP with matching H-code",reconciliation_notes))

# There are two CheSIFP samples with no code(s) in the 'Other_info' column - first of these I've called 'NA', the second as 'NA2'
# ****NOTE***** During a check of the date, it actually appears the Other_info column is not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive <- NZCETA_archive %>% 
  rowwise() %>% 
  mutate(scrubbed_uoa_code=ifelse(Code=="CheSIFP",
                                  paste(Code,"_",gsub("from.*","NA2",gsub("/.*","",gsub(" ","",gsub(".* =","",Other_info)))),sep=""),
                                  scrubbed_uoa_code))

# Commenting on the CheSIFP ones being unreliable in the reconciliation_notes field
NZCETA_archive <- NZCETA_archive %>% mutate(reconciliation_notes=ifelse(Code=="CheSIFP","uncertain record/tissue match up",reconciliation_notes))

# We are getting closer! Only 77 duplicated records are suggested by the following expression:
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

# A quick scan of the records at the bottom of the next expression suggests inconsistent capitalization could be driving some of this:
full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Let's double check changing the case isn't going to lead to duplicates WITHIN our datasets
length(unique(NZCETA_archive$scrubbed_uoa_code)) - length(unique(toupper(NZCETA_archive$scrubbed_uoa_code)))
length(unique(DOC_Massey_database$scrubbed_uoa_code)) - length(unique(toupper(DOC_Massey_database$scrubbed_uoa_code)))

# Perfect, no problems with doing that, so let's do that conversion to upper case for both datasets
NZCETA_archive$scrubbed_uoa_code <- toupper(NZCETA_archive$scrubbed_uoa_code)
DOC_Massey_database$scrubbed_uoa_code <- toupper(DOC_Massey_database$scrubbed_uoa_code)

# We are getting closer! Only 65 duplicated records are suggested by the following expression:
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

# A quick scan of the records at the bottom of the next expression suggests inconsistent leading 0s in the sample codes could be driving this
full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Starting to chase down the issues, first by focusing on samples prefixed with "CHEWC":
full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% filter(grepl("CHEW",scrubbed_uoa_code)) %>% print(n=47)

# It seems that in NZCETA, there aren't as many leading zeroes as in the DOC_Massey database, so we'll need to tweak that to standardize things.
# We are excluding the sample that is just "CHEWC" without any code (by !grepl("^CHEWC$"...) and any code with "-" in it, because these seem to match up)
NZCETA_archive <- NZCETA_archive %>% mutate(scrubbed_uoa_code=ifelse(grepl("CHEWC",toupper(Code)),ifelse(!grepl("-",Code),ifelse(!grepl("^CHEWC$",toupper(Code)),ifelse(nchar(Code)<8,paste("CHEWC",rep(0,9-nchar(Code)),gsub("CHEWC","",Code, ignore.case = T),sep=""),scrubbed_uoa_code),scrubbed_uoa_code),scrubbed_uoa_code),scrubbed_uoa_code))

# However, there was a sample that this didn't work for (CheWC12), looks like it might be because it had trailing spaces (larger number of characters)
# First going to make sure that removing trailing spaces isn't going ot lead to duplicates WITHIN our datasets
length(unique(NZCETA_archive$scrubbed_uoa_code)) - length(unique(toupper(gsub(" *$","",NZCETA_archive$scrubbed_uoa_code))))
length(unique(DOC_Massey_database$scrubbed_uoa_code)) - length(unique(toupper(gsub(" *$","",DOC_Massey_database$scrubbed_uoa_code))))

# Perfect, no problems with doing that, so let's do that conversion to remove trailing spaces
NZCETA_archive$scrubbed_uoa_code <- gsub(" *$","",NZCETA_archive$scrubbed_uoa_code)
DOC_Massey_database$scrubbed_uoa_code <- gsub(" *$","",DOC_Massey_database$scrubbed_uoa_code)

# That still didn't work for the problematic sample (CHEWC12). After pulling it up, it turns out it had two sample names, separated by "/ " in the code field
# Double checked to make sure that this was the only sample that fell into this category, and yup, it was
NZCETA_archive$Code[(which(grepl("/ ",NZCETA_archive$Code)))]

# Need to capture this info into an alternative ID field.
NZCETA_archive <- NZCETA_archive %>% mutate(reconciled_alternate_code=ifelse(grepl("/ ",Code),gsub(".*/ ","",Code),"NA"))

# Finally, manually tweak the scrubbed_uoa_code entry for that sample
NZCETA_archive$scrubbed_uoa_code[(which(grepl("/ ",NZCETA_archive$Code)))] <- "CHEWC012"

# Alright, let's see where we are in terms of duplicates between datasets. Still 47 to go!
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

# Let's check out the list to see what other problem children we might have:
full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Looks like we have similar problems to CHEWC with CHENI, also, it looks like "9362" in DOC_Massey should be CHE9362 to match up
# and finally, CHETW in DOC_Massey looks to be equivalent to CHETWW in NZCETA_archive. We'll get to fixing those here:
NZCETA_archive <- NZCETA_archive %>% mutate(scrubbed_uoa_code=ifelse(grepl("CHENI",toupper(Code)),ifelse(!grepl("-",Code),ifelse(!grepl("^CHENI$",toupper(Code)),ifelse(nchar(Code)<8,paste("CHENI",rep(0,9-nchar(Code)),gsub("CHENI","",Code, ignore.case = T),sep=""),scrubbed_uoa_code),scrubbed_uoa_code),scrubbed_uoa_code),scrubbed_uoa_code))

DOC_Massey_database <- DOC_Massey_database %>% mutate(scrubbed_uoa_code=ifelse(grepl("9362",UoA_Code),"CHE9362",scrubbed_uoa_code))

DOC_Massey_database <- DOC_Massey_database %>% mutate(scrubbed_uoa_code=ifelse(grepl("CHETW",scrubbed_uoa_code),gsub("CHETW","CHETWW",scrubbed_uoa_code),scrubbed_uoa_code))

# Alright, let's see where we are in terms of duplicates between datasets. 31 to go!
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]
full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# I think we are now getting to the stage where seeing if we can match up additional fields might be the way to go (e.g. H-codes etc)
# First up, Che07NZ01
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="Che07NZ01")])))]
#"Che06NZ10 (H131/06; U07-007)"
# All other information matches up between the two databases, so going to go with CHE07NZ01 as the scrubbed_uoa_code for NZCETA, and Che06NZ10 as the alternative code
NZCETA_archive$scrubbed_uoa_code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="Che07NZ01")])))] <- "CHE07NZ01"
NZCETA_archive$reconciled_alternate_code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="Che07NZ01")])))] <- "Che06NZ10"
# We'll tweak the reconciliation_notes column in the NZCETA sheet for this sample
NZCETA_archive <- NZCETA_archive %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHE07NZ01","sample name changed based on DOC_Massey",reconciliation_notes))

# Next up, CheBP15
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheBP15")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheBP15")]
# CETOS 9578, so going to search for "9578" in any field in NZCETA
grep("9578",NZCETA_archive)
#integer(0)
# So still no dice. Let's filter NZCETA on aspects of the sample given in the DOC_Massey_database.
# We'll search for '94' as well as 1994. 
NZCETA_archive %>% filter(grepl("1994|94",Date_stranded)|grepl("1994|94",Date_received)) %>% select(Code,Location,Region,Ocean)
# Checking the resultant codes against DOC_Massey, because if they are already present in this database, then
# we won't be able to match them up to any of the above codes anyway (so no need to drill more into these potentials)
DOC_Massey_database %>% filter(scrubbed_uoa_code %in% as.matrix(NZCETA_archive %>% filter(grepl("1994|94",Date_stranded)|grepl("1994|94",Date_received)) %>% select(scrubbed_uoa_code)))
# All of the above scrubbed_uoa_code are already present in the DOC_Massey dataset. Now, we'll look for Pegasus Bay samples
# that might have a similar date associated with them:
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("egasus",Location)|grepl("egasus",Other_info)|grepl("egasus",Archiving_info)) 
# Only potentials are two samples with 'NA' associated with them. Let's check if they are already present in the DOC_Massey dataset
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
DOC_Massey_database %>% filter(scrubbed_uoa_code %in% as.matrix(NZCETA_archive %>% filter(grepl("egasus",Location)|grepl("egasus",Other_info)|grepl("egasus",Archiving_info)) %>% filter(is.na(Date_stranded)) %>% select(scrubbed_uoa_code)))
# All of the above scrubbed_uoa_code are already present in the DOC_Massey dataset, so this is one sample we cannot
# reconcile back to NZCETA. Instead, we'll tweak the  reconciliation_notes column to the DOC_Massey dataset about this sample
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHEBP15","sample not found in NZCETA","NA"))

# That was a long one! Let's hope we are luckier on the next one. Also, now we are looking for the following to be 1 not 0, as 
# we know there is one in the DOC_Massey database not present in the NZCETA. It is still at 30 :)
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up, we have CheBP17
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheBP17")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheBP17")]
# CETOS 9580, so going to search for "9580" in any field in NZCETA
grep("9580",NZCETA_archive)
#integer(0)
# So still no dice. Let's filter NZCETA on aspects of the sample given in the DOC_Massey_database.
# We'll search for '93' as well as 1993. 
NZCETA_archive %>% filter(grepl("1993|93",Date_stranded)|grepl("1993|93",Date_received)) %>% select(Code,Location,Region,Ocean)
# Checking the resultant codes against DOC_Massey, because if they are already present in this database, then
# we won't be able to match them up to any of the above codes anyway (so no need to drill more into these potentials)
DOC_Massey_database %>% filter(scrubbed_uoa_code %in% as.matrix(NZCETA_archive %>% filter(grepl("1993|93",Date_stranded)|grepl("1993|93",Date_received)) %>% select(scrubbed_uoa_code)))
# All of these samples are in DOC_Massey except for CheDu03 Waitaki River Mouth Otago      Pacific, which doesn't seem like a 
# likely match because it is from Otago. This sample (as for CheBP15) is also from Pegasus Bay, so no need to redo the code 
# looking at location, because we know all those samples are already found in the DOC_Massey file.
#Instead, we'll add a reconciliation_notes column to the DOC_Massey dataset about this sample
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHEBP17","sample not found in NZCETA",reconciliation_notes))

#Also, now we are looking for the following to be 2 not 1, as 
# we know there are two in the DOC_Massey database not present in the NZCETA. It is still at 30 :)
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up, we have CheBP18
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheBP18")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheBP18")]
# CETOS 9582, so going to search for "9582" in any field in NZCETA
grep("9582",NZCETA_archive)
#integer(0)
# So still no dice. As for CheBP15, this sample was taken in 1994 from Pegasus Bay
# so we are going to fall into the same gaps as for that sample. We can just go
# ahead and mention that CHEP18 is not found in NZCETA
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHEBP18","sample not found in NZCETA",reconciliation_notes))

#Also, now we are looking for the following to be 3 not 2, as 
# we know there are three in the DOC_Massey database not present in the NZCETA. It is still at 30 :)
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up, we have CheCt01-04
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheCt01-04")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheCt01-04")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date was 24-Dec-01, Location Timaru. Let's do some combos of 24-Dec-01
# and see what we fish up
NZCETA_archive %>% filter(grepl("24",Date_stranded)|grepl("24",Date_received)) %>% select(Code,Location,Region,Ocean)
# Bingo! There is a "CheCb01-04" with a Date_stranded of 24/12/2001, and Location
# of Timaru. Given the similarity of names, guessing that these are the same sample.
# Will go with DOC_Massey's name, because CB generally = Cloudy Bay, and Timaru !=
# Cloudy Bay
NZCETA_archive$scrubbed_uoa_code[which(NZCETA_archive$scrubbed_uoa_code=="CHECB01-04")] <- "CHECT01-04"

NZCETA_archive <- NZCETA_archive %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHECT01-04","sample name changed based on DOC_Massey",reconciliation_notes))

# Down to 29! Only 26 more to go (because there are three in the DOC_Massey database not 
# present in the NZCETA. 
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up, we have "CheCt03-01"
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheCt03-01")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheCt03-01")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date was 15-Feb-03, Location Banks Peninsula. Let's grep 15 in the date
# fields and see what we fish up.
NZCETA_archive %>% filter(grepl("15",Date_stranded)|grepl("15",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean)
# Alas, no great match. We'll now try looking by Location
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("anks",Location)|grepl("anks",Other_info)|grepl("anks",Archiving_info)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean)
# No close matches here etiher, so we will just go ahead and mention that CheCt03-01 is not found in NZCETA
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHECT03-01","sample not found in NZCETA",reconciliation_notes))

# Down to 29! Only 25 more to go (because there are four in the DOC_Massey database not 
# present in the NZCETA. 
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up, we have "CheCt04-01"
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheCt04-01")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheCt04-01")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date was 30-Jan-04, location was Canterbury. Let's grep 30 in the date
# fields and see what we fish up.
NZCETA_archive %>% filter(grepl("30",Date_stranded)|grepl("30",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean)
# The following could be a match, Going to check whether it's H-code or sample name 
# matches up to any existing samples in the Massey Database
# Che04NZ06 (H67/03)           30/01/2004    NA            Sumner Beach                       NA         NA
DOC_Massey_database %>% filter(scrubbed_uoa_code=="CHE04NZ06")
# No matching names, but there is a Che04NZ07 (diff H-code though) sampled on same date from same location
# Checking H-code....
DOC_Massey_database %>% filter(grepl("H67",pathology_code)|grepl("H67",Doc_incident_no)|grepl("H067",pathology_code)|grepl("H067",Doc_incident_no)) %>% select(UoA_Code,Collection_Date,Location)
# No matching H-code in the DOC_Massey_database
# Tentatively, I think that "CheCt04-01" and "Che04NZ06" could be the same sample. Will reconcile Massey's
# scrubbed_uoa_code to "CHE04NZ06" but will make a note that not 100% on this one.
# The reason I'm not as confident on this match because the Massey location was 'Canterbury' and NZCeTA 
# 'Sumner Beach', and because CHE04NZ07 has same sample location and date (but already had 
# an entry in both files).
DOC_Massey_database$scrubbed_uoa_code[which(DOC_Massey_database$scrubbed_uoa_code=="CHECT04-01")] <- "CHE04NZ06"

DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHE04NZ06","sample name changed based on NZCETA, but not 100% confident",reconciliation_notes))

# Down to 28! Only 24 more to go (because there are four in the DOC_Massey database not 
# present in the NZCETA. 
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up, we have "CheKK03"
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheKK03")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheKK03")]
# CETOS 9577, so going to search for "9577" in any field in NZCETA
grep("9577",NZCETA_archive)
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 1994, location Kaikoura
NZCETA_archive %>% filter(grepl("94",Date_stranded)|grepl("94",Date_received)|grepl("1994",Date_stranded)|grepl("1994",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code)
# CheKk01 and CheKk02 are matched on date/location, but are found in DOC_Massey
# under these names already. I'll just double check that the other Canterbury samples
# are already in DOC_Massey
DOC_Massey_database %>% filter(scrubbed_uoa_code=="CHEBP01"|scrubbed_uoa_code=="CHEBP02"|scrubbed_uoa_code=="CHEBP03"|scrubbed_uoa_code=="CHETI03"|scrubbed_uoa_code=="CHE9362")
# And yup, they are. OK, we'll try looking just for Kaikoura.
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("aikoura",Location)|grepl("aikoura",Other_info)|grepl("aikoura",Archiving_info)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean,scrubbed_uoa_code)
# OK, double checking all of these are already in the DOC Massey database too:
DOC_Massey_database %>% filter(scrubbed_uoa_code=="CHE03NZ01"|scrubbed_uoa_code=="CHE03NZ02"|scrubbed_uoa_code=="CHEKK")
# CHEKK is the only one that didn't turn up in DOC_Massey. It has a Massey code
# WS97.59Ch so looking this up to see if it already has an entry. It does, CheNI017,
# (i.e. a Māui dolphin), so not likely to be our missing Kaikoura individual.
# The Other_info seems to be incorrect for CHEKK in NZCETA so will note this too
# So we will just go ahead and mention that "CHEKK03" is not found in NZCETA
# ****NOTE***** During a check of the date, it actually appears the Other_info column is not reliable (incomplete sort), so
# may need to redo these steps
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHEKK03","sample not found in NZCETA",reconciliation_notes))
NZCETA_archive <- NZCETA_archive %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHEKK","Other_info field seems to be incorrect based on DOC_Massey",reconciliation_notes))

# Only 23 more to go (because there are five in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows left)
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up, we have "CheNI011"
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheNI011")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheNI011")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 1921 (...from a stuffed dolphin!)
NZCETA_archive %>% filter(grepl("1921",Date_stranded)|grepl("1921",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code)
# No results - will now do a grep for "tuffed"
grep("tuffed",NZCETA_archive)
# No results - prepared to call this one as unlikely to be present in NZCeTA
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHENI011","sample not found in NZCETA",reconciliation_notes))

# Only 22 more to go (because there are six in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows left)
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up, we have "CheNI020"
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheNI020")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheNI020")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 1-Jan-67, Location: Waikanae (wrongly Oakura in Baker et al 2002)
# Museum sample, so likely to end up similar to the previous sample is my guess
NZCETA_archive %>% filter(grepl("1967",Date_stranded)|grepl("1967",Date_received)|grepl("67",Date_stranded)|grepl("67",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code)
# And yup, no records. OK, we'll try looking just for Waikanae and/or Oakura
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("aikanae",Location)|grepl("aikanae",Other_info)|grepl("aikanae",Archiving_info)|grepl("akura",Location)|grepl("akura",Other_info)|grepl("akura",Archiving_info)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean,scrubbed_uoa_code)
# No joy there either. I think it is likely this sample was never part of NZCeTA
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHENI020","sample not found in NZCETA",reconciliation_notes))

# Only 21 more to go (because there are seven in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows left) 
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have "CheNI021 (KTUW2)"
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheNI021")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheNI021")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 1999, location Manukau Habour, Franz code KTUW2
# Also not going to be surprised if we can't find this one in NZCeTA as it was stomach 
# contents (I double checked and "99" didn't result in addtional sane matches)
NZCETA_archive %>% filter(grepl("1999",Date_stranded)|grepl("1999",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code) %>%  print(n=61)
# Only CHE99NZ01 and CHEWP?? are potentials
DOC_Massey_database %>% filter(scrubbed_uoa_code=="CHE99NZ01" | scrubbed_uoa_code=="CHEWP??")
# Neither sample is in DOC_Massey, but CHE99NZ01 has info CETOS 8737, monz2002
# Not enough information was present for CHEWP?? for a positive ID either way.
grep("8737",DOC_Massey_database$CETOS_code)
# The plot thickens - that CETOS record and MONZ2002 record is actually recorded
# for CheBP12 in DOC_Massey_database. Nothing else matches up between those samples
# though. I'll come back to this conundrum below. In the meantime...
# KTUW 1, 2, 3 (NZCETA) and CheNZ021 (KTUW) (DOC_Massey) might be the same sample, but they # have vastly different dates (06/11/1997 vs 1999)
# In terms of "CheNI021": happy to say it isn't in NZCeTA (spesh as a stomach sample)
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHENI021","sample not found in NZCETA",reconciliation_notes))

# Alright in terms of sorting the others out
# In NZCETA, CHE99NZ01 has the following Other_info: "from cards on Kirsty's desk, Franz's 
# notes gillnet alternate code = CETOS 8737, monz2002"
# In the DOC_Massey file, CheBP12 has MONZ 2002/CETOS 8737
# In NZCETA, CheBP12 has the following Other_info "from cards on Kirsty's desk, Franz's 
# notes netmarks weight 28.2kg alternate code = CETOS 98114"
# In the DOC_Massey file, CheBP53 has CETOS 98114
# In NZCETA, CheBP53 has no CETOS code, but has an H-Code of H15. This matches up to the 
# DOC_Massey file.
# Suggested solution: in the NZCETA database, the Other_info associated with Che99NZ01 
# should be transferred to CheBP12, and the Other_info for CheBP12 should be transferred to # CheBP53. This should be flagged in reconciliation notes.
# ****NOTE***** During a check of the date, it actually appears the Other_info column is not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive$Other_info[which(NZCETA_archive$scrubbed_uoa_code=="CHEBP53")] <- NZCETA_archive$Other_info[which(NZCETA_archive$scrubbed_uoa_code=="CHEBP12")] 
NZCETA_archive <- NZCETA_archive %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHEBP53","Other_info originally associated with CheBP12, changed based on DOC_Massey",reconciliation_notes))
NZCETA_archive$Other_info[which(NZCETA_archive$scrubbed_uoa_code=="CHEBP12")] <- NZCETA_archive$Other_info[which(NZCETA_archive$scrubbed_uoa_code=="CHE99NZ01")] 
NZCETA_archive <- NZCETA_archive %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHEBP12","Other_info originally associated with CHE99NZ01, changed based on DOC_Massey",reconciliation_notes))
NZCETA_archive <- NZCETA_archive %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHE99NZ01","CETOS 8737, monz2002 also associated with CHEBP12",reconciliation_notes))

# Phew. OK. Only 20 more to go (because there are eight in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra) 
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have CheNI030
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheNI030")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheNI030")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date of 5-Mar-00, Location Albatross Bay, Kawhia (Te Maka), Waikato
NZCETA_archive %>% filter(grepl("5",Date_stranded)| grepl("5",Date_received)) %>% filter(!grepl("-05",Date_received)) %>% filter(!grepl("2005",Date_stranded)) %>% filter(!grepl("2004",Date_stranded))  %>% filter(!grepl("2007",Date_stranded)) %>% filter(!grepl("2008",Date_stranded)) %>% filter(!grepl("2009",Date_stranded)) %>% filter(!grepl("2010",Date_stranded)) %>% filter(!grepl("2012",Date_stranded)) %>% filter(!grepl("1985",Date_stranded)) %>% filter(!grepl("1995",Date_stranded))  %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code)
# No good candidates based on date. Let's try location
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("lbatross",Location)|grepl("lbatross",Other_info)|grepl("lbatross",Archiving_info)|grepl("awhia",Location)|grepl("awhia",Other_info)|grepl("awhia",Archiving_info)|grepl("aka",Location)|grepl("aka",Other_info)|grepl("aka",Archiving_info)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean,scrubbed_uoa_code)
# No good candidates here either
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHENI030","sample not found in NZCETA",reconciliation_notes))

# Only 19 more to go (because there are nine in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)  
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have CheNI057
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheNI057")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheNI057")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 5-Mar-01, Location Karioitahi Beach, South Auckland
NZCETA_archive %>% filter(grepl("5",Date_stranded)| grepl("5",Date_received)) %>% filter(!grepl("-05",Date_received)) %>% filter(!grepl("2005",Date_stranded)) %>% filter(!grepl("2004",Date_stranded))  %>% filter(!grepl("2007",Date_stranded)) %>% filter(!grepl("2008",Date_stranded)) %>% filter(!grepl("2009",Date_stranded)) %>% filter(!grepl("2010",Date_stranded)) %>% filter(!grepl("2012",Date_stranded)) %>% filter(!grepl("1985",Date_stranded)) %>% filter(!grepl("1995",Date_stranded))  %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code)
# No good candidates based on date. Let's try location
# Based on some findings below, trying alternative spellings of "Karioitahi"
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("arioitahi",Location)|grepl("arioitahi",Other_info)|grepl("arioitahi",Archiving_info)|grepl("ariotahi",Location)|grepl("ariotahi",Other_info)|grepl("ariotahi",Archiving_info)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean,scrubbed_uoa_code)
# ONly two candidates who do not match up based on year.
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHENI057","sample not found in NZCETA",reconciliation_notes))

# Only 18 more to go (because there are ten in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)   
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have "CheNI058"
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheNI058")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheNI058")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 27-May-01, Location Karioitahi Beach, South Auckland
NZCETA_archive %>% filter(grepl("27",Date_stranded)|grepl("27",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code) 
# None of htese match up as they are South Island samples. Didn't get any matches for CheNI057 which had same location and year
# so we don't need to bother doing that.
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHENI058","sample not found in NZCETA",reconciliation_notes))

# Only 17 more to go (because there are eleven in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)   
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have "CheNI059"
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheNI059")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheNI059")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 22-Jul-01, location Port Waikato, South Auckland
NZCETA_archive %>% filter(grepl("22",Date_stranded)|grepl("22",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code) 
# None of htese match up as they are South Island samples. Let's double check location.
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("aikato",Location)|grepl("aikato",Other_info)|grepl("aikato",Archiving_info)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean,scrubbed_uoa_code)
# No great date matches
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHENI059","sample not found in NZCETA",reconciliation_notes))

# Only 16 more to go (because there are twelve in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)   
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have "CheNI060"
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheNI060")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheNI060")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 3-Feb-02, Location Manukau Bar, 6 miles off Manukau Harbour Entrance on 61-65 metre mark (Manukau Heads -at sea)
NZCETA_archive %>% filter(grepl("3",Date_stranded)|grepl("3",Date_received)) %>% filter(!grepl("2003",Date_stranded)) %>% filter(!grepl("2004",Date_stranded)) %>% filter(!grepl("2005",Date_stranded)) %>% filter(!grepl("2006",Date_stranded)) %>% filter(!grepl("2007",Date_stranded)) %>% filter(!grepl("2008",Date_stranded)) %>% filter(!grepl("2009",Date_stranded)) %>% filter(!grepl("201",Date_stranded))  %>% filter(!grepl("199",Date_stranded)) %>% filter(!grepl("198",Date_stranded)) %>% filter(!grepl("2003",Date_stranded))%>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code)
# None of htese match up as they are South Island samples. Let's double check location.
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("anukau",Location)|grepl("anukau",Other_info)|grepl("anukau",Archiving_info)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean,scrubbed_uoa_code)
# No good matches based on date
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHENI060","sample not found in NZCETA",reconciliation_notes))

# Only 15 more to go (because there are 13 in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)   
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have "CheNI061"
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheNI061")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheNI061")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 21-Feb-02, location 35 metre mark, NW Wattle Bay, Manukau Harbour (floating)
NZCETA_archive %>% filter(grepl("21",Date_stranded)|grepl("21",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code) 
# None match up on rough date plus location. Let's check location (using Wattle Bay, because we checked Manukau previously
# and didn't get a good match for 2002.
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("attle",Location)|grepl("attle",Other_info)|grepl("attle",Archiving_info)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean,scrubbed_uoa_code)
# No matches
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHENI061","sample not found in NZCETA",reconciliation_notes))

# Only 14 more to go (because there are 14 in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)   
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have "CheNI062"
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheNI062")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheNI062")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 21-Feb-02, Location North of Karioitahi Beach entrance, Waiuku, Manukau Harbour
NZCETA_archive %>% filter(grepl("21",Date_stranded)|grepl("21",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code) 
# None match up on rough date plus location. Let's try searching using Waiuku, because previously no results for "arioitahi"
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("aiuku",Location)|grepl("aiuku",Other_info)|grepl("aiuku",Archiving_info)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean,scrubbed_uoa_code)
# Interesting! Match for Kariotahi (cf. Karioitahi). Let's add that as an alternative spelling for the previous sample too.
# Still didn't add any matches to our list though, and definitely no match for this one (only same location samples from 2006)
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHENI062","sample not found in NZCETA",reconciliation_notes))

# Only 13 more to go (because there are 15 in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)  
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have "CheNI092"
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheNI092")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheNI092")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 4-Jun-03, Location O'Neils Beach, north of Bethell's Beach
NZCETA_archive %>% filter(grepl("4",Date_stranded)|grepl("4",Date_received)) %>% filter(!grepl("200[4-9]", Date_stranded))  %>% filter(!grepl("201", Date_stranded)) %>% filter(!grepl("199", Date_stranded)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code) 
# Only CHE03NZ06 and CHEBP04 potentials (but BP probs = Banks Peninsula).
# After checking, nope, neither are from the North Island
# Let's try searching using "eils" and "ethell"
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("eils",Location)|grepl("eils",Other_info)|grepl("eils",Archiving_info)|grepl("ethell",Location)|grepl("ethell",Other_info)|grepl("ethell",Archiving_info)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean,scrubbed_uoa_code)
# Nope, only South Island locations
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHENI092","sample not found in NZCETA",reconciliation_notes))

# Only 12 more to go (because there are 16 in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)   
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have CheSI29
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheSI29")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheSI29")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 25-Feb-01, Location Westport 1
NZCETA_archive %>% filter(grepl("25",Date_stranded)|grepl("25",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code) 
# None match up on rough date plus location
# Let's try searching using "Westport"
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("estport",Location)|grepl("estport",Other_info)|grepl("estport",Archiving_info)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean,scrubbed_uoa_code)
# No similar dates
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHESI29","sample not found in NZCETA",reconciliation_notes))

# Only 11 more to go (because there are 17 in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)   
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have CheSI30
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheSI30")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheSI30")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 25-Feb-01, Location Westport 2
NZCETA_archive %>% filter(grepl("25",Date_stranded)|grepl("25",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code) 
# None match up on rough date plus location. A rerun of the previous "Westport" code, shows no similar dates
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHESI30","sample not found in NZCETA",reconciliation_notes))

# Only 10 more to go (because there are 18 in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)  
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have CheSI52
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheSI52")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheSI52")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 23-Jan-01, Locaiton Timaru, Rangitata River
NZCETA_archive %>% filter(grepl("23",Date_stranded)|grepl("23",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code) 
# CHESI28 looks like it could be a match - same location and date. However, CheSI28 is present, and has 
# an H-Code (H38) in both DOC_Massey and NZCETA, that matches up between DOC_Massey and NZCeTA
# I suspect CheSI52 is a duplicate of CheSI28, but cannot confirm this for certain. 
# NZCETA's notes for CheSI28 mention that it is only a skeleton. Going to check location and then
# add a note to say suspected duplicate if nothing interesting pulled up
# Let's try searching using "Westport"
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("angitata",Location)|grepl("angitata",Other_info)|grepl("angitata",Archiving_info)|grepl("imaru",Location)|grepl("imaru",Other_info)|grepl("imaru",Archiving_info)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean,scrubbed_uoa_code)
# No close dates
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHESI52","sample not found in NZCETA, but suspected duplicate of CHESI28",reconciliation_notes))

# Only 9 more to go (because there are 19 in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)   
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have CheTI04
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheTI04")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheTI04")]
# CETOS code! "CETOS 9581"
grep("9581",NZCETA_archive)
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 1995, Location Timaru
NZCETA_archive %>% filter(grepl("95",Date_stranded)|grepl("95",Date_received)|grepl("1995",Date_stranded)|grepl("1995",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code) 
# CHETI08, CHETI09, and CHETI10 could all be consistent. However the DOC_Massey database
# already has entries for them.
# Let's try searching using "Timaru"
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("imaru",Location)|grepl("imaru",Other_info)|grepl("imaru",Archiving_info)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean,scrubbed_uoa_code)
# Only the previously mentioned TI08 and TI10 fit the dates.
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHETI04","sample not found in NZCETA",reconciliation_notes))

# Only 8 more to go (because there are 20 in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)  
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have CheTI05
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheTI05")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheTI05")]
# "CETOS 9583"
grep("9583",NZCETA_archive)
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 1995, Location Timaru
NZCETA_archive %>% filter(grepl("95",Date_stranded)|grepl("95",Date_received)|grepl("1995",Date_stranded)|grepl("1995",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code) 
# CHETI08, CHETI09, and CHETI10 could all be consistent. However the DOC_Massey database
# already has entries for them. We previously checked Timaru on the last sample
# and no 1995 Timaru samples other than TI08-10 were present
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHETI05","sample not found in NZCETA",reconciliation_notes))

# Only 7 more to go (because there are 21 in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)   
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have CheTI06
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheTI06")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheTI06")]
# "CETOS 9584"
grep("9584",NZCETA_archive)
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 1995, Location Timaru
NZCETA_archive %>% filter(grepl("95",Date_stranded)|grepl("95",Date_received)|grepl("1995",Date_stranded)|grepl("1995",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code) 
# CHETI08, CHETI09, and CHETI10 could all be consistent. However the DOC_Massey database
# already has entries for them. Same as the previous two samples for searching by location.
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHETI06","sample not found in NZCETA",reconciliation_notes))

# Only 6 more to go (because there are 22 in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)   
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have CheTW01
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheTW01")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheTW01")]
# "CETOS 8507"
grep("8507",NZCETA_archive)
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 1985, Te Waewae Bay
NZCETA_archive %>% filter(grepl("85",Date_stranded)|grepl("85",Date_received)|grepl("1985",Date_stranded)|grepl("1985",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code) 
# None of the three samples that come up are a match based on geography
# Let's do a check based on Te Waewae
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("aewae",Location)|grepl("aewae",Other_info)|grepl("aewae",Archiving_info)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean,scrubbed_uoa_code)
# No 1985 samples
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHETWW01","sample not found in NZCETA",reconciliation_notes))

# Only 5 more to go (because there are 23 in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra) 
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have CheTW02
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheTW02")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheTW02")]
# "CETOS 8728"
grep("8728",NZCETA_archive)
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 1986, Te Waewae Bay
NZCETA_archive %>% filter(grepl("86",Date_stranded)|grepl("86",Date_received)|grepl("1986",Date_stranded)|grepl("1986",Date_received)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code) 
# Only sample that came up from 1986 was sampled from Canterbury.
# Rerunning the previous samples Te Waewae location code didn't help either
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHETWW02","sample not found in NZCETA",reconciliation_notes))

# Only 4 more to go (because there are 24 in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)  
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have CheWC03-05 
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheWC03-05")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheWC03-05")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 2-May-07, Westport
NZCETA_archive %>% filter(grepl("2",Date_stranded)|grepl("2",Date_received)) %>% filter(!grepl("200[0-6]",Date_stranded)) %>% filter(!grepl("200[8-9]",Date_stranded)) %>% filter(!grepl("201",Date_stranded))  %>% filter(!grepl("19[8-9]",Date_stranded)) %>%  select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code) %>% print(n=54)
# No strong candidates jumping out based on date. Checking location:
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_archive %>% filter(grepl("estport",Location)|grepl("estport",Other_info)|grepl("estporti",Archiving_info)) %>% select(Code,Date_stranded,Date_received,Location,Region,Ocean,scrubbed_uoa_code)
# No close matches on date here either
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHEWC03-05","sample not found in NZCETA",reconciliation_notes))

# Only 3 more to go (because there are 25 in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)   
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have CheWC132
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="CheWC132")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="CheWC132")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# Collection date 1992, Greymouth
NZCETA_archive %>% filter(grepl("1992",Date_stranded)|grepl("1992",Date_received)|grepl("92",Date_stranded)|grepl("92",Date_received)) %>%  select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code)
# CHEWC04 and CHEWC02-06 could both be matches. However, both of these sammples already
# exist in both sheets, so no dice. A search for Westport (see above) also turns up
# CheWC05, but it is also already present in both datasheets
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHEWC132","sample not found in NZCETA",reconciliation_notes))

# Only 2 more to go (because there are 26 in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)  
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have (WB04-03)
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="(WB04-03)")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="(WB04-03)")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
# 2004, and no location. Not going to be enough to go on. Let's try grepping
# the Massey code, otherwise we'll just call it not being in NZCeTA.
grep("WB04-03",NZCETA_archive)
# No dice
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="(WB04-03)","sample not found in NZCETA",reconciliation_notes))

# Only 1 more to go (because there are 26 in the DOC_Massey database not 
# present in the NZCETA, and 28 total rows extra)  
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code") %>% select(UoA_Code,Code,scrubbed_uoa_code) %>% print(n=400)

# Next up we have (WS99-22Ch)
# Following gives no result. From running subsets of the following code, it appears there is no H-code to index on.
NZCETA_archive$Code[(which(NZCETA_archive$pathology_code==(DOC_Massey_database$pathology_code[which(DOC_Massey_database$UoA_Code=="(WS99-22Ch)")])))]
# Trying on CETOS code instead.
DOC_Massey_database$CETOS_code[which(DOC_Massey_database$UoA_Code=="(WS99-22Ch)")]
# NA, so nothing to match up on there either. Let's check the DOC_Massey_database
# for any other information that might help them match up.
#Collection date 9 Jul 1999 (23 Jul 99-WendiRoe), Location North Coast Arawhata River, Haast
NZCETA_archive %>% filter(grepl("1999",Date_stranded)|grepl("1999",Date_received)|grepl("99",Date_stranded)|grepl("99",Date_received)) %>% filter(!grepl("199[0-8]",Date_stranded)) %>%  select(Code,Date_stranded,Date_received,Location,Region,Ocean, scrubbed_uoa_code)
# BOOM! Finally, a match! CHE99NZ01 has same stranding date and location
# Going to further check each of those before giving (WS99-22Ch) CHE99NZ01 as its
# scrubbed_uoa_code. All good to combine. No conflicting information.
DOC_Massey_database$scrubbed_uoa_code[which(DOC_Massey_database$scrubbed_uoa_code=="(WS99-22CH)")] <- "CHE99NZ01"
DOC_Massey_database <- DOC_Massey_database %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code=="CHE99NZ01","sample renamed based on NZCETA",reconciliation_notes))

# Finally, we've reconciled the names! 27 extra records are in DOC_Massey_database that are not present
# in NZCETA. Next job will be to combine them and check that we don't have obvious duplicates
# based on stranding dates, and that there is consistent information from the two different sheets
dim(full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code"))[1] - dim(NZCETA_archive)[1]

NZCETA_DOC_joined <- full_join(NZCETA_archive,DOC_Massey_database,by = "scrubbed_uoa_code")

# Going to first double check that all the samples we dealt with above are all good
# and have notes associated in the reconciliation_notes section for them
# This tibble should be 48 rows long
NZCETA_DOC_joined %>% filter(reconciliation_notes.x!="NA"|reconciliation_notes.y!="NA") %>% select(Code,UoA_Code,scrubbed_uoa_code,reconciliation_notes.x,reconciliation_notes.y) %>%  print(n=48)

# Further checking the CHETI12 and CHETI13 have Other_info propogated, that all CHESIFP hve notes on uncertain record/tissue match up, and that all NAs for 'Code' have reconciliation_notes explanation
# ****NOTE***** During a check of the date, it actually appears the Other_info column is not reliable (incomplete sort), so
# may need to redo these steps
NZCETA_DOC_joined %>% filter(scrubbed_uoa_code=="CHETI12"|scrubbed_uoa_code=="CHETI13"|Code=="CheSIFP"|Code=="NA"|is.na(Code)) %>% select(Code,UoA_Code,Other_info,reconciliation_notes.x,reconciliation_notes.y) %>% print(n=40)

# Before proceeding, going to to make a combined reconciliation_notes column, combining the two different columns
# and then deleting the two separate columns from NZCETA_DOC_joined
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(((reconciliation_notes.x=="NA"|is.na(reconciliation_notes.x))&(reconciliation_notes.y=="NA"|is.na(reconciliation_notes.y))),"NA",
                                                         ifelse((reconciliation_notes.x=="NA"|is.na(reconciliation_notes.x)),reconciliation_notes.y,
                                                                ifelse((reconciliation_notes.y=="NA"|is.na(reconciliation_notes.y)),reconciliation_notes.x,paste(reconciliation_notes.x,reconciliation_notes.y,sep=";"))))) %>% 
  select(-reconciliation_notes.x,-reconciliation_notes.y)

# Also going to get rid of columns that should exist solely of NAs due to parsing errors when originally importing file
# First double check that these are actually all NAs...
unique(NZCETA_DOC_joined$X13)
unique(NZCETA_DOC_joined$X21)
# Yup, they are all NAs, so...
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% select(-X13,-X21)
# Alright, ready to check things out based on other fields.

###################################################################
#### 5. Consolidating additional columns in  NZCETA_DOC_joined ####
###################################################################
# There is duplicate information in NZCETA_DOC_joined that came in separately from DOC_Massey and NZCETA
# Here, we are going to check some of the different columns have the same information and combine them
# if so.

# First fields we are going to look at are the pathology_code fields
# There are 98 samples that have pathology codes that agree between the two versions of the spreadsheets
NZCETA_DOC_joined %>% filter(pathology_code.x==pathology_code.y) %>% select(Code,UoA_Code,scrubbed_uoa_code,pathology_code.x,pathology_code.y)
# There are 0 samples that have pathology codes that do not agree between the two versions of the spreadshets
NZCETA_DOC_joined %>% filter(pathology_code.x!=pathology_code.y) %>% select(Code,UoA_Code,scrubbed_uoa_code,pathology_code.x,pathology_code.y)
# There are 14 records where there is a pathology code for DOC_Massey, but not for NZCETA
NZCETA_DOC_joined %>% filter(pathology_code.x=="NA"|is.na(pathology_code.x))  %>% filter(pathology_code.y!="NA"|!is.na(pathology_code.y)) %>% select(Code,UoA_Code,scrubbed_uoa_code,pathology_code.x,pathology_code.y)
# There are 40 records where there is not a pathology code for DOC_Massey, but is for NZCETA
NZCETA_DOC_joined %>% filter(pathology_code.y=="NA"|is.na(pathology_code.y))  %>% filter(pathology_code.x!="NA"|!is.na(pathology_code.x)) %>% select(Code,UoA_Code,scrubbed_uoa_code,pathology_code.x,pathology_code.y)
# There are 175 records where neither file had a pathology_code
NZCETA_DOC_joined %>% filter(pathology_code.y=="NA"|is.na(pathology_code.y)) %>% filter(pathology_code.x=="NA"|is.na(pathology_code.x)) %>% select(Code,UoA_Code,scrubbed_uoa_code,pathology_code.x,pathology_code.y)
# Do all these instances add up to the length of NZCETA_DOC_joined?
98+14+40+175==(dim(NZCETA_DOC_joined)[1])
# Yes they do! Cool, so we are free to combine the pathology codes fields into a single column
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(pathology_code=ifelse(((pathology_code.x=="NA"|is.na(pathology_code.x))&(pathology_code.y=="NA"|is.na(pathology_code.y))),NA,
                                                                              ifelse((pathology_code.x=="NA"|is.na(pathology_code.x)),pathology_code.y,
                                                                                     ifelse((pathology_code.y=="NA"|is.na(pathology_code.y)),pathology_code.x,pathology_code.x)))) %>% 
  select(-pathology_code.x,-pathology_code.y)

# Let's double check that the same total number of records have and do not have pathology_codes now we've tweaked it
NZCETA_DOC_joined %>% filter(is.na(pathology_code)) # Still 175, cool!
NZCETA_DOC_joined %>% filter(!is.na(pathology_code)) # 152 (this should be equal to 98+14+40, and it is, yes!)

# During this process, realised that I had inadvertantly entered "text NAs" instead of NAs in a bunch of places above
# Instead of trying to go back and change it (which could end up with me messing up additional parts)
# going to find and replace those here
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% replace("NA", NA)

# Let's do common name next
# 179 of the records have the same common name
NZCETA_DOC_joined %>% filter(Common_name==Common_Name) %>% select(Common_name,Common_Name)
# 11 records have differing common names (and differing scientific names). 
NZCETA_DOC_joined %>% filter(Common_name!=Common_Name) %>% select(Common_name,Species_name,Common_Name,Species_subspecies)
# We'll need to drill into other fields to see what might have driven this
NZCETA_DOC_joined %>% filter(Common_name!=Common_Name) %>% select(Code,UoA_Code,scrubbed_uoa_code,Date_stranded,Location.x,Region,Ocean,Location.y)
# Looks like the NZCETA sheet is in error for this, so we'll take "Common_Name" from the DOC_Massey file as the correct version
# and add a note to reconciliation_notes about this. At the same time we might as well change the common names too.
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% 
  mutate(reconciliation_notes=ifelse(Common_name!=Common_Name,ifelse(reconciliation_notes=="NA","common and species names modfied based on DOC_Massey",paste(reconciliation_notes,"common and species names modfied based on DOC_Massey",sep=";")),reconciliation_notes)) %>% 
  mutate(Species_name=ifelse(Common_name!=Common_Name,Species_subspecies,Species_name)) %>% mutate(Common_name=ifelse(Common_name!=Common_Name,Common_Name,Common_name))

# OK, so let's redo some of the calls we did before:
# 190 records now have the same Common names across the different component spreadsheets
NZCETA_DOC_joined %>% filter(Common_name==Common_Name)
# 0 don't match up
NZCETA_DOC_joined %>% filter(Common_name!=Common_Name)
# That leaves 137 samples that likely have an NA in one or the other names. Let's check our numbers are right.
NZCETA_DOC_joined %>% filter(is.na(Common_name)|is.na(Common_Name))
# Yes! Of these, how many have NA for both spreadsheets for common and species names (these are ones we'll need to fill in)
NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies))
# 110 of them! Let's do the previous statement as a 'View' so we can sort and see if there is info to characterize
# the samples as either subspecies (otherwise will need an "unknown" category).
NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% View()

# Alright, time to start filling in some data for these guys. First up is CheWC04-01, where we are guessing "West Coast"
# based on the sample label but have no other information, so we'll also make a note in the reconciliation_notes about this too
# In addition, I'm going to have to eventually parse these out into locations, so going to do that at the same time b/c I'm
# looking at these samples in detail anyway. We are going to do this with the new variable 'hologenome_region'.

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(Code=="CheWC04-01",ifelse(is.na(reconciliation_notes),"Location/species is uncertain, taken from sample code",paste(reconciliation_notes,"Location/species is uncertain, taken from sample code",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(Code=="CheWC04-01","Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(Code=="CheWC04-01","Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(Code=="CheWC04-01","Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(Code=="CheWC04-01","Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(Code=="CheWC04-01","SI West Coast",NA))

# KTUW 1, 2, 3 is next. It would seem to be a Māui based on location/date, but will modify reconcilation_notes to say that subspecies 
# designation is based on location
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(Code=="KTUW 1, 2, 3",ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(Code=="KTUW 1, 2, 3","Cephalorhynchus hectori maui",Species_name)) %>% mutate(Species_subspecies=ifelse(Code=="KTUW 1, 2, 3","Cephalorhynchus hectori maui",Species_subspecies)) %>% mutate(Common_name=ifelse(Code=="KTUW 1, 2, 3","Maui dolphin",Common_name)) %>% mutate(Common_Name=ifelse(Code=="KTUW 1, 2, 3","Maui dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(Code=="KTUW 1, 2, 3","Maui",hologenome_region)) 

# U12-091 is next. SI WC based on Location
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(Code=="U12-091",ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(Code=="U12-091","Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(Code=="U12-091","Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(Code=="U12-091","Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(Code=="U12-091","Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(Code=="U12-091","SI West Coast",hologenome_region))

# Next up we have a block of samples from Cantebury and Canterbury (some alternate spellings :) . It is a little more complex
# with a group of samples, so going to obtain the sample names before running the code.
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% filter(Region=="Cantebury"|Region=="Canterbury") %>% select(scrubbed_uoa_code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(scrubbed_uoa_code %in% sample_code_list,"SI East Coast",hologenome_region))

# Going to replace "Cantebury" with "Canterbury" for the two samples that didn't have this spelled right.
NZCETA_DOC_joined$Region[which(NZCETA_DOC_joined$Code=="U15-116")] <- "Canterbury"
NZCETA_DOC_joined$Region[which(NZCETA_DOC_joined$Code=="U17-064")] <- "Canterbury"

# "U13-018 ( H239-13)" is next. SI WC based on Location
NZCETA_DOC_joined  <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(Code=="U13-018 ( H239-13)",ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(Code=="U13-018 ( H239-13)","Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(Code=="U13-018 ( H239-13)","Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(Code=="U13-018 ( H239-13)","Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(Code=="U13-018 ( H239-13)","Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(Code=="U13-018 ( H239-13)","SI West Coast",hologenome_region))

# U18-007 is next. SI EC based on location
NZCETA_DOC_joined  <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(Code=="U18-007",ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(Code=="U18-007","Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(Code=="U18-007","Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(Code=="U18-007","Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(Code=="U18-007","Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(Code=="U18-007","SI East Coast",hologenome_region))

# "Che05NZ08 (H106/05)" is next. SI WC based on location.
NZCETA_DOC_joined  <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(Code=="Che05NZ08 (H106/05)",ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(Code=="Che05NZ08 (H106/05)","Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(Code=="Che05NZ08 (H106/05)","Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(Code=="Che05NZ08 (H106/05)","Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(Code=="Che05NZ08 (H106/05)","Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(Code=="Che05NZ08 (H106/05)","SI West Coast",hologenome_region))

# Che02NZ03 is Māui based on location/date
NZCETA_DOC_joined  <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(Code=="Che02NZ03",ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(Code=="Che02NZ03","Cephalorhynchus hectori maui",Species_name)) %>% mutate(Species_subspecies=ifelse(Code=="Che02NZ03","Cephalorhynchus hectori maui",Species_subspecies)) %>% mutate(Common_name=ifelse(Code=="Che02NZ03","Maui dolphin",Common_name)) %>% mutate(Common_Name=ifelse(Code=="Che02NZ03","Maui dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(Code=="Che02NZ03","Maui",hologenome_region))

# Next up, we have several samples from Otago. It is a little more complex
# with a group of samples, so going to obtain the sample names before running the code.
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% filter(Region=="Otago") %>% select(scrubbed_uoa_code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(scrubbed_uoa_code %in% sample_code_list,"SI East Coast",hologenome_region)) 

# Next up, we have several samples from Southland. It is a little more complex
# with a group of samples, so going to obtain the sample names before running the code.
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% filter(Region=="Southland") %>% select(scrubbed_uoa_code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(scrubbed_uoa_code %in% sample_code_list,"SI South Coast",hologenome_region)) 

# Opunake sample (U12-006)
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(Code=="U12-006",ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(Code=="U12-006","Cephalorhynchus hectori maui",Species_name)) %>% mutate(Species_subspecies=ifelse(Code=="U12-006","Cephalorhynchus hectori maui",Species_subspecies)) %>% mutate(Common_name=ifelse(Code=="U12-006","Maui dolphin",Common_name)) %>% mutate(Common_Name=ifelse(Code=="U12-006","Maui dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(Code=="U12-006","Maui",hologenome_region))

NZCETA_DOC_joined$Region[which(NZCETA_DOC_joined$Code=="U12-006")] <- "Taranaki"

# All three sampled from "Tasman", are SI North Coast. It is a little more complex
# with a group of samples, so going to obtain the sample names before running the code.
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% filter(Region=="Tasman") %>% select(scrubbed_uoa_code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(scrubbed_uoa_code %in% sample_code_list,"SI North Coast",hologenome_region)) 

# Both sampled from "Waikato", are Maui. It is a little more complex
# with a group of samples, so going to obtain the sample names before running the code.
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% filter(Region=="Waikato") %>% select(scrubbed_uoa_code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori maui",Species_name)) %>% mutate(Species_subspecies=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori maui",Species_subspecies)) %>% mutate(Common_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Maui dolphin",Common_name)) %>% mutate(Common_Name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Maui dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(scrubbed_uoa_code %in% sample_code_list,"Maui",hologenome_region))

# All animals sampled from "West Coast" are (you guessed it!) SI West Coast. It is a little more complex
# with a group of samples, so going to obtain the sample names before running the code.
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% filter(Region=="West Coast") %>% select(scrubbed_uoa_code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(scrubbed_uoa_code %in% sample_code_list,"SI West Coast",hologenome_region))

# "U15-119" is a West Coast sample.
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(Code=="U15-119",ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(Code=="U15-119","Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(Code=="U15-119","Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(Code=="U15-119","Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(Code=="U15-119","Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(Code=="U15-119","SI West Coast",hologenome_region))

# OK, that's all the samples that have a region given. I'm going to rerun the 'View' command from earlier
# to check that we have actually managed to record data for all of these guys, and also so we can now
# sort on location.x to try and figure out the subspecies for the remaining dolphins.
NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% View()

# Samples with Kaikoura and Port Underwood in Location.x are SI East Coast samples
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% filter(Location.x=="Kaikoura"|Location.x=="Port Underwood") %>% select(scrubbed_uoa_code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(scrubbed_uoa_code %in% sample_code_list,"SI East Coast",hologenome_region))

# Samples with Location.x as Buller, Greymouth, and Wesport, SI West Coast. Also need to change 'Wesport' to 'Westport'
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% filter(Location.x=="Buller"|Location.x=="Greymouth"|Location.x=="Wesport") %>% select(scrubbed_uoa_code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(scrubbed_uoa_code %in% sample_code_list,"SI West Coast",hologenome_region)) 

NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Wesport")] <- "Westport"

# Sample with Maia is SI East Coast. Only one sample, but actually easier just to use sample_code_list way of doing it
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% filter(Location.x=="Maia") %>% select(scrubbed_uoa_code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(scrubbed_uoa_code %in% sample_code_list,"SI East Coast",hologenome_region))

# Otago? sample is going to need something added to the reconciliation notes saying we are not 100% sure
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% filter(Location.x=="Otago?") %>% select(scrubbed_uoa_code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location, but location is uncertain",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(scrubbed_uoa_code %in% sample_code_list,"SI East Coast",hologenome_region))

# Queen Charlotte sound samples are from SI North Coast
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% filter(Location.x=="Queen Charlotte sound") %>% select(scrubbed_uoa_code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(scrubbed_uoa_code %in% sample_code_list,"SI North Coast",hologenome_region))

# OK, that's all the location.x samples, so let's do the view statement again to see what other fields we can look at
NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% View()

# Going to go on Location.y (the Massey_DOC location) for the next few samples. The following samples should all be Maui
# "35 metre mark, NW Wattle Bay, Manukau Harbour (floating)", "Albatross Bay, Kawhia (Te Maka), Waikato", "Karioitahi Beach, South 
# Auckland", "Manukau Bar, 6 miles off Manukau Harbour Entrance on 61-65 metre mark (Manukau Heads -at sea)", "Manukau Habour", "North 
# of Karioitahi Beach entrance, Waiuku, Manukau Harbour", "O'Neils Beach, north of Bethell's Beach", "Port Waikato, South Auckland"

sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% filter(Location.y=="35 metre mark, NW Wattle Bay, Manukau Harbour (floating)"|Location.y=="Albatross Bay, Kawhia (Te Maka), Waikato"|Location.y=="Karioitahi Beach, South Auckland"|Location.y=="Manukau Bar, 6 miles off Manukau Harbour Entrance on 61-65 metre mark (Manukau Heads -at sea)"|Location.y=="Manukau Habour"|Location.y=="North of Karioitahi Beach entrance, Waiuku, Manukau Harbour"|Location.y=="O'Neils Beach, north of Bethell's Beach"|Location.y=="Port Waikato, South Auckland") %>% select(UoA_Code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(UoA_Code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(UoA_Code %in% sample_code_list,"Cephalorhynchus hectori maui",Species_name)) %>% mutate(Species_subspecies=ifelse(UoA_Code %in% sample_code_list,"Cephalorhynchus hectori maui",Species_subspecies)) %>% mutate(Common_name=ifelse(UoA_Code %in% sample_code_list,"Maui dolphin",Common_name)) %>% mutate(Common_Name=ifelse(UoA_Code %in% sample_code_list,"Maui dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(UoA_Code %in% sample_code_list,"Maui",hologenome_region)) 

# Following samples are East Coast: "Banks Peninsula", "Kaikoura", "Pegasus Bay", "Timaru", "Timaru, Rangitata River"
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% filter(Location.y=="Banks Peninsula"|Location.y=="Kaikoura"|Location.y=="Pegasus Bay"|Location.y=="Timaru"|Location.y=="Timaru, Rangitata River") %>% select(UoA_Code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(UoA_Code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(UoA_Code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(UoA_Code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(UoA_Code %in% sample_code_list,"Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(UoA_Code %in% sample_code_list,"Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(UoA_Code %in% sample_code_list,"SI East Coast",hologenome_region)) 

# I missed a Maui sample:
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% filter(Location.y=="Waikanae (wrongly Oakura in Baker et al 2002)") %>% select(UoA_Code))

NZCETA_DOC_joined <-NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(UoA_Code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(UoA_Code %in% sample_code_list,"Cephalorhynchus hectori maui",Species_name)) %>% mutate(Species_subspecies=ifelse(UoA_Code %in% sample_code_list,"Cephalorhynchus hectori maui",Species_subspecies)) %>% mutate(Common_name=ifelse(UoA_Code %in% sample_code_list,"Maui dolphin",Common_name)) %>% mutate(Common_Name=ifelse(UoA_Code %in% sample_code_list,"Maui dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(UoA_Code %in% sample_code_list,"Maui",hologenome_region))

# Have a few SI South Coast from Te Waewae Bay
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% filter(Location.y=="Te Waewae Bay") %>% select(UoA_Code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(UoA_Code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(UoA_Code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(UoA_Code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(UoA_Code %in% sample_code_list,"Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(UoA_Code %in% sample_code_list,"Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(UoA_Code %in% sample_code_list,"SI East Coast",hologenome_region)) 

# Now the only remaining samples with info for Location.y are SI West Coast:
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% filter(Location.y!="NA") %>% select(UoA_Code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(UoA_Code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Subspecies designation based on location",paste(reconciliation_notes,"Subspecies designation based on location",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(UoA_Code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_name)) %>% mutate(Species_subspecies=ifelse(UoA_Code %in% sample_code_list,"Cephalorhynchus hectori hectori",Species_subspecies)) %>% mutate(Common_name=ifelse(UoA_Code %in% sample_code_list,"Hector's dolphin",Common_name)) %>% mutate(Common_Name=ifelse(UoA_Code %in% sample_code_list,"Hector's dolphin",Common_Name)) %>% mutate(hologenome_region=ifelse(UoA_Code %in% sample_code_list,"SI West Coast",hologenome_region)) 

# We'll run the View() command again to see what else we can try and glean the location off
NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% View()

# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps
# No other columns I feel comfortable gleaning location from (because Other_info/Archiving_info seems to have undergone
# an incomplete sort at some point). If this problem is solved, worth running this step again to see if additional info can be
# gleaned, but in the meantime, we are going to note that we are unsure on the subspecies/species designation and hologenome_region
# for these guys
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies)) %>% select(scrubbed_uoa_code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code %in% sample_code_list,ifelse(is.na(reconciliation_notes),"Not enough information to determine subspecies",paste(reconciliation_notes,"Not enough information to determine subspecies",sep=";")),reconciliation_notes)) %>% mutate(Species_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori ?",Species_name)) %>% mutate(Species_subspecies=ifelse(scrubbed_uoa_code %in% sample_code_list,"Cephalorhynchus hectori ?",Species_subspecies)) %>% mutate(Common_name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Uncertain",Common_name)) %>% mutate(Common_Name=ifelse(scrubbed_uoa_code %in% sample_code_list,"Uncertain",Common_Name)) %>% mutate(hologenome_region=ifelse(scrubbed_uoa_code %in% sample_code_list,"Unknown",hologenome_region))

# OK, so let's redo some of the calls we did before:
# 327 records now have the same Common names across the different component spreadsheets
NZCETA_DOC_joined %>% filter(Common_name==Common_Name)
# 0 don't match up
NZCETA_DOC_joined %>% filter(Common_name!=Common_Name)
# 0 have an NA in one or the other names.
NZCETA_DOC_joined %>% filter(is.na(Common_name)|is.na(Common_Name))
# 0 have NA for both spreadsheets for common and species names
NZCETA_DOC_joined %>% filter(is.na(Common_name) & is.na(Common_Name) & is.na(Species_name) & is.na(Species_subspecies))

# OK, common names are now sorted.
# NOTE: this doesn't mean that the common names are necessarily correct (we will check that later!)
# but that they are now at least in agreement across the two datasets, so we can drop one of these columns
# going to drop Common_Name (and keep Common_name)
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% select(-Common_Name)

# OK, now let's work on species names. These should match up to our common names. We'll do this
# for the NZCETA columns first, and then for the DOC_Massey ones, and then we can see if there
# discrepancies after that (there shouldn't be).

NZCETA_DOC_joined %>% filter(!(Species_name=="Cephalorhynchus hectori ?" & Common_name=="Uncertain")) %>% 
  filter(!(Species_name=="Cephalorhynchus hectori hectori" & Common_name=="Hector's dolphin")) %>% 
  filter(!(Species_name=="Cephalorhynchus hectori maui" & Common_name=="Maui dolphin")) %>% 
  select(Species_name,Common_name) %>% unique()

# OK, there are a ton (173) entries where Hector's dolphin has been given as the common name, but just Cephalorhynchus hectori
# without the subspecies designation has been given as the Species_name. Here we'll tweak that.

sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(!(Species_name=="Cephalorhynchus hectori ?" & Common_name=="Uncertain")) %>% 
                                filter(!(Species_name=="Cephalorhynchus hectori hectori" & Common_name=="Hector's dolphin")) %>% 
                                filter(!(Species_name=="Cephalorhynchus hectori maui" & Common_name=="Maui dolphin")) %>% select(scrubbed_uoa_code))

NZCETA_DOC_joined$Species_name[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list)] <- "Cephalorhynchus hectori hectori"

# We now shouldn't get any rows for the following:
NZCETA_DOC_joined %>% filter(!(Species_name=="Cephalorhynchus hectori ?" & Common_name=="Uncertain")) %>% 
  filter(!(Species_name=="Cephalorhynchus hectori hectori" & Common_name=="Hector's dolphin")) %>% 
  filter(!(Species_name=="Cephalorhynchus hectori maui" & Common_name=="Maui dolphin")) %>% 
  select(Species_name,Common_name) %>% unique()

# Cool! So that's NZCETA solved. Now let's double check the DOC_Massey entries.
NZCETA_DOC_joined %>% filter(!(Species_subspecies=="Cephalorhynchus hectori ?" & Common_name=="Uncertain")) %>% 
  filter(!(Species_subspecies=="Cephalorhynchus hectori hectori" & Common_name=="Hector's dolphin")) %>% 
  filter(!(Species_subspecies=="Cephalorhynchus hectori maui" & Common_name=="Maui dolphin")) %>% 
  select(Species_subspecies,Common_name) %>% unique()

# Looks like there is a typo, and some of the Species_subspecies names are Cephalorhynchus hectori hector 
# rather than Cephalorhynchus hectori hectori
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter((Species_subspecies=="Cephalorhynchus hectori hector")) %>% select(scrubbed_uoa_code))

NZCETA_DOC_joined$Species_subspecies[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list)] <- "Cephalorhynchus hectori hectori"

# Let's double check the DOC_Massey entries:
NZCETA_DOC_joined %>% filter(!(Species_subspecies=="Cephalorhynchus hectori ?" & Common_name=="Uncertain")) %>% 
  filter(!(Species_subspecies=="Cephalorhynchus hectori hectori" & Common_name=="Hector's dolphin")) %>% 
  filter(!(Species_subspecies=="Cephalorhynchus hectori maui" & Common_name=="Maui dolphin")) %>% 
  select(Species_subspecies,Common_name) %>% unique()

# Cool! No entries, so now we should be good ot double check that NZCETA and DOC_Massey match up:
NZCETA_DOC_joined %>% filter(!(Species_subspecies==Species_name))

# Cool, no records don't argree, so we can get rid of one of them. I think we'll get rid of Species_name
# because Species_subspecies is a little more descriptive
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% select(-Species_name)

# What else can we match up?
names(NZCETA_DOC_joined)
# First off, we can get rid of the column "NA", as it only consists of NAs. Not sure how that got in there!
NZCETA_DOC_joined[,48] %>% unique()
NZCETA_DOC_joined <- NZCETA_DOC_joined[,-48]

# OK, of the rest, it looks like Sex_observed and Sex should match up. These should be observed sex of the
# animal (as opposed to 'genetic sex'). Let's do a quick check to see how many of these calls agree/disagree
# and whether is any difference in formatting e.g. F vs Female
NZCETA_DOC_joined %>% group_by(Sex,Sex_observed) %>% count() %>% print(n=29)

# OK, so "Sex" has a pretty standard formatting (e.g. Female, Male, Unknown), but Sex_observed is all over
# the place. Let's change the obvious ones that can be standardized, and then we can look at the
# ones that have disagreement.
NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$Sex_observed=="-")] <- "Unknown"
NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$Sex_observed=="F")] <- "Female"
NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$Sex_observed=="female")] <- "Female"
NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$Sex_observed=="male")] <- "Male"
NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$Sex_observed=="M")]  <- "Male"
NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$Sex_observed=="unknown")]  <- "Unknown"
NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$Sex_observed=="?")]  <- "Unknown"

# There are two samples where there is additional information that I'm going to 
# push to the reconciliation notes
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$Sex_observed=="unknown - probably female (genitals scavenged)")]
# Cool, the reconciliation notes are NA, so we are good to just go ahead and change this:
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$Sex_observed=="unknown - probably female (genitals scavenged)")] <- "DOC_Massey made this observation of Sex_observed 'unknown - probably female (genitals scavenged)'"
# Let's double check the rest of the entries for this sample to see if there is anything that puts us off going
# with Sex==Female for it
as.matrix(NZCETA_DOC_joined[which(NZCETA_DOC_joined$Sex_observed=="unknown - probably female (genitals scavenged)"),])
# Looks good, so going to tweak the Observed_sex field to "Female"
NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$Sex_observed=="unknown - probably female (genitals scavenged)")] <- "Female"

# Next example has unknown (scavenged), whereas 'Sex' has unknown - entirely compatible with each other
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$Sex_observed=="unknown (scavenged)")]
# Cool, the reconciliation notes are NA, so we are good to just go ahead and change this:
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$Sex_observed=="unknown (scavenged)")] <- "DOC_Massey made this observation of Sex_observed 'unknown (scavenged)'"
NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$Sex_observed=="unknown (scavenged)")] <- "Unknown"

# OK, next let's look at the samples that don't match up (ignoring the NA guys for now)
# Starting with the single instances of mismatch
NZCETA_DOC_joined %>% filter(Sex!=Sex_observed) %>% group_by(Sex,Sex_observed) %>% count() %>% filter(n==1)

# Female/Unknown mismatch
as.matrix(NZCETA_DOC_joined[which((NZCETA_DOC_joined$Sex=="Female")&(NZCETA_DOC_joined$Sex_observed=="Unknown")),])
# Seems no reason why we can't go with Female for this one, and put DOC_Massey's call into the reconciliation notes
NZCETA_DOC_joined$reconciliation_notes[which(((NZCETA_DOC_joined$Sex=="Female")&(NZCETA_DOC_joined$Sex_observed=="Unknown")))]
# Cool, the reconciliation notes are NA, so we are good to just go ahead and change this:
NZCETA_DOC_joined$reconciliation_notes[which(((NZCETA_DOC_joined$Sex=="Female")&(NZCETA_DOC_joined$Sex_observed=="Unknown")))] <- "DOC_Massey made this observation of Sex_observed 'Unknown'"
NZCETA_DOC_joined$Sex_observed[which(((NZCETA_DOC_joined$Sex=="Female")&(NZCETA_DOC_joined$Sex_observed=="Unknown")))] <- "Female"

# Male/Female mismatch
as.matrix(NZCETA_DOC_joined[which((NZCETA_DOC_joined$Sex=="Male")&(NZCETA_DOC_joined$Sex_observed=="Female")),])
# Given Genetic_Sex is "F", going to go with "Female", and make a note that NZCETA had a different call.
NZCETA_DOC_joined$reconciliation_notes[which(((NZCETA_DOC_joined$Sex=="Male")&(NZCETA_DOC_joined$Sex_observed=="Female")))]
# Cool, the reconciliation notes are NA, so we are good to just go ahead and change this:
NZCETA_DOC_joined$reconciliation_notes[which(((NZCETA_DOC_joined$Sex=="Male")&(NZCETA_DOC_joined$Sex_observed=="Female")))] <- "NZCETA made this observation of Sex 'Male'"
NZCETA_DOC_joined$Sex[which(((NZCETA_DOC_joined$Sex=="Male")&(NZCETA_DOC_joined$Sex_observed=="Female")))] <- "Female"

# Unknown/Male mismatch
as.matrix(NZCETA_DOC_joined[which((NZCETA_DOC_joined$Sex=="Unknown")&(NZCETA_DOC_joined$Sex_observed=="Male")),])
# Given Genetic_Sex is "M", going to go with "Male", and make a note that NZCETA had a different call.
NZCETA_DOC_joined$reconciliation_notes[which((NZCETA_DOC_joined$Sex=="Unknown")&(NZCETA_DOC_joined$Sex_observed=="Male"))]
# Cool, the reconciliation notes are NA, so we are good to just go ahead and change this:
NZCETA_DOC_joined$reconciliation_notes[which((NZCETA_DOC_joined$Sex=="Unknown")&(NZCETA_DOC_joined$Sex_observed=="Male"))] <- "NZCETA made this observation of Sex 'Unknown'"
NZCETA_DOC_joined$Sex[which((NZCETA_DOC_joined$Sex=="Unknown")&(NZCETA_DOC_joined$Sex_observed=="Male"))] <- "Male"

# Unknown\vMale/Male mismatch
as.matrix(NZCETA_DOC_joined[which((NZCETA_DOC_joined$Sex=="Unknown\vMale")&(NZCETA_DOC_joined$Sex_observed=="Male")),])
# Given Genetic_Sex is "M", going to go with "Male", and make a note that NZCETA was a little less sure
NZCETA_DOC_joined$reconciliation_notes[which((NZCETA_DOC_joined$Sex=="Unknown\vMale")&(NZCETA_DOC_joined$Sex_observed=="Male"))]
# Cool, the reconciliation notes are NA, so we are good to just go ahead and change this:
NZCETA_DOC_joined$reconciliation_notes[which((NZCETA_DOC_joined$Sex=="Unknown\vMale")&(NZCETA_DOC_joined$Sex_observed=="Male"))] <- "NZCETA made this observation of Sex 'Unknown\vMale'"
NZCETA_DOC_joined$Sex[which((NZCETA_DOC_joined$Sex=="Unknown\vMale")&(NZCETA_DOC_joined$Sex_observed=="Male"))] <- "Male"

# OK let's remove on to the remainging mismatches
NZCETA_DOC_joined %>% filter(Sex!=Sex_observed) %>% group_by(Sex,Sex_observed) %>% count() 

# The first of the two Female/Male mismatches
as.matrix(NZCETA_DOC_joined[which((NZCETA_DOC_joined$Sex=="Female")&(NZCETA_DOC_joined$Sex_observed=="Male")),])[1,]
# Given genetic sex was F going to go with female, and make a note that DOC_Massey thought Male
NZCETA_DOC_joined$reconciliation_notes[which((NZCETA_DOC_joined$Sex=="Female")&(NZCETA_DOC_joined$Sex_observed=="Male"))[1]]
# Cool, the reconciliation notes are NA, so we are good to just go ahead and change this:
NZCETA_DOC_joined$reconciliation_notes[which((NZCETA_DOC_joined$Sex=="Female")&(NZCETA_DOC_joined$Sex_observed=="Male"))[1]] <- "DOC_Massey made this observation of Sex_observed 'Male'"
NZCETA_DOC_joined$Sex_observed[which((NZCETA_DOC_joined$Sex=="Female")&(NZCETA_DOC_joined$Sex_observed=="Male"))[1]] <- "Female"

# The first of the two Unknown/Female mismatches
as.matrix(NZCETA_DOC_joined[which((NZCETA_DOC_joined$Sex=="Unknown")&(NZCETA_DOC_joined$Sex_observed=="Female")),])[1,]
# Given genetic sex was F going to go with female, and make a note that NZCETA thought Unkown
NZCETA_DOC_joined$reconciliation_notes[which((NZCETA_DOC_joined$Sex=="Unknown")&(NZCETA_DOC_joined$Sex_observed=="Female"))[1]]
# Cool, the reconciliation notes are NA, so we are good to just go ahead and change this:
NZCETA_DOC_joined$reconciliation_notes[which((NZCETA_DOC_joined$Sex=="Unknown")&(NZCETA_DOC_joined$Sex_observed=="Female"))[1]] <- "NZCETA made this observation of Sex 'Unknown'"
NZCETA_DOC_joined$Sex[which((NZCETA_DOC_joined$Sex=="Unknown")&(NZCETA_DOC_joined$Sex_observed=="Female"))[1]] <- "Female"

# OK, down to one per category again
NZCETA_DOC_joined %>% filter(Sex!=Sex_observed) %>% group_by(Sex,Sex_observed) %>% count() 

# Remaining Female/Male mismatch
as.matrix(NZCETA_DOC_joined[which((NZCETA_DOC_joined$Sex=="Female")&(NZCETA_DOC_joined$Sex_observed=="Male")),])
# Given Genetic_Sex is "F", going to go with "Female", and make a note that DOC_Massey disagreed
NZCETA_DOC_joined$reconciliation_notes[which((NZCETA_DOC_joined$Sex=="Female")&(NZCETA_DOC_joined$Sex_observed=="Male"))]
# Cool, the reconciliation notes are NA, so we are good to just go ahead and change this:
NZCETA_DOC_joined$reconciliation_notes[which((NZCETA_DOC_joined$Sex=="Female")&(NZCETA_DOC_joined$Sex_observed=="Male"))] <- "DOC_Massey made this observation of Sex_observed 'Male'"
NZCETA_DOC_joined$Sex_observed[which((NZCETA_DOC_joined$Sex=="Female")&(NZCETA_DOC_joined$Sex_observed=="Male"))]   <- "Female"

# Remaining Unknown/Female mismatch
as.matrix(NZCETA_DOC_joined[which((NZCETA_DOC_joined$Sex=="Unknown")&(NZCETA_DOC_joined$Sex_observed=="Female")),])
# Given genetic sex was F going to go with female, and make a note that NZCETA thought Unknown
NZCETA_DOC_joined$reconciliation_notes[which((NZCETA_DOC_joined$Sex=="Unknown")&(NZCETA_DOC_joined$Sex_observed=="Female"))]
# Cool, the reconciliation notes are NA, so we are good to just go ahead and change this:
NZCETA_DOC_joined$reconciliation_notes[which((NZCETA_DOC_joined$Sex=="Unknown")&(NZCETA_DOC_joined$Sex_observed=="Female"))] <- "NZCETA made this observation of Sex 'Unknown'"
NZCETA_DOC_joined$Sex[which((NZCETA_DOC_joined$Sex=="Unknown")&(NZCETA_DOC_joined$Sex_observed=="Female"))] <- "Female"

# OK, now we should be down to perfect matches + NAs
NZCETA_DOC_joined %>% group_by(Sex,Sex_observed) %>% count() 

# Alrighty, not quite - there are a few other weird ones in there (that are coupled with NAs) 
# that we should check out, starting with "Unknown\vMale" in 'Sex'
as.matrix(NZCETA_DOC_joined[which((NZCETA_DOC_joined$Sex=="Unknown\vMale")&(is.na(NZCETA_DOC_joined$Sex_observed))),])
# Alright, of these two, the first one has a genetic sex of Male, so we are going to tweak it for both Sex and Sex_observed
# and note the uncertainty in the reconciliaton_notes
# The second one has no info for the DOC_Massey side of the sheet, so we'll stick with Unknown for this, but note the potential
# Male call in the reconciliaiton_notes

# Because we'll be modifying both 'sex' columns, we need to record the rows, because the which statement
# won't work after we tweak those columns
sample_rows <- which((NZCETA_DOC_joined$Sex=="Unknown\vMale")&(is.na(NZCETA_DOC_joined$Sex_observed)))

# Working on the first row first
NZCETA_DOC_joined$reconciliation_notes[sample_rows[1]]
# Cool, this is NA, so we can just go ahead and modify it
NZCETA_DOC_joined$reconciliation_notes[sample_rows[1]] <- "NZCETA made this observation of Sex 'Unknown\vMale'. DOC_Massey had no information for this individual"
NZCETA_DOC_joined$Sex[sample_rows[1]] <- "Male"
NZCETA_DOC_joined$Sex_observed[sample_rows[1]] <- "Male"

# Working on the next row
NZCETA_DOC_joined$reconciliation_notes[sample_rows[2]]
# Already got some data in there, so need to do a paste command
NZCETA_DOC_joined$reconciliation_notes[sample_rows[2]] <- paste(NZCETA_DOC_joined$reconciliation_notes[sample_rows[2]],"NZCETA made this observation of Sex 'Unknown\vMale'. DOC_Massey had no information for this individual",sep=";")
NZCETA_DOC_joined$Sex[sample_rows[2]] <- "Unknown"
NZCETA_DOC_joined$Sex_observed[sample_rows[2]] <- "Unknown"

# OK, next one we are going to look at is "M?" for Sex_observed
sample_rows <- which(NZCETA_DOC_joined$Sex_observed=="M?")
as.matrix(NZCETA_DOC_joined[sample_rows,])
# Genetic sex is "M", so going to change this to Male and make a note about uncertainty in reconciliation notes.
NZCETA_DOC_joined$reconciliation_notes[sample_rows[1]]
# Cool, this is NA, so we can just go ahead and modify it
NZCETA_DOC_joined$reconciliation_notes[sample_rows[1]] <- "DOC_Massey made this observation of Observed_sex 'M?'. NZCETA had no information for this individual"
NZCETA_DOC_joined$Sex[sample_rows[1]] <- "Male"
NZCETA_DOC_joined$Sex_observed[sample_rows[1]] <- "Male"

# The final 'weirdos' we have to look at are "TBC" in the Sex_observed category.
sample_rows <- which(NZCETA_DOC_joined$Sex_observed=="TBC")
as.matrix(NZCETA_DOC_joined[sample_rows,])
# No other information is available for these guys, so don't think it is appropriate for it to be 'Unknown'
# instead, will record as 'NA, and put note about "TBC" in reconciliation notes.
NZCETA_DOC_joined$reconciliation_notes[sample_rows]
# Both are NA, so can just go ahead and replace them
NZCETA_DOC_joined$reconciliation_notes[sample_rows] <-  "DOC_Massey made this observation of Observed_sex 'TBC'. NZCETA had no information for this individual"
NZCETA_DOC_joined$Sex[sample_rows][1:2] <- c(NA,NA)
NZCETA_DOC_joined$Sex_observed[sample_rows][1:2] <- c(NA,NA)

# We've got quite a few NA/sex match ups in both direction - presumably this is because one or the other databases
# are missing information about the samples (including sex). Let's double check this before 'propagating sex'.
NZCETA_DOC_joined %>% filter(is.na(Sex)) %>% group_by(Sex,Sex_observed) %>% count() 
NZCETA_DOC_joined %>% filter(is.na(Sex)&is.na(Code)) %>% group_by(Sex,Sex_observed) %>% count() 
# Nope, not quite. Let's look at the samples that have information for Code, but not for sex
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Sex)) %>% filter(!is.na(Code)) %>% filter(!is.na(Sex_observed)) %>% select(scrubbed_uoa_code))

# OK, so the first sample has very scarce information in NZCETA, which is why it has a sex call for DOC_Massey, but not for NCETA
# We'll propagate sex, and make a note that there was no information in NZCETA for sex
as.matrix(NZCETA_DOC_joined[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[1]),])
# Again, more info is available in DOC_Massey than NZCETA for this one, so we can make the same note as above
as.matrix(NZCETA_DOC_joined[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[2]),])
# Again, more info is available in DOC_Massey than NZCETA for this one, so we can make the same note as above
as.matrix(NZCETA_DOC_joined[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[3]),])
# Again, more info is available in DOC_Massey than NZCETA for this one, so we can make the same note as above
as.matrix(NZCETA_DOC_joined[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[4]),])
# Again, more info is available in DOC_Massey than NZCETA for this one, so we can make the same note as above
as.matrix(NZCETA_DOC_joined[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[5]),])
# Again, more info is available in DOC_Massey than NZCETA for this one, so we can make the same note as above
as.matrix(NZCETA_DOC_joined[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[6]),])
# Again, more info is available in DOC_Massey than NZCETA for this one, so we can make the same note as above
as.matrix(NZCETA_DOC_joined[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[7]),])

# All are NA so we can change them at the same time
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list)] <- "Although NZCETA had some information for this sample, it did not have info for 'Sex'. This was taken from DOC_Massey"

NZCETA_DOC_joined$Sex[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list)] <- NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list)]

# OK, that deals with NAs for Sex in NZCETA when NZCETA actually had other info for samples
# Now let's deal with the NAs when NZCETA has no info for the sample. In this case we don't
# need to make a note, because NZCETA not having an entry for 'Code' signals that it isn't
# present in NZCETA
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Sex)&is.na(Code)&!is.na(Sex_observed)) %>% select(scrubbed_uoa_code))
NZCETA_DOC_joined$Sex[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list)] <- NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list)]

# Cool, that just leaves samples where Sex is.na and Sex_observed match up in terms of NA calls
NZCETA_DOC_joined %>% filter(is.na(Sex)) %>% group_by(Sex,Sex_observed) %>% count() 

# OK, on the flip side let's double check the ones where Sex_observed is NA, but Sex is not.
NZCETA_DOC_joined %>% filter(is.na(Sex_observed)) %>% group_by(Sex,Sex_observed) %>% count()
# There's a few that have information for DOC_Massey, but not sex information. 
NZCETA_DOC_joined %>% filter(is.na(Sex_observed)&!is.na(UoA_Code)) %>% group_by(Sex,Sex_observed) %>% count() 
# Let's look at these samples (samples that have information for UoA_Code, but not for sex)
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Sex_observed)) %>% filter(!is.na(UoA_Code)) %>% filter(!is.na(Sex)) %>% select(scrubbed_uoa_code))

# Alright, bunch of these to go through. Here we go. We'll look at the codes, the dates, and sex calls, and Genetic Sex to
# double check.
NZCETA_DOC_joined %>% filter(scrubbed_uoa_code %in% sample_code_list) %>% select(Code,UoA_Code,Date_stranded,Collection_Date,Sex,Sex_observed,Genetic_Sex) %>% View()
# Only ones I want to check into (discrepancy in stranding dates) more are CHETWW06, CHETWW07, and CHEWC012
NZCETA_DOC_joined %>% filter(scrubbed_uoa_code=="CHETWW06") %>% as.matrix()
# OK, not completely confident on propagating sex on this one given the unexplained mismatch in stranding dates
# will make a reconciliation_notes entry for this sample highlighting why we are not 100% sure 
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW06")]
# NA so can go ahead and replace it
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW06")] <- "Although DOC_Massey had some information for this sample, it did not have info for 'Observed_Sex'. This was taken from NZCETA's Sex field instead, but not 100% confident due to differences in stranding date"
NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW06")] <- NZCETA_DOC_joined$Sex[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW06")] 

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code=="CHETWW07") %>% as.matrix()
# OK, this one is a little less problematic because it sems it was a museum sample. Will make a note, but happier propagating
# this one than the last one.
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW07")]
# NA so can go ahead and replace it
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW07")] <- "Although DOC_Massey had some information for this sample, it did not have info for 'Observed_Sex'. This was taken from NZCETA's Sex field instead. Differences in date seem to be driven by this being a museum sample"
NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW07")] <- NZCETA_DOC_joined$Sex[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW07")] 

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code=="CHEWC012") %>% as.matrix()
# This is another one I'm not 100% on because the location differs slightly, and the date is not the same.
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC012")]
# NA so can go ahead and replace it
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC012")] <- "Although DOC_Massey had some information for this sample, it did not have info for 'Observed_Sex'. This was taken from NZCETA's Sex field instead, but not 100% confident due to differences in stranding date and small difference in location"
NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC012")] <- NZCETA_DOC_joined$Sex[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC012")] 

# OK, for everyone else, we can batch replace Sex_observed with Sex, and make the same reconciliation notes. We
# do need to regenerate a list of samples, because we've already handled the three above.
NZCETA_DOC_joined %>% filter(is.na(Sex_observed)&!is.na(UoA_Code)) %>% group_by(Sex,Sex_observed) %>% count() 
# Regenerating the sample_code_list
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% filter(is.na(Sex_observed)) %>% filter(!is.na(UoA_Code)) %>% filter(!is.na(Sex)) %>% select(scrubbed_uoa_code))
# Some of the following are not NA, so going to have to do an ifelse statement to rename it
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse(scrubbed_uoa_code %in% sample_code_list,ifelse((is.na(reconciliation_notes)|reconciliation_notes=="NA"),"Although DOC_Massey had some information for this sample, it did not have info for 'Observed_Sex'. This was taken from NZCETA's Sex field instead",paste(reconciliation_notes,"Although DOC_Massey had some information for this sample, it did not have info for 'Observed_Sex'. This was taken from NZCETA's Sex field instead",sep=";")),reconciliation_notes))

# Alright, let's replace the actual calls
NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list)] <- NZCETA_DOC_joined$Sex[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list)]

# OK, let's double check our sex columns
NZCETA_DOC_joined %>% filter(!is.na(Sex)|Sex!="NA") %>% 
  filter(Sex_observed=="NA"|is.na(Sex_observed)) %>% select(UoA_Code) %>% unique()

# Cool! All the rest of the Observed_sex NAs are associated with records that don't have DOC_Massey info to reconcile
# We  therefore have no hard conflicts and can replace all Sex_observed with Sex and then drop a column
NZCETA_DOC_joined$Sex_observed <- NZCETA_DOC_joined$Sex
# OK, now we can drop one of the columns because they are both the same. I'm going to drop "Sex", because I think
# Sex_observed is a little more informative of a name to distinguish it from Genetic sex
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% select(-Sex)

# OK, let's do another one that hopefully won't be too evil...ha ha ha.
# Date_stranded and Collection_Date from my look at things seem to mostly match up
# Planning on holding on to Collection_Date because that's probably a bit more accurate than
# calling it Date_stranded (because in some cases it may have stranded ages before that).
# Also the date format in Collection_Date is more internationally legible than DD/MM/YYYY (which the
# Americans have trouble with). However, we will tweak Collection_Date so that it has 4-digit year codes

# Only 25 records where there is a hard mismatch between dates in the two databases
# (but we will also need to look at NAs too eventually. We'll check these ones first though!)
NZCETA_DOC_joined %>% mutate(date_stranded_mod=gsub("^0","",gsub("/09/","-Sep-",gsub("/08/","-Aug-",gsub("/07/","-Jul-",gsub("/06/","-Jun-",gsub("/05/","-May-",gsub("/04/","-Apr-",gsub("/03/","-Mar-",gsub("/02/","-Feb-",gsub("/01/","-Jan-",gsub("/12/","-Dec-",gsub("/11/","-Nov-",gsub("/10/","-Oct-",gsub("/9/","-Sep-",gsub("/8/","-Aug-",gsub("/7/","-Jul-",gsub("/6/","-Jun-",gsub("/5/","-May-",gsub("/4/","-Apr-",gsub("/3/","-Mar-",gsub("/2/","-Feb-",gsub("/1/","-Jan-",Date_stranded,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE))) %>% mutate(collection_date_mod=gsub("-7","-197",gsub("-8","-198",gsub("-9","-199",gsub("-1","-201",gsub("-0","-200",Collection_Date,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE)) %>% filter(date_stranded_mod!=collection_date_mod) %>% select(date_stranded_mod,collection_date_mod) %>% print(n=98)

# First things first, let's permanently save the scrubbed_uoa_code of these guys so we can
# go through them
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% mutate(date_stranded_mod=gsub("^0","",gsub("/09/","-Sep-",gsub("/08/","-Aug-",gsub("/07/","-Jul-",gsub("/06/","-Jun-",gsub("/05/","-May-",gsub("/04/","-Apr-",gsub("/03/","-Mar-",gsub("/02/","-Feb-",gsub("/01/","-Jan-",gsub("/12/","-Dec-",gsub("/11/","-Nov-",gsub("/10/","-Oct-",gsub("/9/","-Sep-",gsub("/8/","-Aug-",gsub("/7/","-Jul-",gsub("/6/","-Jun-",gsub("/5/","-May-",gsub("/4/","-Apr-",gsub("/3/","-Mar-",gsub("/2/","-Feb-",gsub("/1/","-Jan-",Date_stranded,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE))) %>% mutate(collection_date_mod=gsub("-7","-197",gsub("-8","-198",gsub("-9","-199",gsub("-1","-201",gsub("-0","-200",Collection_Date,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE)) %>% filter(date_stranded_mod!=collection_date_mod) %>% select(scrubbed_uoa_code))

# Next, let's save collection_date_mod, as this will be our "final" collection_date column.
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(collection_date_mod=gsub("-7","-197",gsub("-8","-198",gsub("-9","-199",gsub("-1","-201",gsub("-0","-200",Collection_Date,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE))

# OK, getting back to our problem children
NZCETA_DOC_joined %>% filter(scrubbed_uoa_code %in% sample_code_list) %>% 
  select(Date_stranded,Date_received,collection_date_mod) %>% print(n=25)

# Looks like going through these one by one is going to be the ticket. Many of them just have minor differences
# so shouldn't be too difficult to record in the reconciliation_notes what we've done and why.
NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[1]) %>% as.matrix()
# They found this one floating in the surf, which seems to be why it was given an uncertain Date_stranded in 
# the NZCETA. It doesn't have anything in the reconciliation notes, so we can just go ahead and tweak this
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[1])] <- "NZCETA had '?' for Date_stranded, likely because it was found floating in surf, so exact time of stranding unknown"
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[1])] <- NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[1])]

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[2]) %>% as.matrix()
# Collection date has two different dates, one of which matches Dat_stranded. There is already some info for reconciliaiton_notes so will need to paste this.
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[2])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[2])],"DOC_Massey had '15 Jan 2005 or 28 June 2005' for Collection_Date",sep=";")
NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[2])] <- "15-Jan-2005"

# For the remainder of samples, if I think my reconcilaition_notes are easy enough to follow I'm not
# going to bother commenting on each step
NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[3]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[3])] <- "Based on information in DOC_Massey Collection_Date, stranded after 31-Dec-2004, but wasn't sampled until 5-Jan-2005"
NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[3])] <- "5-Jan-2005"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[4]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[4])] <- "NZCETA had '?' for Date_stranded. This uncertainty seems to be captured in collection_date_mod"
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[4])] <- "2005"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[5]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[5])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[5])],"NZCETA had '03/12/2006' for Date_stranded. This might reflect a single day delay between stranding and collection",sep=";")
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[5])] <- "04/12/2006"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[6]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[6])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[6])], "DOC_Massey had a Collection_Date of '7-Dec-07', but based on the mix of 06 and 07 codes, likely to have stranded at the end of 2006",sep=";")
NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[6])] <- "7-Dec-2006"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[7]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[7])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[7])],"NZCETA had '?' for Date_stranded. This uncertainty seems to be captured in collection_date_mod",sep=";")
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[7])] <- "2008"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[8]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[8])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[8])],"DOC_Massey had '2008' for Collection_Date",sep=";")
NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[8])] <- "7-Nov-2008"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[9]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[9])] <- "NZCETA had a Date_stranded of '09/08/2018'. This would seem to be incorrrect based on the various codes with '2009' in them"
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[9])] <- "03/08/2009"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[10]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[10])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[10])],"NZCETA had a Date_stranded of '24/01/2011', one day later than Collection_Date of DOC_Massey",sep=";")
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[10])] <- "23/01/2011"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[11]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[11])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[11])],"DOC_Massey had the following for Collection_Date '9 Jul 1999 (23 Jul 99-WendiRoe)'",sep=";")
NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[11])] <- "9-Jul-1999"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[12]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[12])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[12])], "NZCETA had 'Jan-86' for Date_stranded",sep=";")
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[12])] <- "20-Jan-1986"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[13]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[13])] <- "NZCETA had '18/02/1998' for Date_stranded"
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[13])] <- "8/02/1998"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[14]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[14])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[14])],"NZCETA had 'Mar-00' as Date_stranded",sep=";")
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[14])] <- "27-Mar-2000"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[15]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[15])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[15])],"NZCETA had '01/11/1997' for Date_stranded",sep=";")
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[15])] <- "25/11/1997"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[16]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[16])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[16])],"NZCETA had '01/11/1997' for Date_stranded",sep=";")
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[16])] <- "25/11/1997"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[17]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[17])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[17])],"NZCETA had a Date_stranded of 01/03/1996, DOC_Massey had a Collection_Date of '1/8/1997 (Mar96 strand databse & FP)'. Given DOC_Massey had noted difference in dates, going with its date",sep=";")
NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[17])] <- "1-Aug-1997"
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[17])] <- "1/8/1997"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[18]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[18])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[18])], "NZCETA had a Date_stranded of '11/04/1996', DOC_Massey had a Collection_Date of '6/8/1996 or 4/11/96' (assume 4/11 is actually meant to be 11/4). Going with 11-Apr-1996 seeing as both fields had that date",sep=";")
NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[18])] <- "11-Apr-1996"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[19]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[19])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[19])],"DOC_Massey had '1996' as Collection_Date, NZCETA had '18/06/2005'. In absence of additional information, going to put date as 1996-2005",sep=";")
NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[19])] <- "1996-2005"
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[19])] <- "1996-2005"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[20]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[20])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[20])],"NZCETA had Date_stranded of '24/04/2005', DOC_Massey had Collection_Date of '1937'",sep=";")
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[20])] <- "1937"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[21]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[21])] <- "NZCETA had a Date_stranded of '25/11/1992', but based on H-codes etc, this is incorrect"
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[21])] <- "25/11/2002"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[22]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[22])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[22])],"DOC_Massey had '11/08/1995' as Collection_Date, NZCETA had '1993'. In absence of additional information, going to put date as 1993-1995",sep=";")
NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[22])] <- "1993-1995"
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[22])] <- "1993-1995"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[23]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[23])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[23])],"NZCETA had a Date_stranded of 11/03/1996, DOC_Massey had a Collection_Date of '6/7/1991 (11Mar96-strand database & FP)'. Given DOC_Massey had noted difference in dates, going with its date",sep=";")
NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[23])] <- "6-Jul-1991"
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[23])] <- "6/7/1991"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[24]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[24])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[24])],"NZCETA had a Date_stranded of 11/03/1996, DOC_Massey had a Collection_Date of '6/7/1996 (11Mar96-strand database & FP)'. Given DOC_Massey had noted difference in dates, going with its date",sep=";")
NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[24])] <- "6-Jul-1996"
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[24])] <- "6/7/1996"

NZCETA_DOC_joined %>% filter(scrubbed_uoa_code==sample_code_list[25]) %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[25])] <- "NZCETA had a Date_stranded of '03/12/2006', one day before Collection_Date of DOC_Massey"
NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[25])] <- "4/12/2006"

# Cool, we've solved all those problem children!
NZCETA_DOC_joined %>% mutate(date_stranded_mod=gsub("^0","",gsub("/09/","-Sep-",gsub("/08/","-Aug-",gsub("/07/","-Jul-",gsub("/06/","-Jun-",gsub("/05/","-May-",gsub("/04/","-Apr-",gsub("/03/","-Mar-",gsub("/02/","-Feb-",gsub("/01/","-Jan-",gsub("/12/","-Dec-",gsub("/11/","-Nov-",gsub("/10/","-Oct-",gsub("/9/","-Sep-",gsub("/8/","-Aug-",gsub("/7/","-Jul-",gsub("/6/","-Jun-",gsub("/5/","-May-",gsub("/4/","-Apr-",gsub("/3/","-Mar-",gsub("/2/","-Feb-",gsub("/1/","-Jan-",Date_stranded,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE))) %>% filter(date_stranded_mod!=collection_date_mod) %>% select(date_stranded_mod,collection_date_mod)

# Now let's double check those with NAs so we can see whether we can propagate dates across
# There are 11 records where NZCETA has data, but no Date_stranded, but where DOC_Massey has information
NZCETA_DOC_joined %>% mutate(date_stranded_mod=gsub("^0","",gsub("/09/","-Sep-",gsub("/08/","-Aug-",gsub("/07/","-Jul-",gsub("/06/","-Jun-",gsub("/05/","-May-",gsub("/04/","-Apr-",gsub("/03/","-Mar-",gsub("/02/","-Feb-",gsub("/01/","-Jan-",gsub("/12/","-Dec-",gsub("/11/","-Nov-",gsub("/10/","-Oct-",gsub("/9/","-Sep-",gsub("/8/","-Aug-",gsub("/7/","-Jul-",gsub("/6/","-Jun-",gsub("/5/","-May-",gsub("/4/","-Apr-",gsub("/3/","-Mar-",gsub("/2/","-Feb-",gsub("/1/","-Jan-",Date_stranded,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE))) %>% filter(is.na(date_stranded_mod)&(!is.na(Code)|Code!="NA")) %>% filter(!is.na(Collection_Date)|Collection_Date!="NA") %>% select(Code,UoA_Code,Date_stranded,Date_received,Collection_Date,Location.x,Location.y)

# CHEWC020 was the only one where there was conflict. Remainder look clean to double-check collection_date_mod
# and add note to reconciliation notes
as.matrix(NZCETA_DOC_joined[(which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC020")),])
NZCETA_DOC_joined$reconciliation_notes[(which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC020"))] <- paste(NZCETA_DOC_joined$reconciliation_notes[(which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC020"))],"NZCETA had no Date_stranded, but had a Date_received of 'Oct-98', while DOC_Massey had '9/12/2000 (FP-1996)'. Because of this confusion, put down 1998-2000 as the collection_date_mod",sep=";")
NZCETA_DOC_joined$collection_date_mod[(which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC020"))] <- "1998-2000"
NZCETA_DOC_joined$Date_stranded[(which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC020"))] <- "1998-2000"

# For everyone else, we'll just mention in the reconciliation_notes that the date has been taken from DOC_Massey because
# no information was available in NZCETA
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% mutate(date_stranded_mod=gsub("^0","",gsub("/09/","-Sep-",gsub("/08/","-Aug-",gsub("/07/","-Jul-",gsub("/06/","-Jun-",gsub("/05/","-May-",gsub("/04/","-Apr-",gsub("/03/","-Mar-",gsub("/02/","-Feb-",gsub("/01/","-Jan-",gsub("/12/","-Dec-",gsub("/11/","-Nov-",gsub("/10/","-Oct-",gsub("/9/","-Sep-",gsub("/8/","-Aug-",gsub("/7/","-Jul-",gsub("/6/","-Jun-",gsub("/5/","-May-",gsub("/4/","-Apr-",gsub("/3/","-Mar-",gsub("/2/","-Feb-",gsub("/1/","-Jan-",Date_stranded,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE))) %>% filter((is.na(date_stranded_mod)|date_stranded_mod=="NA")&(!is.na(Code)|Code!="NA")) %>% filter(!is.na(Collection_Date)|Collection_Date!="NA") %>% select(scrubbed_uoa_code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(reconciliation_notes=ifelse((scrubbed_uoa_code %in% sample_code_list),
                                                         ifelse((is.na(reconciliation_notes)|reconciliation_notes=="NA"),
                                                                "collection_date_mod taken from DOC_Massey database because no date information available in NZCETA",paste(reconciliation_notes,"collection_date_mod taken from DOC_Massey database because no date information available in NZCETA",sep=";")),reconciliation_notes))

NZCETA_DOC_joined$Date_stranded[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list)] <- NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list)]

# There are just 2 records where DOC_Massey has no Collection_Date, but does have other info, and where NZCETA has Date infomation
NZCETA_DOC_joined %>% mutate(date_stranded_mod=gsub("^0","",gsub("/09/","-Sep-",gsub("/08/","-Aug-",gsub("/07/","-Jul-",gsub("/06/","-Jun-",gsub("/05/","-May-",gsub("/04/","-Apr-",gsub("/03/","-Mar-",gsub("/02/","-Feb-",gsub("/01/","-Jan-",gsub("/12/","-Dec-",gsub("/11/","-Nov-",gsub("/10/","-Oct-",gsub("/9/","-Sep-",gsub("/8/","-Aug-",gsub("/7/","-Jul-",gsub("/6/","-Jun-",gsub("/5/","-May-",gsub("/4/","-Apr-",gsub("/3/","-Mar-",gsub("/2/","-Feb-",gsub("/1/","-Jan-",Date_stranded,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE))) %>% filter(!is.na(date_stranded_mod)|date_stranded_mod!="NA") %>% filter(((is.na(Collection_Date)|Collection_Date=="NA"))&(UoA_Code!="NA"|!is.na(UoA_Code))) %>% select(Code,UoA_Code,Date_stranded,Date_received,Collection_Date,Location.x,Location.y)

# Let's capture their names and look at them in greater detail
sample_code_list <- as.matrix(NZCETA_DOC_joined %>% mutate(date_stranded_mod=gsub("^0","",gsub("/09/","-Sep-",gsub("/08/","-Aug-",gsub("/07/","-Jul-",gsub("/06/","-Jun-",gsub("/05/","-May-",gsub("/04/","-Apr-",gsub("/03/","-Mar-",gsub("/02/","-Feb-",gsub("/01/","-Jan-",gsub("/12/","-Dec-",gsub("/11/","-Nov-",gsub("/10/","-Oct-",gsub("/9/","-Sep-",gsub("/8/","-Aug-",gsub("/7/","-Jul-",gsub("/6/","-Jun-",gsub("/5/","-May-",gsub("/4/","-Apr-",gsub("/3/","-Mar-",gsub("/2/","-Feb-",gsub("/1/","-Jan-",Date_stranded,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE))) %>% filter(!is.na(date_stranded_mod)|date_stranded_mod!="NA") %>% filter((is.na(Collection_Date)|Collection_Date=="NA")&(UoA_Code!="NA"|!is.na(UoA_Code))) %>% select(scrubbed_uoa_code))

# Neither of these seems to have any problem with taking the date from Date_stranded, so will
# do this and modify reconciliation_notes
as.matrix(NZCETA_DOC_joined[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[1]),])
as.matrix(NZCETA_DOC_joined[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[2]),])

NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[1])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[1])],"collection_date_mod taken from NZCETA because Collection_Date not given for DOC_Massey",sep=";")
NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[1])] <- "23-Jan-2001"

NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[2])] <- "collection_date_mod taken from NZCETA because Collection_Date not given for DOC_Massey"
NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$scrubbed_uoa_code==sample_code_list[2])] <- "28-Nov-1994"

# OK, let's have another look. 190 records have data that are now consistent across data_stranded_mod and collection_date_mod
# (in some cases because we've physically changed stuff after figurring it out)
NZCETA_DOC_joined %>% mutate(date_stranded_mod=gsub("^0","",gsub("/09/","-Sep-",gsub("/08/","-Aug-",gsub("/07/","-Jul-",gsub("/06/","-Jun-",gsub("/05/","-May-",gsub("/04/","-Apr-",gsub("/03/","-Mar-",gsub("/02/","-Feb-",gsub("/01/","-Jan-",gsub("/12/","-Dec-",gsub("/11/","-Nov-",gsub("/10/","-Oct-",gsub("/9/","-Sep-",gsub("/8/","-Aug-",gsub("/7/","-Jul-",gsub("/6/","-Jun-",gsub("/5/","-May-",gsub("/4/","-Apr-",gsub("/3/","-Mar-",gsub("/2/","-Feb-",gsub("/1/","-Jan-",Date_stranded,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE))) %>% filter(date_stranded_mod==collection_date_mod) 

# There are 84 records where there is no collection_date, but there is date_stranded_mod information
# 27 records where there is no date_stranded_mod information, but there is collection_date_information (these will be
# the records that are not found in NZCETA), and a further 26 where neither field has data
NZCETA_DOC_joined %>% mutate(date_stranded_mod=gsub("^0","",gsub("/09/","-Sep-",gsub("/08/","-Aug-",gsub("/07/","-Jul-",gsub("/06/","-Jun-",gsub("/05/","-May-",gsub("/04/","-Apr-",gsub("/03/","-Mar-",gsub("/02/","-Feb-",gsub("/01/","-Jan-",gsub("/12/","-Dec-",gsub("/11/","-Nov-",gsub("/10/","-Oct-",gsub("/9/","-Sep-",gsub("/8/","-Aug-",gsub("/7/","-Jul-",gsub("/6/","-Jun-",gsub("/5/","-May-",gsub("/4/","-Apr-",gsub("/3/","-Mar-",gsub("/2/","-Feb-",gsub("/1/","-Jan-",Date_stranded,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE))) %>% group_by(is.na(date_stranded_mod),is.na(collection_date_mod)) %>% count()

# Fingers crossed, the ones that have NA for collection_date_mod (the n = 84 bunch) don't have any sample data, and then we can just
# propagate across date_stranded_mod if it is appropriately formatted. Let's check this out.
NZCETA_DOC_joined %>% mutate(date_stranded_mod=gsub("^0","",gsub("/09/","-Sep-",gsub("/08/","-Aug-",gsub("/07/","-Jul-",gsub("/06/","-Jun-",gsub("/05/","-May-",gsub("/04/","-Apr-",gsub("/03/","-Mar-",gsub("/02/","-Feb-",gsub("/01/","-Jan-",gsub("/12/","-Dec-",gsub("/11/","-Nov-",gsub("/10/","-Oct-",gsub("/9/","-Sep-",gsub("/8/","-Aug-",gsub("/7/","-Jul-",gsub("/6/","-Jun-",gsub("/5/","-May-",gsub("/4/","-Apr-",gsub("/3/","-Mar-",gsub("/2/","-Feb-",gsub("/1/","-Jan-",Date_stranded,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE))) %>% filter(is.na(collection_date_mod)) %>% filter(!is.na(date_stranded_mod))

# Sweet. OK, let's check the dates to see if they are formatted OK
NZCETA_DOC_joined %>% mutate(date_stranded_mod=gsub("^0","",gsub("/09/","-Sep-",gsub("/08/","-Aug-",gsub("/07/","-Jul-",gsub("/06/","-Jun-",gsub("/05/","-May-",gsub("/04/","-Apr-",gsub("/03/","-Mar-",gsub("/02/","-Feb-",gsub("/01/","-Jan-",gsub("/12/","-Dec-",gsub("/11/","-Nov-",gsub("/10/","-Oct-",gsub("/9/","-Sep-",gsub("/8/","-Aug-",gsub("/7/","-Jul-",gsub("/6/","-Jun-",gsub("/5/","-May-",gsub("/4/","-Apr-",gsub("/3/","-Mar-",gsub("/2/","-Feb-",gsub("/1/","-Jan-",Date_stranded,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE))) %>% filter(is.na(collection_date_mod)) %>% filter(!is.na(date_stranded_mod)) %>% select(date_stranded_mod) %>% print(n=84)

# Perfect! These all look well formated except for "?" and "Jan-00", but we can fish these out afterwards and tweak them
# what we need to do is to create an ifelse statement to create collection_date_mod off the current date_stranded_mod formula
# but only if it fulfils the filter conditions (e.g. is.na()!)
# because we won't be able to double-check that this has worked using an is.na(collection_date_mod)
# because the collection date will be already filled in, we are going to get a list
# of sample names and go from there

sample_code_list <- as.matrix(NZCETA_DOC_joined %>% mutate(date_stranded_mod=gsub("^0","",gsub("/09/","-Sep-",gsub("/08/","-Aug-",gsub("/07/","-Jul-",gsub("/06/","-Jun-",gsub("/05/","-May-",gsub("/04/","-Apr-",gsub("/03/","-Mar-",gsub("/02/","-Feb-",gsub("/01/","-Jan-",gsub("/12/","-Dec-",gsub("/11/","-Nov-",gsub("/10/","-Oct-",gsub("/9/","-Sep-",gsub("/8/","-Aug-",gsub("/7/","-Jul-",gsub("/6/","-Jun-",gsub("/5/","-May-",gsub("/4/","-Apr-",gsub("/3/","-Mar-",gsub("/2/","-Feb-",gsub("/1/","-Jan-",Date_stranded,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE))) %>% filter(is.na(collection_date_mod)) %>% filter(!is.na(date_stranded_mod)) %>% select(scrubbed_uoa_code))

NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(collection_date_mod=ifelse(is.na(collection_date_mod),ifelse((scrubbed_uoa_code %in% sample_code_list),(gsub("^0","",gsub("/09/","-Sep-",gsub("/08/","-Aug-",gsub("/07/","-Jul-",gsub("/06/","-Jun-",gsub("/05/","-May-",gsub("/04/","-Apr-",gsub("/03/","-Mar-",gsub("/02/","-Feb-",gsub("/01/","-Jan-",gsub("/12/","-Dec-",gsub("/11/","-Nov-",gsub("/10/","-Oct-",gsub("/9/","-Sep-",gsub("/8/","-Aug-",gsub("/7/","-Jul-",gsub("/6/","-Jun-",gsub("/5/","-May-",gsub("/4/","-Apr-",gsub("/3/","-Mar-",gsub("/2/","-Feb-",gsub("/1/","-Jan-",Date_stranded,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE))),collection_date_mod),collection_date_mod)) 

# OK, now just need to fix the "?" and the "Jan-00". We are going to change the "?" to "Unknown" and the "Jan-00" to "Jan-2000".
NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$collection_date_mod=="?")] <- "Unknown"
NZCETA_DOC_joined$collection_date_mod[which(NZCETA_DOC_joined$collection_date_mod=="Jan-00")] <- "Jan-2000"

# Alright, time to double-check:
NZCETA_DOC_joined %>% mutate(date_stranded_mod=gsub("^0","",gsub("/09/","-Sep-",gsub("/08/","-Aug-",gsub("/07/","-Jul-",gsub("/06/","-Jun-",gsub("/05/","-May-",gsub("/04/","-Apr-",gsub("/03/","-Mar-",gsub("/02/","-Feb-",gsub("/01/","-Jan-",gsub("/12/","-Dec-",gsub("/11/","-Nov-",gsub("/10/","-Oct-",gsub("/9/","-Sep-",gsub("/8/","-Aug-",gsub("/7/","-Jul-",gsub("/6/","-Jun-",gsub("/5/","-May-",gsub("/4/","-Apr-",gsub("/3/","-Mar-",gsub("/2/","-Feb-",gsub("/1/","-Jan-",Date_stranded,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE))) %>% group_by(is.na(date_stranded_mod),is.na(collection_date_mod)) %>% count()

# Great! We now have 274 records where collection_date_mod and date_stranded_mod both have entries
# 27 records where there is no data for data_stranded_mod (our 27 records not present in NZCETA)
# and 26 records where neither database has data for date. One final check: that date_stranded_mod and 
# collection_date_mod match up for those entries where they both have data.
NZCETA_DOC_joined %>% mutate(date_stranded_mod=gsub("^0","",gsub("/09/","-Sep-",gsub("/08/","-Aug-",gsub("/07/","-Jul-",gsub("/06/","-Jun-",gsub("/05/","-May-",gsub("/04/","-Apr-",gsub("/03/","-Mar-",gsub("/02/","-Feb-",gsub("/01/","-Jan-",gsub("/12/","-Dec-",gsub("/11/","-Nov-",gsub("/10/","-Oct-",gsub("/9/","-Sep-",gsub("/8/","-Aug-",gsub("/7/","-Jul-",gsub("/6/","-Jun-",gsub("/5/","-May-",gsub("/4/","-Apr-",gsub("/3/","-Mar-",gsub("/2/","-Feb-",gsub("/1/","-Jan-",Date_stranded,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE))) %>% filter(!is.na(date_stranded_mod)) %>% filter(date_stranded_mod!=collection_date_mod) %>% select(date_stranded_mod,Collection_Date,collection_date_mod)

# Just our ? and our "Jan-2000" entries. Ready to ditch our unnecessary columns!
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% select(-Date_stranded,-Collection_Date)

# OK, next job: NZCETA has a 'Who' column, which in DOC_Massey is split into 'Collected_By' and 'DoC_Agency'
# I prefer the DOC_Massey way of doing things, because it splits out component information into separate
# columns. Let's see if this info mostly matches up for our samples. We are filtering here just for records
# where 'Collected_By' is not in Who, and Doc_Agency is not in Who
NZCETA_DOC_joined %>% filter(!grepl(Collected_By,Who)) %>% filter(!grepl(DoC_Agency,Who))
# Only 23 records where the info didn't entirely match up. Let's pull up some info to help us look
# at these cases. First off we are going to eyeball the locations to make sure these are the same
NZCETA_DOC_joined %>% filter(!grepl(Collected_By,Who)) %>% 
  filter(!grepl(DoC_Agency,Who)) %>% select(scrubbed_uoa_code,Location.x,Location.y) %>% print(n=23)

# Yup, all the locations are consistent, so let's double check those other fields we are interested in
NZCETA_DOC_joined %>% filter(!grepl(Collected_By,Who)) %>% 
  filter(!grepl(DoC_Agency,Who)) %>% select(scrubbed_uoa_code,Who,Collected_By,DoC_Agency) %>% print(n=23)

# OK, mostly we can go with DOC_Massey, but a few things to comment on. For CHE09NZ03, DoC Central Otago is given for NZCETA
# based on google, this should be DOC Coastal Otago. This is also not the same as the Collected_By field, where Steve Dawson
# is given. In other instances where Steve has collected a sample, he's been given NA as his DoC_Agency (not being a DoC employee)
# Let's tweak all that here.

NZCETA_DOC_joined$Collected_By[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ03")] <- "Steve Dawson/Matt Elison"
NZCETA_DOC_joined$DoC_Agency[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ03")] <- "NA/Coastal Otago"
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ03")] <- "Matt Elison of DOC Coastal Otago (erroneously Central Otago) given as collector in NZCETA. Steve Dawson in DOC_Massey"

# CHE10NZ03, CHE10NZ04, and CHE10NZ05 have D. Neale in Collected_By instead of Don Neale. Going to change this to "Don Neale"
# because more information always better than less, and also change the "West Coast" in the DoC_Agency to Hokitika
sample_code_list <- c("CHE10NZ03", "CHE10NZ04", "CHE10NZ05")

NZCETA_DOC_joined$Collected_By[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list)] <- "Don Neale"
NZCETA_DOC_joined$DoC_Agency[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list)] <- "Hokitika"
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list[2])] <- "DOC_Massey had 'D. Neale' for 'Collected_By' and 'West Coast' for 'DoC_Agency'"
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list[1])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list[1])], "DOC_Massey had 'D. Neale' for 'Collected_By' and 'West Coast' for 'DoC_Agency'", sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list[3])] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code %in% sample_code_list[3])], "DOC_Massey had 'D. Neale' for 'Collected_By' and 'West Coast' for 'DoC_Agency'", sep=";")

# Everything else looks good, so we can now ditch the 'Who' column
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% select(-Who)

# Alright, been procrastinating for long enough...time to reconcile the Location columns
NZCETA_DOC_joined %>% filter(Location.x!=Location.y)
# 98 records - I knew it was going to be sticky :) Let's focus in on the Location fields so we can
# more easily compare them
NZCETA_DOC_joined %>% filter(Location.x!=Location.y) %>% select(scrubbed_uoa_code,Location.x,Location.y)
# The first three records had slightly different capitalization/more details in Location.y, so replaced Location.x with Location.y
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE03NZ02")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE03NZ02")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE03NZ05")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE03NZ05")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE04NZ02")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE04NZ02")]
# The next sample had what looks to be NZMS260 coordinates (northing and easting). Checking to see if they are recorded in other fields
as.matrix(NZCETA_DOC_joined[(which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE04NZ05")),])
# Yup, cool - captured in the LatitudeS_Northing and LongitudeE_Easting fields.
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE04NZ05")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE04NZ05")]

# Every now and then I'm going to rerun the following code to get the up to date list of who's location we need to look at:
NZCETA_DOC_joined %>% filter(Location.x!=Location.y) %>% select(scrubbed_uoa_code,Location.x,Location.y)
# First up is CHE04NZ06. This is the record we combined although we are not 100% on it.
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE04NZ06")] <- "Sumner Beach, Canterbury"
NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE04NZ06")] <- "Sumner Beach, Canterbury"
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE04NZ06")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE04NZ06")],"Location in NZCETA was given as 'Sumner Beach'. Location in DOC_Massey given as 'Canterbury'",sep=";")
# The following have additional detail for Location.y compared with Location.x or differing capitalization
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE04NZ13")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE04NZ13")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ09")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ09")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ12")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ12")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ13")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ13")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ14")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ14")]

NZCETA_DOC_joined %>% filter(Location.x!=Location.y) %>% select(scrubbed_uoa_code,Location.x,Location.y)

NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEM06NZ05")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEM06NZ05")]
# The next sample had what looks to be NZMS260 coordinates (northing and easting). Checking to see if they are recorded in other fields
as.matrix(NZCETA_DOC_joined[(which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE06NZ08")),])
# Yup, cool - captured in the LatitudeS_Northing and LongitudeE_Easting fields.
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE06NZ08")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE06NZ08")]

# The next sample had what looks to be NZMS260 coordinates (northing and easting). Checking to see if they are recorded in other fields
as.matrix(NZCETA_DOC_joined[(which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ01")),])
# Yup, cool - captured in Location.y
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ01")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ01")]

# The following had more information/differing capialization in Location.y versus Location.x
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ02")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ02")]
# OK, this one is a bit weird: it has Port Craig, Kaiapoi for Location.x and Te Wae Wae Bay, Port Craig, Kaiapoi for Location.y. 
# I think the Kaiapoi part is wrong because that's in Canterbury, so let's look at the rest of the entry for more clues
as.matrix(NZCETA_DOC_joined[(which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ03")),])
# DoC_Agency is given as Hurihiku, which is almost certainly meant to be 'Murihiku' (Southland), which suggests Kaiapoi shouldn't
# bet there, and the region should be Southland, not Canterbury as well. A few things to correct on this one!
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ03")] <- "Port Craig, Te Waewae Bay"
NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ03")] <- "Port Craig, Te Waewae Bay"
NZCETA_DOC_joined$Region[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ03")] <- "Southland"
NZCETA_DOC_joined$DoC_Agency[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ03")] <- "Murihiku"
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ03")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ03")],"NZCETA had 'Port Craig, Kaiapoi' as the Location, and 'Canterbury' as the region. DOC_Massey had 'Te Wae Wae Bay, Port Craig, Kaiapoi' as the Location, and 'Hurihiku' as the DoC_Agency. This likely should have been 'Murihiku' i.e. Southland, so the Kaiapoi/Canterbury part is not correct (Port Craig is in Te Waewae Bay)",sep=";")

NZCETA_DOC_joined %>% filter(Location.x!=Location.y) %>% select(scrubbed_uoa_code,Location.x,Location.y)

# More detail available for location.x compared to location.y
NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ04")] <- NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ04")]
# More detail available for location.y compared to location.x
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ05")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ05")]

# River spelled Motuhinau for Location.x, Mokihinau for Location.y. Going to look at rest of entry to try and figure out what 
# river it is referring to (because neither coming up in a google search)
as.matrix(NZCETA_DOC_joined[(which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ06")),])
# OK, I think it is probably referring to Mokihinui River, so going to modify both, as well as leave a note in the 
# reconciliation notes 
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ06")] <- "Westport, 200m N of Mokihinui River mouth"
NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ06")] <- "Westport, 200m N of Mokihinui River mouth"
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ06")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ06")],"NZCETA has Motuhinau for Location, DOC_Massey has Mokihinau, but I think location should likely refer to the Mokihinui River", sep=";")

NZCETA_DOC_joined %>% filter(Location.x!=Location.y) %>% select(scrubbed_uoa_code,Location.x,Location.y)

# Location.x more informative than location.y
NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ08")] <- NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ08")] 
# Location.y more informative than location.x
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ13")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ13")] 

# Both locations for CHE07NZ14 refer to Central Otago - from context, folks probably can get that means the centre
# of coastal Otago, but going to modify it just in case!
as.matrix(NZCETA_DOC_joined[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ14"),])
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ14")] <- "Long Beach, North of Heyward Point, Central Coastal Otago"
NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ14")] <- "Long Beach, North of Heyward Point, Central Coastal Otago"
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ14")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE07NZ14")],"Modified the location from 'Central Otago' to 'Central Coastal Otago'", sep=";")

NZCETA_DOC_joined %>% filter(Location.x!=Location.y) %>% select(scrubbed_uoa_code,Location.x,Location.y)

# Location.x has more information than location.y
as.matrix(NZCETA_DOC_joined[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE08NZ03"),])
NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE08NZ03")]  <- NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE08NZ03")] 

# Location.y has more information and/or correct capitalizaiton compared with location.x
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE08NZ05")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE08NZ05")] 
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE08NZ06")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE08NZ06")] 
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE08NZ07")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE08NZ07")] 
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ02")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ02")] 
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ03")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ03")] 
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ04")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ04")] 
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ06")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ06")] 
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ09")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ09")] 
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ10")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ10")] 
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ05")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ05")] 
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ01")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ01")] 

NZCETA_DOC_joined %>% filter(Location.x!=Location.y) %>% select(scrubbed_uoa_code,Location.x,Location.y)

# Location.x gives better information but spelling out abbreviations
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ03")] <- "1km south of Ruatapu, near Hokitika"
NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ03")] <- "1km south of Ruatapu, near Hokitika"

# Combination of location.x and location.y gives best detail
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ04")] <- "Serpentine Creek mouth, 20km north Hokitika (1/2 way between HK & GM)"  
NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ04")] <- "Serpentine Creek mouth, 20km north Hokitika (1/2 way between HK & GM)" 
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ10")] <- "Serpentine Creek mouth, 20km north Hokitika (1/2 way between HK & GM)"  
NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ10")] <- "Serpentine Creek mouth, 20km north Hokitika (1/2 way between HK & GM)" 

# Location.y gives better detail/capitalization than Location.x
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE98NZ01")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE98NZ01")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP01")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP01")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP02")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP02")]

NZCETA_DOC_joined %>% filter(Location.x!=Location.y) %>% select(scrubbed_uoa_code,Location.x,Location.y)

# Location.y gives better detail/capitalization than Location.x
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP03")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP03")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP04")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP04")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP05")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP05")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP06")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP06")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP07")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP07")]

# Missing final vowel off Waimakariri for both entries.
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP08")] <- "Pegasus Bay, Pines Beach, Waimakariri River Mouth"
NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP08")] <- "Pegasus Bay, Pines Beach, Waimakariri River Mouth"

# Location.y gives better detail/capitalization than Location.x
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP09")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP09")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP10")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP10")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP11")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP11")]

NZCETA_DOC_joined %>% filter(Location.x!=Location.y) %>% select(scrubbed_uoa_code,Location.x,Location.y)

# Location.y gives better detail/capitalization than Location.x
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP12")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP12")]

# Pretty sure the following should be 50m N of not or Waimari beach surf club
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP13")] <- "Pegasus Bay, 50m N of Waimari beach surf club"
NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP13")] <- "Pegasus Bay, 50m N of Waimari beach surf club"

# Location.y gives better detail/capitalization than Location.x
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP14")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP14")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP16")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP16")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP26")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP26")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP33")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP33")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP40")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP40")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP41")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP41")]

NZCETA_DOC_joined %>% filter(Location.x!=Location.y) %>% select(scrubbed_uoa_code,Location.x,Location.y)

# Location.y gives better detail/capitalization than Location.x
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP42")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP42")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP53")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP53")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP57")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP57")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP58")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP58")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP59")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP59")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP61")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP61")]

# This sample has Pegasus Bay, Akaroa Harbour, which are obviously not the same place!
as.matrix(NZCETA_DOC_joined[(which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP62")),])
# The sample was collected by Al Hutt associated with DOC Akaroa, so Akaroa would seem like a good bet
# but this will need to be noted in the reconciliation notes as well.
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP62")] <- "Akaroa Harbour"
NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP62")] <- "Akaroa Harbour"
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP62")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP62")],"NZCETA had 'Pegasus Bay, Akaroa Harbour', DOC_Massey had 'Pegasus Bay, Akaroa Harbour ??'. Given the DOC agency involved in collecting, think this should probably just be Akaroa Harbour",sep=";")

NZCETA_DOC_joined %>% filter(Location.x!=Location.y) %>% select(scrubbed_uoa_code,Location.x,Location.y)

# Location.y gives better detail/capitalization than Location.x
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHENI001")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHENI001")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHENI002")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHENI002")]

# Lat/long given in NZCETA, making sure it is present in other fields
as.matrix(NZCETA_DOC_joined[(which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHENI004")),])
#yup cool
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHENI004")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHENI004")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHENI007")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHENI007")]

# Location x was more informative than location.y
NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHENI012")] <- NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHENI012")]

# Location.y was more informative than location.x
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHENI016")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHENI016")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHENI017")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHENI017")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI03")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI03")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI04")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI04")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI27")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI27")]

NZCETA_DOC_joined %>% filter(Location.x!=Location.y) %>% select(scrubbed_uoa_code,Location.x,Location.y)

#CHESI28 has Rangitata River Mouth for location.x, and Greymouth, North Beach for location.y (different coasts!)
as.matrix(NZCETA_DOC_joined[(which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI28")),])
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI28")] <- "Rangitata River Mouth"
NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI28")] <- "Rangitata River Mouth"
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI28")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI28")],"NZCETA had Rangitata as Location, DOC_Massey had Greymouth, North Beach. Given Region recorded as Canterbury, have gone with Rangitata",sep=";")

# Location.y has more information than Location.x/correct capitalization
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETI09")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETI09")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETI12")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETI12")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETI13")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETI13")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW03")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW03")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW04")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW04")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW05")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW05")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW07")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW07")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC001")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC001")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC02-06")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC02-06")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC006")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC006")]

NZCETA_DOC_joined %>% filter(Location.x!=Location.y) %>% select(scrubbed_uoa_code,Location.x,Location.y)

# Location.y has more information than Location.x/correct capitalization
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC007")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC007")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC008")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC008")]

# Location.x was "Okarito beach netween Waitahi Bluff and Commisioner Bluff", Location.y was "Jackson Bay"
as.matrix(NZCETA_DOC_joined[(which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC012")),])
# Based on Code of "CheWC12/ CheOk01", think Location.x is the correct location.
NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC012")] <- NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC012")]
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC012")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC012")],"DOC_Massey had Location as 'Jackson Bay'. Based on Code, Location taken from NZCETA",sep=";")

# Location.y has more information than Location.x/correct capitalization
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC016")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC016")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC018")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC018")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC019")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC019")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEM06NZ02")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEM06NZ02")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC021")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC021")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEM06NZ04")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEM06NZ04")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEM07NZ01")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEM07NZ01")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEM07NZ09")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEM07NZ09")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEM10NZ06")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEM10NZ06")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEM13NZ01")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEM13NZ01")]
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW06")] <- NZCETA_DOC_joined$Location.y[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETWW06")]

# Let's check if we got everyone
NZCETA_DOC_joined %>% filter(Location.x!=Location.y) %>% select(scrubbed_uoa_code,Location.x,Location.y)

# Cool, sorted out all the locations, so now we can ditch one of the location columns
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% select(-Location.y)

##############################################################
#### 6. QC'ing and obtaining the location for each sample ####
##############################################################
# OK, so that is all the columns we can combine I think. Next up we are going to QC
# the individual location columns by looking at the unique values present (to correct for typos etc)
# We'll eventually use this info to populate 'hologenome_region' which is what we'll be using
# to select samples based on geography.
NZCETA_DOC_joined %>% group_by(Location.x) %>% count() %>% print(n=224)

# OK, some to fix/standardize here:
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="1.3km north of the Waiairi surf club, pegasus Bay")] <- "1.3km north of Waimairi Surf club, Pegasus bay"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="100m S of the south Brighton Surf Club, Pegasus Bay")] <- "100m Sth of Brighton Surf Club, Pegasus Bay"   
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Fossil Point base of farewell spit M24 777 87200")] <- "Fossil Point base of Farewell Spit M24 777 87200"                                                                                 
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Karotahi Beach, Waiaku")] <- "Karioitahi Beach, Waiaku" 
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Leithfield Beach, Pegasus Bay")] <- "Leithfield beach; Pegasus Bay"                                                                                                    
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Quail Island, Lyttleton")] <- "Quail Island, Lyttelton Harbour" 
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Waikowaiiti Beach, Dunedin")] <- "Waikouaiti Beach, Dunedin"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Waikowaiti")] <- "Waikouaiti Beach, Dunedin"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Washdyke, timaru")] <- "Washdyke, Timaru"

# It might actually be worth creating a new variable that captures some of the information about the broad locations
# in Location.x. This could also help with QCing the data as well. 'Region' is already preent, and we can double check
# to make sure this is consistent after creating our broader locations

# To decide on these raw locations, let's see what get mentioned the most in our data
raw_location_names <- unlist(strsplit((paste(as.matrix(NZCETA_DOC_joined$Location.x),collapse=" ")),split=" "))
location_count <- matrix(nrow=length(unique(raw_location_names)),ncol=2)
location_count[,1] <- unique(raw_location_names)
for (i in 1:dim(location_count)[1]) {
  location_count[i,2] <- sum(raw_location_names %in% location_count[i,1])
}
location_count[order(-as.numeric(location_count[,2])),]

# Based on this list and a few iterations, I came up with the following broad locations
NZCETA_DOC_joined %>% mutate(broad_location=ifelse(grepl("Northland",Location.x),"Northland",ifelse(grepl("Karamea",Location.x),"Karamea",ifelse(grepl("Golden Bay",Location.x),"Golden Bay",ifelse(grepl("Stewart Island",Location.x),"Stewart Island",ifelse(grepl("Cloudy Bay",Location.x),"Cloudy Bay",ifelse(grepl("Kapiti",Location.x),"Kapiti",ifelse(grepl("Waikato|Raglan",Location.x),"Waikato",ifelse(grepl("Dunedin",Location.x),"Dunedin",ifelse(grepl("Blaketown",Location.x),"Blaketown",ifelse(grepl("Paroa",Location.x),"Paroa",ifelse(grepl("Barrytown|Punakaiki",Location.x),"Barrytown/Punakaiki",ifelse(grepl("Okarito|Franz Josef",Location.x),"Okarito/Franz Josef",ifelse(grepl("Banks Peninsula|BP",Location.x),"Banks Peninsula",ifelse(grepl("Taranaki",Location.x),"Taranaki",ifelse(grepl("Farewell",Location.x),"Farewell Spit",ifelse(grepl("Lyttelton",Location.x),"Lyttelton",ifelse(grepl("Haast|Neils",Location.x),"Haast/Jackson Bay",ifelse(grepl("Kaikoura",Location.x),"Kaikoura",ifelse(grepl("Auckland",Location.x),"Auckland",ifelse(grepl("Akaroa",Location.x),"Akaroa",ifelse(grepl("Westport",Location.x),"Westport",ifelse(grepl("Greymouth",Location.x),"Greymouth",ifelse(grepl("Timaru",Location.x),"Timaru",ifelse(grepl("Waewae",Location.x),"Te Waewae",ifelse(grepl("Queen Charlotte",Location.x),"Queen Charlotte Sound",ifelse(grepl("Hokitika",Location.x,),"Hokitika",ifelse(grepl("Pegasus",Location.x),"Pegasus Bay",ifelse(grepl("Buller",Location.x),"Buller",ifelse(grepl("Westland",Location.x),"Westland",ifelse(grepl("Marlborough",Location.x),"Marlborough",ifelse(grepl("Canterbury",Location.x),"Canterbury",ifelse(grepl("Otago",Location.x),"Otago",ifelse(grepl("Nelson",Location.x),"Nelson",ifelse(grepl("Piha",Region),"Auckland",ifelse(grepl("Canterbury",Region),"Canterbury",ifelse(grepl("Buller",Region),"Buller",ifelse(grepl("West Coast",Region),"West Coast",NA)))))))))))))))))))))))))))))))))))))) %>% group_by(broad_location) %>% count() %>% print(n=36)

# Let's see what locations are left out by this though, and whether we can tweak them to be included
# based on other samples that have a bit more information (or googling the locations!)
NZCETA_DOC_joined %>% mutate(broad_location=ifelse(grepl("Northland",Location.x),"Northland",ifelse(grepl("Karamea",Location.x),"Karamea",ifelse(grepl("Golden Bay",Location.x),"Golden Bay",ifelse(grepl("Stewart Island",Location.x),"Stewart Island",ifelse(grepl("Cloudy Bay",Location.x),"Cloudy Bay",ifelse(grepl("Kapiti",Location.x),"Kapiti",ifelse(grepl("Waikato|Raglan",Location.x),"Waikato",ifelse(grepl("Dunedin",Location.x),"Dunedin",ifelse(grepl("Blaketown",Location.x),"Blaketown",ifelse(grepl("Paroa",Location.x),"Paroa",ifelse(grepl("Barrytown|Punakaiki",Location.x),"Barrytown/Punakaiki",ifelse(grepl("Okarito|Franz Josef",Location.x),"Okarito/Franz Josef",ifelse(grepl("Banks Peninsula|BP",Location.x),"Banks Peninsula",ifelse(grepl("Taranaki",Location.x),"Taranaki",ifelse(grepl("Farewell",Location.x),"Farewell Spit",ifelse(grepl("Lyttelton",Location.x),"Lyttelton",ifelse(grepl("Haast|Neils",Location.x),"Haast/Jackson Bay",ifelse(grepl("Kaikoura",Location.x),"Kaikoura",ifelse(grepl("Auckland",Location.x),"Auckland",ifelse(grepl("Akaroa",Location.x),"Akaroa",ifelse(grepl("Westport",Location.x),"Westport",ifelse(grepl("Greymouth",Location.x),"Greymouth",ifelse(grepl("Timaru",Location.x),"Timaru",ifelse(grepl("Waewae",Location.x),"Te Waewae",ifelse(grepl("Queen Charlotte",Location.x),"Queen Charlotte Sound",ifelse(grepl("Hokitika",Location.x,),"Hokitika",ifelse(grepl("Pegasus",Location.x),"Pegasus Bay",ifelse(grepl("Buller",Location.x),"Buller",ifelse(grepl("Westland",Location.x),"Westland",ifelse(grepl("Marlborough",Location.x),"Marlborough",ifelse(grepl("Canterbury",Location.x),"Canterbury",ifelse(grepl("Otago",Location.x),"Otago",ifelse(grepl("Nelson",Location.x),"Nelson",ifelse(grepl("Piha",Region),"Auckland",ifelse(grepl("Canterbury",Region),"Canterbury",ifelse(grepl("Buller",Region),"Buller",ifelse(grepl("West Coast",Region),"West Coast",NA)))))))))))))))))))))))))))))))))))))) %>%  filter(is.na(broad_location)) %>% select(Location.x) %>% filter(!is.na(Location.x)) %>% print(n=32)

# OK, so we have a few locations that have not landed inside any of our broad categories, so going to tweak them below
# so they land into some of these categories and/or create some new broad categories for them if not
# TARANAKI
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Opunake")] <- "Opunake, Taranaki"

# TIMARU
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="North Side of Rangitata Huts")] <- "North Side of Rangitata Huts, north of Timaru"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Rangitata River Mouth")] <- "Timaru, Rangitata River Mouth"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Opihi River Mouth")] <- "Opihi River mouth north of Timaru"

# PEGASUS BAY
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Taylors Mistake, Christchurch")] <- "Taylors Mistake, Christchurch, Pegasus Bay"                                                                                               
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Ashworth's Beach, north of Christchurch; (Beach between Ashley River Mouth and  Ash----- Lagoon? can't read incident report)")] <- "Ashworth's Beach, north of Christchurch; (Beach between Ashley River Mouth and  Ash----- Lagoon? can't read incident report), Pegasus Bay"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Below MHWS, opposite Leithfield Motor Camp")] <- "Below MHWS, opposite Leithfield Motor Camp, Pegasus Bay"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Gore Bay")] <- "Gore Bay, near Pegasus Bay"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Near Waimakariri River")] <- "Near Waimakariri River, Pegasus Bay"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Waimakariri River mouth")] <- "Waimakariri River mouth, Pegasus Bay"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Sumner Beach, Canterbury")] <- "Sumner Beach, Pegasus Bay,Canterbury"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="New Brighton Beach")] <- "New Brighton Beach, Pegasus Bay"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="200m south of South Brighton surf clubrooms")] <- "200m south of South Brighton surf clubrooms, Pegasus Bay"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="600m Sth Waikuku Beach Settlement")] <- "600m Sth Waikuku Beach Settlement, Pegasus Bay"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="In Rig Net off beach behind 39 Chambelain Ave, Amberley Beach")] <- "In Rig Net off beach behind 39 Chambelain Ave, Amberley Beach, Pegasus Bay"

# TE WAEWAE
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Orepuki-Southland. Halfway along the beach from Orepuki. Map D46; halfway between Dudley Street and Shilo")] <- "Orepuki-Southland. Halfway along the beach from Orepuki. Map D46; halfway between Dudley Street and Shilo, Te Waewae Bay"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Orepuki Southland 5423500 2004600")] <- "Orepuki Southland 5423500 2004600, Te Waewae Bay"    
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Pahia Point, Southland")] <- "Pahia Point, Southland, near Te Waewae Bay"

# AUCKLAND
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Karioitahi Beach, Waiaku")] <- "Karioitahi Beach, Waiaku, South Auckland"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Muriwai Beach")] <- "Muriwai Beach, North Auckland"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Clark's Beach, Manukau Harbour")] <-  "Clark's Beach, Manukau Harbour, Auckland"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Whatipu")] <- "Whatipu, Auckland"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Manukau Harbour")] <- "Manukau Harbour, Auckland"

# HOKITIKA
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="1km South of Waimea Creek between Arahura and Taramakau Rivers NZMS260 2351300 5839400")] <- "1km South of Waimea Creek between Arahura and Taramakau Rivers NZMS260 2351300 5839400, north of Hokitika"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Takutai Beach")] <- "Takutai Beach, south of Hokitika"

# DUNEDIN
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="3N miles north of Potato Point, 1.5-2Nmiles east of seacliff")] <- "3N miles north of Potato Point, 1.5-2Nmiles east of Seacliff, Dunedin"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Maia")] <- "Maia, Dunedin"

# KAPITI
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="neonate stranded on lower NI, Pekapeka Beach")] <- "neonate stranded on lower NI, Pekapeka Beach, Kapiti Coast"

# WESTPORT
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Larsen Rd Beach Access, West Coast")] <- "Larsen Rd Beach Access, West Coast, near Westport"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="9 Mile Beach, South of Cape Foulwind")] <- "9 Mile Beach, South of Cape Foulwind, near Westport"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="1.5km South of Granity, Buller")] <- "1.5km South of Granity, Buller, near Westport"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Waimangaroa North")] <- "Waimangaroa North, near Westport"
# The river name is spelled wrong apparently, as well
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Halfway between Beach Rd(Fairdown) and Whareakea R mouth")] <- "Halfway between Beach Rd(Fairdown) and Whareatea R mouth, near Westport"

# CANTERBURY
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Rakaia River Mouth - 5 km north beside sea/lagoon barrier")] <- "Rakaia River Mouth - 5 km north beside sea/lagoon barrier, Canterbury"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="3km N of Kekerengu")] <- "3km N of Kekerengu, Canterbury"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Coopers Lagoon beach, Cantebury")] <- "Coopers Lagoon beach, Canterbury"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Kaitorete Spit")] <- "Kaitorete Spit, Canterbury"

# OTAGO
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Oamaru")] <- "Oamaru, Otago"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Waitaki River Mouth")] <- "Waitaki River Mouth, Otago"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Measley Beach/Tokomairiro River Mouth")] <- "Measley Beach/Tokomairiro River Mouth, Otago"

# CLOUDY BAY
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Port Underwood")] <- "Port Underwood, near Cloudy Bay"

# PUNAKAIKI
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Fox River Beach, Paparoa")] <- "Fox River Beach, Paparoa, north of Punakaiki"

# BANKS PENINSULA
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="150m inshore marker buoy in Port Levy")] <- "150m inshore marker buoy in Port Levy, Banks Peninsula"
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Head of Harbour, Port Levy")] <- "Head of Harbour, Port Levy, Banks Peninsula"

# WAIKATO
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Gibson Beach, Te Akau")] <- "Gibson Beach, Te Akau, Waikato"

# NORTHLAND
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="Ripiro Beach, south of Glinks Gully, Dargaville")] <- "Ripiro Beach, south of Glinks Gully, Dargaville, Northland"

# Cool, OK, let's double-check everyone has a broad location now.
NZCETA_DOC_joined %>% mutate(broad_location=ifelse(grepl("Northland",Location.x),"Northland",ifelse(grepl("Karamea",Location.x),"Karamea",ifelse(grepl("Golden Bay",Location.x),"Golden Bay",ifelse(grepl("Stewart Island",Location.x),"Stewart Island",ifelse(grepl("Cloudy Bay",Location.x),"Cloudy Bay",ifelse(grepl("Kapiti",Location.x),"Kapiti",ifelse(grepl("Waikato|Raglan",Location.x),"Waikato",ifelse(grepl("Dunedin",Location.x),"Dunedin",ifelse(grepl("Blaketown",Location.x),"Blaketown",ifelse(grepl("Paroa",Location.x),"Paroa",ifelse(grepl("Barrytown|Punakaiki",Location.x),"Barrytown/Punakaiki",ifelse(grepl("Okarito|Franz Josef",Location.x),"Okarito/Franz Josef",ifelse(grepl("Banks Peninsula|BP",Location.x),"Banks Peninsula",ifelse(grepl("Taranaki",Location.x),"Taranaki",ifelse(grepl("Farewell",Location.x),"Farewell Spit",ifelse(grepl("Lyttelton",Location.x),"Lyttelton",ifelse(grepl("Haast|Neils",Location.x),"Haast/Jackson Bay",ifelse(grepl("Kaikoura",Location.x),"Kaikoura",ifelse(grepl("Auckland",Location.x),"Auckland",ifelse(grepl("Akaroa",Location.x),"Akaroa",ifelse(grepl("Westport",Location.x),"Westport",ifelse(grepl("Greymouth",Location.x),"Greymouth",ifelse(grepl("Timaru",Location.x),"Timaru",ifelse(grepl("Waewae",Location.x),"Te Waewae",ifelse(grepl("Queen Charlotte",Location.x),"Queen Charlotte Sound",ifelse(grepl("Hokitika",Location.x,),"Hokitika",ifelse(grepl("Pegasus",Location.x),"Pegasus Bay",ifelse(grepl("Buller",Location.x),"Buller",ifelse(grepl("Westland",Location.x),"Westland",ifelse(grepl("Marlborough",Location.x),"Marlborough",ifelse(grepl("Canterbury",Location.x),"Canterbury",ifelse(grepl("Otago",Location.x),"Otago",ifelse(grepl("Nelson",Location.x),"Nelson",ifelse(grepl("Piha",Region),"Auckland",ifelse(grepl("Canterbury",Region),"Canterbury",ifelse(grepl("Buller",Region),"Buller",ifelse(grepl("West Coast",Region),"West Coast",NA)))))))))))))))))))))))))))))))))))))) %>%  filter(is.na(broad_location)) %>% select(Location.x) %>% filter(!is.na(Location.x)) %>% print(n=32)

# Awesome, everyone except the one that has a "?" for stranding location. Can save broad variable as a
# new variable
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% mutate(broad_location=ifelse(grepl("Northland",Location.x),"Northland",ifelse(grepl("Golden Bay",Location.x),"Golden Bay",ifelse(grepl("Stewart Island",Location.x),"Stewart Island",ifelse(grepl("Cloudy Bay",Location.x),"Cloudy Bay",ifelse(grepl("Kapiti",Location.x),"Kapiti",ifelse(grepl("Waikato|Raglan",Location.x),"Waikato",ifelse(grepl("Dunedin",Location.x),"Dunedin",ifelse(grepl("Barrytown|Punakaiki",Location.x),"Barrytown/Punakaiki",ifelse(grepl("Okarito|Franz Josef",Location.x),"Okarito/Franz Josef",ifelse(grepl("Banks Peninsula|BP",Location.x),"Banks Peninsula",ifelse(grepl("Taranaki",Location.x),"Taranaki",ifelse(grepl("Farewell",Location.x),"Farewell Spit",ifelse(grepl("Lyttelton",Location.x),"Lyttelton",ifelse(grepl("Neils|Haast",Location.x),"Haast/Jackson Bay",ifelse(grepl("Kaikoura",Location.x),"Kaikoura",ifelse(grepl("Auckland",Location.x),"Auckland",ifelse(grepl("Akaroa",Location.x),"Akaroa",ifelse(grepl("Westport",Location.x),"Westport",ifelse(grepl("Paroa|Blaketown|Greymouth",Location.x),"Greymouth",ifelse(grepl("Timaru",Location.x),"Timaru",ifelse(grepl("Waewae",Location.x),"Te Waewae",ifelse(grepl("Queen Charlotte",Location.x),"Queen Charlotte Sound",ifelse(grepl("Hokitika",Location.x),"Hokitika",ifelse(grepl("Pegasus",Location.x),"Pegasus Bay",ifelse(grepl("Karamea|Buller",Location.x),"Buller",ifelse(grepl("Westland",Location.x),"Westland",ifelse(grepl("Marlborough",Location.x),"Marlborough",ifelse(grepl("Canterbury",Location.x),"Canterbury",ifelse(grepl("Otago",Location.x),"Otago",ifelse(grepl("Nelson",Location.x),"Nelson",ifelse(grepl("Piha",Region),"Auckland",ifelse(grepl("Canterbury",Region),"Canterbury",ifelse(grepl("Buller",Region),"Buller",ifelse(grepl("West Coast",Region),"West Coast",NA)))))))))))))))))))))))))))))))))))

# Going to go through these broad locations now, and double check that all samples that in that location
# do in fact belong to that location. After doing that, will double check 'Region' compared to 'broad location'
# finally after that can populate 'hologenome_region' based on this.
NZCETA_DOC_joined %>% group_by(broad_location) %>% count() %>% print(n=33)

NZCETA_DOC_joined %>% filter(broad_location=="Akaroa") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined$broad_location[which(NZCETA_DOC_joined$Location.x=="Akaroa, middle of Le Bons Bay, 1nm offshore in net")] <- "Banks Peninsula"
NZCETA_DOC_joined %>% filter(broad_location=="Auckland") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined$broad_location[which(NZCETA_DOC_joined$Location.x=="Taharoa Beach, South Auckland")] <- "Waikato"
NZCETA_DOC_joined %>% filter(broad_location=="Banks Peninsula") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined$broad_location[which(NZCETA_DOC_joined$Location.x=="Robinson Bay, Banks Peninsula")] <- "Akaroa"
NZCETA_DOC_joined %>% filter(broad_location=="Barrytown/Punakaiki") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Buller") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Canterbury") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Cloudy Bay") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Dunedin") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Farewell Spit") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Golden Bay") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Greymouth") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined$broad_location[which(NZCETA_DOC_joined$Location.x=="Canoe Creek area, south of Burke Road, Greymouth")] <- "Barrytown/Punakaiki"
NZCETA_DOC_joined %>% filter(broad_location=="Haast/Jackson Bay") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Hokitika") %>% select(Location.x,broad_location,Region,Ocean) %>%  print(n=28)
NZCETA_DOC_joined %>% filter(broad_location=="Kaikoura") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Kapiti") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Lyttelton") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Marlborough") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Nelson") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Northland") %>% select(Location.x,broad_location,Region,Ocean) 
NZCETA_DOC_joined %>% filter(broad_location=="Okarito/Franz Josef") %>% select(Location.x,broad_location,Region,Ocean) 
NZCETA_DOC_joined %>% filter(broad_location=="Otago") %>% select(Location.x,broad_location,Region,Ocean) 
NZCETA_DOC_joined$broad_location[which(NZCETA_DOC_joined$Location.x=="Long Beach, North of Heyward Point, Central Coastal Otago")] <- "Dunedin"
NZCETA_DOC_joined %>% filter(broad_location=="Pegasus Bay") %>% select(Location.x,broad_location,Region,Ocean) %>% print(n=40)
NZCETA_DOC_joined$broad_location[which(NZCETA_DOC_joined$Location.x=="Pegasus Bay, Church Bay, Lyttleton")]  <- "Lyttelton"
NZCETA_DOC_joined %>% filter(broad_location=="Queen Charlotte Sound") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Stewart Island") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Taranaki") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Te Waewae") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Timaru") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Waikato") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="West Coast") %>% select(Location.x,broad_location,Region,Ocean)
NZCETA_DOC_joined %>% filter(broad_location=="Westport") %>% select(Location.x,broad_location,Region,Ocean) %>% print(n=23)

# OK, all done!!! Location matches with 'broad region'. Next check is if 'broad region' matches with 'region' and 'ocean'.
NZCETA_DOC_joined %>% group_by(broad_location,Region,Ocean) %>% count() %>% print(n=78)
# Akaroa is in Canterbury/Pacific (or NAs), so all good
# Auckland is in Auckland/North Piha/Tasman/Tasman Sea (or NAs), so all good
# Banks Peninsula is in Canterbury/Pacific (or NAs), so all good
# Barrytown/Punakaiki is in West Coast/Tasman Sea (or NAs), so all good
# Buller is Buller/West Coast/Tasman Sea (or NAs), so all good
# Canterbury is Canterbury/Pacific (or NAs), so all good
# Cloudy Bay is Marlborough/Pacific (or NAs), so all good
# Dunedin is Otago/Pacific (or NAs), so all good
# Farewell Spit is Marlborough/Nelson/Tasman (or NAs), so all good
# Golden Bay had Tasman, Tasman (or NAs), so all good (but will code as North Coast)
# Greymouth had West Coast/South Island/Tasman (or NAs), so all good
# Haast/Jackson Bay had West Coast/Tasman (or NAs), so all good
# Hokitika had West Coast/South Island/Westland (or NAs), so all good
# Kaikoura had Canterbury/Pacific Ocean (or NAs), so all good
# Kapiti had NAs, so all good
# Lyttelton had Canterbury/Pacific (or NAs), so all good
# Malborough had Malborough/Western Malborough Sounds/Pacific/Tasman (or NAs), so all good.
# Nelson had Tasman/Tasman so all good.
# Northland had Northland/Tasman so all good.
# Okarito/Franz Josef had West Coast/Tasman, so all good
# Otago had Otago/Pacific (or NAs), so all good
# Pegasus Bay had Canterbury/Christchurch/Pacific (or NAs), so all good
# Queen Charlotte sound had Cook Strait, so all good
# Stewart Island had Southland/Pacific so all good
# Taranaki had Taranaki/Tasman (or NAs), so all good
# Te Waewae had Southland/Pacific/Tasman (or NAs), so all good
# Timaru had Canterbury/Timaru/Pacific (or NAs), so all good
# Waikato had Waikato/Tasman (or NAs), so all good
# West Coast had West Coast/Tasman, so all good
# Westport had West Coast/Tasman, or NAs, so all good

NZCETA_DOC_joined %>% group_by(hologenome_region) %>%  count()

# Trialing out the mutate statement before saving it into the NZCETA_DOC_joined data object
NZCETA_DOC_joined %>% 
  mutate(hologenome_region=ifelse((hologenome_region=="NA"|is.na(hologenome_region)),
                                  ifelse((broad_location=="Akaroa"|broad_location=="Banks Peninsula"|broad_location=="Canterbury"|broad_location=="Cloudy Bay"|broad_location=="Kaikoura"|broad_location=="Lyttelton"|broad_location=="Otago"|broad_location=="Pegasus Bay"|broad_location=="Timaru"|broad_location=="Dunedin"),"SI East Coast",
                                         ifelse((broad_location=="Auckland"|broad_location=="Kapiti"|broad_location=="Northland"|broad_location=="Taranaki"|broad_location=="Waikato"),"Maui",
                                                ifelse((broad_location=="Barrytown/Punakaiki"|broad_location=="Buller"|broad_location=="Greymouth"|broad_location=="Buller"|broad_location=="Haast/Jackson Bay"|broad_location=="Hokitika"|broad_location=="Okarito/Franz Josef"|broad_location=="West Coast"|broad_location=="Westport"),"SI West Coast",
                                                       ifelse((broad_location=="Golden Bay"|broad_location=="Queen Charlotte Sound"),"SI North Coast",
                                                              ifelse((broad_location=="Stewart Island"|broad_location=="Te Waewae"),"SI South Coast",hologenome_region))))),hologenome_region)) %>% group_by(broad_location,hologenome_region) %>% count() %>% print(n=40)

# Looks good - we can capture that into the hologenome_region variable now, and then have a look
# at a few problem children below (as well as those that have NAs just to make sure these are all
# good calls.)
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% 
  mutate(hologenome_region=ifelse((hologenome_region=="NA"|is.na(hologenome_region)),
                                  ifelse((broad_location=="Akaroa"|broad_location=="Banks Peninsula"|broad_location=="Canterbury"|broad_location=="Cloudy Bay"|broad_location=="Kaikoura"|broad_location=="Lyttelton"|broad_location=="Otago"|broad_location=="Pegasus Bay"|broad_location=="Timaru"|broad_location=="Dunedin"),"SI East Coast",
                                         ifelse((broad_location=="Auckland"|broad_location=="Kapiti"|broad_location=="Northland"|broad_location=="Taranaki"|broad_location=="Waikato"),"Maui",
                                                ifelse((broad_location=="Barrytown/Punakaiki"|broad_location=="Buller"|broad_location=="Greymouth"|broad_location=="Buller"|broad_location=="Haast/Jackson Bay"|broad_location=="Hokitika"|broad_location=="Okarito/Franz Josef"|broad_location=="West Coast"|broad_location=="Westport"),"SI West Coast",
                                                       ifelse((broad_location=="Golden Bay"|broad_location=="Queen Charlotte Sound"),"SI North Coast",
                                                              ifelse((broad_location=="Stewart Island"|broad_location=="Te Waewae"),"SI South Coast",hologenome_region))))),hologenome_region))

# OK, problem children! Farewell Spit is a bit ambiguous as to whether it should be considered West Coast or North Coast, so will modify hologenome_region to reflect this
NZCETA_DOC_joined %>% 
  mutate(hologenome_region=ifelse((broad_location=="Farewell Spit"),"Farewell Spit",hologenome_region)) %>% 
  group_by(broad_location,hologenome_region) %>% count() %>% print(n=32)
# Cool, test looks good, so let's save it:
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% 
  mutate(hologenome_region=ifelse((broad_location=="Farewell Spit"),"Farewell Spit",hologenome_region))

# Malborough samples will also need to be looked at on an individual case-by-case basis to attribute them to 
# hologenome_regions
NZCETA_DOC_joined %>% filter(broad_location=="Marlborough") %>% select(scrubbed_uoa_code,Location.x)
# All samples have pretty precise locations except for CHEMB03-01, which just gives 'South Malborough'
# we'll check this one for additional info, otherwise add a note that we are fairly certain it is
# EC, but aren't completely sure.
NZCETA_DOC_joined %>% filter(scrubbed_uoa_code=="CHEMB03-01") %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEMB03-01")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEMB03-01")], "Fairly sure this sample is SI East Coast, but 'South Malborough' is a little vague",sep=";")
NZCETA_DOC_joined$hologenome_region[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEMB03-01")] <- "SI East Coast"

# Ure Stream/River is also known as Waima, and is definitely "East Coast" in our definition.
# Lake Grassmere is south of Cloudy Bay, so same diff
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% 
  mutate(hologenome_region=ifelse((scrubbed_uoa_code %in% c("CHE07NZ08","U18-007","CHE07NZ10")),"SI East Coast",hologenome_region))

# Otarawao Bay is on the North Coast, however
NZCETA_DOC_joined$hologenome_region[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ10")] <- "SI North Coast"

# Ditto Nelson
NZCETA_DOC_joined %>% filter(broad_location=="Nelson") %>% select(scrubbed_uoa_code,Location.x,hologenome_region)
# Both of these look great as North Coast, so we'll leave them as they are

# Going to make a note on the Kapiti sample that based on it being sampled so far south, that without
# additional genetic info, we wouldn't be sure about whether it was a Māui sample or not
NZCETA_DOC_joined %>% filter(broad_location=="Kapiti") %>% as.matrix()
# In fact, it already is assigned to Hector's, so will assign it to North Coast but add 
# a note indicating our uncertainty.
NZCETA_DOC_joined$hologenome_region[which(NZCETA_DOC_joined$broad_location=="Kapiti")] <- "SI North Coast"
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$broad_location=="Kapiti")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$broad_location=="Kapiti")],"Given this individual is a Hector's, have put down SI North Coast for hologenome_region, but this is not certain",sep=";")

# OK, let's double check the samples that have an 'NA' for hologenome_region to see if we might be able to dig up any more info.
NZCETA_DOC_joined %>% filter(is.na(hologenome_region)) %>% filter(!is.na(Location.x)|!is.na(Region)|!is.na(Ocean)) %>% as.matrix()
# Just one, and well put down "Unknown" for both the hologenome_region and broad_location becase
# the ony information for Location.x and Region is "?". Not going to bother adding a reconciliation
# note, because should be pretty obvious from the scant information available for that sample
NZCETA_DOC_joined$hologenome_region[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC04-01")] <- "Unknown"
NZCETA_DOC_joined$broad_location[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC04-01")] <- "Unknown"

# OK, where are we at? As good as we can be! n = 61 records without any location info at all
NZCETA_DOC_joined %>% group_by(broad_location,hologenome_region) %>% count() %>% print(n=33)
NZCETA_DOC_joined %>% filter(is.na(hologenome_region)) %>% group_by(Location.x,Region,Ocean) %>% count()

# Let's do a final double check on our hologenome_regions before moving on
NZCETA_DOC_joined %>% filter(hologenome_region=="Maui") %>% group_by(broad_location) %>% count()
NZCETA_DOC_joined %>% filter(hologenome_region=="SI North Coast") %>% group_by(broad_location) %>% count()
NZCETA_DOC_joined %>% filter(hologenome_region=="SI South Coast") %>% group_by(broad_location) %>% count()
NZCETA_DOC_joined %>% filter(hologenome_region=="SI West Coast") %>% group_by(broad_location) %>% count()
NZCETA_DOC_joined %>% filter(hologenome_region=="SI East Coast") %>% group_by(broad_location) %>% count()
NZCETA_DOC_joined %>% filter(hologenome_region=="Unknown") %>% group_by(broad_location) %>% count()
NZCETA_DOC_joined %>% filter(is.na(hologenome_region)) %>% group_by(broad_location) %>% count()

# Alright, location is pretty locked down then :)

################################################################################################################
#### 6a. QC'ing and correcting the values in the remaining columns (Checked,Common_name,Code,Date_received) ####
################################################################################################################
# In this step we are checking the unique values present in the remaining columns
# to double-check for typos. Following this we cross-check duplicated values (where informative)
# for duplicate samples.

names(NZCETA_DOC_joined)

# Not sure what the Checked field is, but all but n=1 has 'No', n=164 has 'Yes', and n=162 has 'NA'
NZCETA_DOC_joined %>% group_by(Checked) %>% count()
NZCETA_DOC_joined %>% filter(Checked=="No") %>% as.matrix() 
# Not sure why this sample is "No". Looks pretty regular to me!

# n=33 are 'uncertain'. These ones have no other information to assign them to a species
NZCETA_DOC_joined %>% group_by(Common_name) %>% count()

# All are unique except for CHESIFP and 'NA' samples, as expected
NZCETA_DOC_joined %>% group_by(Code) %>% count() %>% print(n=291)

# A few samples received at the same time. Let's double check that these aren't duplicates
NZCETA_DOC_joined %>% group_by(Date_received) %>% count() %>% filter(n>1)
# Very similar date/location, but different Hcodes and sex
NZCETA_DOC_joined %>% filter(Date_received=="Aug-07") %>% as.matrix()
# One Hector's, so deffo not the same sample. Maui samples were an adult + neonate/juvenile
# so co-stranding makes sense
NZCETA_DOC_joined %>% filter(Date_received=="Dec-06") %>% as.matrix()
# Animals distinct based on sex/mtDNA combo, and have differing collection dates, even if locations similar
NZCETA_DOC_joined %>% filter(Date_received=="Dec-94") %>% as.matrix()
# Different stranding dates, TL, locations etc
NZCETA_DOC_joined %>% filter(Date_received=="Feb-05") %>% as.matrix()
# Different stranding dates, TL, locations etc
NZCETA_DOC_joined %>% filter(Date_received=="Jan-06") %>% as.matrix()
# Different stranding dates, TL, locations etc
NZCETA_DOC_joined %>% filter(Date_received=="Jan-07") %>% as.matrix()
# Different stranding dates, TL, locations etc
NZCETA_DOC_joined %>% filter(Date_received=="Jul-07") %>% as.matrix()
# Different stranding dates, TL, locations etc
NZCETA_DOC_joined %>% filter(Date_received=="Mar-07") %>% as.matrix()
# Different stranding dates, TL, locations etc
NZCETA_DOC_joined %>% filter(Date_received=="May-07") %>% as.matrix()
# Same date/location, but net entanglement given as cause of death, so that makes sense.
NZCETA_DOC_joined %>% filter(Date_received=="Nov-05") %>% as.matrix()
# Very different animals
NZCETA_DOC_joined %>% filter(Date_received=="Nov-06") %>% as.matrix()
# Seem to be distinct based on other metadata
NZCETA_DOC_joined %>% filter(Date_received=="Nov-08") %>% as.matrix()
# Seem to be distinct based on other metadata
NZCETA_DOC_joined %>% filter(Date_received=="Oct-96") %>% as.matrix()
# all look pretty distinct
NZCETA_DOC_joined %>% filter(Date_received=="unsure") %>% as.matrix()

####################################################################################
#### 6b. QC'ing and correcting the values in the remaining columns (Location.x) ####
####################################################################################
# In this step we are checking the unique values present in the remaining columns
# to double-check for typos. Following this we cross-check duplicated values (where informative)
# for duplicate samples.

NZCETA_DOC_joined %>% group_by(Location.x) %>% count() %>% filter(n>1) %>% print(n=30)

# Consistent collection date: potentially these two are the same sample. Will add a note. Also will
# Fix capitalizaiton on Pegasus Bay (just in case)
NZCETA_DOC_joined %>% filter(Location.x=="1.3km north of Waimairi Surf club, Pegasus bay") %>% as.matrix()
NZCETA_DOC_joined$Location.x[which(NZCETA_DOC_joined$Location.x=="1.3km north of Waimairi Surf club, Pegasus bay")] <- "1.3km north of Waimairi Surf club, Pegasus Bay"
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP63")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP63")],"Potentialy same individual as 'CHESIFP_CETOS8846' based on identical Location.x and consistent collection_date_mod",sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESIFP_CETOS8846")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESIFP_CETOS8846")],"Potential duplicate of 'CHEBP63' based on identical Location.x and consistent collection_date_mod",sep=";")

# Another potential set of duplicate samples
NZCETA_DOC_joined %>% filter(Location.x=="100m Sth of Brighton Surf Club, Pegasus Bay") %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP64")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP64")],"Likely same individual as 'CHESIFP_CETOS96103' based on identical Location.x and identical collection_date_mod. This (CHEBP64) should be considered 'sample of reference' as more information available",sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESIFP_CETOS96103")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESIFP_CETOS96103")],"Likely duplicate of 'CHEBP64' based on identical Location.x and identical collection_date_mod. Only use if additional/duplicate tissue needed",sep=";")

# No obvious issues with the following pairs/groups of samples to suggest they are likely duplicates
NZCETA_DOC_joined %>% filter(Location.x=="4 - 5 km north of Karioitahi Beach, Waiuku, South Auckland") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Location.x=="Blaketown Beach, Greymouth") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Location.x=="Buller") %>% as.matrix()

# Known duplicate (in the UoA_Code field, and same pathology_code field)
NZCETA_DOC_joined %>% filter(Location.x=="Fossil Point base of Farewell Spit M24 777 87200") %>% as.matrix()
# "CHE05NZ18" has more info in it from Massey, so suggest taking it as the "sample of reference", but also referring to 'Che05NZ08' for additional/duplicate tissue if needed. Will also update the NAs for
# CHE05NZ18 based on Che05NZ08
NZCETA_DOC_joined$Date_received[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ18")] <- NZCETA_DOC_joined$Date_received[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ08")]
NZCETA_DOC_joined$Region[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ18")] <- NZCETA_DOC_joined$Region[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ08")]
NZCETA_DOC_joined$Ocean[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ18")] <- NZCETA_DOC_joined$Ocean[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ08")]
NZCETA_DOC_joined$Info_sent[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ18")] <- NZCETA_DOC_joined$Info_sent[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ08")]
NZCETA_DOC_joined$Info_requested[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ18")] <- NZCETA_DOC_joined$Info_requested[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ08")]
NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ18")] <- NZCETA_DOC_joined$Sex_observed[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ08")]
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ08")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ08")], "This sample is a duplicate of CHE05NZ18. Only use if additional/duplicate tissue necessary", sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE05NZ18")] <- "CHE05NZ08 is a duplicate of this sample. This (CHE05NZ18) should be considered the 'sample of reference' as it had more information in the DOC_Massey database. Information for the following columns taken from CHE05NZ08: Date_received, Region, Ocean, Info_sent, Info_requested, Sex_observed"

# Location.x (just because we now have some guff on the standard out so we can be reminded on where we are up to)
NZCETA_DOC_joined %>% group_by(Location.x) %>% count() %>% filter(n>1) %>% print(n=30)

# No obvious issues with the following pairs/groups of samples to suggest they are likely duplicates
NZCETA_DOC_joined %>% filter(Location.x=="Greymouth") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Location.x=="Halfway between Beach Rd(Fairdown) and Whareatea R mouth, near Westport") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Location.x=="Hokitika River mouth") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Location.x=="Kaikoura") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Location.x=="Leithfield beach; Pegasus Bay") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Location.x=="Muriwai Beach, 12 miles mark, North Auckland") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Location.x=="Neils Beach Jackson Bay, South Westland") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Location.x=="New Brighton Beach, Pegasus Bay") %>% as.matrix()

# Potential duplicate based on same collection date and Location.x
NZCETA_DOC_joined %>% filter(Location.x=="Nikan Beach 35km Sth Westport") %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESIXX")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESIXX")],"Likely same individual as 'CHEWC02-06' based on identical Location.x and identical collection_date_mod. Only use if additional/duplicate tissue needed",sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC02-06")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC02-06")],"Likely duplicate of 'CHESIXX' based on identical Location.x and identical collection_date_mod This (CHEWC02-06) should be considered the 'sample of reference', because more information available for it",sep=";")

# No obvious issues with the following pairs/groups of samples to suggest they are likely duplicates
NZCETA_DOC_joined %>% filter(Location.x=="North Side of Rangitata Huts, north of Timaru") %>% as.matrix()

# Potential duplicate based on same collection date and Location.x
NZCETA_DOC_joined %>% filter(Location.x=="Opihi River mouth north of Timaru") %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESIFP_CETOS9699")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESIFP_CETOS9699")],"Likely same individual as 'CHETI13' based on identical Location.x and identical collection_date_mod. This sample should only be used if additional tissue needed, as other sample has more information.",sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETI13")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETI13")],"Likely duplicate of 'CHESIFP_CETOS9699' based on identical Location.x and identical collection_date_mod. This sample (CHETI13) should be considered 'sample of reference' as it has more information available",sep=";")

# Location.x (just because we now have some guff on the standard out so we can be reminded on where we are up to)
NZCETA_DOC_joined %>% group_by(Location.x) %>% count() %>% filter(n>1) %>% print(n=30)

# Potential duplicate based on same collection date and Location.x
# OK, so "CHE12NZ02" is associated with a U-code that is also found with "Che12NZ01" (a South Island sample)
# So the U-code attributed to this sample doesn't seem to be correct. Instead it should probably be "U12-006"
# which is the other sample with the same stranding location/date.
NZCETA_DOC_joined %>% filter(Location.x=="Opunake, Taranaki") %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="U12-006")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="U12-006")],"Likely same individual as 'CHE12NZ02' based on identical Location.x and identical collection_date_mod",sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE12NZ02")] <- "Likely duplicate of 'U12-006' based on identical Location.x and identical collection_date_mod. Based on this that the U-code given in 'Code' is incorrect, however this sample has more information available than U12-006, so should be taken as the 'sample of reference. Note this is an apparent Hector's sample within Maui rohe"

# No obvious issues with the following pairs/groups of samples to suggest they are likely duplicates
NZCETA_DOC_joined %>% filter(Location.x=="Quail Island, Lyttelton Harbour") %>% as.matrix()

# All have a distinct U-code, so probably not the same samples?
NZCETA_DOC_joined %>% filter(Location.x=="Queen Charlotte sound") %>% as.matrix()

# Known duplicate (in the UoA_Code field, and same pathology_code field)
NZCETA_DOC_joined %>% filter(Location.x=="Serpentine Creek mouth, 20km north Hokitika (1/2 way between HK & GM)") %>% as.matrix()
# "CHE10NZ04" has more info in it from Massey, so suggest taking it as the "sample of reference", but also referring to 'CHE10NZ10' for additional/duplicate tissue if needed. Will also update the NAs for
# CHE10NZ10 based on CHE10NZ04
NZCETA_DOC_joined$Collected_By[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ10")] <- NZCETA_DOC_joined$Collected_By[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ04")] 
NZCETA_DOC_joined$DoC_Agency[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ10")] <- NZCETA_DOC_joined$DoC_Agency[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ04")] 
NZCETA_DOC_joined$Age_Class[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ10")] <- NZCETA_DOC_joined$Age_Class[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ04")] 
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ10")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ10")], "This sample is a duplicate of CHE10NZ04. Only use if additional/duplicate tissue necessary.Information for the following columns taken from CHE10NZ04: Collected_By, DoC_Agency, Age_Class", sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ04")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE10NZ04")],"CHE10NZ10 is a duplicate of this sample. This (CHE10NZ04) should be considered the 'sample of reference' as it had more information in the DOC_Massey database",sep=";")

# Location.x (just because we now have some guff on the standard out so we can be reminded on where we are up to)
NZCETA_DOC_joined %>% group_by(Location.x) %>% count() %>% filter(n>1) %>% print(n=30)

# No obvious issues with the following pairs/groups of samples to suggest they are likely duplicates
NZCETA_DOC_joined %>% filter(Location.x=="Te Waewae Bay") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Location.x=="Timaru") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Location.x=="Timaru, Rangitata River Mouth") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Location.x=="Waikouaiti Beach, Dunedin") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Location.x=="Westport") %>% as.matrix()

# Similar collection date, but different CETOS code. Although had same sex and mtDNA haplotype, this is a common haplotype on the West Coast
NZCETA_DOC_joined %>% filter(Location.x=="Westport, Granity") %>% as.matrix()

# No obvious issues with the following pairs/groups of samples to suggest they are likely duplicates
NZCETA_DOC_joined %>% filter(Location.x=="Westport, Ngakawau NZMS L28 150534") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Location.x=="Whatipu, Auckland") %>% as.matrix()

################################################################################
#### 6c. QC'ing and correcting the values in the remaining columns (Region) ####
################################################################################

# Region
NZCETA_DOC_joined %>% group_by(Region) %>% count() %>% print(n=50)

# A few we can standardize here:
NZCETA_DOC_joined$Region[which(NZCETA_DOC_joined$Region=="Buller, West Coast")] <- "Buller"
NZCETA_DOC_joined$Region[which(NZCETA_DOC_joined$Region=="marlborough")] <- "Marlborough"
NZCETA_DOC_joined$Region[which(NZCETA_DOC_joined$Region=="Hokitika, West Coast")] <- "Hokitika"
NZCETA_DOC_joined$Region[which(NZCETA_DOC_joined$Region=="taranaki")] <- "Taranaki"
NZCETA_DOC_joined$Region[which(NZCETA_DOC_joined$Region=="West Coast South Island")] <- "West Coast"
NZCETA_DOC_joined$Region[which(NZCETA_DOC_joined$Region=="West Coast, South Island")] <- "West Coast"
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$Region=="Western Marlborough Sounds")] <- "Region was originally 'Western Marlborough Sounds'. Modified to 'Marlborough' for consistency with other samples"
NZCETA_DOC_joined$Region[which(NZCETA_DOC_joined$Region=="Western Marlborough Sounds")] <- "Marlborough"

NZCETA_DOC_joined %>% group_by(Region) %>% count() %>% print(n=50)
# Looks all good now. Going to check locations with two-three for duplicates
# but not more common categories, as too broad to be super useful flicking through
NZCETA_DOC_joined %>% group_by(Region) %>% count() %>% filter(n>1&n<4) %>% print(n=50)

# No obvious issues with the following pairs/groups of samples to suggest they are likely duplicates
NZCETA_DOC_joined %>% filter(Region=="Buller") %>% as.matrix()

# Known duplicates already identified based on Location.x
NZCETA_DOC_joined %>% filter(Region=="Nelson") %>% as.matrix()

# No obvious issues with the following pairs/groups of samples to suggest they are likely duplicates
NZCETA_DOC_joined %>% filter(Region=="Taranaki") %>% as.matrix()

# I have some suspicions about U15-005 and U15-006 being duplicates give how close
# in stranding location and date they are
NZCETA_DOC_joined %>% filter(Region=="Tasman") %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="U15-005")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="U15-005")], "Suspect this may be a duplicate of 'U15-006' based on very similar stranding location and collection date", sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="U15-006")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="U15-006")],"Suspect this may be a duplicate of 'U15-005' based on very similar stranding location and collection date", sep=";")

###################################################################################################################################################
#### 6d. QC'ing and correcting the values in the remaining columns (Ocean, Cause_of_death, Source, What, Info_sent, Extracted, Info_requested) ####
###################################################################################################################################################
# Ocean
NZCETA_DOC_joined %>% group_by(Ocean) %>% count() %>% print(n=50)

# A few we can standardize here:
NZCETA_DOC_joined$Ocean[which(NZCETA_DOC_joined$Ocean=="Pacifc")] <- "Pacific"
NZCETA_DOC_joined$Ocean[which(NZCETA_DOC_joined$Ocean=="Pacific Ocean")] <- "Pacific"
NZCETA_DOC_joined$Ocean[which(NZCETA_DOC_joined$Ocean=="tasman")] <- "Tasman"
NZCETA_DOC_joined$Ocean[which(NZCETA_DOC_joined$Ocean=="Tasman Sea")] <- "Tasman"

# These categories are too braod and have too many samples in them to be useful for identifying duplicates
NZCETA_DOC_joined %>% group_by(Ocean) %>% count() %>% print(n=50)

# Cause of death
NZCETA_DOC_joined %>% group_by(Cause_of_death) %>% count() %>% print(n=50)

# One we can standardize here
NZCETA_DOC_joined$Cause_of_death[which(NZCETA_DOC_joined$Cause_of_death=="net entranglement")] <- "net entanglement"

# Can now check the non-NAs, becase there are only 5 samples with info for this field
# Four of these were the 2-Nov-2005 net entanglement, others were distinct events, so all good
NZCETA_DOC_joined %>% filter(!is.na(Cause_of_death)) %>% as.matrix()

# Source
NZCETA_DOC_joined %>% group_by(Source) %>% count() %>% print(n=50)
# All of these values are NA, so going to remove the column
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% select(-Source)

# What
NZCETA_DOC_joined %>% group_by(What) %>% count() %>% print(n=50)
# This field seems like it might actually need to go into 'Info_requested' field. Let's check that out and see
# what is in that column, and then whether these samples that are not NA for 'What' have anything in that column
# (otherwise we'll copy that across to the 'Info_requested' field and get rid of 'What').
NZCETA_DOC_joined %>% group_by(Info_requested) %>% count() %>% print(n=50)
NZCETA_DOC_joined %>% group_by(Info_sent) %>% count() %>% print(n=50)
# Nope, it looks like the 'What' field is pretty distinct. Because this field is sample-specific, not going to 
# standardize it, but will check to make sure are no duplicates among the non-NA values
NZCETA_DOC_joined %>% filter(!is.na(What)) %>% as.matrix()
# All look pretty distinct based on this.

# Info_sent
NZCETA_DOC_joined %>% group_by(Info_sent) %>% count() %>% print(n=50)
# Just 8 samples with a 'Yes' so we'll check them out
NZCETA_DOC_joined %>% filter(Info_sent=="No") %>% as.matrix()
# Cool, they look fine - all have separate H-codes etc.

# Extracted
NZCETA_DOC_joined %>% group_by(Extracted) %>% count() %>% print(n=50)
# One sample that has details in Extracted rather than just a 'Yes/No'
NZCETA_DOC_joined %>% filter(Extracted=="MLD 1 Dec 94\vNo") %>% as.matrix()
# OK, this likely seems like an error because it was collected in 2016. Making a note in the reconciliation notes
# and changing this to an NA
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$Extracted=="MLD 1 Dec 94\vNo")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$Extracted=="MLD 1 Dec 94\vNo")], "'MLD 1 Dec 94\vNo' was originally entered under 'Extracted', but this would seem to be an error based on collection_date_mod of this sample, and also every other entry being Yes/No for this column",sep=";")
NZCETA_DOC_joined$Extracted[which(NZCETA_DOC_joined$Extracted=="MLD 1 Dec 94\vNo")] <- NA

# Info_requested
NZCETA_DOC_joined %>% group_by(Info_requested) %>% count() %>% print(n=50)
# All looks good, too broad a category to be much use identifying duplciates

# Skipped the columns "Other_info" and "Archiving_info" due to the previously identified incomplete_sort
# ****NOTE***** During a check of the date, it actually appears the Other_info/Archiving columns are not reliable (incomplete sort), so
# may need to redo these steps

#################################################################################################################################################
#### 6e. QC'ing and correcting the values in the remaining columns (scrubbed_uoa_code,reconciled_alternate_code,UoA_Code,Species_subspecies, ####
#### LatitudeS_Northing,LongitudeE_Easting). Also filled in hologenome_region/broad_location for samples with lat/longs but no other info    ####
#################################################################################################################################################
names(NZCETA_DOC_joined)
# All scrubbed_uoa_code unique as they should be :)
NZCETA_DOC_joined %>% group_by(scrubbed_uoa_code) %>% count() 

# Just two we'll check on for reconciled_alternative_code
NZCETA_DOC_joined %>% group_by(reconciled_alternate_code) %>% count() 
NZCETA_DOC_joined$reconciled_alternate_code[which(NZCETA_DOC_joined$reconciled_alternate_code=="NA")] <- NA
# These are all good - definitely not a match to each other!
NZCETA_DOC_joined %>% filter(!is.na(reconciled_alternate_code)) %>% as.matrix()

# UoA_Code - looks all good
NZCETA_DOC_joined %>% group_by(UoA_Code) %>% count() %>% filter(n>1)

# Species_subspecies
NZCETA_DOC_joined %>% group_by(Species_subspecies) %>% count() 
# Let's double check whether the "Cephalorhynchus hectori ?" ones should really be unknown. None have a Location.x
NZCETA_DOC_joined %>% filter(Species_subspecies=="Cephalorhynchus hectori ?") %>% group_by(Location.x) %>% count()
# None have a region
NZCETA_DOC_joined %>% filter(Species_subspecies=="Cephalorhynchus hectori ?") %>% group_by(Region) %>% count()
# None have an Ocean
NZCETA_DOC_joined %>% filter(Species_subspecies=="Cephalorhynchus hectori ?") %>% group_by(Ocean) %>% count()
# None have a coordinate
NZCETA_DOC_joined %>% filter(Species_subspecies=="Cephalorhynchus hectori ?") %>% group_by(LatitudeS_Northing,LongitudeE_Easting) %>% count()
# None have a DOC agency
NZCETA_DOC_joined %>% filter(Species_subspecies=="Cephalorhynchus hectori ?") %>% group_by(DoC_Agency) %>% count()
# None have a hologenome_region or broad_location, so yes: we don't know if any of these are Māui versus Hector's.
# We should exclude thse ones from our sample selection
NZCETA_DOC_joined %>% filter(Species_subspecies=="Cephalorhynchus hectori ?") %>% group_by(hologenome_region,broad_location) %>% count()

# LatitudeS_Northing/LongitudeE_Easting (doing both at the same time, b/c generally if there is an entry for one there should be for the other)
NZCETA_DOC_joined %>% group_by(LatitudeS_Northing,LongitudeE_Easting) %>% count() %>% filter(n>1)
# We have several that have the same coordinates, so we'll double check them
# All good (mum and foetus)
NZCETA_DOC_joined %>% filter(LatitudeS_Northing=="-36.7217" & LongitudeE_Easting=="174.3546") %>% as.matrix()
# All good (adult/juevenile + different collection dates), however they are missing a hologenome_region
# Let's fill that in for them and also see if any other pairs are in the same boat (e.g. with lat/long but no broad_location/hologenome_region)
# These two were sampled at Karioitahi (so Waikato)
NZCETA_DOC_joined %>% filter(LatitudeS_Northing=="-37.2823" & LongitudeE_Easting=="174.6531") %>% as.matrix()
NZCETA_DOC_joined %>% group_by(broad_location) %>% count() %>% print(n=32)
NZCETA_DOC_joined$broad_location[which(NZCETA_DOC_joined$LatitudeS_Northing=="-37.2823" & NZCETA_DOC_joined$LongitudeE_Easting=="174.6531")] <- "Waikato"
NZCETA_DOC_joined$hologenome_region[which(NZCETA_DOC_joined$LatitudeS_Northing=="-37.2823" & NZCETA_DOC_joined$LongitudeE_Easting=="174.6531")] <- "Maui"
NZCETA_DOC_joined %>% group_by(hologenome_region) %>% count() %>% print(n=32)

# OK, let's check out who doesn't have broad_locations and/or hologenome_regions:
NZCETA_DOC_joined %>% filter(is.na(broad_location) | is.na(hologenome_region)) %>% group_by(broad_location,hologenome_region) %>% count()
# 59 samples, and none of them have data for one and not the other (e.g. if they are NA for one, they are NA for the other).
# Let's examine the other 'location' fields for these guys.
NZCETA_DOC_joined %>% filter(is.na(broad_location) | is.na(hologenome_region)) %>% group_by(Location.x,Region,Ocean,LatitudeS_Northing,LongitudeE_Easting,DoC_Agency) %>% count()
# Cool, there are a few with coordinates, so let's fill in broad_location and hologenome_region for these, and then we can keep going on checking
# duplicates based on lat/longs
# This one is from O'Neill Bay, just north of Bethell's beach
NZCETA_DOC_joined %>% filter(LatitudeS_Northing=="-36.8848" & LongitudeE_Easting=="174.4376") %>% as.matrix()
NZCETA_DOC_joined$broad_location[which(NZCETA_DOC_joined$LatitudeS_Northing=="-36.8848" & NZCETA_DOC_joined$LongitudeE_Easting=="174.4376")] <- "Auckland"
NZCETA_DOC_joined$hologenome_region[which(NZCETA_DOC_joined$LatitudeS_Northing=="-36.8848" & NZCETA_DOC_joined$LongitudeE_Easting=="174.4376")] <- "Maui"

# Found floating off the Manukau Heads based on lat/long
NZCETA_DOC_joined %>% filter(LatitudeS_Northing=="-37.0835" & LongitudeE_Easting=="174.4284") %>% as.matrix()
NZCETA_DOC_joined$broad_location[which(NZCETA_DOC_joined$LatitudeS_Northing=="-37.0835" & NZCETA_DOC_joined$LongitudeE_Easting=="174.4284")] <- "Auckland"
NZCETA_DOC_joined$hologenome_region[which(NZCETA_DOC_joined$LatitudeS_Northing=="-37.0835" & NZCETA_DOC_joined$LongitudeE_Easting=="174.4284")] <- "Maui"

# Down as being collected inside Port Waikato
NZCETA_DOC_joined %>% filter(LatitudeS_Northing=="-37.3889" & LongitudeE_Easting=="174.7262") %>% as.matrix()
NZCETA_DOC_joined$broad_location[which(NZCETA_DOC_joined$LatitudeS_Northing=="-37.3889" & NZCETA_DOC_joined$LongitudeE_Easting=="174.7262")] <- "Waikato"
NZCETA_DOC_joined$hologenome_region[which(NZCETA_DOC_joined$LatitudeS_Northing=="-37.3889" & NZCETA_DOC_joined$LongitudeE_Easting=="174.7262")] <- "Maui"

# Converted the northing/easting at http://apps.linz.govt.nz/coordinate-conversion. Collected off Sumner Beach
NZCETA_DOC_joined %>% filter(LatitudeS_Northing=="5738070" & LongitudeE_Easting=="2490418") %>% as.matrix()
NZCETA_DOC_joined$broad_location[which(NZCETA_DOC_joined$LatitudeS_Northing=="5738070" & NZCETA_DOC_joined$LongitudeE_Easting=="2490418")] <- "Pegasus Bay"
NZCETA_DOC_joined$hologenome_region[which(NZCETA_DOC_joined$LatitudeS_Northing=="5738070" & NZCETA_DOC_joined$LongitudeE_Easting=="2490418")] <- "SI East Coast"

# Also converted this one at http://apps.linz.govt.nz/coordinate-conversion. Timaru.
NZCETA_DOC_joined %>% filter(LatitudeS_Northing=="NZMS: 260J39, K39713455") %>% as.matrix()
NZCETA_DOC_joined$broad_location[which(NZCETA_DOC_joined$LatitudeS_Northing=="NZMS: 260J39, K39713455")] <- "Timaru"
NZCETA_DOC_joined$hologenome_region[which(NZCETA_DOC_joined$LatitudeS_Northing=="NZMS: 260J39, K39713455")] <- "SI East Coast"

# Manukau heads based on lat/long
NZCETA_DOC_joined %>% filter(LatitudeS_Northing=="-37.0443" & LongitudeE_Easting=="174.5772") %>% as.matrix()
NZCETA_DOC_joined$broad_location[which(NZCETA_DOC_joined$LatitudeS_Northing=="-37.0443" & NZCETA_DOC_joined$LongitudeE_Easting=="174.5772")] <- "Auckland"
NZCETA_DOC_joined$hologenome_region[which(NZCETA_DOC_joined$LatitudeS_Northing=="-37.0443" & NZCETA_DOC_joined$LongitudeE_Easting=="174.5772")] <- "Maui"

# Karioitahi based on lat/long, but on Auckland side
NZCETA_DOC_joined %>% filter(LatitudeS_Northing=="-37.2602" & LongitudeE_Easting=="174.6414") %>% as.matrix()
NZCETA_DOC_joined$broad_location[which(NZCETA_DOC_joined$LatitudeS_Northing=="-37.2602" & NZCETA_DOC_joined$LongitudeE_Easting=="174.6414")] <- "Auckland"
NZCETA_DOC_joined$hologenome_region[which(NZCETA_DOC_joined$LatitudeS_Northing=="-37.2602" & NZCETA_DOC_joined$LongitudeE_Easting=="174.6414")] <- "Maui"

# Next to Kawhia (near Te Wharu Bay)
NZCETA_DOC_joined %>% filter(LatitudeS_Northing=="-38.0646" & LongitudeE_Easting=="174.8248") %>% as.matrix()
NZCETA_DOC_joined$broad_location[which(NZCETA_DOC_joined$LatitudeS_Northing=="-38.0646" & NZCETA_DOC_joined$LongitudeE_Easting=="174.8248")] <- "Waikato"
NZCETA_DOC_joined$hologenome_region[which(NZCETA_DOC_joined$LatitudeS_Northing=="-38.0646" & NZCETA_DOC_joined$LongitudeE_Easting=="174.8248")] <- "Maui"

# Final one has DoC_Agency being Wellington, which is less of a basis for assigning location, so going to make a reconciliation_note as well
# This is a museum sample collected in 1967, so exact location is uncertain, however believe this is the historical Māui sample identified
# in Pichler and Baker (2000) as having haplotype N
NZCETA_DOC_joined %>% filter(is.na(broad_location) & is.na(hologenome_region) & DoC_Agency=="Wellington") %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(is.na(NZCETA_DOC_joined$broad_location) & is.na(NZCETA_DOC_joined$hologenome_region) & NZCETA_DOC_joined$DoC_Agency=="Wellington")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(is.na(NZCETA_DOC_joined$broad_location) & is.na(NZCETA_DOC_joined$hologenome_region) & NZCETA_DOC_joined$DoC_Agency=="Wellington")],"Believe this is Māui sample from Pichler and Baker (2000) identified as having 'N' haplotype, but exact collection location unknown",sep=";")
NZCETA_DOC_joined$hologenome_region[which(is.na(NZCETA_DOC_joined$broad_location) & is.na(NZCETA_DOC_joined$hologenome_region) & NZCETA_DOC_joined$DoC_Agency=="Wellington")] <- "Maui"

# OK cool! That 'rescued' the location of about 10 samples. Finally back to checking our duplicate lat/longs.
# Up to the third one down
NZCETA_DOC_joined %>% group_by(LatitudeS_Northing,LongitudeE_Easting) %>% count() %>% filter(n>1)
# All good, mum and juvenile
NZCETA_DOC_joined %>% filter(LatitudeS_Northing=="-37.28423" & LongitudeE_Easting=="174.65421") %>% as.matrix()
# Very broad location, so probably OK (also different size class and collection date). Coordinates too broad to be used in further localizing samples
# (inland from Geraldine!)
NZCETA_DOC_joined %>% filter(LatitudeS_Northing=="44 S" & LongitudeE_Easting=="171 E") %>% as.matrix()
# All good, female and foetus
NZCETA_DOC_joined %>% filter(LatitudeS_Northing=="5743832" & LongitudeE_Easting=="248510") %>% as.matrix()

##################################################################################################################
#### 6f. QC'ing and correcting the values in the remaining columns (Tissue_Type,Collected_By,DoC_Agency,TL_m) ####
##################################################################################################################
names(NZCETA_DOC_joined)

# There is some variation in the way Tissue_Type is coded, but not going to standardize it, as this field will mainly be used for 
# a double check on categories of death rather than selecting samples
NZCETA_DOC_joined %>% group_by(Tissue_Type) %>% count()

# Collected_By
NZCETA_DOC_joined %>% group_by(Collected_By) %>% count() %>% filter(n>1)
# Dan Neale should be Don Neale
NZCETA_DOC_joined$Collected_By[which(NZCETA_DOC_joined$Collected_By=="Dan Neale")] <- "Don Neale"
# Now let's check out all the other people who've collected more than one sample. All of these are distinct samples.
NZCETA_DOC_joined %>% filter(Collected_By=="Al Hutt" | Collected_By=="Al Hutt/Jim Lilley") %>% as.matrix()
# All look to be distinct (except for the known replicates)
NZCETA_DOC_joined %>% filter(Collected_By=="Don Neale") %>% as.matrix()
# Look all good and distinct
NZCETA_DOC_joined %>% filter(Collected_By=="Garry Hickman") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Collected_By=="Henk Stengs") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Collected_By=="Jim Fyfe") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Collected_By=="Karl McLeod") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Collected_By=="Liz Slooten" | Collected_By=="Steve Dawson and Liz Slooten") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Collected_By=="Martin Abel") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Collected_By=="Mike Morrissey") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Collected_By=="Simon Mowbray") %>% as.matrix()
NZCETA_DOC_joined %>% filter(Collected_By=="Taranaki Museum") %>% as.matrix()

#DoC_Agency
NZCETA_DOC_joined %>% group_by(DoC_Agency) %>% count() %>% filter(n>1)
NZCETA_DOC_joined %>% filter(DoC_Agency=="Akaroa" | DoC_Agency=="Akaroa/Canterbury") %>% as.matrix()
NZCETA_DOC_joined %>% filter(DoC_Agency=="Auckland" | DoC_Agency=="Auckland (North Auckland)" | DoC_Agency=="Auckland (South Auckland)") %>% as.matrix()
NZCETA_DOC_joined %>% filter(DoC_Agency=="Buller") %>% as.matrix()
NZCETA_DOC_joined %>% filter(DoC_Agency=="Golden Bay") %>% as.matrix()
NZCETA_DOC_joined %>% filter(DoC_Agency=="Greymouth") %>% as.matrix()
NZCETA_DOC_joined %>% filter(DoC_Agency=="Hokitika") %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix() 
NZCETA_DOC_joined %>% filter(DoC_Agency=="Kaikoura") %>% as.matrix()
NZCETA_DOC_joined %>% filter(DoC_Agency=="Murihiku") %>% as.matrix()
NZCETA_DOC_joined %>% filter(DoC_Agency=="South Westland") %>% as.matrix()
NZCETA_DOC_joined %>% filter(DoC_Agency=="Waikato" | DoC_Agency=="Waikato (South Auckland)") %>% as.matrix()

#TL_m
NZCETA_DOC_joined %>% group_by(TL_m) %>% count() %>% print(n=44)
# Going to check dolphins within 10cm of each other and any duplicate lengths
NZCETA_DOC_joined %>% filter(TL_m=="-") %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="0.66" | TL_m=="0.67" | TL_m=="0.69" | TL_m=="0.72" | TL_m=="0.73" | TL_m=="0.76")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="0.67" | TL_m=="0.69" | TL_m=="0.72" | TL_m=="0.73" | TL_m=="0.76" | TL_m=="0.765" )  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="0.69" | TL_m=="0.72" | TL_m=="0.73" | TL_m=="0.76" | TL_m=="0.765" | TL_m=="0.78" | TL_m=="0.79")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="0.72" | TL_m=="0.73" | TL_m=="0.76" | TL_m=="0.765" | TL_m=="0.78" | TL_m=="0.79" | TL_m=="0.8" | TL_m=="0.818" | TL_m=="0.82")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter( TL_m=="0.78" | TL_m=="0.79" | TL_m=="0.8" | TL_m=="0.818" | TL_m=="0.82" | TL_m=="0.88")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="0.818" | TL_m=="0.82" | TL_m=="0.88" | TL_m=="0.92" )  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="0.92" | TL_m=="1.005" | TL_m=="1.02")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="1.005" | TL_m=="1.02" | TL_m=="1.095")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="1.02" | TL_m=="1.095"  | TL_m=="1.12")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="1.12" | TL_m=="1.2" | TL_m=="1.21" | TL_m=="1.22")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="1.2" | TL_m=="1.21" | TL_m=="1.22" | TL_m=="1.23" | TL_m=="1.24" | TL_m=="1.25" | TL_m=="1.27" | TL_m=="1.284" | TL_m=="1.3")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="1.21" | TL_m=="1.22" | TL_m=="1.23" | TL_m=="1.24" | TL_m=="1.25" | TL_m=="1.27" | TL_m=="1.284" | TL_m=="1.3" | TL_m=="1.305")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="1.23" | TL_m=="1.24" | TL_m=="1.25" | TL_m=="1.27" | TL_m=="1.284" | TL_m=="1.3" | TL_m=="1.305" | TL_m=="1.33")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="1.24" | TL_m=="1.25" | TL_m=="1.27" | TL_m=="1.284" | TL_m=="1.3" | TL_m=="1.305" | TL_m=="1.33" | TL_m=="1.34")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="1.27" | TL_m=="1.284" | TL_m=="1.3" | TL_m=="1.305" | TL_m=="1.33" | TL_m=="1.34" | TL_m=="1.36")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="1.284" | TL_m=="1.3" | TL_m=="1.305" | TL_m=="1.33" | TL_m=="1.34" | TL_m=="1.36" | TL_m=="1.39")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="1.305" | TL_m=="1.33" | TL_m=="1.34" | TL_m=="1.36" | TL_m=="1.39" | TL_m=="1.41" )  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="1.33" | TL_m=="1.34" | TL_m=="1.36" | TL_m=="1.39" | TL_m=="1.41" | TL_m=="1.43")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter( TL_m=="1.36" | TL_m=="1.39" | TL_m=="1.41" | TL_m=="1.43" | TL_m=="1.45")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="1.41" | TL_m=="1.43" | TL_m=="1.51")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="1.43" | TL_m=="1.45" | TL_m=="1.51" | TL_m=="1.52")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="1.51" | TL_m=="1.52" | TL_m=="1.56")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(TL_m=="1.51" | TL_m=="1.52" | TL_m=="1.56" |  TL_m=="1.58" | TL_m=="1.595")  %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()

##########################################################################################################################
#### 6g. QC'ing and correcting the values in the remaining columns (Age_Class,Sex_observed,Pregnant?,Doc_incident_no, ####
#### Doc_animal_no, Franz_source_code, Massey_Code, MONZ_code, CETOS_code, Necropsy_file_no_Pathology_no)             ####
##########################################################################################################################
names(NZCETA_DOC_joined)

# Age_Class
NZCETA_DOC_joined %>% group_by(Age_Class) %>% count() %>% filter(n>1)
NZCETA_DOC_joined %>% filter(Age_Class=="3y") %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(Age_Class=="5y") %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(Age_Class=="adult") %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(Age_Class=="calf") %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(Age_Class=="foetus") %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(Age_Class=="juvenile") %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(Age_Class=="subadult") %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()
NZCETA_DOC_joined %>% filter(Age_Class=="TBC") %>% arrange(Genetic_Sex,mtDNA_haplotype) %>% as.matrix()

# Sex_observed. Values look all good - no point checking these as categories too broad
NZCETA_DOC_joined %>% group_by(Sex_observed) %>% count()

# Pregnant?
NZCETA_DOC_joined %>% group_by(`Pregnant?`) %>% count()
NZCETA_DOC_joined %>% filter(!is.na(`Pregnant?`)) %>% as.matrix()

# Doc_incident_no. There are 2 records with 'H200/10' and 11 with 'N/A'
NZCETA_DOC_joined %>% group_by(Doc_incident_no) %>% count() %>% filter(n>1)
# All good, known recorded duplicate
NZCETA_DOC_joined %>% filter(Doc_incident_no=="H200/10") %>% as.matrix() 
# Going to leave these records as 'N/A' because that suggests they were specifically recorded as not having one
NZCETA_DOC_joined %>% filter(Doc_incident_no=="N/A") %>% as.matrix() 

# Doc_animal_no - all good
NZCETA_DOC_joined %>% group_by(Doc_animal_no) %>% count() %>% filter(n>1)

# Franz_source_code - all good
NZCETA_DOC_joined %>% group_by(Franz_source_code) %>% count() %>% filter(n>1)

# Massey_Code - all good
NZCETA_DOC_joined %>% group_by(Massey_Code) %>% count() %>% filter(n>1)

# MONZ_code - all good
NZCETA_DOC_joined %>% group_by(MONZ_code) %>% count() %>% filter(n>1)

# CETOS_code - all good
NZCETA_DOC_joined %>% group_by(CETOS_code) %>% count() %>% filter(n>1)

# Necropsy_file_no_Pathology_no - 36528 is duplicated
NZCETA_DOC_joined %>% group_by(Necropsy_file_no_Pathology_no) %>% count() %>% filter(n>1)
# All good - mum/foetus, so same necropsy
NZCETA_DOC_joined %>% filter(Necropsy_file_no_Pathology_no=="36528") %>% as.matrix()

########################################################################################################################
#### 6h. QC'ing and correcting the values in the remaining columns (Genetic_Sex,mtDNA_haplotype,no_microsatellites, ####
#### MHC_DQA, MHC_DQB)                                                                                              ####
########################################################################################################################

# Going to check combinations of haplotypes that could be from the same individuals
NZCETA_DOC_joined %>% filter(!is.na(Genetic_Sex) | !is.na(mtDNA_haplotype)) %>% group_by(mtDNA_haplotype,Genetic_Sex, broad_location,collection_date_mod,pathology_code) %>% count() %>% print(n=218)
# "BP" sample code, but not enough detail to confirm  match to other samples
NZCETA_DOC_joined %>% filter(mtDNA_haplotype=="C" & Genetic_Sex=="F" & is.na(broad_location) & collection_date_mod=="1994") %>% as.matrix()
# Seem to be different samples
NZCETA_DOC_joined %>% filter(mtDNA_haplotype=="C" & Genetic_Sex=="M" & broad_location=="Akaroa") %>% as.matrix()
NZCETA_DOC_joined %>% filter(mtDNA_haplotype=="C" & Genetic_Sex=="M" & broad_location=="Canterbury") %>% as.matrix()
# Sample codes of "BP","KK","TI", but not enough detail to confirm  match to other samples
NZCETA_DOC_joined %>% filter(mtDNA_haplotype=="C" & Genetic_Sex=="M" & (collection_date_mod=="1994"|collection_date_mod=="1995")) %>% as.matrix()
# Potentially the same sample based on matching location and collection-date_mod (and no conflicting information)
NZCETA_DOC_joined %>% filter(mtDNA_haplotype=="Ca" & Genetic_Sex=="M" & broad_location=="Haast/Jackson Bay") %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE03NZ07")] <- "Potential duplicate of 'CHEWC03-03' based on sampling location and identical collection_date_mod"
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC03-03")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC03-03")],"Potential duplicate of 'CHE03NZ07' based on sampling location and identical collection_date_mod",sep=";")
# Not enough detail to match up to other samples definitively. Sample code is "WB"
NZCETA_DOC_joined %>% filter(mtDNA_haplotype=="Ca" & is.na(Genetic_Sex) & is.na(broad_location)) %>% as.matrix()
# Not enough detail to match up to other samples definitively
NZCETA_DOC_joined %>% filter(mtDNA_haplotype=="Cb" & Genetic_Sex=="M" & is.na(broad_location)) %>% as.matrix()
# Checking this one because it is down as a Hector's dolphin, but was found stranded in the Manukau
NZCETA_DOC_joined %>% filter(mtDNA_haplotype=="Cb1" & Genetic_Sex=="F" & broad_location=="Auckland") %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$mtDNA_haplotype=="Cb1" & NZCETA_DOC_joined$Genetic_Sex=="F" & NZCETA_DOC_joined$broad_location=="Auckland")] <- "Down as Hector's dolphin, but apparently sampled within range of Maui dolphin"
# Not enough information to definitively match it up to other samples
NZCETA_DOC_joined %>% filter(mtDNA_haplotype=="Cb1" & Genetic_Sex=="F" & is.na(broad_location)) %>% as.matrix()
# Not enough information to definitively match it up to other samples, but location code is "TI"
NZCETA_DOC_joined %>% filter(mtDNA_haplotype=="D" & Genetic_Sex=="M" & is.na(broad_location)) %>% as.matrix()
# Distinct based on a number of fields
NZCETA_DOC_joined %>% filter(mtDNA_haplotype=="G" & Genetic_Sex=="M" & broad_location=="Waikato" & grepl("2001",collection_date_mod)) %>% as.matrix()
# Not enough information to definitively match up samples
NZCETA_DOC_joined %>% filter(mtDNA_haplotype=="I" & grepl("1992",collection_date_mod)) %>% as.matrix()
# Distinct based on a number of fields
NZCETA_DOC_joined %>% filter(mtDNA_haplotype=="J" & Genetic_Sex=="M" & broad_location=="Westport" & grepl("1992",collection_date_mod)) %>% as.matrix()
# Potentially the same sample based on matching location and collection_date_mod (and no conflicting information)
NZCETA_DOC_joined %>% filter(mtDNA_haplotype=="Jb" & Genetic_Sex=="F" & broad_location=="Hokitika" & collection_date_mod=="24-Oct-2003") %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC03-02")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC03-02")] ,"Likely duplicate of 'CHE03NZ06' based on sampling location and identical collection_date_mod. Only use if additional tissue required, as CHE03NZ06 has more information available",sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE03NZ06")] <- "Likely duplicate of 'CHEWC03-02' based on sampling location and identical collection_date_mod. This (CHE03NZ06) should be considered sample of reference as more information available for it."
# Collected on the same date, but no location information sample for each individual so not going to add anything to the reconciliation notes
NZCETA_DOC_joined %>% filter(mtDNA_haplotype=="Jb" & Genetic_Sex=="M" & collection_date_mod=="25-Feb-2001") %>% as.matrix()
# slightly diferent location so probably distinct
NZCETA_DOC_joined %>% filter(mtDNA_haplotype=="M" & Genetic_Sex=="M" & grepl("200",collection_date_mod)) %>% as.matrix()

# no_microsatellites - all good - not checking for 'repeats' because having same number of microsatellite loci doesn't imply match of those loci
NZCETA_DOC_joined %>% group_by(no_microsatellites) %>% count() %>% print(n=27)

# MHC_DQA - as for no_microsatellites
NZCETA_DOC_joined %>% group_by(MHC_DQA) %>% count() %>% print(n=27)

# MHC_DQB - as for no_microsatellites
NZCETA_DOC_joined %>% group_by(MHC_DQB) %>% count() %>% print(n=27)

########################################################################################################################
#### 6i. QC'ing and correcting the values in the remaining columns (pathology_code                                      ####
########################################################################################################################
names(NZCETA_DOC_joined)

# pathology_code
NZCETA_DOC_joined %>% group_by(pathology_code) %>% count() %>% filter(n>1)
# Six pairs of samples share the same pathology code

# Based on Pathology code, these two samples are a match. More information is available for "CHESI56" than "CHE01NZ01"
NZCETA_DOC_joined %>% filter(pathology_code=="H039") %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE01NZ01")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE01NZ01")],"This sample is a duplicate of CHESI56 based on matching pathology_codes. Only use if additional/duplicate tissue necessary", sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI56")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI56")],"CHE01NZ01 is a duplicate of this sample based on matching pathology_codes. This (CHESI56) should be considered the 'sample of reference' as it had more information in the DOC_Massey database.",sep=";")

# Mum and foetus, so same pathology_code
NZCETA_DOC_joined %>% filter(pathology_code=="H084") %>% as.matrix()

# Based on Pathology code, these two samples are a match. More information is available for 'CHE04NZ13' than for 'CHE04NZ01'
NZCETA_DOC_joined %>% filter(pathology_code=="H086") %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE04NZ01")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE04NZ01")],"This sample is a duplicate of CHE04NZ13 based on matching pathology_codes. Only use if additional/duplicate tissue necessary", sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE04NZ13")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE04NZ13")],"CHE04NZ01 is a duplicate of this sample based on matching pathology_codes. This (CHE04NZ13) should be considered the 'sample of reference' as it had more information in the DOC_Massey database.",sep=";")

# Known duplicates
NZCETA_DOC_joined %>% filter(pathology_code=="H106") %>% as.matrix()

# Based on Pathology code, these two samples are a match. More information is available for 'CHE09NZ03' than for 'CHE09NZ17'
NZCETA_DOC_joined %>% filter(pathology_code=="H192") %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ17")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ17")],"This sample is a duplicate of CHE09NZ03 based on matching pathology_codes. Only use if additional/duplicate tissue necessary", sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ03")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE09NZ03")],"CHE09NZ17 is a duplicate of this sample based on matching pathology_codes. This (CHE09NZ03) should be considered the 'sample of reference' as it had more information in the DOC_Massey database",sep=";")

# Known duplicates
NZCETA_DOC_joined %>% filter(pathology_code=="H200") %>% as.matrix()

# hologenome_region
NZCETA_DOC_joined %>% group_by(hologenome_region) %>% count()
NZCETA_DOC_joined %>% filter(is.na(hologenome_region) | hologenome_region=="Unknown") %>% group_by(Location.x,Region,Ocean,LatitudeS_Northing,LongitudeE_Easting) %>% count()

# collection_date_mod
NZCETA_DOC_joined %>% group_by(collection_date_mod) %>% count() %>% filter(n>1) %>% print(n=35)
# Going to examine the samples pairs/groups with same collection_date_mod
# Massey code mentions that Che03NZ05 is a foetus. CheWC03-01 is either the same sample, or the mother of the foetus
NZCETA_DOC_joined %>% filter(collection_date_mod=="10-Oct-2003") %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHE03NZ05")] <- "CHEWC03-01 is either a duplicate, or the mother, of this sample based on matching Location.x and collection_date_mod. This (Che03NZ05) should be considered the 'sample of reference' as it had more information in the DOC_Massey database. Likely foetal tissue"
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC03-01")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC03-01")],"This sample is either a duplicate of CHE03NZ05, or its mother, based on matching Location.x and collection_date_mod. Only use if additional/duplicate tissue necessary",sep=";")

# Based on similar sampling locations and identical collecton_mod_date, likely the same sample
NZCETA_DOC_joined %>% filter(collection_date_mod=="11-Mar-2000") %>% as.matrix()
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESIFP_CETOS9698")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESIFP_CETOS9698")],"This sample is a duplicate of CHEBP62 based on similar Location.x and matching collection_date_mod. Only use if additional/duplicate tissue necessary", sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP62")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEBP62")],"CHESIFP_CETOS9698 is a duplicate of this sample based on similar Location.x and matching collection_date_mod. This (CHEBP62) should be considered the 'sample of reference' as it had more information in the DOC_Massey database. CHESIFP_CETOS9698 had 'PC Bay, Akaroa' as its Location.x, which probably explains where the Pegasus Bay confusion arose from",sep=";")

# Queen Charlotte Sound samples
NZCETA_DOC_joined %>% filter(collection_date_mod=="13-Jun-2016") %>% as.matrix()

# Previously identified as likely duplicates based on Genetic_Sex, mtDNA_haplotype
NZCETA_DOC_joined %>% filter(collection_date_mod=="13-Nov-2003") %>% as.matrix()

# Known duplicate
NZCETA_DOC_joined %>% filter(collection_date_mod=="13-Oct-2010") %>% as.matrix()

# Previouslyl identified as likely duplicate based on pathology code
NZCETA_DOC_joined %>% filter(collection_date_mod=="14-Nov-2004") %>% as.matrix()

# Previouslyl identified as likely duplicate based on Location.x and identical collection_date_mod
NZCETA_DOC_joined %>% filter(collection_date_mod=="19-Mar-2000") %>% as.matrix()

# Look distinct based on CETOS_codes and/or other information
NZCETA_DOC_joined %>% filter(collection_date_mod=="1994") %>% as.matrix()
NZCETA_DOC_joined %>% filter(collection_date_mod=="1995") %>% as.matrix()
NZCETA_DOC_joined %>% filter(collection_date_mod=="1998") %>% as.matrix()
NZCETA_DOC_joined %>% filter(collection_date_mod=="2-Jan-1998") %>% as.matrix() 
NZCETA_DOC_joined %>% filter(collection_date_mod=="2-Nov-2005") %>% as.matrix() 
NZCETA_DOC_joined %>% filter(collection_date_mod=="20-Oct-1998") %>% as.matrix() 
NZCETA_DOC_joined %>% filter(collection_date_mod=="21-Feb-2002") %>% as.matrix() 

# Based on similar sampling locations and identical collecton_mod_date, likely the same sample
NZCETA_DOC_joined %>% filter(collection_date_mod=="22-Aug-1994") %>% as.matrix() 
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC009")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC009")],"This sample is a duplicate of CHESI27 based on similar Location.x and matching collection_date_mod. Only use if additional/duplicate tissue necessary", sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI27")] <- "CHEWC009 is a duplicate of this sample based on similar Location.x and matching collection_date_mod. This (CHESI27) should be considered the 'sample of reference' as it had more information in the DOC_Massey database"

# Based on similar sampling locations and identical collecton_mod_date, likely the same sample
NZCETA_DOC_joined %>% filter(collection_date_mod=="22-Dec-1999") %>% as.matrix() 
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESIFP_CETOS8841")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESIFP_CETOS8841")],"This sample is a duplicate of CHETI12 based on similar Location.x and matching collection_date_mod. Only use if additional/duplicate tissue necessary", sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETI12")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHETI12")] ,"CHESIFP_CETOS8841 is a duplicate of this sample based on similar Location.x and matching collection_date_mod. This (CHETI12) should be considered the 'sample of reference' as it had more information in the DOC_Massey database",sep=";")

# Based on identical collection_mod_date and matching sex/mtDNA haplotype, likely the same sample
NZCETA_DOC_joined %>% filter(collection_date_mod=="23-Jan-2001") %>% as.matrix() 
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI52")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI52")],"This sample is a duplicate of CHESI28 based on matching collection_date_mod, Genetic_sex, and mtDNA_haplotype. Only use if additional/duplicate tissue necessary", sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI28")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI28")] ,"CHESI52 is a duplicate of this sample based on matching collection_date_mod, Genetic_sex, and mtDNA_haplotype. This (CHESI28) should be considered the 'sample of reference' as it had more information in the DOC_Massey database",sep=";")

# Mismatching sex and no other information to help match them up, so left them separate
NZCETA_DOC_joined %>% filter(collection_date_mod=="23-Nov-1999") %>% as.matrix() 

# Known duplicate
NZCETA_DOC_joined %>% filter(collection_date_mod=="23-Nov-2005") %>% as.matrix() 

# Previously identified based on sampling location and collection_date_mod
NZCETA_DOC_joined %>% filter(collection_date_mod=="24-Oct-2003") %>% as.matrix() 
NZCETA_DOC_joined %>% filter(collection_date_mod=="25-Apr-2012") %>% as.matrix() 

# Based on identical collection_mod_date and matching sex/mtDNA haplotype, likely the same sample
NZCETA_DOC_joined %>% filter(collection_date_mod=="25-Feb-2001") %>% as.matrix() 
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI29")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI29")],"This sample is a duplicate of CHESI30 based on matching collection_date_mod, Genetic_sex, and mtDNA_haplotype. Only use if additional/duplicate tissue necessary", sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI30")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI30")] ,"CHESI29 is a duplicate of this sample based on matching collection_date_mod, Genetic_sex, and mtDNA_haplotype. This (CHESI30) should be considered the 'sample of reference' based on a coin toss (both samples have the same amount of information available)",sep=";")

# Mum and foetus
NZCETA_DOC_joined %>% filter(collection_date_mod=="25-Nov-1997") %>% as.matrix() 

# Previoiusly identified based on Location.x/collection_date_mod
NZCETA_DOC_joined %>% filter(collection_date_mod=="25-Nov-2002") %>% as.matrix() 

# Just happened to share the same collection_date_mod - very distinct samples!
NZCETA_DOC_joined %>% filter(collection_date_mod=="26-Aug-1993") %>% as.matrix() 

# Previoiusly identified based on Location.x/collection_date_mod
NZCETA_DOC_joined %>% filter(collection_date_mod=="27-Mar-2000") %>% as.matrix() 

# Just happened to share the same collection_date_mod - distinct samples!
NZCETA_DOC_joined %>% filter(collection_date_mod=="30-Jan-2004") %>% as.matrix() 

# Mum and neonate
NZCETA_DOC_joined %>% filter(collection_date_mod=="4-Dec-2006") %>% as.matrix() 

# Distinct animals, entangled
NZCETA_DOC_joined %>% filter(collection_date_mod=="4-Feb-2005") %>% as.matrix() 

# Just happened to share the same collection_date_mod - distinct samples!
NZCETA_DOC_joined %>% filter(collection_date_mod=="5-Jan-2005") %>% as.matrix() 

# Likely same sample based on shared location and collection_date_mod
NZCETA_DOC_joined %>% filter(collection_date_mod=="6-Jun-2001") %>% as.matrix() 
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC130")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHEWC130")],"This sample is likely a duplicate of CHESI01 based on similar Location.x and matching collection_date_mod. Only use if additional/duplicate tissue necessary", sep=";")
NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI01")] <- paste(NZCETA_DOC_joined$reconciliation_notes[which(NZCETA_DOC_joined$scrubbed_uoa_code=="CHESI01")],"CHEWC130 is likely a duplicate of this sample based on similar Location.x and matching collection_date_mod. This (CHESI01) should be considered the 'sample of reference' as it had more information in the DOC_Massey database",sep=";")

# Just happened to share the same collection_date_mod - distinct samples!
NZCETA_DOC_joined %>% filter(collection_date_mod=="8-Feb-1998") %>% as.matrix() 

# Mum and foetus
NZCETA_DOC_joined %>% filter(collection_date_mod=="9-Oct-2004") %>% as.matrix() 

# Not enough information to match any of these samples up
NZCETA_DOC_joined %>% filter(collection_date_mod=="Unknown") %>% as.matrix() 

# broad_location
# Second to last field to check!
NZCETA_DOC_joined %>% group_by(Location.x,Region,Ocean,broad_location,hologenome_region) %>% count() %>% print(n=230)
# All look consistent.



# Leaving reconciliation_notes for the end because these are contextual



# Population broad_location based on sample code where this is known e.g. BP = Banks Peninsula, KK = Kaikoura, TI = Timaru

# Ditch duplicates before combining with Pathology data


# EXCLUDE "Cephalorhynchus hectori ?"

# PRIORITIZE BECTORS (GIVES US AN IDEA OF ENVIRONMENTAL VS INHERITED) (AND HECTORS IN RANGE OF MAUI E.G. CHE12NZ02)
# PRIORITIZE SAMPLED BY BIOPSY AND STRANDING (COMPARATIVE)
# PRIORITIZE KNOWN DEATHS
# INCLUDE BIOPSIES OF SIMILAR AGES TO STRANDED SAMPLES FOR LIVE VS DEATH COMPARISON
# BALANCED SAMPLING NORTH ISLAND, NORTH COAST SOUTH ISLAND, WEST COAST SOUTH ISLAND, EAST COAST SOUTH ISLAND, SOUTH COAST SOUTH ISLAND
# MTDNA/MICROSATELLITES AVAILABLE (MAKES IT MORE LIKELY IT IS A VIABLE SAMPLE/DOUBLE CHECK FOR SAMPLE MIX UPS)
# MHC available
# LARGE NUMBER OF MICROSATELLITES LOCI BETTER THAN LESS?
# AGE CLASS?

# Potentially don't want the Checked=="No" sample. Don't want Common_name=="Uncertain"
# Date_received=="Nov-05" might be interesting - all caught in same net

# Double check no problems in the reconciliation_notes after high-grading for samples.

# "CHE05NZ18" has more info in it from Massey, so suggest taking it as the "sample of reference", but also referring to 'Che05NZ08' for additional/duplicate tissue if needed. 
# Question: how independent are these likely to be if given a different code?

# "CHE10NZ04" has more info in it from Massey, so suggest taking it as the "sample of reference", but also referring to 'CHE10NZ10' for additional/duplicate tissue if needed. Will also update the NAs for

# bectors 1 =  NI10-03, Bectors2 = Chem15NZ08 and Chem16NZ31, Bectors 3 = NI10-24, NI11-11 and Chem15NZ04

Hector's within the rohe of Māui
# A tibble: 2 x 1
scrubbed_uoa_code
<chr>            
  1 CHE11NZ06        
2 CHE12NZ02        

# Neils Beach Jackson Bay, South Westland could be interesting samples ( four co-caught dolphins)

# Would the parent/fetus pairs be of any interest?

# Museum samples to look at genomic loss of diversity?

#DOC_Massey_database ($DOC_incident_no) should be able to be massaged to match up to Pathology_data ($H_no.)

#Maui_recapture should be able to be matched up to DOC_Massey_database ($UoA_Code) and NZCETA_archive ($Code) based on $Indiv_ID


