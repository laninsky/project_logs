#################################################################
#### 1. Description of data sets that were reconciled across ####
#################################################################
# (also see tibble previews below for description of columns)

## "NZCETA_DOC_joined.txt": The result of reconciling DOC_Massey_database.txt (IDs sent to # DOC and Massey to inform their databases), and  NZCETA_archive.txt (NZCeTA Māui/Hector's # tissue info, not 100% reconciled) in reconciling_databases_1.R

## "hectors_mauis_incidents_30Aug2019.txt": DOC's master list of samples, which will be compared to the final list we whittle down
# https://www.doc.govt.nz/our-work/hectors-and-maui-dolphin-incident-database/

## "Pathology_data.txt": List of cases where a diagnosis of cause of death
# was possible.

## "Maui_recapture.txt": Reconciled biopsy and stranding information for
# Māui dolphins. Similar information for Hector's is more fragmentary
# so will need to narrow sample list down first.

# All original data sets were delivered in excel format. I saved all of them as 
# tab delimited for importing into R, in some cases after modifying them
# to make them more appropriate as flat files e.g. adding column with
# information given as colour coding in original excel file.

# Setting wd where these files found
setwd("/Users/alanaalexander/Dropbox/hologenome/original_files")

#####################################################################
#### 2. Loading libraries, reading in files, tibble descriptions ####
#####################################################################
library(tidyverse)
NZCETA_DOC_joined <- read_tsv("NZCETA_DOC_joined.txt")
NZCETA_DOC_joined
# A tibble: 327 x 45
#Checked Common_name Code  Date_received Location.x Region Ocean Cause_of_death What 
#<chr>   <chr>       <chr> <chr>         <chr>      <chr>  <chr> <chr>          <chr>
#  1 Yes     Hector's d… Che0… NA            Barrytown… NA     NA    NA             NA   
# 2 Yes     Hector's d… Che0… NA            Mikonui m… NA     NA    NA             NA   
#3 Yes     Hector's d… Che0… NA            Haast Bea… NA     NA    NA             NA   
# 4 Yes     Hector's d… Che0… NA            Orepuki-S… South… NA    NA             NA   
#5 Yes     Hector's d… Che0… Dec-05        Fossil Po… Nelson Tasm… NA             NA   
# 6 Yes     Hector's d… Che0… NA            Purakaunu… Otago  Paci… NA             NA   
#7 Yes     Hector's d… Che1… NA            Serpentin… West … Tasm… NA             NA   
# 8 Yes     Hector's d… Che1… NA            Opunake, … NA     NA    NA             NA   
#9 Yes     Hector's d… CheB… NA            Akaroa Ha… Cante… Paci… NA             NA   
#10 NA      Hector's d… CheB… NA            1.3km nor… Cante… Paci… NA             NA   
## … with 317 more rows, and 36 more variables: Info_sent <chr>, Extracted <chr>,
##   Info_requested <chr>, Other_info <chr>, Archiving_info <chr>, scrubbed_uoa_code <chr>,
##   reconciled_alternate_code <chr>, UoA_Code <chr>, Species_subspecies <chr>,
##   LatitudeS_Northing <chr>, LongitudeE_Easting <chr>, Tissue_Type <chr>,
##   Collected_By <chr>, DoC_Agency <chr>, TL_m <chr>, Age_Class <chr>, Sex_observed <chr>,
##   `Pregnant?` <chr>, Doc_incident_no <chr>, Doc_animal_no <dbl>, Franz_source_code <chr>,
##   Massey_Code <chr>, MONZ_code <chr>, CETOS_code <chr>,
##   Necropsy_file_no_Pathology_no <chr>, Genetic_Sex <chr>, mtDNA_haplotype <chr>,
##   no_microsatellites <dbl>, MHC_DQA <chr>, MHC_DQB <chr>, reconciliation_notes <chr>,
##   pathology_code <chr>, hologenome_region <chr>, collection_date_mod <chr>,
##   broad_location <chr>, review <chr>

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

DOC_doublecheck <- read_tsv("hectors_mauis_incidents_30Aug2019.txt")
DOC_doublecheck
#A tibble: 629 x 31
#`MarMam observa… `MarMam individ… `Animal ID` `Vernacular nam… `Date event obs… `Date event obs… `Date event rep… `Observation ty…
#<dbl>            <dbl> <chr>       <chr>            <chr>            <chr>            <chr>            <chr>           
#  1              293               NA NA          Hector's dolphin 1/01/22 0:00     Estimated        1/01/99          Historic mortal…
#2              912                1 NA          Maui dolphin     1/05/21 0:00     Actual           1921-05-01       Historic mortal…
#3              913                2 NA          Hector's dolphin 1/01/45 0:00     Actual           1945-01-01       Historic mortal…
#4              914                3 NA          Hector's dolphin 1/01/48 0:00     Actual           1948-01-01       Historic mortal…
#5              915                4 NA          Hector's dolphin 1/03/48 0:00     Actual           1948-03-01       Historic mortal…
#6              916                5 NA          Maui dolphin     20/12/50 0:00    Actual           1950-12-20       Historic mortal…
#7              917                6 NA          Hector's dolphin 1/03/53 0:00     Actual           1953-03-01       Historic mortal…
#8              918                7 NA          Hector's dolphin 14/08/53 0:00    Actual           1953-08-14       Historic mortal…
#9              919                8 NA          Maui dolphin     20/12/53 0:00    Actual           1953-12-20       Historic mortal…
#10              920                9 NA          Hector's dolphin 1/01/56 0:00     Actual           1956-01-01       Historic mortal…
## … with 619 more rows, and 23 more variables: `Gear type` <chr>, Latitude <dbl>, Longitude <dbl>, `Coordiante Capture method` <chr>,
##   Location <chr>, `Area descriptor` <chr>, `NZ Regions` <chr>, `Physical population` <chr>, `General description of conditions` <chr>,
##   Description <chr>, `Event remarks` <chr>, Platform <chr>, `DOC response` <chr>, `Agency notified` <chr>, `DNA test results
##   Haplotype` <chr>, sex <chr>, `Age class` <chr>, `Length cm` <dbl>, `Suspected primary cause of death` <chr>, `Necropsy status` <chr>,
##   `Necropsy results` <chr>, `Necropsy results details` <chr>, `Result confidence (for human-related cause of death only)` <chr>


######################################################################################################
#### 3. Jettisoning apparent duplicates in NZCETA_DOC_joined before combining with Pathology_data ####
######################################################################################################

# Getting rid of the "second class" duplicates
NZCETA_DOC_joined <- NZCETA_DOC_joined %>% filter(review!="Duplicate (secondary sample)")

# Getting rid of NA only rows and columns in Pathology_data
Pathology_data <- Pathology_data %>% filter(!is.na(H_no.) | !is.na(Path_No.)) %>% select(-(X10:X15))

# OK, so there are 35 records that are in Pathology_data, but not in NZCETA_DOC_joined.
# This seems to be due to some of the recent records present in Pathology_data not present in NZCETA_joined
# So I think we can go ahead and join them and if those samples in Pathologylook like they should be included
# we can hopefully chase down locations etc for them
dim(full_join(NZCETA_DOC_joined,Pathology_data, by = c("pathology_code" = "H_no.")))[1] - dim(NZCETA_DOC_joined)[1]

NZCETA_DOC_pathology <- full_join(NZCETA_DOC_joined,Pathology_data, by = c("pathology_code" = "H_no."))

#############################################################
#### 4. Identifying samples with recapture data for Māui ####
#############################################################

Maui_recapture %>% filter(!is.na(Stranded_sample)) %>% rowwise() %>% filter(!grepl(Stranded_sample,Indiv_ID)) %>% as.matrix()

# Following stranded Māui samples have biopsies also:
# NI92
# ChemU18-004
# U18-42 (Debbie advised me of this one via skype - not in Maui_recapture)

# Their names in NZCETA_DOC_pathology are:
NZCETA_DOC_pathology %>% filter(grepl("U18-004|U18-042|NI092",scrubbed_uoa_code)) %>% select(scrubbed_uoa_code)
# CHENI092         
# U18-004          
# U18-042

###################################################################################
#### 5. Identifying 'Bector's dolphins (Hector's sampled in the range of Māui) ####
###################################################################################

# Names in Hamner et al. are: Che11NZ06, Che12NZ02
# Their names in NZCETA_DOC_pathology are:
NZCETA_DOC_pathology %>% filter(grepl("11NZ06|12NZ02",scrubbed_uoa_code)) %>% select(scrubbed_uoa_code)
# CHE12NZ02        
# CHE11NZ06 

##################################################################################################################
#### 6. Whittling down samples to just those with H-codes/biopsy recaptures, then combining with DOC database ####
##################################################################################################################

# Excluding samples without pathology_codes (because pathology_code is what we'll match them up to the DOC_doublecheck based on)
NZCETA_DOC_pathology <- NZCETA_DOC_pathology %>% filter(!is.na(pathology_code) | grepl("U18-004|U18-042|NI092|CHE12NZ02|CHE11NZ06",scrubbed_uoa_code))

# Have to massage DOC_doublecheck's 'Animal ID' into the right format to match up to pathology_code
# Then filtering only to include samples that have an H-code
DOC_doublecheck <- DOC_doublecheck %>% mutate(pathology_code=ifelse(grepl("H",`Animal ID`),gsub("-.*","",gsub("\\/","-",gsub(".*H","H",`Animal ID`))),NA)) %>% mutate(pathology_code=ifelse(nchar(pathology_code)<=3,ifelse(nchar(pathology_code)==3,gsub("H","H0",pathology_code),ifelse(nchar(pathology_code)==2,gsub("H","H00",pathology_code),pathology_code)),pathology_code)) %>% filter(!is.na(pathology_code))

# Before smushing together, looking at NZCETA_DOC_pathology for records without H-Code, just to make sure there
# aren't any records for them in the database based on date etc, as we are merging based on H-Code
NZCETA_DOC_pathology %>% filter(is.na(pathology_code)) %>% as.matrix()
# OK, so there are two, and they have a collection date of 21-Jan-2018 and 30-Sep-2018
# Let's see if the DOC_doublecheck has those dates in it
DOC_doublecheck %>% filter(grepl("2018-01-21|2018-09-30",`Date event reported`)) %>% select(`Animal ID`,`Date event reported`,Location)
# These two are a match based on date and location. Next step is to see whether those H-codes are already present in NZCETA_DOC_pathology
#`Animal ID` `Date event reported` Location                  
#<chr>       <chr>                 <chr>                     
#  1 H267        2018-01-21            Sunset Beach, Port Waikato
#2 H273/M019   2018-09-30            Gibson Beach, Te Akau 
NZCETA_DOC_pathology %>% filter(pathology_code=="H267" | pathology_code=="H273") %>% as.matrix()
# OK cool! These two are already in NZCETA_DOC_pathology so we can combined them

# Let's start with pathology_code=="H267" and scrubbed_uoa_code=="U18-004" and make sure no info conflicts
NZCETA_DOC_pathology %>% filter(pathology_code=="H267" | scrubbed_uoa_code=="U18-004") %>% select_if(~!any(is.na(.)))
# There are no columns where both data sources have data, so we can go ahead and just fill in the missing values in the H267 row
missing_columns <- which(is.na(NZCETA_DOC_pathology[which(NZCETA_DOC_pathology$pathology_code=="H267"),]))
NZCETA_DOC_pathology[which(NZCETA_DOC_pathology$pathology_code=="H267"),missing_columns] <- NZCETA_DOC_pathology[which(NZCETA_DOC_pathology$scrubbed_uoa_code=="U18-004"),missing_columns]
# Double checking that looks all good
NZCETA_DOC_pathology[which(NZCETA_DOC_pathology$pathology_code=="H267"),] %>% as.matrix()
# Deleting row with scrubbed_uoa_code=="U18-004" but no pathology_code (because now captured in H267 row)
NZCETA_DOC_pathology <- NZCETA_DOC_pathology %>% filter(!(scrubbed_uoa_code=="U18-004" & is.na(pathology_code)))

# Moving on to pathology_code=="H273" and scrubbed_uoa_code=="U18-042"
NZCETA_DOC_pathology %>% filter(pathology_code=="H273" | scrubbed_uoa_code=="U18-042") %>% select_if(~!any(is.na(.)))
# There are no columns where both data sources have data, so we can go ahead and just fill in the missing values in the H273 row
missing_columns <- which(is.na(NZCETA_DOC_pathology[which(NZCETA_DOC_pathology$pathology_code=="H273"),]))
NZCETA_DOC_pathology[which(NZCETA_DOC_pathology$pathology_code=="H273"),missing_columns] <- NZCETA_DOC_pathology[which(NZCETA_DOC_pathology$scrubbed_uoa_code=="U18-042"),missing_columns]
# Double checking that looks all good
NZCETA_DOC_pathology[which(NZCETA_DOC_pathology$pathology_code=="H273"),] %>% as.matrix()
# Deleting row with scrubbed_uoa_code=="U18-042" but no pathology_code (because now captured in H267 row)
NZCETA_DOC_pathology <- NZCETA_DOC_pathology %>% filter(!(scrubbed_uoa_code=="U18-042" & is.na(pathology_code)))

# OK, now we've handled the duplicate records in NZCETA_DOC_pathology, let's smush NZCETA_DOC_pathology and DOC_doublecheck together!
final_data <- left_join(NZCETA_DOC_pathology,DOC_doublecheck,by="pathology_code")

# Filter down to Wendi's confident necropsy calls and/or to the other samples mentioned above
final_data <- final_data %>% filter(!is.na(COD_category) | grepl("U18-004|U18-042|NI092|CHE12NZ02|CHE11NZ06",scrubbed_uoa_code))

# Time to reconcile some columns again :)
names(final_data)

# Starting off with common_name
final_data %>% group_by(Common_name,`Vernacular name`) %>% count()
# Everything else either matches up, or provides information where there is none in either database
# Two records have an NA for both databases, so we'll take a look at them, but probably need to filter them out
final_data %>% filter(is.na(Common_name) & is.na(`Vernacular name`)) %>% as.matrix()
# These two records are for a probable bycatch, and a fetus death due to brucella (suspect the foetus of the Māui we are genome sequencing)
# Will filter them out seeing as they don't really have enough info to be going on with
final_data <- final_data %>% filter(!(is.na(Common_name) & is.na(`Vernacular name`)))
# Now going to copy across where Common_name has NAs and ditch `Vernacular name`
final_data <- final_data %>% mutate(Common_name=ifelse((is.na(Common_name)|Common_name=="Uncertain"),`Vernacular name`,Common_name)) %>% select(-`Vernacular name`)
# Double checking everything went to plan
final_data %>% group_by(Common_name) %>% count()

# Getting the names of our columns in order to see what we will keep/reconcile/throw out
names(final_data)

# Going to start jettisoning other fields that aren't going to be directly relevant to us and/or useful for reconciling (b/c column only present in one data source)
# These columns we know the contents of because they were in our original NZCETA sheet. For the remainder we probably want to look at their contents
final_data <- final_data %>% select(-Checked,-What,-Info_sent,-Extracted,-Info_requested,-Other_info,-Archiving_info,-Franz_source_code,-Massey_Code,-MONZ_code,-CETOS_code)

# Classifying columns into keep/delete after reconciliation/delete now (e.g. if only NAs remain after filtering out samples)
final_data %>% group_by(Common_name) %>% count() # keep
final_data %>% group_by(Code) %>% count() # keep - this will be how we identify UoA samples                                                  
final_data %>% group_by(Date_received) %>% count() # reconcile with date fields from DOC_doublecheck and then delete
final_data %>% group_by(Location.x) %>% count() # reconcile with Location field from DOC_doublecheck and then delete one or the other                                              
final_data %>% group_by(Region) %>% count() # reconcile with NZ regions from DOC_doublecheck and then delete
final_data %>% group_by(Ocean) %>% count()  # reconcile with Location field from DOC_doublecheck and then delete                                                    
final_data %>% group_by(Cause_of_death.x) %>% count() # deleting because all NAs
final_data <- final_data %>% select(-Cause_of_death.x)
final_data %>% group_by(scrubbed_uoa_code) %>% count() # keep - this will be our reference for these samples                                       
final_data %>% group_by(reconciled_alternate_code) %>% count() # deleting because all NAs
final_data <- final_data %>% select(-reconciled_alternate_code)
final_data %>% group_by(UoA_Code) %>% count() # reconcile with Code and scrubbed_uoa_code and then delete                                                
final_data %>% group_by(Species_subspecies) %>% count() # need to update based on Common_name and then keep
final_data %>% group_by(LatitudeS_Northing) %>% count() # reconcile with coordinates from DOC_doublecheck and then delete                                      
final_data %>% group_by(LongitudeE_Easting) %>% count() # reconcile with coordinates from DOC_doublecheck and then delete    
final_data %>% group_by(Tissue_Type) %>% count() # reconcile woth Observation type and then delete                                             
final_data %>% group_by(Collected_By) %>% count() # delete - not needed and not needed for reconciliation
final_data <- final_data %>% select(-Collected_By)
final_data %>% group_by(DoC_Agency) %>% count() # delete -  not needed and not needed for reconciliation                                            
final_data <- final_data %>% select(-DoC_Agency)
final_data %>% group_by(TL_m) %>% count() # Match up to Length cm and then delete
final_data %>% group_by(Age_Class) %>% count() # Match up to the other age_class fields and then delete                                                
final_data %>% group_by(Sex_observed) %>% count() # Match up to the other sex fields and then delete        
final_data %>% group_by(`Pregnant?`) %>% count()  # Match up to various necropsy fields and then delete                                   
final_data %>% group_by(Doc_incident_no) %>% count() # Match up to pathology_code and then delete
final_data %>% group_by(Doc_animal_no) %>% count() # Match up to MarMam individual ID num PK and then delete                                            
final_data %>% group_by(Necropsy_file_no_Pathology_no) %>% count() # Matchup to Path_No. and then delete
final_data %>% group_by(Genetic_Sex) %>% count() # Compare to sex column and only keep if differences present                                              
final_data %>% group_by(mtDNA_haplotype) %>% count() # Compare to DNA test results Haplotype and keep whichever columns has more information
final_data %>% group_by(no_microsatellites) %>% count() # delete -  not needed and not needed for reconciliation  
final_data <- final_data %>% select(-no_microsatellites)
final_data %>% group_by(MHC_DQA) %>% count() # delete - only NAs and nos remaining
final_data <- final_data %>% select(-MHC_DQA)
final_data %>% group_by(MHC_DQB) %>% count() # delete - only NAs and nos remaining
final_data <- final_data %>% select(-MHC_DQB)
final_data %>% group_by(reconciliation_notes) %>% count() # delete following reconciliation if it adds no further information
final_data %>% group_by(pathology_code) %>% count() # keep - this is actually "animal id" and will need to be renamed at the end.
final_data %>% group_by(hologenome_region) %>% count() # compare to Physical population and delete
final_data %>% group_by(collection_date_mod) %>% count() # compare to date event reported and delete                                     
final_data %>% group_by(broad_location) %>% count() # keep - but need to populate for samples that have location data from DOC_doublecheck only
final_data %>% group_by(review) %>% count() # keep - has information on duplicate sample                                                   
final_data %>% group_by(Path_No.) %>% count() # keep - assume useful for Wendi tracking sample
final_data %>% group_by(`Maui?`) %>% count() # compare to subspecies designation and then delete                                                    
final_data %>% group_by(Age_class) %>% count() # compare to other age_class columns and keep column with most information
final_data %>% group_by(Sex) %>% count() # compare to other sex columns and keep column with most information                                                       
final_data %>% group_by(COD_category) %>% count() # keep
final_data %>% group_by(Cause_of_death.y) %>% count() # keep                                       
final_data %>% group_by(`Death_by_pathogen?`) %>% count() # keep   
final_data %>% group_by(Notes) %>% count() # keep                                                   
final_data %>% group_by(`MarMam observation ID num`) %>% count() # delete -  not needed and not needed for reconciliation
final_data <- final_data %>% select(-`MarMam observation ID num`)
final_data %>% group_by(`MarMam individual ID num PK`) %>% count() # delete following reconciliation                             
final_data %>% group_by(`Animal ID`) %>% count() # delete following reconcilition to pathology_code
final_data %>% group_by(`Date event observed`) %>% count()  # reconcile to collection_date_mod and date event reported         
final_data %>% group_by(`Date event observed confidence`) %>% count() # delete after reconciling to collection_date_mod and date event reported 
final_data %>% group_by(`Date event reported`) %>% count() # Keep but change format to collection_data_mod                                     
final_data %>% group_by(`Observation type`) %>% count() # keep
final_data %>% group_by(`Gear type`) %>% count() # keep                                               
final_data %>% group_by(Latitude) %>% count() #keep
final_data %>% group_by(Longitude) %>% count()   #keep                                              
final_data %>% group_by(`Coordiante Capture method`) %>% count() # Not really needed
final_data <- final_data %>% select(-`Coordiante Capture method`)
final_data %>% group_by(Location) %>% count()  # Keep                                               
final_data %>% group_by(`Area descriptor`) %>% count() # delete after reconciling to other geographic fields
final_data %>% group_by(`NZ Regions`) %>% count() # Keep                                              
final_data %>% group_by(`Physical population`) %>% count() # Keep, but investigate the NA
final_data %>% group_by(`General description of conditions`) %>% count() # Not really needed 
final_data <- final_data %>% select(-`General description of conditions`)
final_data %>% group_by(Description) %>% count() # keep
final_data %>% group_by(`Event remarks`) %>% count() # keep                                           
final_data %>% group_by(Platform) %>% count() # not needed (captured by cause of death fields)
final_data <- final_data %>% select(-Platform)
final_data %>% group_by(`DOC response`) %>% count()  # not needed
final_data <- final_data %>% select(-`DOC response`)
final_data %>% group_by(`Agency notified`) %>% count() # not needed 
final_data <- final_data %>% select(-`Agency notified`)
final_data %>% group_by(`DNA test results Haplotype`) %>% count() # keep, but update with additional haplotype break down from other column                             
final_data %>% group_by(sex) %>% count() # compare to other sex columns and keep whichever column has the greatest amount of info
final_data %>% group_by(`Age class`) %>% count() # compare to other age_class columns and keep whatever has the greatest amount of info           
final_data %>% group_by(`Length cm`) %>% count() # keep
final_data %>% group_by(`Suspected primary cause of death`) %>% count()  # not needed
final_data <- final_data %>% select(-`Suspected primary cause of death`)
final_data %>% group_by(`Necropsy status`) %>% count()  # not needed
final_data <- final_data %>% select(-`Necropsy status`)
final_data %>% group_by(`Necropsy results`) %>% count() # compare to Wendi's various categories and then delete                                        
final_data %>% group_by(`Necropsy results details`) %>% count() # compare to Wendi's various categories and then make decision on whether to keep or not
final_data %>% group_by(`Result confidence (for human-related cause of death only)`) %>% count()  # not needed
final_data <- final_data %>% select(-`Result confidence (for human-related cause of death only)`) 

# Date_recieved reflects info found in all other date fields
final_data %>% filter(!is.na(Date_received)) %>% select(Date_received,collection_date_mod,`Date event observed`,`Date event observed confidence`,`Date event reported`)
final_data <- final_data %>% select(-Date_received)

# Location.x (matching up to Location)
final_data %>% filter(Location.x!=Location) %>% select(Location.x,Location)
# Majority of locations match up, so going to 'keep' Location column, with a few modifications for greater specificity in the Location.x column
final_data$Location[which(final_data$Location.x=="Opunake, Taranaki" & final_data$Location=="330m south of river mouth, Kina Road, Opunake")] <- "330m south of river mouth, Kina Road, Opunake, Taranaki"          
final_data$Location[which(final_data$Location.x=="Te Waewae Bay" & final_data$Location=="Te Waewae bay, on cobled beach, below cliffs")]<- "Te Waewae Bay, on cobbled beach, below cliffs"          
final_data$Location[which(final_data$Location.x=="Paroa Beach 200m North of School, near Greymouth Map Reference J32 593359" & final_data$Location=="Paroa Beach, 200m north of school (near Greymouth)")] <-"Paroa Beach 200m North of School, near Greymouth Map Reference J32 593359"       
final_data$Location[which(final_data$Location.x=="Hector Beach, Hector, West Coast (N of Westport)" & final_data$Location=="Hector Beach")] <- "Hector Beach, Hector, West Coast (N of Westport)"
final_data$Location[which(final_data$Location.x=="Rakaia River Mouth - 5 km north beside sea/lagoon barrier, Canterbury" & final_data$Location=="Rakaia River Mouth - 5 km north beside sea/lagoon barrier")] <- "Rakaia River Mouth - 5 km north beside sea/lagoon barrier, Canterbury"    
final_data$Location[which(final_data$Location.x=="3km N of Kekerengu, Canterbury" & final_data$Location=="3km N of Kekerengu")] <- "3km N of Kekerengu, Canterbury"                                           
final_data$Location[which(final_data$Location.x=="Ashworth's Beach, north of Christchurch; (Beach between Ashley River Mouth and  Ash----- Lagoon? can't read incident report), Pegasus Bay" & final_data$Location=="Beach between Ashley River mouth and Ashworth Lagoon")]  <- "Beach between Ashley River mouth and Ashworth Lagoon, north of Christchurch, Pegasus Bay"        
final_data$Location[which(final_data$Location.x=="Waikuku Beach, Canterbury, Pegasus Bay" & final_data$Location=="Waikuku Beach, Canterbury")] <- "Waikuku Beach, Canterbury, Pegasus Bay"                                  
final_data$Location[which(final_data$Location.x=="Matau Branch, Clutha River mouth, Otago" & final_data$Location=="Matau Branch, Clutha River mouth")] <- "Matau Branch, Clutha River mouth, Otago"                         
final_data$Location[which(final_data$Location.x=="Clark's Beach, Manukau Harbour, Auckland" & final_data$Location=="South side of Manukau Harbour at Seagrove, near Clarks Beach")] <- "South side of Manukau Harbour at Seagrove, near Clarks Beach, Auckland" 
final_data$Location[which(final_data$Location.x=="Raglan, floating ~500m off Indicators Point" & final_data$Location=="Approx 500m off Indicators Point, Raglan")] <- "Raglan, floating ~500m off Indicators Point"
final_data$Location[which(final_data$Location.x=="Ripiro Beach, south of Glinks Gully, Dargaville, Northland" & final_data$Location=="Ripiro Beach, South of Glinks Gully")] <- "Ripiro Beach, south of Glinks Gully, Dargaville, Northland"                          
final_data$Location[which(final_data$Location.x=="South Bay ,Timaru" & final_data$Location=="South bay, Timaru")] <- "South Bay, Timaru"                                           
final_data$Location[which(final_data$Location.x=="Gibson Beach, Te Akau, Waikato" & final_data$Location=="Gibson Beach, Te Akau")] <- "Gibson Beach, Te Akau, Waikato" 
final_data$Location[which(final_data$Location.x=="Greymouth" & final_data$Location=="Greynmouth Beach, off Miro St")] <- "Greymouth Beach, off Miro St"

# Double checking there are no NAs in Location that need to be filled in using Location.x
final_data %>% group_by(Location.x,Location) %>% count() %>% print(n=57)
# One sample from Timaru needs to be filled in for Location from Location.x
final_data$Location[which(final_data$Location.x=="Timaru" & is.na(final_data$Location))] <- "Timaru"
# Deleting now superfluous -Location.x
final_data <- final_data %>% select(-Location.x)

# Region (matching up to NZ regions)
final_data %>% filter(Region!=`NZ Regions`) %>% select(Region,`NZ Regions`)
# No regions that mismatch - double check there are no NAs in NZ Regions that need to be filled in from Region
final_data %>% group_by(Region,`NZ Regions`) %>% count() %>% print(n=57)
# Nope, all information available in NZ Regions (but going to double check the sample who has an NA for both)
final_data %>% filter(is.na(Region) & is.na(`NZ Regions`)) %>% as.matrix()
# This sample can be updated as combing from Canterbury
final_data$`NZ Regions`[which(is.na(final_data$Region) & is.na(final_data$`NZ Regions`))] <- "Canterbury"
# Deleting now superfluous Region
final_data <- final_data %>% select(-Region)
# Double checking that NZ Regions matches up to Location
final_data %>% group_by(Location,`NZ Regions`) %>% count() %>% print(n=57)
                                             
# Ocean (checking matches up to NZ regions and then deleting)
final_data %>% group_by(Ocean,`NZ Regions`) %>% count() %>% print(n=57)
# Yup matches up well, so ditching Ocean
final_data <- final_data %>% select(-Ocean)

# UoA_Code (checking matches up to Code and/or scrubbed_uoa_code)
final_data %>% group_by(Code,UoA_Code,scrubbed_uoa_code) %>% count() %>% print(n=57)
final_data <- final_data %>% select(-UoA_Code)

# Need to check that Species_subspecies has been updated to reflect our edits to Common_name
final_data %>% group_by(Common_name,Species_subspecies) %>% count()
final_data <- final_data %>% mutate(Species_subspecies=ifelse((is.na(Species_subspecies) | Species_subspecies=="Cephalorhynchus hectori ?"),ifelse(Common_name=="Hector's dolphin","Cephalorhynchus hectori hectori",ifelse(Common_name=="Maui dolphin","Cephalorhynchus hectori maui",Species_subspecies)),Species_subspecies))
final_data %>% group_by(Common_name,Species_subspecies) %>% count()

# LatitudeS_Northing and LongitudeE_Easting vs Latitude Longitude
final_data %>% group_by(LatitudeS_Northing,LongitudeE_Easting,Latitude,Longitude) %>% count() %>% print(n=55) %>% as.matrix()
# The three with lat/longs in LatitudeS_Northing and LongitudeE_Easting matched up (rest had mapgrid), so am going to ditch LatitudeS_Northing and LongitudeE_Easting
final_data <- final_data %>% select(-LatitudeS_Northing,-LongitudeE_Easting)

# Tissue_type - reconciling with Observation_type and tehn delete
final_data %>% group_by(Tissue_Type,`Observation type`) %>% count()
# One instance of Beachcast being given in Tissue_Type, and nothing available in Observation type
final_data$`Observation type`[which(final_data$Tissue_Type=="Beachcast" & is.na(final_data$`Observation type`))] <- "Beachcast (dead on shore)"
final_data <- final_data %>% select(-Tissue_Type)

# TL_m need to match up to Length_cm
final_data %>% mutate(Length_m=(`Length cm`/100)) %>% group_by(TL_m,Length_m) %>% count() %>% print(n=51)
# All records were within about 10cm, so going to ditch TL_m column and just retain Length cm
final_data <- final_data %>% select(-TL_m)

# Age class fields:
final_data %>% group_by(Age_Class,Age_class,`Age class`) %>% count()
# It seems like the Massey fields are the most comprehensive, but we'll make a note of those that conflict
# Or where info had to be taken from other fields. Not going to worry about subadult/juvenile/neonate categories being different
# because this seems to reflect more categories in NZCETA (e.g. neonate) and fewer categories in DOC (e.g. Adult vs Juvenile)
final_data$reconciliation_notes[which(final_data$Age_Class=="subadult" & is.na(final_data$Age_class) & final_data$`Age class`=="Juvenile")] <- paste(final_data$reconciliation_notes[which(final_data$Age_Class=="subadult" & is.na(final_data$Age_class) & final_data$`Age class`=="Juvenile")], "Age_class based on NZCETA: Massey had NA, and DOC had 'Juvenile'",sep=";")
final_data$Age_class[which(final_data$Age_Class=="subadult" & is.na(final_data$Age_class) & final_data$`Age class`=="Juvenile")] <- "S"

final_data$reconciliation_notes[which(is.na(final_data$Age_Class) & final_data$Age_class=="S" & final_data$`Age class`=="Adult")][1] <- paste(final_data$reconciliation_notes[which(is.na(final_data$Age_Class) & final_data$Age_class=="S" & final_data$`Age class`=="Adult")][1],"Age_class based on Massey: NZCETA had NA, DOC had 'Adult'",sep=";")
final_data$reconciliation_notes[which(is.na(final_data$Age_Class) & final_data$Age_class=="S" & final_data$`Age class`=="Adult")][2:3] <- "Age_class based on Massey: NZCETA had NA, DOC had 'Adult'"

# There's also one case where `Age class` has info, and Age_class does not
final_data$reconciliation_notes[which(is.na(final_data$Age_Class) & is.na(final_data$Age_class) & final_data$`Age class`=="Adult")] <- paste(final_data$reconciliation_notes[which(is.na(final_data$Age_Class) & is.na(final_data$Age_class) & final_data$`Age class`=="Adult")], "Age_class based on DOC: NZCETA and Massey had NA",sep=";")
final_data$Age_class[which(is.na(final_data$Age_Class) & is.na(final_data$Age_class) & final_data$`Age class`=="Adult")] <- "A"
# Can not jettison the other age_class coli,ms
final_data <- final_data %>% select(-Age_Class,-`Age class`)
final_data %>% group_by(Age_class) %>% count()

# Now we have Sex_observed,Sex,sex, and Genetic_Sex. Only one record is not consistent - F from Massey, Male from DOC
final_data %>% group_by(Sex_observed,Sex,sex,Genetic_Sex) %>% count()
# Because Massey has more extensive records, will keep its column, but make a note for this individual in
# the reconciliation_notes field
final_data$reconciliation_notes[which(final_data$Sex=="F" & final_data$sex=="Male")] <- "DOC database had 'Male' for this individual"
# There are a few records which have information in Sex_observed but not Sex that we'll copy over
final_data <- final_data %>% mutate(Sex=ifelse(is.na(Sex),ifelse(Sex_observed=="Female","F",ifelse(Sex_observed=="Male","M",ifelse(Sex_observed=="Unknown","U",Sex))),Sex))
# Finally, there is one that has info in sex, but not for Sex
final_data <- final_data %>% mutate(Sex=ifelse(is.na(Sex),ifelse(sex=="Female","F",Sex),Sex))
final_data <- final_data %>% select(-Sex_observed,-sex,-Genetic_Sex)
final_data %>% group_by(Sex) %>% count() 

# Now we have the `Preganant?` field
final_data %>% filter(!is.na(`Pregnant?`)) %>% as.matrix()
# only one individual has information in the column, so will capture that in the reconciliation notes
# because it is not mentioned in any of the pathology comments
final_data$reconciliation_notes[which(!is.na(final_data$`Pregnant?`))] <- paste(final_data$reconciliation_notes[which(!is.na(final_data$`Pregnant?`))],"Only row with entry in the 'Pregnant?' column: 'not pregnant (evidence of previous pregnancies)'. This column has now been deleted",sep=";")
final_data <- final_data %>% select(-`Pregnant?`)

# DOC_incident_no is completely recovered by pathology_code so an be ditched
final_data %>% group_by(Doc_incident_no,pathology_code) %>% count() %>% print(n=59)
final_data <- final_data %>% select(-Doc_incident_no)

# Only three records had Doc_animal_no. All of these were 2-5 more than the number listed in `MarMam individual ID num PK` 
# given the consistency and that this is a DOC field anyway, going to keep `MarMam individual ID num PK` 
final_data %>% group_by(Doc_animal_no,`MarMam individual ID num PK`) %>% count() %>% print(n=59)
final_data <- final_data %>% select(-Doc_animal_no)
# No replicate values of `MarMam individual ID num PK` present, so not useful for reconciliation
# Therefore going to jettison this column too
final_data %>% group_by(`MarMam individual ID num PK`) %>% count() %>% filter(n>1)
final_data <- final_data %>% select(-`MarMam individual ID num PK`)

# Necropsy_file_no_Pathology_no matches up completeley to Path_No. so can be ditched.
final_data %>% group_by(Path_No.,Necropsy_file_no_Pathology_no) %>% filter(!is.na(Necropsy_file_no_Pathology_no)) %>% count()
# However, first, 34780 needs to be copied over to Path_No. becaue not present in Path_No.
final_data <- final_data %>% mutate(Path_No.=ifelse((is.na(Path_No.) & Necropsy_file_no_Pathology_no=="34780"),"34780",Path_No.))
final_data <- final_data %>% select(-Necropsy_file_no_Pathology_no)

# Comparing mtDNA_haplotype to DNA test results Haplotype and keeping whichever columns have more information
final_data %>% group_by(mtDNA_haplotype,`DNA test results Haplotype`) %>% count()
# mtDNA_haplotype has more information, with the exception of one 'G' in `DNA test results Haplotype`
final_data$mtDNA_haplotype[which(is.na(final_data$mtDNA_haplotype) & final_data$`DNA test results Haplotype`=="G")] <- "G"
final_data <- final_data %>% select(-`DNA test results Haplotype`)

# Comparing hologenome_region to Physical population and NZ region
final_data %>% group_by(`NZ Regions`,hologenome_region,`Physical population`) %>% count()
# Only one entry where hologenome_region had data but Physical population did not
final_data$`Physical population`[which(final_data$hologenome_region=="SI East Coast" & is.na(final_data$`Physical population`))] <- "ECSI"
final_data <- final_data %>% select(-hologenome_region)
final_data %>% group_by(`NZ Regions`,`Physical population`) %>% count()
final_data %>% group_by(`Physical population`) %>% count()

# collection_date_mod - compare `Date event reported`, with `Date event observed` as the tie breaker.
# First converting `Date event reported` to preferred format 
final_data <- final_data %>% rowwise() %>% mutate(`Date event reported`=paste(unlist(strsplit(`Date event reported`,"-"))[3],unlist(strsplit(`Date event reported`,"-"))[2],unlist(strsplit(`Date event reported`,"-"))[1],sep="-")) %>% 
  mutate(`Date event reported`=gsub("-12-","-Dec-",gsub("-11-","-Nov-",gsub("-10-","-Oct-",gsub("-09-","-Sep-",gsub("-08-","-Aug-",gsub("-07-","-Jul-",gsub("-06-","-Jun-",gsub("-05-","-May-",gsub("-04-","-Apr-",gsub("-03-","-Mar-",gsub("-02-","-Feb-",gsub("-01-","-Jan-",`Date event reported`))))))))))))) 

# Then looking for discrepancies
final_data %>% filter(`Date event reported`!=collection_date_mod) %>% select(`Date event reported`,collection_date_mod,`Date event observed`)
#`Date event reported` collection_date_mod `Date event observed`
# 01-Nov-2009           3-Nov-2009          1/11/09 0:00  
# NA-NA-NA              12-May-2010         NA                   
# 16-Aug-2010           16-Mar-2010         16/08/10 0:00        
# 26-Oct-2012           26-Oct-2011         26/10/11 0:00        

final_data$reconciliation_notes[which(final_data$`Date event reported`=="01-Nov-2009" & final_data$collection_date_mod=="3-Nov-2009")] <- paste(final_data$reconciliation_notes[which(final_data$`Date event reported`=="01-Nov-2009" & final_data$collection_date_mod=="3-Nov-2009")],"NZCETA_DOC_pathology had 3-Nov-2009 for the collection date",sep=";")
final_data$`Date event reported`[which(final_data$`Date event reported`=="NA-NA-NA" & final_data$collection_date_mod=="12-May-2010")] <- "12-May-2010"
final_data$reconciliation_notes[which(final_data$`Date event reported`=="16-Aug-2010" & final_data$collection_date_mod=="16-Mar-2010")] <- "NZCETA_DOC_pathology had 16-Mar-2010 for the collection date"
final_data$reconciliation_notes[which(final_data$`Date event reported`=="26-Oct-2012" & final_data$collection_date_mod=="26-Oct-2011")] <- paste(final_data$reconciliation_notes[which(final_data$`Date event reported`=="26-Oct-2012" & final_data$collection_date_mod=="26-Oct-2011")],"NZCETA_DOC_pathology and Date event observed both had 2011, Date event reported had 2012",sep=";")
final_data$`Date event reported`[which(final_data$`Date event reported`=="26-Oct-2012" & final_data$collection_date_mod=="26-Oct-2011")] <- "26-Oct-2011"

# Checking to make sure there aren't any informative NAs in collection_date_mod
final_data %>% group_by(`Date event reported`,collection_date_mod) %>% count() %>% print(n=60)
# Nope, so all good to delete collection_date_mod
final_data <- final_data %>% select(-collection_date_mod)
# Now going to compare to other date fields
final_data %>% group_by(`Date event reported`,`Date event observed`,`Date event observed confidence`) %>% count() %>% print(n=60)
#Date event reported` `Date event observed` `Date event observed confidence`     n
# 01-Jan-9999           10/02/17 0:00         Actual                               1
# 06-Mar-2017           11/02/17 19:30        Actual                               1
# 10-Jan-2015           9/01/15 0:00          Actual                               1
# 14-May-2009           8/05/09 0:00          Actual                               1
# 27-Mar-2012           26/03/12 0:00         Estimated                            1
# 27-Mar-2017           26/03/17 0:00         Actual                               1

final_data$reconciliation_notes[which(final_data$`Date event reported`=="01-Jan-9999" & final_data$`Date event observed`=="10/02/17 0:00")] <- "Date event reported was 01-Jan-9999"
final_data$`Date event reported`[which(final_data$`Date event reported`=="01-Jan-9999" & final_data$`Date event observed`=="10/02/17 0:00")] <- "10-Feb-2017"
final_data$reconciliation_notes[which(final_data$`Date event reported`=="06-Mar-2017" & final_data$`Date event observed`=="11/02/17 19:30")] <- "Date event observed was 11/02/17 19:30"
final_data$reconciliation_notes[which(final_data$`Date event reported`=="10-Jan-2015" & final_data$`Date event observed`=="9/01/15 0:00")] <- "Date event observed was 9/01/15 0:00"
final_data$reconciliation_notes[which(final_data$`Date event reported`=="14-May-2009" & final_data$`Date event observed`=="8/05/09 0:00")] <- paste(final_data$reconciliation_notes[which(final_data$`Date event reported`=="14-May-2009" & final_data$`Date event observed`=="8/05/09 0:00")],"Date event observed was 8/05/09 0:00",sep=";")
final_data$reconciliation_notes[which(final_data$`Date event reported`=="27-Mar-2012" & final_data$`Date event observed`=="26/03/12 0:00")]  <- "Date event observed was 26/03/12 0:00"
final_data$reconciliation_notes[which(final_data$`Date event reported`=="27-Mar-2017" & final_data$`Date event observed`=="26/03/17 0:00")]  <- "Date event observed was 26/03/17 0:00"

# Double checking we caught all the funny business in reconciliation_notes
final_data %>% group_by(`Date event reported`,`Date event observed`,reconciliation_notes) %>% count() %>% print(n=60)
# Deleting the unneeded columns
final_data <- final_data %>% select(-`Date event observed`,-`Date event observed confidence`)
# Doublechecking for duplicate dates
final_data %>% group_by(`Date event reported`) %>% count() %>% filter(n>1)
# Three have 30-Dec-2018 as date - going to double check to make sure they aren't duplicate records
final_data %>% filter(`Date event reported`=="20-Dec-2018") %>% as.matrix()
# All good, entangled individuals

# Need to capture broad location for the samples that don't have this based on the Location column
# going to copy this code from reconciling_databases_1.R
final_data %>% group_by(broad_location) %>% count()
#broad_location     n
#<chr>          <int>
#  1 Auckland           2
#2 Canterbury         2
#3 Greymouth          2
#4 Hokitika           1
#5 Lyttelton          2
#6 Northland          1
#7 Otago              2
#8 Pegasus Bay        2
#9 Taranaki           1
#10 Te Waewae          1
#11 Timaru             2
#12 Waikato            4
#13 Westport           1
#14 NA                36

# Testing out the variable
final_data %>% mutate(broad_location=ifelse(grepl("Northland",Location),"Northland",ifelse(grepl("Golden Bay",Location),"Golden Bay",ifelse(grepl("Stewart Island",Location),"Stewart Island",ifelse(grepl("Cloudy Bay",Location),"Cloudy Bay",ifelse(grepl("Kapiti",Location),"Kapiti",ifelse(grepl("Waikato|Raglan",Location),"Waikato",ifelse(grepl("Dunedin",Location),"Dunedin",ifelse(grepl("Barrytown|Punakaiki",Location),"Barrytown/Punakaiki",ifelse(grepl("Okarito|Franz Josef",Location),"Okarito/Franz Josef",ifelse(grepl("Banks Peninsula|BP",Location),"Banks Peninsula",ifelse(grepl("Taranaki",Location),"Taranaki",ifelse(grepl("Farewell",Location),"Farewell Spit",ifelse(grepl("Lyttelton",Location),"Lyttelton",ifelse(grepl("Neils|Haast",Location),"Haast/Jackson Bay",ifelse(grepl("Kaikoura",Location),"Kaikoura",ifelse(grepl("Auckland",Location),"Auckland",ifelse(grepl("Akaroa",Location),"Akaroa",ifelse(grepl("Westport",Location),"Westport",ifelse(grepl("Paroa|Blaketown|Greymouth",Location),"Greymouth",ifelse(grepl("Timaru",Location),"Timaru",ifelse(grepl("Waewae",Location),"Te Waewae",ifelse(grepl("Queen Charlotte",Location),"Queen Charlotte Sound",ifelse(grepl("Hokitika",Location),"Hokitika",ifelse(grepl("Pegasus",Location),"Pegasus Bay",ifelse(grepl("Karamea|Buller",Location),"Buller",ifelse(grepl("Westland",Location),"Westland",ifelse(grepl("Marlborough",Location),"Marlborough",ifelse(grepl("Canterbury",Location),"Canterbury",ifelse(grepl("Otago",Location),"Otago",ifelse(grepl("Nelson",Location),"Nelson",ifelse(grepl("Piha",`NZ Regions`),"Auckland",ifelse(grepl("Canterbury",`NZ Regions`),"Canterbury",ifelse(grepl("Buller",`NZ Regions`),"Buller",ifelse(grepl("West Coast",`NZ Regions`),"West Coast",NA))))))))))))))))))))))))))))))))))) %>% group_by(broad_location) %>% count() %>% print(n=22)

# Still have n=8 with NA. Going to see what their broad_location looks like
final_data %>% mutate(broad_location=ifelse(grepl("Northland",Location),"Northland",ifelse(grepl("Golden Bay",Location),"Golden Bay",ifelse(grepl("Stewart Island",Location),"Stewart Island",ifelse(grepl("Cloudy Bay",Location),"Cloudy Bay",ifelse(grepl("Kapiti",Location),"Kapiti",ifelse(grepl("Waikato|Raglan",Location),"Waikato",ifelse(grepl("Dunedin",Location),"Dunedin",ifelse(grepl("Barrytown|Punakaiki",Location),"Barrytown/Punakaiki",ifelse(grepl("Okarito|Franz Josef",Location),"Okarito/Franz Josef",ifelse(grepl("Banks Peninsula|BP",Location),"Banks Peninsula",ifelse(grepl("Taranaki",Location),"Taranaki",ifelse(grepl("Farewell",Location),"Farewell Spit",ifelse(grepl("Lyttelton",Location),"Lyttelton",ifelse(grepl("Neils|Haast",Location),"Haast/Jackson Bay",ifelse(grepl("Kaikoura",Location),"Kaikoura",ifelse(grepl("Auckland",Location),"Auckland",ifelse(grepl("Akaroa",Location),"Akaroa",ifelse(grepl("Westport",Location),"Westport",ifelse(grepl("Paroa|Blaketown|Greymouth",Location),"Greymouth",ifelse(grepl("Timaru",Location),"Timaru",ifelse(grepl("Waewae",Location),"Te Waewae",ifelse(grepl("Queen Charlotte",Location),"Queen Charlotte Sound",ifelse(grepl("Hokitika",Location),"Hokitika",ifelse(grepl("Pegasus",Location),"Pegasus Bay",ifelse(grepl("Karamea|Buller",Location),"Buller",ifelse(grepl("Westland",Location),"Westland",ifelse(grepl("Marlborough",Location),"Marlborough",ifelse(grepl("Canterbury",Location),"Canterbury",ifelse(grepl("Otago",Location),"Otago",ifelse(grepl("Nelson",Location),"Nelson",ifelse((grepl("Piha",`NZ Regions`)|grepl("Piha",Location)),"Auckland",ifelse((grepl("Canterbury",`NZ Regions`)|grepl("Canterbury",Location)),"Canterbury",ifelse((grepl("Buller",`NZ Regions`)|grepl("Buller",Location)),"Buller",ifelse((grepl("West Coast",`NZ Regions`)|grepl("West Coast",Location)),"West Coast",ifelse((grepl("Southland",`NZ Regions`)|grepl("Southland",Location)),"Southland",NA)))))))))))))))))))))))))))))))))))) %>% filter(is.na(broad_location)) %>% select(Location)

# Following broad locations need a bit more info in order for automatic broad_locations to work
## A tibble: 8 x 1
#Location                                                
#<chr>                                                   
#  1 Coal Point, Kaitangata                                  
#2 between Taupata and Billy King Creek                    
#3 Warrington mid beach, near first blow out               
#4 Freshwater Basin - Milford sound                        
#5 Dolphin watch cnr of London Street and Wellington street
#6 Tihaka beach end of Colac Bay (by surfer's carpark)     
# 7 Beach ROad, Oamaru. 200m south of Golf course           
# 8 Rabbit Island 

final_data$Location[which(final_data$Location=="Coal Point, Kaitangata")] <- "Coal Point, Kaitangata, Otago"
final_data$Location[which(final_data$Location=="between Taupata and Billy King Creek")] <- "Between Taupata and Billy King Creek, Golden Bay" 
final_data$Location[which(final_data$Location=="Warrington mid beach, near first blow out")] <- "Warrington mid beach, near first blow out (north of Dunedin)"
final_data$Location[which(final_data$Location=="Freshwater Basin - Milford sound")] <- "Freshwater Basin - Milford Sound, West Coast"
final_data$Location[which(final_data$Location=="Dolphin watch cnr of London Street and Wellington street")] <- "Dolphin watch cnr of London Street and Wellington street, Picton, Marlborough"
final_data$Location[which(final_data$Location=="Tihaka beach end of Colac Bay (by surfer's carpark)")] <- "Tihaka beach end of Colac Bay (by surfer's carpark), Southland"
final_data$Location[which(final_data$Location=="Beach ROad, Oamaru. 200m south of Golf course")] <- "Beach Road, Oamaru. 200m south of Golf course, Otago"
final_data$Location[which(final_data$Location=="Rabbit Island")] <- "Rabbit Island, near Nelson"

# OK re-running the code above shows no more samples with an NA, but we need to check that they have
# been binned in the correct locations so let's first record broad_location
final_data <- final_data %>% mutate(broad_location=ifelse(grepl("Northland",Location),"Northland",ifelse(grepl("Golden Bay",Location),"Golden Bay",ifelse(grepl("Stewart Island",Location),"Stewart Island",ifelse(grepl("Cloudy Bay",Location),"Cloudy Bay",ifelse(grepl("Kapiti",Location),"Kapiti",ifelse(grepl("Waikato|Raglan",Location),"Waikato",ifelse(grepl("Dunedin",Location),"Dunedin",ifelse(grepl("Barrytown|Punakaiki",Location),"Barrytown/Punakaiki",ifelse(grepl("Okarito|Franz Josef",Location),"Okarito/Franz Josef",ifelse(grepl("Lyttelton",Location),"Lyttelton",ifelse(grepl("Banks Peninsula|BP",Location),"Banks Peninsula",ifelse(grepl("Taranaki",Location),"Taranaki",ifelse(grepl("Farewell",Location),"Farewell Spit",ifelse((grepl("Neils|Haast",Location) & !(grepl("O'Neils",Location))),"Haast/Jackson Bay",ifelse(grepl("Kaikoura",Location),"Kaikoura",ifelse(grepl("Auckland",Location),"Auckland",ifelse(grepl("Akaroa",Location),"Akaroa",ifelse(grepl("Westport",Location),"Westport",ifelse(grepl("Paroa|Blaketown|Greymouth",Location),"Greymouth",ifelse(grepl("Timaru",Location),"Timaru",ifelse(grepl("Waewae",Location),"Te Waewae",ifelse(grepl("Queen Charlotte",Location),"Queen Charlotte Sound",ifelse(grepl("Hokitika",Location),"Hokitika",ifelse(grepl("Pegasus",Location),"Pegasus Bay",ifelse(grepl("Karamea|Buller",Location),"Buller",ifelse(grepl("Westland",Location),"Westland",ifelse(grepl("Marlborough",Location),"Marlborough",ifelse(grepl("Canterbury",Location),"Canterbury",ifelse(grepl("Otago",Location),"Otago",ifelse(grepl("Nelson",Location),"Nelson",ifelse((grepl("Piha|Auckland",`NZ Regions`)|grepl("Piha|Auckland",Location)),"Auckland",ifelse((grepl("Canterbury",`NZ Regions`)|grepl("Canterbury",Location)),"Canterbury",ifelse((grepl("Buller",`NZ Regions`)|grepl("Buller",Location)),"Buller",ifelse((grepl("West Coast",`NZ Regions`)|grepl("West Coast",Location)),"West Coast",ifelse((grepl("Southland",`NZ Regions`)|grepl("Southland",Location)),"Southland",NA))))))))))))))))))))))))))))))))))))

# Now let's see what broad_locations we've ended up with
final_data %>% group_by(broad_location) %>% count() %>% print(n=23)
# A tibble: 22 x 2
# Groups:   broad_location [22]
#broad_location            n
#<chr>                 <int>
#1 Akaroa                    1
#2 Auckland                  2
#3 Canterbury               11
#4 Dunedin                   2
#5 Golden Bay                2
#6 Greymouth                 4
#7 Hokitika                  3
#8 Kaikoura                  2
#9 Lyttelton                 2
#10 Marlborough               1
#11 Nelson                    2
#12 Northland                 1
#13 Otago                     5
#14 Pegasus Bay               6
#15 Queen Charlotte Sound     1
#16 Southland                 1
#17 Taranaki                  1
#18 Te Waewae                 1
#19 Timaru                    4
#20 Waikato                   4
#21 West Coast                2
#22 Westport                  1

# We'll now check each broad_location and make sure none of the samples should be in a different
# category. If I don't say anything, they look all good!
final_data %>% filter(broad_location=="Akaroa") %>% as.matrix()
final_data %>% filter(broad_location=="Auckland") %>% as.matrix()
# Both of these Banks Peninsula samples should be Lyttelton. Tweaked code above so
# Lyttelton came before Banks Peninsula, and now there are no BP samples
final_data %>% filter(broad_location=="Banks Peninsula") %>% as.matrix()
final_data %>% filter(broad_location=="Canterbury") %>% select(Location) %>% as.matrix()
# Bunch of these should be Pegasus Bay I think. Will modify their locations so they
# get picked up by the Pegasus Bay rather than Canterbury catch-all
final_data$Location[which(final_data$Location=="North of Waipara river, Amberley Beach")] <- "North of Waipara river, Amberley Beach, Pegasus Bay"
# The following are all good - not Pegasus Bay
# "Rakaia River Mouth - 5 km north beside sea/lagoon barrier, Canterbury"
# "North of the Conway River"
# The following failed to get picked up as Kaikoura because it wasn't capitalized
final_data$Location[which(final_data$Location=="south of kaikoura")] <- "South of Kaikoura"
# The following is all good because is Canterbury:
# "3km N of Kekerengu, Canterbury"
# The following should be Waimairi Beach, not Maimairi (which doesn't exist in NZ as far as I can see)
final_data$Location[which(final_data$Location=="Maimairi Beach, 1.5-3km north of Spencer Park")] <- "Waimairi Beach, 1.5-3km north of Spencer Park, Pegasus Bay"
# Following should be Pegasus Bay
final_data$Location[which(final_data$Location=="South Brighton - near Jerrico St (I think means Jellicoe street as could not find a Jerrico St, coordinate based of this)")] <- "South Brighton - near Jerrico St (I think means Jellicoe street as could not find a Jerrico St, coordinate based of this), Pegasus Bay"
final_data$Location[which(final_data$Location=="South Brighton Surf Club")] <- "South Brighton Surf Club, Pegasus Bay"
final_data$Location[which(final_data$Location=="Upper Taylor's Mistake beach")] <- "Upper Taylor's Mistake beach, Pegasus Bay"
# Following should be Canterbury but Kaitorete also spelled incorrectly
final_data$Location[which(final_data$Location=="Kaitoreti Spit (coordinates provided are inaccurate so guessed based off map provided)")] <- "Kaitorete Spit (coordinates provided are inaccurate so guessed based off map provided)"
final_data$Location[which(final_data$Location=="Waimairi Surf Club, Christchurch")] <- "Waimairi Surf Club, Christchurch, Pegasus Bay"

# On to Dunedin etc
final_data %>% filter(broad_location=="Dunedin") %>% as.matrix()
final_data %>% filter(broad_location=="Golden Bay") %>% as.matrix()
final_data %>% filter(broad_location=="Greymouth") %>% as.matrix()
# Taking the wrong location because this is O'Neils, but Neils is assigning to Haast/Jackson Bay
# Going to tweak the code above to make sure this doesn't happen :)
# Also tweaked the code above to take "Auckland" from the NZ regions field if not in Location
final_data %>% filter(broad_location=="Haast/Jackson Bay") %>% as.matrix()
final_data %>% filter(broad_location=="Hokitika") %>% as.matrix()
final_data %>% filter(broad_location=="Kaikoura") %>% as.matrix()
# The sample that is "South of Kaikoura", is actually more near Conway Creek (e.g. generic Canterbury)
# so will change that manually here
final_data$Location[which(final_data$Location=="South of Kaikoura")] <- "Near Conway River, Canterbury"
final_data %>% filter(broad_location=="Lyttelton") %>% as.matrix()
final_data %>% filter(broad_location=="Marlborough") %>% as.matrix()
final_data %>% filter(broad_location=="Nelson") %>% as.matrix()
final_data %>% filter(broad_location=="Northland") %>% as.matrix()
final_data %>% filter(broad_location=="Otago") %>% as.matrix()
final_data %>% filter(broad_location=="Pegasus Bay") %>% as.matrix()
final_data %>% filter(broad_location=="Queen Charlotte Sound") %>% as.matrix()
final_data %>% filter(broad_location=="Southland") %>% as.matrix()
final_data %>% filter(broad_location=="Taranaki") %>% as.matrix()
final_data %>% filter(broad_location=="Te Waewae") %>% as.matrix()
final_data %>% filter(broad_location=="Timaru") %>% as.matrix()
final_data %>% filter(broad_location=="Waikato") %>% as.matrix()
final_data %>% filter(broad_location=="West Coast") %>% as.matrix()
final_data %>% filter(broad_location=="Westport") %>% as.matrix()

# After tweaking all of these calls, need to update the broad_location field
final_data <- final_data %>% mutate(broad_location=ifelse(grepl("Northland",Location),"Northland",ifelse(grepl("Golden Bay",Location),"Golden Bay",ifelse(grepl("Stewart Island",Location),"Stewart Island",ifelse(grepl("Cloudy Bay",Location),"Cloudy Bay",ifelse(grepl("Kapiti",Location),"Kapiti",ifelse(grepl("Waikato|Raglan",Location),"Waikato",ifelse(grepl("Dunedin",Location),"Dunedin",ifelse(grepl("Barrytown|Punakaiki",Location),"Barrytown/Punakaiki",ifelse(grepl("Okarito|Franz Josef",Location),"Okarito/Franz Josef",ifelse(grepl("Lyttelton",Location),"Lyttelton",ifelse(grepl("Banks Peninsula|BP",Location),"Banks Peninsula",ifelse(grepl("Taranaki",Location),"Taranaki",ifelse(grepl("Farewell",Location),"Farewell Spit",ifelse((grepl("Neils|Haast",Location) & !(grepl("O'Neils",Location))),"Haast/Jackson Bay",ifelse(grepl("Kaikoura",Location),"Kaikoura",ifelse(grepl("Auckland",Location),"Auckland",ifelse(grepl("Akaroa",Location),"Akaroa",ifelse(grepl("Westport",Location),"Westport",ifelse(grepl("Paroa|Blaketown|Greymouth",Location),"Greymouth",ifelse(grepl("Timaru",Location),"Timaru",ifelse(grepl("Waewae",Location),"Te Waewae",ifelse(grepl("Queen Charlotte",Location),"Queen Charlotte Sound",ifelse(grepl("Hokitika",Location),"Hokitika",ifelse(grepl("Pegasus",Location),"Pegasus Bay",ifelse(grepl("Karamea|Buller",Location),"Buller",ifelse(grepl("Westland",Location),"Westland",ifelse(grepl("Marlborough",Location),"Marlborough",ifelse(grepl("Canterbury",Location),"Canterbury",ifelse(grepl("Otago",Location),"Otago",ifelse(grepl("Nelson",Location),"Nelson",ifelse((grepl("Piha|Auckland",`NZ Regions`)|grepl("Piha|Auckland",Location)),"Auckland",ifelse((grepl("Canterbury",`NZ Regions`)|grepl("Canterbury",Location)),"Canterbury",ifelse((grepl("Buller",`NZ Regions`)|grepl("Buller",Location)),"Buller",ifelse((grepl("West Coast",`NZ Regions`)|grepl("West Coast",Location)),"West Coast",ifelse((grepl("Southland",`NZ Regions`)|grepl("Southland",Location)),"Southland",NA))))))))))))))))))))))))))))))))))))

# And now we do a final check through of the broad_locations
final_data %>% group_by(broad_location) %>% count() %>% print(n=23)

final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Akaroa") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Auckland") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Canterbury") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Dunedin") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Golden Bay") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Greymouth") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Hokitika") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Kaikoura") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Lyttelton") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Marlborough") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Nelson") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Northland") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Otago") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Pegasus Bay") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Queen Charlotte Sound") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Southland") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Taranaki") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Te Waewae") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Timaru") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Waikato") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="West Coast") %>% select(broad_location,mapping) 
final_data %>% rowwise() %>% mutate(mapping=paste(Latitude,Longitude,sep=",")) %>% filter(broad_location=="Westport") %>% select(broad_location,mapping) 

# Alright, all of those look good. Now on to checking the rest of the fields!
names(final_data)

# Useful information in review is available for just one sample, and this information is already available in reconciliation_notes
# so deleting this column
final_data %>% group_by(review) %>% count() 
final_data <- final_data %>% select(-review)

# keep Path_No. - assume useful for Wendi tracking sample
final_data %>% group_by(Path_No.) %>% count() %>% print(n=59) 
# one sample has an NA so we'll double-check that
final_data %>% filter(is.na(Path_No.)) %>% as.matrix()
# All good, just happens to not have data available from the pathology databases

# comparing `Maui?` to subspecies designation and then deleting
final_data %>% group_by(`Maui?`,Common_name) %>% count()
# There's two samples that do not have a 'y' for Maui?, but for which other information suggests they are Māui
final_data %>% filter(Common_name=="Maui dolphin" & is.na(`Maui?`)) %>% as.matrix()
# All data suggests they are Māui so going to ditch this column
final_data <- final_data %>% select(-`Maui?`)

# Keep COD_category (coarse groupings for disease vs non-disease contrasts)
final_data %>% group_by(COD_category) %>% count() 
# There are two records without COD_category that I'm going to double-check (likely the Bector's we added in to dataset)
final_data %>% filter(is.na(COD_category)) %>% as.matrix()
# Yes, all good - these are samples added in as Bector's (CHE11NZ06) or re-sampled Māui (CHENI092)

# Keep Cause_of_death.y - finer scale cause of death results from Wendi (should be considered more accurate than info from DOC's database)
final_data %>% group_by(Cause_of_death.y) %>% count() %>% print(n=30)
# There are two records without COD_category that I'm going to double-check (likely  Bector's (CHE11NZ06) and re-sampled Māui (CHENI092))
final_data %>% filter(is.na(Cause_of_death.y)) %>% as.matrix()
# Confirmed

# Keep `Death_by_pathogen?` as gives Wendi's strength of conviction about whether the cause of death was disease or not
final_data %>% group_by(COD_category,`Death_by_pathogen?`) %>% count()

# Keep Notes - gives additional context to pathology results
final_data %>% group_by(Notes) %>% count() # keep    

# Reconciling `Animal ID` to pathology_code and Date event reported
final_data %>% group_by(`Animal ID`,pathology_code,`Date event reported`) %>% count() %>% print(n=59)
# one sample does not have an Animal ID - going to double check this
final_data %>% filter(is.na(`Animal ID`)) %>% as.matrix()
# Not in DOC database, but is in UoA and Massey. All other columns match, so can delete Animal ID
final_data <- final_data %>% select(-`Animal ID`)

# Keep Observation Type - have double-checked it is consistent with COD_category
final_data %>% group_by(COD_category,`Observation type`) %>% count() 

# Keep Gear Type - have checked to make sure it is consistent with COD_category
final_data %>% group_by(COD_category,`Gear type`) %>% count()
# One sample has an NA for Gear Type despite being known bycatch - double checking this
final_data %>% filter(COD_category=="Known bycatch" & is.na(`Gear type`)) %>% as.matrix()
# Same individual (H195) as before, where not present in DOC database

# We'e already checked latitude/longitude, but let's make sure these duplicates are nothing to worry about
final_data %>% group_by(Latitude,Longitude) %>% count() %>% filter(n>1) %>% as.matrix()
final_data %>% mutate(Latitude=as.character(Latitude), Longitude=as.character(Longitude)) %>% filter(Latitude=="-43.358914" & Longitude=="172.831687") %>% as.matrix()
# All good - these guys were all caught in the same net, which is why they have the same lat/long (H278-H280)                                            

# We've already already checked Location but let's double check any duplicates
final_data %>% group_by(Location) %>% count() %>% filter(n>1)  
# Three with the same location - probably the same as above
final_data %>% filter(Location=="Pegasus Bay, Christchurch") %>% as.matrix()
# Confirmed

# Reconciling Area descriptor to other geographic fields before deleting
final_data %>% group_by(broad_location,`Area descriptor`) %>% count() %>% print(n=25)
# All consistent, can delete this variable
final_data <- final_data %>% select(-`Area descriptor`)

# Double checking NZ Regions against broad_location
final_data %>% group_by(broad_location,`NZ Regions`) %>% count() %>% print(n=25)        
# One record has Nelson for broad_location, and Tasman for NZ Region
# One also has West Coast for broad_location and Southland for NZ Region
final_data %>% filter(broad_location=="Nelson" & `NZ Regions`=="Tasman") %>% as.matrix() # All good, near Nelson
final_data %>% filter(broad_location=="West Coast" & `NZ Regions`=="Southland") %>% as.matrix() # All good, technically Southland, but on the West Coast
# Can now remove NZ Regions
final_data <- final_data %>% select(-`NZ Regions`)

# Double check physical population against other location fields
final_data %>% filter(`Physical population`=="WCNI") %>% group_by(broad_location) %>% count() %>% print(n=22) # 8 from WCNI
final_data %>% filter(`Physical population`=="WCSI") %>% group_by(broad_location) %>% count() %>% print(n=22) # 10 from WCSI
final_data %>% filter(`Physical population`=="SCSI") %>% group_by(broad_location) %>% count() %>% print(n=22) # 2 from SCSI
final_data %>% filter(`Physical population`=="ECSI") %>% group_by(broad_location) %>% count() %>% print(n=22) # 33 from ECSI
final_data %>% filter(`Physical population`=="NCSI") %>% group_by(broad_location) %>% count() %>% print(n=22) # 6 from NCSI
# All are consistent, but let's double check the Marlborough/NCSI sample just to make sure
final_data %>% filter(broad_location=="Marlborough") %>% as.matrix() # Yup, all good, from Picton

# Keep these fields - double check consistency after exporting file
final_data %>% group_by(Description) %>% count() 
final_data %>% group_by(`Event remarks`) %>% count() 

# Differs from Wendi's more updated COD_category, so will retain for records (but going with Wendi's calls)
final_data %>% group_by(COD_category,`Necropsy results`,`Necropsy results details`) %>% count()  %>% print(n=57)

# Keeping Necropsy results details as gives more information on animals
final_data %>% group_by(`Necropsy results details`) %>% count() 

# Keep reconciliation_notes as gives context to reconciliation process
final_data %>% group_by(reconciliation_notes) %>% count()

# Keep - this is actually "animal id" and will need to be renamed at the end.
final_data %>% group_by(pathology_code) %>% count() 

######################################################################################################################################
#### 7. Renaming columns to make sure they are uniform and informative, re-orderig, and exporting sheet for further row by row QC ####
######################################################################################################################################

names(final_data)[1] # Common_name: Common subspecies name of sample
names(final_data)[2] <- "UoA_sample_code" # UoA_sample_code: original University of Auckland sample code from NZCETA database
names(final_data)[3] <- "Standardized_UoA_code" # Standardized_UoA_code: Univeristy of Auckland sample code compared to Massey_DOC database, and standardized (eg. all caps) for reconciling databases
names(final_data)[4] # "Species_subspecies": Scientific name of sample
names(final_data)[5] # mtDNA_haplotype: Mitochondrial control region haplotype of sample (if known). G = Māui, other haplotypes, Hector's
names(final_data)[6] <- "Reconciliation_notes" # Reconciliation_notes: notes made during reconciling of different datasets (e.g. discrepancies etc), including the previous step of reconciling NZCETA and DOC_Massey sheets (so some comments may have been made irrelevant after pulling in additional data)
names(final_data)[7] <- "Animal_ID" # Animal_ID: DOC Animal ID (H-code)
names(final_data)[8] <- "Broad_geographic_location" # Broad_geographic_location: Broad geographic location based on 'Location' field
names(final_data)[9] # Path_No.: Pathology report number from Massey necropsy
names(final_data)[10] # Age_class: Broad age class of individuals (A = Adult, S = Subadult, J = Juvenile, including neonates)
names(final_data)[11] # Sex: F (Female), M (Male), or U (Unknown)
names(final_data)[12] # COD_category: Generic cause of death for samples from Wendi Roe (Massey)
names(final_data)[13] <- "Detailed_cause_of_death" # Detailed_cause_of_death: More specific cause of death for samples from Wendi Roe (Massey) 
names(final_data)[14] <- "Death_by_pathogen" # Death_by_pathogen: Confidence in whether death of individual was caused by a pathogen from Wendi Roe (Massey)
names(final_data)[15] <- "Cause_of_death_notes" # Cause_of_death_notes: Additional comments on cause of death from Wendi Roe (Massey)
names(final_data)[16] <- "Date_event_reported" # Date_event_reported: Date of stranding event - discrepancies with other date fields noted in Reconciliation_notes
names(final_data)[17] <- "Sampling_event" # Sampling_event: Nature of stranding event/sampling event for sample (e.g. beachcast, entangled)
names(final_data)[18] <- "Fisheries_gear_type" # Fisheries_gear_type: For bycaught animals, type of fishing gear involved (Not Applicable for non-bycaught individuals)
names(final_data)[19] # Latitude: Latitude of location where sample recovered
names(final_data)[20] # Longitude: Longitude of location where sample recovered
names(final_data)[21] <- "Specific_geographic_location" # Specific_geographic_location: Detailed text description of location sample was collected from
names(final_data)[22] <- "Physical_population" # Physical_population: Regional area sample was collected from (WCNI, WCSI, SCSI, ECSI, NCSI)
names(final_data)[23] <- "Description_of_collection_event" # Description_of_collection_event: Details about the stranding/bycatch event
names(final_data)[24] <- "Additional_remarks" # Additional_remarks: Additional details about the stranding/bycatch event
names(final_data)[25] <- "Length_cm" # Length_cm: Length of sampled dolphin in cm
names(final_data)[26] <- "Necropsy_results" # Necropsy_results: DOC's record of necropsy_results. Compare to COD_category from Wendi Roe (Massey) for up to date details.
names(final_data)[27] <- "Necropsy_results_details" # Necropsy_results_details: Detailed necropsy results from DOC's database.

names(final_data)

# Reordering columns to give more coherent layout
final_data <- final_data %>% select(Standardized_UoA_code,UoA_sample_code,Animal_ID,Common_name,Species_subspecies,Date_event_reported, Latitude, Longitude, Specific_geographic_location,Broad_geographic_location, Physical_population, Age_class, Sex, Length_cm, mtDNA_haplotype, Sampling_event, Fisheries_gear_type, Description_of_collection_event, Additional_remarks, Necropsy_results, Necropsy_results_details, Path_No., COD_category, Detailed_cause_of_death, Death_by_pathogen, Cause_of_death_notes, Reconciliation_notes)

# writing out file 
write_delim(final_data,"hologenome_project_samples.txt",col_names = TRUE,quote_escape = FALSE,delim = "\t")

# Generating summaries of the dataset
final_data %>% group_by(Common_name,Physical_population) %>% count()
final_data %>% filter(Physical_population=="ECSI") %>% group_by(Age_class,Sex,Death_by_pathogen) %>% count() %>% spread(Death_by_pathogen,n)
final_data %>% filter(Physical_population=="NCSI") %>% group_by(Age_class,Sex,Death_by_pathogen) %>% count() %>% spread(Death_by_pathogen,n)
final_data %>% filter(Physical_population=="WCSI") %>% group_by(Age_class,Sex,Death_by_pathogen) %>% count() %>% spread(Death_by_pathogen,n)
final_data %>% filter(Physical_population=="WCNI") %>% group_by(Age_class,Sex,Death_by_pathogen) %>% count() %>% spread(Death_by_pathogen,n)


