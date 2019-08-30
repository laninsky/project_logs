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
# Will include them in the final set of samples

################################################################################################
#### 5. Whittling down samples to just those with H-codes, then combining with DOC database ####
################################################################################################

# Excluding samples without pathology_codes (because pathology_code is what we'll match them up to the DOC_doublecheck based on
NZCETA_DOC_pathology <- NZCETA_DOC_pathology %>% filter(!is.na(pathology_code))

# Have to massage DOC_doublecheck's 'Animal ID' into the right format to match up to pathology_code
# Then filtering only to include samples that have an H-code
DOC_doublecheck <- DOC_doublecheck %>% mutate(pathology_code=ifelse(grepl("H",`Animal ID`),gsub("-.*","",gsub("\\/","-",gsub(".*H","H",`Animal ID`))),NA)) %>% mutate(pathology_code=ifelse(nchar(pathology_code)<=3,ifelse(nchar(pathology_code)==3,gsub("H","H0",pathology_code),ifelse(nchar(pathology_code)==2,gsub("H","H00",pathology_code),pathology_code)),pathology_code)) %>% filter(!is.na(pathology_code))

# OK, let's smush them together!
final_data <- left_join(NZCETA_DOC_pathology,DOC_doublecheck,by="pathology_code")

# Filter down to Wendi's confident necropsy calls:
final_data <- final_data %>% filter(!is.na(COD_category))

# Time to reconcile some columns again :)
names(final_data)

# Starting off with common_name
final_data %>% group_by(Common_name,`Vernacular name`) %>% count()
# Everything else either matches up, or provides information where there is none in either database
# Two records have an NA for both databases, so we'll take a look at them, but probably need to filter them out
final_data %>% filter(is.na(Common_name) & is.na(`Vernacular name`)) %>% as.matrix()
# These two records are for a probably bycatch, and a fetus death due to brecella (suspect the foetus of the Māui we are genome sequencing)
# Will filter them out seeing as they don't really have enough info to be going on with
final_data <- final_data %>% filter(!(is.na(Common_name) & is.na(`Vernacular name`)))
# Now going to copy across where Common_name has NAs and ditch `Vernacular name`
final_data <- final_data %>% mutate(Common_name=ifelse((is.na(Common_name)|Common_name=="Uncertain"),`Vernacular name`,Common_name)) %>% select(-`Vernacular name`)

# Actually, might just start jettisoning other fields that aren't going to be directly relevant to us too
# As well as ones that now just have NAs in them!
final_data <- final_data %>% select(-Checked,-What,-Info_sent,-Info_requested,-Other_info,-Archiving_info,-MONZ_code,-CETOS_code)
final_data <- final_data %>% select(-Cause_of_death.x,-Extracted,-reconciled_alternate_code)

# Length
final_data %>% mutate(Length_m=(`Length cm`/100)) %>% group_by(TL_m,Length_m) %>% count() %>% print(n=51)
# All records were within about 10cm, so going to ditch TL_m column and just retain Length_m
final_data <- final_data %>% select(-TL_m)

# LatitudeS_Northing and LongitudeE_Easting vs Latitude Longitude
final_data %>% group_by(LatitudeS_Northing,LongitudeE_Easting,Latitude,Longitude) %>% count() %>% print(n=55) %>% as.matrix()
# The two with lat/longs in LatitudeS_Northing and LongitudeE_Easting matched up (rest had mapgrid), so am going to ditch LatitudeS_Northing and LongitudeE_Easting
final_data <- final_data %>% select(-LatitudeS_Northing,-LongitudeE_Easting)
final_data <- final_data %>% select(-Franz_source_code)
final_data <- final_data %>% select(-Massey_Code)

# Necropsy_file_no_Pathology_no matches up completeley to Path_No. so can be ditched
final_data <- final_data %>% select(-Necropsy_file_no_Pathology_no)

# DOC_incident_no is completely recovered by pathology_code so an be ditched
final_data <- final_data %>% select(-Doc_incident_no)

# Only two records had Doc_animal_no. both of these wre 5 mmore than the number listed in `MarMam individual ID num PK` 
# given the consistentcy and that this is a DOC field anyway, going to keep `MarMam individual ID num PK` 
final_data <- final_data %>% select(-Doc_animal_no)

# Now we have Sex_observed,Sex,sex. Only one record is not consistent - F from Massey, Male from DOC
# Because Massey has more extensive records, will keep its column, but make a note for this individual in
# the reconciliation_notes field
final_data$reconciliation_notes[which(final_data$Sex=="F" & final_data$sex=="Male")] <- "DOC database had 'Male' for this individual"
final_data <- final_data %>% select(-Sex_observed,-sex)


UP TO HERE DOUBLE CHECK MASSEY MAUI FIELD VERSUS PREVIOUS CALL
ALSO NEED TO UPDATE SPECIES_SUBSPECIES BASED ON COMMON NAME



final_data %>% filter(Common_name=="Maui dolphin") %>% as.matrix()

# Some we should keep until after reconciling:
Date_received, Location.x,Region, Ocean, Cause_of_death.x, Tissue_type, Collected_By,DOC_Agency


DOC_doublecheck$pathology_code[184]==NZCETA_DOC_pathology$pathology_code[100]
H188
H201
H244
H273

NZCETA_DOC_pathology %>% group_by(COD_category,Cause_of_death.y,`Death_by_pathogen?`) %>% count() %>% print(n=32)

masseycodes <- as.matrix(NZCETA_DOC_pathology %>% select(pathology_code) %>% unique())

checktheseguysout <- which(!masseycodes %in% totalcodes)

NZCETA_DOC_pathology[checktheseguysout,] %>% as.matrix()



# Cloudy Bay 2011, 2012: Biopsy samples for comparison
# Kaikoura 2014, 2015: Biopsy samples for comparison
# Bector's: CheNI10‐03, CheNI10‐24, Che11NZ06, Che12NZ02
# South of North Island Che05NZ20, Che09WH01


# Population broad_location based on sample code where this is known e.g. BP = Banks Peninsula, KK = Kaikoura, TI = Timaru

# EXCLUDE "Cephalorhynchus hectori ?"

# PRIORITIZE BECTORS (GIVES US AN IDEA OF ENVIRONMENTAL VS INHERITED) (AND HECTORS IN RANGE OF MAUI E.G. CHE12NZ02 - these are bectors -- There were two samples that have been classified as Hector's, but sampled in the range of the Māui (CHE11NZ06 and CHE12NZ02)
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

# Neils Beach Jackson Bay, South Westland could be interesting samples ( four co-caught dolphins)

# Would the parent/fetus pairs be of any interest?

# Museum samples to look at genomic loss of diversity?

#DOC_Massey_database ($DOC_incident_no) should be able to be massaged to match up to Pathology_data ($H_no.)

#Maui_recapture should be able to be matched up to DOC_Massey_database ($UoA_Code) and NZCETA_archive ($Code) based on $Indiv_ID


