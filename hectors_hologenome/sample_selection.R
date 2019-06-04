#### 1. Description of data sets that were reconciled across ####
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

#### 2. Loading libraries, reading in files, tibble descriptions ####
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

#### 3. Massaging data so that there are columns that line up ####
NZCETA_archive <- NZCETA_archive %>% rowwise() %>% mutate(pathology_code=gsub("-.*","",gsub(").*","",gsub("/.*","",unlist(strsplit(Code,"\\(H"))[2]))))
NZCETA_archive <- NZCETA_archive %>% mutate(pathology_code=ifelse(nchar(pathology_code)==2,
                                                paste("H0",pathology_code,sep=""),
                                                paste("H",pathology_code,sep="")))
