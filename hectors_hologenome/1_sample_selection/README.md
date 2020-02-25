## 1. Sample selection
For the high-quality reference genomes, we included single representatives from the Māui and Hector's subspecies that had adequate tissue available (for 10x and Minion construction), and that we undertook research consultation with Māori for.

For the larger low coverage genome resequencing/microbiome characterization, we wanted to prioritize a selection of dolphins that were confirmed to have died due to disease OR due to other sudden mortality events to act as controls (e.g. ship strike, predation, fisheries), and include in that selection at least some dolphins that had also been biopsied while alive, preferably more than once (to look at how the microbiome might shift after death, and also from sampling event to sampling event while alive). The final sample selection criterion was maximizing geographic coverage to evaluate differences in hologenomic patterns between subspecies, and between geographic areas within subspecies. [After potential resequencing Hector's individuals were identified](https://github.com/laninsky/project_logs/blob/master/hectors_hologenome/sample_selection.R), we carried out research consultation with Māori at the paptipu rūnaka/hapū level (and in some cases at the iwi level) and only included samples where no concerns were raised about including them in our study.

Original files used in this process are in the /original_files folder:
* `hectors_mauis_incidents_30Aug2019.txt`: DOC's master list of samples, which will be compared to the final list we whittle down
 https://www.doc.govt.nz/our-work/hectors-and-maui-dolphin-incident-database/
* `Pathology_data.txt`: List of cases where a diagnosis of cause of death was possible.
* `Maui_recapture.txt`: Reconciled biopsy and stranding information for Māui dolphins.

All original data sets were delivered in excel format. I saved all of them as tab delimited for importing into R, in some cases after modifying them to make them more appropriate as flat files e.g. adding column with information given as colour coding in original excel file.

Output files used in this process are in the /output_files folder:
* `NZCETA_DOC_joined.txt`: The result of reconciling DOC_Massey_database.txt (IDs sent to # DOC and Massey to inform their databases), and  NZCETA_archive.txt (NZCeTA Māui/Hector's # tissue info, not 100% reconciled) in reconciling_databases_1.R




