# 1. Loading in required libraries
library(tidyverse)
library(gtable)
library(grid)
#install.packages("DescTools")
library(DescTools)
library(ggrepel)

# 2. Setwd
setwd("chickadee/output/")

# 3. Reading in additional files for context on bgc results
genetic_map <- read_delim("../data/bgc/genetic_map.txt",col_names = FALSE,delim=" ")
no_of_loci <- dim(genetic_map)[1]
bgc_chrom_key <- read_tsv("../data/bgc/bgc_chrom_key.txt",col_names = TRUE)
names(genetic_map) <- c("SNP","chromosome","kbp_pos")

actual_chroms <- rep(NA,dim(genetic_map)[1])
actual_scaffolds <- rep(NA,dim(genetic_map)[1])
for (i in 1:dim(bgc_chrom_key)[1]) {
  actual_chroms[which(genetic_map$chromosome==bgc_chrom_key$bgc_chrom[i])] <- bgc_chrom_key$chrom[i]
  actual_scaffolds[which(genetic_map$chromosome==bgc_chrom_key$bgc_chrom[i])] <- bgc_chrom_key$scaffold[i]
}

genetic_map$chromosome <- actual_chroms
genetic_map <- as_tibble(cbind(genetic_map,actual_scaffolds))
names(genetic_map) <- c("SNP","chromosome","kbp_pos","scaffolds")
genetic_map <- genetic_map %>%   mutate_at(vars(scaffolds),funs(as.character))

# 4. Reading in raw bgc files to look at stationarity
likelihood <- read_tsv("../data/bgc/run1_output/OF_LnL_output.txt",col_names = FALSE)
likelihood <- likelihood %>% mutate(state=row_number())
names(likelihood)[1] <- "LnL"

likelihood_run2 <- read_tsv("../data/bgc/run2_output/OF_LnL_output.txt",col_names = FALSE)
likelihood_run2 <- likelihood_run2 %>% mutate(state=row_number())
names(likelihood_run2)[1] <- "LnL"

# Checking stationarity for likelihood
ggplot() + geom_point(likelihood,mapping=aes(x=state,y=LnL)) + geom_point(likelihood_run2,mapping=aes(x=state,y=LnL),color="red")

# Reading in the raw alpha data
# Alpha files unavailable in github repository due to size limits
# instead, they will be available in the dryad repository
alpha <-  read_delim("../../../bgc_run1_output_large_files/OF_alpha_output.txt",col_names = FALSE,delim = ",")
alpha0 <- alpha %>% filter(X1==0)
alpha0 <- alpha0 %>% mutate(state=row_number())
names(alpha0) <- c("locus","alpha","state")

alphaz <- alpha %>% filter(X1==max(X1))
alphaz <- alphaz %>% mutate(state=row_number())
names(alphaz) <- c("locus","alpha","state")

alpha_run2 <-  read_delim("../../../bgc_run2_output_large_files/OF_alpha_output.txt",col_names = FALSE,delim = ",")
alpha0_run2 <- alpha_run2 %>% filter(X1==0)
alpha0_run2 <- alpha0_run2 %>% mutate(state=row_number())
names(alpha0_run2) <- c("locus","alpha","state")

alphaz_run2 <- alpha_run2 %>% filter(X1==max(X1))
alphaz_run2 <- alphaz_run2 %>% mutate(state=row_number())
names(alphaz_run2) <- c("locus","alpha","state")

# Checking stationarity for alpha0 (first locus) and alphaz (last locus)
ggplot() + geom_line(alpha0,mapping=aes(x=state,y=alpha)) + geom_line(alpha0_run2,mapping=aes(x=state,y=alpha),color="red")
  
ggplot() + geom_line(alphaz,mapping=aes(x=state,y=alpha)) + geom_line(alphaz_run2,mapping=aes(x=state,y=alpha),color="red")

ggplot() + geom_point(mapping=aes(x=likelihood$LnL,y=alpha0$alpha)) + geom_point(mapping=aes(x=likelihood_run2$LnL,y=alpha0_run2$alpha),color="red") 

ggplot() + geom_point(mapping=aes(x=likelihood$LnL,y=alphaz$alpha)) + geom_point(mapping=aes(x=likelihood_run2$LnL,y=alphaz_run2$alpha),color="red") 

# Reading in the raw beta data
# Beta files unavailable in github repository due to size limits
# instead, they will be available in the dryad repository
beta <-  read_delim("../../../bgc_run1_output_large_files/OF_beta_output.txt",col_names = FALSE,delim = ",")
beta0 <- beta %>% filter(X1==0)
beta0 <- beta0 %>% mutate(state=row_number())
names(beta0) <- c("locus","beta","state")

betaz <- beta %>% filter(X1==max(X1))
betaz <- betaz %>% mutate(state=row_number())
names(betaz) <- c("locus","beta","state")

beta_run2 <-  read_delim("../../../bgc_run2_output_large_files/OF_beta_output.txt",col_names = FALSE,delim = ",")
beta0_run2 <- beta_run2 %>% filter(X1==0)
beta0_run2 <- beta0_run2 %>% mutate(state=row_number())
names(beta0_run2) <- c("locus","beta","state")

betaz_run2 <- beta_run2 %>% filter(X1==max(X1))
betaz_run2 <- betaz_run2 %>% mutate(state=row_number())
names(betaz_run2) <- c("locus","beta","state")

# Checking stationarity for beta0 (first locus) and betaz (last locus)
ggplot() + geom_line(beta0,mapping=aes(x=state,y=beta)) + geom_line(beta0_run2,mapping=aes(x=state,y=beta),color="red")

ggplot() + geom_line(betaz,mapping=aes(x=state,y=beta)) + geom_line(betaz_run2,mapping=aes(x=state,y=beta),color="red")

ggplot() + geom_point(mapping=aes(x=likelihood$LnL,y=beta0$beta)) + geom_point(mapping=aes(x=likelihood_run2$LnL,y=beta0_run2$beta),color="red") 

ggplot() + geom_point(mapping=aes(x=likelihood$LnL,y=betaz$beta)) + geom_point(mapping=aes(x=likelihood_run2$LnL,y=betaz_run2$beta),color="red") 

# 4. Reading in summmarized results to first investigate convergence before doing any other analyses
alphaest <- read_delim("../data/bgc/run1_output/alphaest.txt",col_names = TRUE,delim = ",")
names(alphaest) <- c("alpha_loci", "alpha_mean", "alpha_median", "alpha_95_LB", "alpha_95_UB")
betaest <- read_delim("../data/bgc/run1_output/betaest.txt",col_names = TRUE,delim = ",")
names(betaest) <- c("beta_loci", "beta_mean", "beta_median", "beta_95_LB", "beta_95_UB")

alphaest_run2 <- read_delim("../data/bgc/run2_output/alphaest.txt",col_names = TRUE,delim = ",")
names(alphaest_run2) <- c("alpha_loci", "alpha_mean", "alpha_median", "alpha_95_LB", "alpha_95_UB")
betaest_run2 <- read_delim("../data/bgc/run2_output/betaest.txt",col_names = TRUE,delim = ",")
names(betaest_run2) <- c("beta_loci", "beta_mean", "beta_median", "beta_95_LB", "beta_95_UB")

# Testing if the medians and means of the alpha values are significantly different between the runs
t.test(alphaest$alpha_median,alphaest_run2$alpha_median,paired=TRUE)
t.test(alphaest$alpha_mean,alphaest_run2$alpha_mean,paired=TRUE)

# Testing if the medians and means of the beta values are significantly different between the runs
t.test(betaest$beta_median,betaest_run2$beta_median,paired=TRUE)
t.test(betaest$beta_mean,betaest_run2$beta_mean,paired=TRUE)

# Creating some combined tibbles so we can plot 
combined <- bind_cols(genetic_map,alphaest,betaest) %>% select(-SNP,-alpha_loci,-beta_loci)
combined_run2 <- bind_cols(genetic_map,alphaest_run2,betaest_run2) %>% select(-SNP,-alpha_loci,-beta_loci)

# Confirming the distribution of alpha and beta values looks similar between the runs
ggplot() + geom_point(combined,mapping=aes(x=alpha_median,y=beta_median)) +
  geom_point(combined_run2,mapping=aes(x=alpha_median,y=beta_median),color="red")

# Confirming positive correlation between the runs for alpha
ggplot() + geom_point(mapping=aes(x=combined$alpha_median,y=combined_run2$alpha_median))
cor(combined$alpha_median,combined_run2$alpha_median)

# Confirming positive correlation between the runs for beta
ggplot() + geom_point(mapping=aes(x=combined$beta_median,y=combined_run2$beta_median))
cor(combined$beta_median,combined_run2$beta_median)  

# 5. Finding outlier loci
combined <- combined %>% mutate(alpha=ifelse((alpha_95_LB < 0 & alpha_95_UB > 0),"Not_outlier",
                                             ifelse((alpha_95_UB < 0),"Neg","Pos")))

combined <- combined %>% mutate(beta=ifelse((beta_95_LB < 0 & beta_95_UB > 0),"Not_outlier",
                                             ifelse((beta_95_UB < 0),"Neg","Pos")))

combined <- combined %>% mutate(alpha_beta=paste(alpha,beta,sep="+"))

combined <- combined %>% mutate(locus_row=row_number())

# 5. Plotting ancestry against hybrid index
hn_values <- seq(0,1,0.01)

slope <- function(hn,a,b) {
  theta <- hn + (2 * (hn - hn^2) * (a + (b * ((2 * hn) - 1))))
  if (theta < 0) {
    theta <- 0
  } else {
    if (theta > 1) {
      theta <- 1
    }
  }
  return(theta)
}

locus_trajectories <- matrix(NA,ncol=3,nrow=(length(hn_values)*dim(combined)[1]))

locus_trajectories[,1] <- rep(hn_values,(dim(combined)[1]))

for (j in 1:(dim(combined)[1])) {
  prob_ancestry <- rep(NA,length(hn_values))
  a <- combined$alpha_median[j]
  b <- combined$beta_median[j]
  
  for (i in 1:length(hn_values)) {
    prob_ancestry[i] <- slope(hn_values[i],a,b)
  }
  local_max <- NULL
  local_min <- NULL
  for (i in 2:(length(prob_ancestry)-1)) {
    if (prob_ancestry[i] > prob_ancestry[i-1] & prob_ancestry[i] > min(prob_ancestry[i:length(prob_ancestry)]) ) {
      local_max <- i
    } 
    if (prob_ancestry[i] < prob_ancestry[i-1] & prob_ancestry[i] < max(prob_ancestry[i:length(prob_ancestry)]) ) {
      local_min <- i
    } 
  }
  if (!is.null(local_max) & !is.null(local_min)) {
    replacementvalue <- 0.5
    startline <- which(prob_ancestry>0.5)[1]
    endline <- which(prob_ancestry<0.5)[length(which(prob_ancestry<0.5))]
    if (startline > endline) {
      tempstart <- max(prob_ancestry[1:50])
      tempend <- min(prob_ancestry[51:101])
      replacementvalue <- (tempstart+tempend)/2
      startline <- which(prob_ancestry>replacementvalue)[1]
      endline <- which(prob_ancestry<replacementvalue)[length(which(prob_ancestry<replacementvalue))]
    }
    prob_ancestry[startline:endline] <- replacementvalue
  }
  locus_trajectories[((j*length(hn_values))-length(hn_values)+1):(j*length(hn_values)),2] <- prob_ancestry
  locus_trajectories[((j*length(hn_values))-length(hn_values)+1):(j*length(hn_values)),3] <- j
}  

locus_trajectories <- as_tibble(locus_trajectories)
names(locus_trajectories) <- c("hybrid_index","ancestry","locus_row")
combined <- full_join(locus_trajectories,combined)

Not_outlier_Not_outlier <- combined %>% filter(alpha_beta=="Not_outlier+Not_outlier")
Pos_Not_outlier <- combined %>% filter(alpha_beta=="Pos+Not_outlier")
Neg_Not_outlier <- combined %>% filter(alpha_beta=="Neg+Not_outlier")
Not_outlier_Pos <- combined %>% filter(alpha_beta=="Not_outlier+Pos")
Not_outlier_Neg <- combined %>% filter(alpha_beta=="Not_outlier+Neg")
Neg_Pos <- combined %>% filter(alpha_beta=="Neg+Pos")
Pos_Neg <- combined %>% filter(alpha_beta=="Pos+Neg")
Neg_Neg <- combined %>% filter(alpha_beta=="Neg+Neg")
Pos_Pos <- combined %>% filter(alpha_beta=="Pos+Pos")

# Making a two by two table for testing sig difference in proportions by chromosome further down
prob_matrix <- rbind(c(length(unique(Pos_Pos$locus_row)),length(unique(Neg_Pos$locus_row)),length(unique(Not_outlier_Pos$locus_row))),c(length(unique(Pos_Neg$locus_row)),length(unique(Neg_Neg$locus_row)),length(unique(Not_outlier_Neg$locus_row))),c(length(unique(Pos_Not_outlier$locus_row)),length(unique(Neg_Not_outlier$locus_row)),length(unique(Not_outlier_Not_outlier$locus_row))))

# Summarizing the counts in each category. Note, no Neg+Neg or Pos+Pos observed
combined %>% filter(hybrid_index==0) %>% group_by(alpha_beta) %>% count()

ggplot() + 
  geom_line(data=Not_outlier_Not_outlier,aes(group=locus_row,x=hybrid_index,y=ancestry), color="grey50",size=1,alpha=0.5) + 
  geom_line(data=Not_outlier_Pos,aes(group=locus_row,x=hybrid_index,y=ancestry), color="red",size=1) + 
  geom_line(data=Not_outlier_Neg,aes(group=locus_row,x=hybrid_index,y=ancestry), color="red4",size=1) + 
  geom_line(data=Neg_Pos,aes(group=locus_row,x=hybrid_index,y=ancestry), color="purple",size=1) + 
  geom_line(data=Pos_Neg,aes(group=locus_row,x=hybrid_index,y=ancestry), color="purple4",size=1) + 
  geom_line(data=Pos_Not_outlier,aes(group=locus_row,x=hybrid_index,y=ancestry), color="steelblue2",size=1) + 
  geom_line(data=Neg_Not_outlier,aes(group=locus_row,x=hybrid_index,y=ancestry), color="blue",size=1) + 
  geom_line(data=Pos_Pos,aes(group=locus_row,x=hybrid_index,y=ancestry), color="magenta",size=1) + 
  theme_bw(base_size=33) +
  theme(legend.position = "none")  +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.title=element_text(size=40,face="bold")) +
  scale_y_continuous(name="Locus specific ancestry") +
  scale_x_continuous(name="Hybrid index") 

ggsave(paste("FigS10A.png",sep=""),width=400,height=200,units="mm")

# Light blue: positive α not overlapping with zero (non-significant β)
# Increase in the probability of black-capped ancestry from the base
# probability (predicted by hybrid index)

# Dark blue: negative α not overlapping with zero (non-significant β)
# Increase in the probability of Carolina ancestry from the base
# probability (predicted by hybrid index)

# Light red: positive β not overlapping with zero (non-significant α)
# Excess ancestry‐based linkage disequilibrium (e.g. Carolina locus-
# specific ancestry restricted to Carolina genomic background).

# Dark red: negative β not overlapping with zero (non-significant α)
# Locus-specific ancestry is less strongly associated with genomic background
# than in other loci.

# Light purple: significantly negative α and positive β. An increase in the 
# probability of Carolina ancestry, while also that that locus background 
# is strongly associated with genomic background

# Dark purple: significantly positive α and negative β. An increase in the
# probability of black-capped ancestry, while also that the locus background
# is less associated with genomic background than in other loci.

# Magenta: significantly positive α and positive β: An increase in the
# probability of black-capped ancestry, while also that that locus background 
# is strongly associated with genomic background

# (Gompert et al. 2012b).

# 6. Plotting and summarizing stats by chromosome
reduced_combined <- combined %>% filter(hybrid_index==0)

reduced_combined <- reduced_combined  %>% arrange(kbp_pos) %>% arrange(scaffolds) %>% arrange(as.numeric(gsub("[A-Z,a-z]+.*","",chromosome)))

# Creating an output variable with the background genome values for comparison to chromosome by chromosome

chrom_output <- c("total",dim(reduced_combined)[1], (dim(reduced_combined)[1]-length(unique(Not_outlier_Not_outlier$locus_row))),NA,NA,length(unique(Pos_Pos$locus_row)),length(unique(Neg_Pos$locus_row)),length(unique(Not_outlier_Pos$locus_row)),length(unique(Pos_Neg$locus_row)),length(unique(Neg_Neg$locus_row)),length(unique(Not_outlier_Neg$locus_row)),length(unique(Pos_Not_outlier$locus_row)),length(unique(Neg_Not_outlier$locus_row)),length(unique(Not_outlier_Not_outlier$locus_row)))

for (i in unique(reduced_combined$chromosome)) {
  tempcombined <- reduced_combined %>% filter(chromosome==i)
  
  tempprob_matrix <- rbind(c(length(which((tempcombined$alpha_beta=="Pos+Pos")==TRUE)),length(which((tempcombined$alpha_beta=="Neg+Pos")==TRUE)),length(which((tempcombined$alpha_beta=="Not_outlier+Pos")==TRUE))),c(length(which((tempcombined$alpha_beta=="Pos+Neg")==TRUE)),length(which((tempcombined$alpha_beta=="Neg+Neg")==TRUE)),length(which((tempcombined$alpha_beta=="Not_outlier+Neg")==TRUE))),c(length(which((tempcombined$alpha_beta=="Pos+Not_outlier")==TRUE)),length(which((tempcombined$alpha_beta=="Neg+Not_outlier")==TRUE)), length(which((tempcombined$alpha_beta=="Not_outlier+Not_outlier")==TRUE))))
  
  mod_prob_matrix <- prob_matrix
  
  if (any(rowSums(tempprob_matrix)==0)) {
    todel <- which(rowSums(tempprob_matrix)==0) 
    tempprob_matrix <- tempprob_matrix[-todel,]
    mod_prob_matrix <- mod_prob_matrix[-todel,]
  }
  if(!is.null(nrow(tempprob_matrix))) {
    if (any(colSums(tempprob_matrix)==0)) {
      todel <- which(colSums(tempprob_matrix)==0) 
      tempprob_matrix <- tempprob_matrix[,-todel]
      mod_prob_matrix <- mod_prob_matrix[,-todel]
    }
  }
  
  total_mod_prob_matrix <- sum(mod_prob_matrix)
  mod_prob_matrix <- mod_prob_matrix/total_mod_prob_matrix
  
  gtest <- GTest(tempprob_matrix,p = mod_prob_matrix) 
  
  temprow <- c(i,sum(tempprob_matrix),(sum(tempprob_matrix)-length(which((tempcombined$alpha_beta=="Not_outlier+Not_outlier")==TRUE))),gtest$statistic,gtest$p.value, length(which((tempcombined$alpha_beta=="Pos+Pos")==TRUE)),length(which((tempcombined$alpha_beta=="Neg+Pos")==TRUE)),length(which((tempcombined$alpha_beta=="Not_outlier+Pos")==TRUE)),length(which((tempcombined$alpha_beta=="Pos+Neg")==TRUE)),length(which((tempcombined$alpha_beta=="Neg+Neg")==TRUE)),length(which((tempcombined$alpha_beta=="Not_outlier+Neg")==TRUE)),length(which((tempcombined$alpha_beta=="Pos+Not_outlier")==TRUE)),length(which((tempcombined$alpha_beta=="Neg+Not_outlier")==TRUE)), length(which((tempcombined$alpha_beta=="Not_outlier+Not_outlier")==TRUE)))
    
  chrom_output <- rbind(chrom_output,temprow)
  
  point_colours <- c(NA,
                     "red",
                     "red4",
                     "purple",
                     "purple4",
                     "steelblue2",
                     "blue",
                     "magenta")
  
  names(point_colours) <- c("Not_outlier+Not_outlier",
                            "Not_outlier+Pos",
                            "Not_outlier+Neg",
                            "Neg+Pos",
                            "Pos+Neg",
                            "Pos+Not_outlier",
                            "Neg+Not_outlier",
                            "Pos+Pos"
  )

  # Reordering our data based on the order we want "names"
  point_colours <- point_colours[order(factor(names(point_colours), levels=rev(c("Not_outlier+Not_outlier",
                                                                                 "Not_outlier+Neg",
                                                                                 "Pos+Neg",
                                                                                 "Not_outlier+Pos",
                                                                                 "Neg+Pos",
                                                                                 "Pos+Pos",
                                                                                 "Neg+Not_outlier",
                                                                                 "Pos+Not_outlier"
  ))))]
  
  
  point_colours <- point_colours[names(point_colours) %in% unique(tempcombined$alpha_beta)]
  
  tempcombined$alpha_beta <- factor(tempcombined$alpha_beta, levels = names(point_colours))
  
  scaffold_loci_count <- tempcombined %>% group_by(scaffolds) %>% count()
  scaffolds_to_remove <- as.matrix(scaffold_loci_count %>% filter(n<=1) %>% select(scaffolds))[,1]
  
  if (length(scaffolds_to_remove>0)) {
    print(paste("The following scaffolds will not be displayed on the plot for chromosome ",i,sep=""))
    print("as they have one or few loci to display:")
    print(scaffolds_to_remove)
  
    remove_rows <- which(as.matrix(tempcombined$scaffolds) %in% scaffolds_to_remove)
  
    tempcombined <- tempcombined[-remove_rows,]
  }
  
  if (length(unique(tempcombined$scaffolds))>1) {
    ggplot(tempcombined) + geom_line(mapping=aes(x=kbp_pos,y=alpha_median)) +
      geom_point(aes(x=kbp_pos,y=alpha_median, color=alpha_beta), size=7) +
      scale_colour_manual(values=c(point_colours)) +
      theme_bw(base_size=33) +
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
      theme(axis.title=element_text(size=40,face="bold")) +
      scale_y_continuous(name="Median α") +
      scale_x_continuous(name="Distance along chromosome (kbp)")  +
      theme(legend.position = "none") +
      facet_wrap (~ scaffolds, scales = "free_x")
  
    ggsave(paste("FigS10_alpha_chrom_",i,".png",sep=""),width=400,height=200,units="mm")
  
    ggplot(tempcombined) + geom_line(mapping=aes(x=kbp_pos,y=beta_median)) +
      geom_point(aes(x=kbp_pos,y=beta_median, color=alpha_beta), size=7) +
      scale_colour_manual(values=c(point_colours)) +
      theme_bw(base_size=33) +
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
      theme(axis.title=element_text(size=40,face="bold")) +
      scale_y_continuous(name="Median β") +
      scale_x_continuous(name="Distance along chromosome (kbp)")  +
      theme(legend.position = "none") +
      facet_wrap (~ scaffolds, scales = "free_x")
  
    ggsave(paste("FigS10_beta_chrom_",i,".png",sep=""),width=400,height=200,units="mm")

  } else {
    ggplot(tempcombined) + geom_line(mapping=aes(x=kbp_pos,y=alpha_median)) +
      geom_point(aes(x=kbp_pos,y=alpha_median, color=alpha_beta), size=7) +
      scale_colour_manual(values=c(point_colours)) +
      theme_bw(base_size=33) +
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
      theme(axis.title=element_text(size=40,face="bold")) +
      scale_y_continuous(name="Median α") +
      scale_x_continuous(name="Distance along chromosome (kbp)")  +
      theme(legend.position = "none") 
    
    ggsave(paste("FigS10_alpha_chrom_",i,".png",sep=""),width=400,height=200,units="mm")
    
    ggplot(tempcombined) + geom_line(mapping=aes(x=kbp_pos,y=beta_median)) +
      geom_point(aes(x=kbp_pos,y=beta_median, color=alpha_beta), size=7) +
      scale_colour_manual(values=c(point_colours)) +
      theme_bw(base_size=33) +
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
      theme(axis.title=element_text(size=40,face="bold")) +
      scale_y_continuous(name="Median β") +
      scale_x_continuous(name="Distance along chromosome (kbp)")  +
      theme(legend.position = "none") 
    
    ggsave(paste("FigS10_beta_chrom_",i,".png",sep=""),width=400,height=200,units="mm")
    
  }

}

chrom_output <- as_tibble(chrom_output)

names(chrom_output) <- c("scaffold_name","total_markers","total_outlying_markers","gtest_statistic","gtest_pvalue","pos_alpha.pos_beta","neg_alpha.pos_beta","NS_alpha.pos_beta","pos_alpha.neg_beta","neg_alpha.neg_beta","NS_alpha.neg_beta","pos_alpha.NS_beta","neg_alpha.NS_beta","NS_alpha.NS_beta")

# If the output table is desired, uncomment following line
# write_csv(chrom_output,"FigS7_outlier_by_chrom.csv")

# Plotting chromosomes by numbers of markers and gtest statistic
chrom_output <- chrom_output %>% mutate_at(vars(total_markers:NS_alpha.NS_beta),funs(as.numeric))

chrom_output <- chrom_output %>% mutate(sig=ifelse(gtest_pvalue<(0.05/dim(chrom_output)[1]),"sig","NS"))

ggplot(data=chrom_output[-1,]) + geom_point(aes(x=gtest_statistic,y=total_markers,fill=sig),shape = 21, colour = "black",size=7) +
  scale_fill_manual(values=c("white","grey")) +
  theme_bw(base_size=33) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.title=element_text(size=40,face="bold")) +
  scale_y_continuous(name="Number of SNP markers") +
  scale_x_continuous(name="G-test statistic") +
  theme(legend.position = "none") +
  geom_text_repel(aes(label=scaffold_name,x=gtest_statistic,y=total_markers),point.padding = 0.1,size=10)

ggsave("FigS10_Gtest_SNP_comparison.png",width=400,height=400,units="mm")  
  
  # Pivoting our data so that we can plot by category on each of the chromosomes
  chrom_output_long <- pivot_longer(data=chrom_output,cols=c(pos_alpha.pos_beta:NS_alpha.NS_beta))
  
  # Tweaking colours to do so
  point_colours <- c("grey50",
                       "red",
                       "red4",
                       "purple",
                       "purple4",
                       "steelblue2",
                       "blue",
                       "magenta")
  
  names(point_colours) <- c("NS_alpha.NS_beta",
                              "NS_alpha.pos_beta",
                              "NS_alpha.neg_beta",
                              "neg_alpha.pos_beta",
                              "pos_alpha.neg_beta",
                              "pos_alpha.NS_beta",
                              "neg_alpha.NS_beta",
                              "pos_alpha.pos_beta"
                            )
  
  # Reordering our data based on the order we want "names"
  point_colours <- point_colours[order(factor(names(point_colours), levels=rev(c("NS_alpha.NS_beta",
                                                                             "NS_alpha.neg_beta",
                                                                             "pos_alpha.neg_beta",
                                                                             "NS_alpha.pos_beta",
                                                                             "neg_alpha.pos_beta",
                                                                             "pos_alpha.pos_beta",
                                                                             "neg_alpha.NS_beta",
                                                                             "pos_alpha.NS_beta"
                                                                             ))))]
  
  
  chrom_output_long$name <- factor(chrom_output_long$name, levels = rev(c("NS_alpha.NS_beta",
                                                                          "NS_alpha.neg_beta",
                                                                          "pos_alpha.neg_beta",
                                                                          "NS_alpha.pos_beta",
                                                                          "neg_alpha.pos_beta",
                                                                          "pos_alpha.pos_beta",
                                                                          "neg_alpha.NS_beta",
                                                                          "pos_alpha.NS_beta"
  )))
  
  
  # Reordering based on chromosome order
  chrom_output_long$scaffold_name <- factor(chrom_output_long$scaffold_name , levels = (chrom_output_long %>% arrange(gtest_statistic) %>% select(scaffold_name) %>% unique() %>% as.matrix())[,1])
  
  ggplot(chrom_output_long,aes(scaffold_name)) + geom_col(aes(y=value,fill=name),position="fill",colour="black") +
    scale_fill_manual(values=point_colours) +
    theme_bw(base_size = 21) +
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
    scale_y_continuous(name=NULL) +
    theme(axis.text.x = element_text(angle = 90,hjust=1)) +
    theme(legend.position = "none") +
    scale_x_discrete(name=NULL)

  ggsave("FigS10_loci_category_chromosome.png",width=400,height=200,units="mm")

  # Because of our focus on positive beta loci, we want to see what chromosomes these are found on
  pos_beta_distribution <- chrom_output %>% mutate(total_pos_beta=pos_alpha.pos_beta+neg_alpha.pos_beta+NS_alpha.pos_beta) %>% select(scaffold_name,total_markers,total_pos_beta) 
  
  ggplot(data=pos_beta_distribution[-1,]) + geom_point(aes(x=total_pos_beta,y=total_markers),shape = 21, colour = "black",fill="grey",size=7) +
    theme_bw(base_size=33) +
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
    theme(axis.title=element_text(size=40,face="bold")) +
    scale_y_continuous(name="Number of SNP markers") +
    scale_x_continuous(name="Number of positive β outliers") +
    theme(legend.position = "none") +
    geom_text_repel(aes(label=scaffold_name,x=total_pos_beta,y=total_markers),point.padding = 0.1,size=10)
  
  ggsave("FigS10_positive_beta_by_total_markers.png",width=400,height=400,units="mm")
  
  # Finally, we wish to identify "plugs" of consecutive loci that are positive beta outliers
  # These may be large regions (i.e. inversions) less free to introgress, or involved in 
  # reproductive isolation
  
  total_pos_beta_loci <- length(unique(Pos_Pos$locus_row)) + length(unique(Neg_Pos$locus_row)) + length(unique(Not_outlier_Pos$locus_row))
  
  # Want a cut off of finding such "plug" as less than one based on our  total SNP dataset
  # Play around with x til you get what you want
  x <- 5
  ((total_pos_beta_loci/dim(reduced_combined)[1])^x)*(dim(reduced_combined)[1]-x)
  
  # Looks like 5 sig positive beta loci in a row is unlikely to happen in our dataset even once by chance
  # (0.066 to be exact) if there is no underlying relationship between signficance and underlying genomic
  # architecture (a VERY simplistic assumption!)
  
 reduced_combined <- reduced_combined %>% arrange(chromosome,kbp_pos)
  
 output <- c("chromosome","scaffold","starting_kbp_pos","ending_kbp_pos","number_of_markers")

for (i in 2:dim(reduced_combined)[1]) {
    if (reduced_combined$beta[i]=="Pos") {
      if (reduced_combined$beta[i-1]=="Pos" & reduced_combined$scaffolds[i]==reduced_combined$scaffolds[i-1]) {
        number_of_markers <- number_of_markers + 1
      } else {
        chromosome <- reduced_combined$chromosome[i]
        scaffold <- reduced_combined$scaffolds[i]
        starting_kbp_pos <- reduced_combined$kbp_pos[i]
        number_of_markers <- 1
      } 
    } else {
      if (reduced_combined$beta[i-1]=="Pos") {
        ending_kbp_pos <- reduced_combined$kbp_pos[i-1]
        output <- rbind(output,c(chromosome,scaffold,starting_kbp_pos,ending_kbp_pos,number_of_markers)) 
        chromosome <- NULL
        scaffold <- NULL
        starting_kbp_pos <- NULL
        ending_kbp_pos <- NULL
        number_of_markers <- NULL
      }
    }
  }  

output <- as_tibble(output[-1,])
names(output) <- c("chromosome","scaffold","starting_kbp_pos","ending_kbp_pos","number_of_markers")
output <- output %>%  mutate_at(vars(starting_kbp_pos:number_of_markers),funs(as.numeric))

# Filtering for things equal and above x
output <- output %>% filter(number_of_markers>=x)

write_delim(output,"FigS10_outlying_marker_blocks.txt")

output <- output %>% mutate(starting_pos=round(starting_kbp_pos*1000-1,1)) %>% mutate(ending_pos=round(ending_kbp_pos*1000,1)) %>% select(-1,-5)
write_delim(output,"FigS10_outlying_marker_bed_format.bed",col_names = FALSE)

# We now wish to output a similar set of files for all of our positive beta SNPs, adding
# 5,000 of "buffer" to be compatible with Wagner et al. (2020)

output <- reduced_combined %>% filter(beta=="Pos") %>% select(chromosome,kbp_pos,scaffolds)
output <- output %>% mutate(starting_pos=round(kbp_pos*1000-5001,1)) %>% mutate(ending_pos=round(kbp_pos*1000+500,1)) %>% select(3,4,5) %>% mutate(starting_pos=ifelse(starting_pos<0,0,starting_pos)) %>% mutate(ending_pos=ifelse(starting_pos==0,10000,ending_pos))
write_delim(output,"FigS10_positive_beta_SNPs_bed_format.bed",col_names = FALSE)

# We now want compare the location of our positive beta SNPs, to the outliers identified by Wagner et al. (2020) in their Table S3
Wagner_data <- read_delim("../data/Wagner_et_al_2020_SNPs.txt",col_names = FALSE,delim ="\t")
# Converting data to kbp and removing duplicate rows (from combining the different columns )
Wagner_data <- Wagner_data %>% mutate(X2=X2/1000)
Wagner_data <- Wagner_data %>% distinct()
output <- reduced_combined %>% filter(beta=="Pos") %>% select(chromosome,kbp_pos,scaffolds)

close_SNPs <- rep(NA,dim(output)[1])

for (i in 1:dim(output)[1]) {
  SNP_pos <- Wagner_data$X2[which((Wagner_data$X1==output$chromosome[i]) & (Wagner_data$X2 < (output$kbp_pos[i]+5)) & (Wagner_data$X2 > (output$kbp_pos[i]-5)))]
  if(length(SNP_pos)>0) {
    close_SNPs[i] <- paste(SNP_pos,collapse=",")
  }
}

output <- cbind(output,close_SNPs)
output <- as_tibble(output)
names(output) <- c("chromosome","kbp_pos","scaffold","close_Wagner_SNPs")

write_delim(output,"FigS10_positive_beta_SNPs.txt",col_names = TRUE)

# What are the chances of obtaining the level of overlap in loci seen between our studies?
prop_genome_covered_by_Wagner_outliers <- (dim(Wagner_data)[1]*10000)/(1047.81*1000*1000)
binom.test(5,671,prop_genome_covered_by_Wagner_outliers,alternative = "less")

