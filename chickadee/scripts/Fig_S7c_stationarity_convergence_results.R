# 1. Loading in required libraries
library(tidyverse)
library(gtable)
library(grid)

# 2. Setwd
setwd("chickadee/output/")

# 3. Reading in additional files for context on bgc results
genetic_map <- read_delim("../data/bgc/genetic_map.txt",col_names = FALSE,delim=" ")
no_of_loci <- dim(genetic_map)[1]
bgc_chrom_key <- read_tsv("../data/bgc/bgc_chrom_key.txt",col_names = TRUE)
names(genetic_map) <- c("SNP","chromosome","kbp_pos")

actual_chroms <- rep(NA,dim(genetic_map)[1])
for (i in 1:dim(bgc_chrom_key)[1]) {
  actual_chroms[which(genetic_map$chromosome==bgc_chrom_key$bgc_chrom[i])] <- bgc_chrom_key$chrom[i]
}

genetic_map$chromosome <- actual_chroms

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
alpha <-  read_delim("../data/bgc/run1_output/OF_alpha_output.txt",col_names = FALSE,delim = ",")
alpha0 <- alpha %>% filter(X1==0)
alpha0 <- alpha0 %>% mutate(state=row_number())
names(alpha0) <- c("locus","alpha","state")

alphaz <- alpha %>% filter(X1==max(X1))
alphaz <- alphaz %>% mutate(state=row_number())
names(alphaz) <- c("locus","alpha","state")

alpha_run2 <-  read_delim("../data/bgc/run2_output/OF_alpha_output.txt",col_names = FALSE,delim = ",")
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
beta <-  read_delim("../data/bgc/run1_output/OF_beta_output.txt",col_names = FALSE,delim = ",")
beta0 <- beta %>% filter(X1==0)
beta0 <- beta0 %>% mutate(state=row_number())
names(beta0) <- c("locus","beta","state")

betaz <- beta %>% filter(X1==max(X1))
betaz <- betaz %>% mutate(state=row_number())
names(betaz) <- c("locus","beta","state")

beta_run2 <-  read_delim("../data/bgc/run2_output/OF_beta_output.txt",col_names = FALSE,delim = ",")
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
  theme_bw() +
  theme(legend.position = "none")


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

# (Gompert et al. 2012b).






# 6. Plotting by chromosome
combined <- combined %>% arrange(as.numeric(gsub("[A-Z,a-z]+.*","",chromosome)))
alphaplot <- ggplot(combined) + geom_line(aes(x=locus_row,y=alpha_median))
betaplot <- ggplot(combined) + geom_line(aes(x=locus_row,y=beta_median))

ap <- ggplotGrob(alphaplot)
bp <- ggplotGrob(betaplot)
g <- rbind(ap, bp, size = "first")
g$widths <- unit.pmax(ap$widths, bp$widths)
grid.newpage()
grid.draw(g)

