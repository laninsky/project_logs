# 1. Loading in required libraries
library(tidyverse)
library(gtable)
library(grid)

# 2. Reading in additional files for context on bgc results
genetic_map <- read_delim("genetic_map.txt",col_names = FALSE,delim=" ")
no_of_loci <- dim(genetic_map)[1]
bgc_chrom_key <- read_tsv("bgc_chrom_key.txt",col_names = TRUE)
names(genetic_map) <- c("SNP","chromosome","kbp_pos")

actual_chroms <- rep(NA,dim(genetic_map)[1])
for (i in 1:dim(bgc_chrom_key)[1]) {
  actual_chroms[which(genetic_map$chromosome==bgc_chrom_key$bgc_chrom[i])] <- bgc_chrom_key$chrom[i]
}

genetic_map$chromosome <- actual_chroms

# 3. Reading in raw bgc files to look at stationarity
likelihood <- read_tsv("OF_LnL_output.txt",col_names = FALSE)
likelihood <- likelihood %>% mutate(state=row_number())
names(likelihood)[1] <- "LnL"

alpha <-  read_delim("OF_alpha_output.txt",col_names = FALSE,delim = ",")
alpha0 <- alpha %>% filter(X1==0)
alpha0 <- alpha0 %>% mutate(state=row_number())
names(alpha0) <- c("locus","alpha","state")
alphaz <- alpha %>% filter(X1==max(X1))
alphaz <- alphaz %>% mutate(state=row_number())
names(alphaz) <- c("locus","alpha","state")

beta <-  read_delim("OF_beta_output.txt",col_names = FALSE,delim = ",")
beta0 <- beta %>% filter(X1==0)
beta0 <- beta0 %>% mutate(state=row_number())
names(beta0) <- c("locus","beta","state")
betaz <- beta %>% filter(X1==max(X1))
betaz <- betaz %>% mutate(state=row_number())
names(betaz) <- c("locus","beta","state")

# Checking stationarity for likelihood
ggplot(likelihood) + geom_point(aes(x=state,y=LnL))
# Checking stationarity for alpha0 (first locus) and alphaz (last locus)
ggplot(alpha0) + geom_point(aes(x=state,y=alpha))
ggplot(alphaz) + geom_point(aes(x=state,y=alpha))
# Checking stationarity for beta0 (first locus) and betaz (last locus)
ggplot(beta0) + geom_point(aes(x=state,y=beta))
ggplot(betaz) + geom_point(aes(x=state,y=beta))

# 4. Reading in summmarized results to find outlier loci
alphaest <- read_delim("alphaest.txt",col_names = TRUE,delim = ",")
names(alphaest) <- c("alpha_loci", "alpha_mean", "alpha_median", "alpha_95_LB", "alpha_95_UB")
betaest <- read_delim("betaest.txt",col_names = TRUE,delim = ",")
names(betaest) <- c("beta_loci", "beta_mean", "beta_median", "beta_95_LB", "beta_95_UB")
combined <- bind_cols(genetic_map,alphaest,betaest) %>% select(-SNP,-alpha_loci,-beta_loci)

combined <- combined %>% mutate(alpha=ifelse((alpha_95_LB < 0 & alpha_95_UB > 0),"Not_outlier",
                                             ifelse((alpha_95_UB < 0),"Neg","Pos")))

combined <- combined %>% mutate(beta=ifelse((beta_95_LB < 0 & beta_95_UB > 0),"Not_outlier",
                                             ifelse((beta_95_UB < 0),"Neg","Pos")))

combined <- combined %>% mutate(both=paste(alpha,beta,sep="_"))


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

locus_trajectories <- matrix(NA,ncol=(dim(combined)[1]+1),nrow=length(hn_values))

locus_trajectories[,1] <- hn_values

for (j in 2:dim(locus_trajectories)[2]) {
  prob_ancestry <- rep(NA,length(hn_values))
  a <- combined$alpha_median[j-1]
  b <- combined$beta_median[j-1]
  
  for (i in 1:length(hn_values)) {
    prob_ancestry[i] <- slope(hn_values[i],a,b)
  }
  local_max <- NULL
  local_min <- NULL
  for (i in 2:(length(prob_ancestry)-1)) {
    if (prob_ancestry[i] > prob_ancestry[i+1] & prob_ancestry[i] > prob_ancestry[i-1] ) {
      local_max <- i
    } 
    if (prob_ancestry[i] < prob_ancestry[i+1] & prob_ancestry[i] < prob_ancestry[i-1] ) {
      local_min <- i
    } 
  }
  if (!is.null(local_max) & !is.null(local_min)) {
    startline <- which(prob_ancestry>0.5)[1]
    endline <- which(prob_ancestry<0.5)[length(which(prob_ancestry<0.5))]
    prob_ancestry[startline:endline] <- 0.5
  }
  locus_trajectories[,j] <- prob_ancestry
}  

locus_trajectories <- as_tibble(locus_trajectories)
combined <- combined %>% mutate(locus_row=row_number())

both_outlier <- combined %>% 
  filter(alpha!="Not_outlier" & beta!="Not_outlier") %>% select(locus_row) %>% as.matrix()

alpha_only <- combined %>% 
  filter(alpha=="Neg|Pos" & beta=="Not_outlier") %>% select(locus_row) %>% as.matrix()

beta_only <- combined %>% 
  filter(alpha=="Not_outlier" & beta=="Neg|Pos") %>% select(locus_row) %>% as.matrix()

neither_outlier <- combined %>% 
  filter(alpha=="Not_outlier" & beta=="Not_outlier") %>% select(locus_row) %>% as.matrix()

plotting <- ggplot(locus_trajectories)

if (length(neither_outlier)>0) {
  for (j in (neither_outlier+1)) {
    plotting <- plotting + geom_line(aes_string(x=names(locus_trajectories)[1],y=names(locus_trajectories)[j]),color="grey",size=1)
  }
}

if (length(both_outlier)>0) {
  for (j in (both_outlier+1)) {
    plotting <- plotting + geom_line(locus_trajectories,aes_string(x=names(locus_trajectories)[1],y=names(locus_trajectories)[j]),color="black",size=1)
  }
}

if (length(alpha_only)>0) {
  for (j in (alpha_only+1)) {
    plotting <- plotting + geom_line(locus_trajectories,aes_string(x=names(locus_trajectories)[1],y=names(locus_trajectories)[j]),color="blue",size=1)
  }
}

if (length(beta_only)>0) {
  for (j in (beta_only+1)) {
    plotting <- plotting + geom_line(locus_trajectories,aes_string(x=names(locus_trajectories)[1],y=names(locus_trajectories)[j]),color="red",size=1)
  }
}

# Figure out the plotting
plotting + theme_bw()

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

