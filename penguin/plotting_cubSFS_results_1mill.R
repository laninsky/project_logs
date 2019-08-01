# This script plots a longer temporal slice than the original plotting_cubsSFS_results script
# Working directory should be where the *resAlpha2.rds and *boot.res.rds files are from CubSFS analysis
setwd(".././../../alanaalexander/Dropbox/penguins/CubSFS/")

# Loading in required libraries
library(CubSFS)
library(parallel)
library(tidyverse)
library(tidyverse)
library(gridExtra)
library(scales)

# Setting variable
data <- NULL
mu <-  2.6*10^(-7)

# Reading in CubSFS results, looking at population size changes over 1,000,000 years
for (i in list.files(pattern="*resAlpha2.rds")) {
  resAlpha2 <- readRDS(i)
  boot.res <- readRDS(gsub("estimated.resAlpha2.rds",".boot.res.rds",i))
  TimeInYears <- seq(0,1000000,by=1000)
  if(grepl("Emperor",i)) {
    genyr <- 14
  } else {
    if(grepl("King",i)) {
      genyr <- 9
    } else {
      genyr <- 8
    }
  }
  resAlpha <- readRDS(gsub("estimated.resAlpha2.rds","original.resAlpha.rds",i))
  x <- (seq(1,length(rev(resAlpha$expSFS))))-1
  model_data <- data.frame(x=x,y=resAlpha$expSF)
  
  theta.0 <- min(model_data$y)*0.5
  model.0 <- lm(log(y-theta.0) ~ x, data=model_data)
  alpha.0 <- exp(coef(model.0)[1])
  beta.0 <- coef(model.0)[2]
  start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)  
  model <- nls(y ~ (alpha * exp(beta * x) + theta), data=model_data, start=start)  
  plot(model_data$x, model_data$y)
  lines(model_data$x, predict(model,list(x=model_data$x)),col='skyblue',lwd=3)
  total.nb.sites <- round(predict(model,list(x=-1))+sum(resAlpha$SFS))  
  
  n.samples <- length(resAlpha2$SFS)
  
  resNt <- PlotCubSFS(n.samples,mu,total.nb.sites,genyr,resAlpha2,boot.res=boot.res, abstime=TimeInYears,is.folded = TRUE)
  resNt$Ntdf
  temptable <- as_tibble(resNt$Ntdf) %>% mutate(Species=gsub("estimated.resAlpha2.rds","",i))
  data <- rbind(data,temptable)
}

# Cleaning up the species names to match the rest of the paper
data$Species <- gsub("Adelie","Adélie",data$Species)
data$Species <- gsub("Tawaki", "Fiordland crested",data$Species)
data$Species <- gsub("Macroy","Macaroni and Royal",data$Species)
data$Species <- gsub("Filholi","Eastern rockhopper",data$Species)
data$Species <- gsub("Chrysocome","Western rockhopper",data$Species)
data$Species <- gsub("Moseleyi","Northern rockhopper",data$Species)

#Creating a vector tying species name to colour so we can manipulate the order
#Using the results from data_summary, rather than manually
penguin_colours <- c("#4FA253",
                     "#BE00FF",
                     "#a67101",
                     "#B73331",
                     "#3E42C1",
                     "#f97509",
                     "#8d8d8d",
                     "#f656a6",
                     "#03BAFF",
                     "#e8e8e8")

names(penguin_colours) <- c("Fiordland crested",
                            "Western rockhopper",
                            "Chinstrap",
                            "Macaroni and Royal",
                            "Eastern rockhopper",
                            "Gentoo",
                            "Adélie",
                            "Emperor",
                            "Northern rockhopper",
                            "King")

# Reordering our variables based on data_summary
penguin_colours <- penguin_colours[order(factor(names(penguin_colours), levels=c("Macaroni and Royal","Eastern rockhopper","Adélie","Gentoo","Chinstrap","King","Emperor","Northern rockhopper","Western rockhopper","Fiordland crested")))]
data$Species <- factor(data$Species, levels = c("Macaroni and Royal","Eastern rockhopper","Adélie","Gentoo","Chinstrap","King","Emperor","Northern rockhopper","Western rockhopper","Fiordland crested"))

# Plotting all CIs on one graph with a logged y axis
ggplot(data) + geom_rect(mapping=aes(xmin=135000, 
                                     xmax=194000,
                                     ymin=min(Nt,LCL,median,UCL),
                                     ymax=max(Nt,LCL,median,UCL)), 
                         color="black", fill="grey",size=0.1) + 
  geom_rect(mapping=aes(xmin=18000, 
                                     xmax=25000,
                                     ymin=min(Nt,LCL,median,UCL),
                                     ymax=max(Nt,LCL,median,UCL)), 
                         color="black", fill="grey",size=0.1) +
  geom_ribbon(aes(x=time,ymin=LCL,ymax=UCL,
                  fill=factor(Species,levels=c("Fiordland crested",
                                               "Western rockhopper",
                                               "Chinstrap",
                                               "Macaroni and Royal",
                                               "Eastern rockhopper",
                                               "Gentoo",
                                               "Adélie",
                                               "Emperor",
                                               "Northern rockhopper",
                                               "King")
                  )),alpha=0.6,color="black",size=0.1) +
  scale_fill_manual(name="Species",values=c("#4FA253",
                                            "#BE00FF",
                                            "#a67101",
                                            "#B73331",
                                            "#3E42C1",
                                            "#f97509",
                                            "#8d8d8d",
                                            "#f656a6",
                                            "#03BAFF",
                                            "#e8e8e8")) +
  theme_bw(base_size = 4) + scale_y_log10(expand=c(0,0)) + ylab("Effective population size") + xlab ("Time (years)") +
  theme(axis.title=element_text(face="bold"), legend.title = element_text(face="bold")) + theme(aspect.ratio = 1) + 
  theme(legend.key.size = unit(0.5,"line")) + scale_x_reverse(expand=c(0,0))

# Saving this graph
ggsave("combined_cubSFS_1mill.pdf",height=8.7,width=8.7,units="cm")

# Getting some summary statistics from our data based on median values
data_summary <- data %>% group_by(Species) %>% summarise(`N at time 0`=median[time==0],`N at time 25000`=median[time==25000],`Is population expanding?`=ifelse(`N at time 0`>`N at time 25000`,"Yes","No"),`N at start of expansion`=ifelse(`Is population expanding?`=="Yes",min(median),NA),`Fold expansion`=ifelse(`Is population expanding?`=="Yes",`N at time 0`/`N at start of expansion`,NA),`Time at start of expansion`=ifelse(`Is population expanding?`=="Yes",time[median==`N at start of expansion`],NA)) %>% arrange(desc(`Time at start of expansion`),`N at time 0`)

# Saving this data
write.csv(data_summary,"CubSFS_pop_expansion_median_summary_25K.csv",quote=FALSE,row.names = FALSE)

# Getting some summary statistics from our data based on observed values
data_summary_observed <- data %>% group_by(Species) %>% summarise(`N at time 0`=Nt[time==0],`N at time 25000`=Nt[time==25000],`Is population expanding?`=ifelse(`N at time 0`>`N at time 25000`,"Yes","No"),`N at start of expansion`=ifelse(`Is population expanding?`=="Yes",min(Nt),NA),`Fold expansion`=ifelse(`Is population expanding?`=="Yes",`N at time 0`/`N at start of expansion`,NA),`Time at start of expansion`=ifelse(`Is population expanding?`=="Yes",time[Nt==`N at start of expansion`],NA)) %>% arrange(desc(`Time at start of expansion`),`N at time 0`)

# Saving this data
write.csv(data_summary_observed,"CubSFS_pop_expansion_observed_summary_25K.csv",quote=FALSE,row.names = FALSE)

# Determining what the cut-off on the y-axis should be based on the maximum minimum in the non expanding species
ymximum <- as.numeric(data %>% filter(Species %in% (data_summary$Species[data_summary$`Is population expanding?`=="No"])) %>% 
                        group_by(Species) %>% summarise(minimum_count=max(LCL)) %>% arrange(desc(minimum_count)) %>% select(minimum_count) %>% slice(1L)) 

# Creating a modified matrix with all values over this trimmed to this
data2 <- data %>% mutate(UCL=ifelse(UCL>ymximum,ymximum,UCL)) %>% 
  mutate(LCL=ifelse(LCL>ymximum,ymximum,LCL)) %>% 
  mutate(Nt=ifelse(Nt>ymximum,ymximum,Nt)) %>% 
  mutate(median=ifelse(median>ymximum,ymximum,median))

# Reordering our variables based on data_summary
penguin_colours <- penguin_colours[order(factor(names(penguin_colours), levels=as.character(data_summary$Species)))]
data2$Species <- factor(data2$Species, levels = as.character(data_summary$Species))

# Plotting all species with the same y-axis (logged)
ggplot(data2) + geom_rect(mapping=aes(xmin=135000, 
                                      xmax=194000,
                                      ymin=min(Nt,LCL,median,UCL),
                                      ymax=max(Nt,LCL,median,UCL)), 
                          color="black", fill="grey",size=0.1)  + 
  geom_rect(mapping=aes(xmin=18000, 
                                       xmax=25000,
                                       ymin=min(Nt,LCL,median,UCL),
                                       ymax=max(Nt,LCL,median,UCL)), 
                           color="black", fill="grey",size=0.1) + 
  geom_ribbon(aes(x=time,ymin=LCL,ymax=UCL,
                  fill=factor(Species,levels=names(penguin_colours)
                  )),color="black",show.legend = FALSE) +
  scale_fill_manual(name="Species",values=penguin_colours) +
  theme_bw(base_size = 6) + ylab("Effective population size") + xlab ("Time") +
  facet_wrap(~Species,ncol=2) + ylim(c(NA,ymximum)) + scale_y_log10(expand=c(0,0)) +
  geom_line(aes(x=time,y=Nt),size=0.5) +
  geom_line(aes(x=time,y=median),size=0.5,linetype="dashed") + xlab ("Time (years)") +
  theme(axis.title=element_text(face="bold")) + scale_x_reverse(expand=c(0,0))

ggsave("facetwrap_same_y_cubSFS_1mill.pdf",width=8.7,height=17.8,units="cm")

# Plotting all species with different (logged) axes
groblist <- list()
data <- data %>% arrange(factor(Species, levels = data_summary$Species))

for (i in 1:length(unique(data$Species))) {
  temp <- data %>% filter(Species==(unique(data$Species))[i])
  groblist[[i]] <- ggplot(temp)  + geom_rect(mapping=aes(xmin=135000, 
                                                         xmax=194000,
                                                         ymin=min(Nt,LCL,median,UCL),
                                                         ymax=max(Nt,LCL,median,UCL)), 
                                             color="black", fill="grey",size=0.1) + 
    geom_rect(mapping=aes(xmin=18000, 
                                                         xmax=25000,
                                                         ymin=min(Nt,LCL,median,UCL),
                                                         ymax=max(Nt,LCL,median,UCL)), 
                                             color="black", fill="grey",size=0.1) + 
    geom_ribbon(aes(x=time,ymin=LCL,ymax=UCL),
                fill=penguin_colours[i],
                color="black",show.legend = FALSE) +
    ylab("Effective population size") + xlab ("Time (years)") +
    geom_line(aes(x=time,y=Nt),size=0.5) +
    geom_line(aes(x=time,y=median),size=0.5,linetype="dashed") +
    theme(axis.title=element_text(face="bold")) + theme_bw(base_size = 6) +
    facet_grid(~ Species) + scale_x_reverse(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))
}

plot_to_write <- arrangeGrob(grobs=groblist, ncol=2)
ggsave("facetwrap_diff_non_log_y_cubSFS_1mill.pdf",plot_to_write,width=8.7,height=17.8,units="cm")

# Same thing as above but logging the y, except for Northern rockhopper, because there was
# not enough variaiton through time for it to be displayed on a log scale
rm(groblist)
groblist <- list()

for (i in 1:length(unique(data$Species))) {
  temp <- data %>% filter(Species==(unique(data$Species))[i])
  groblist[[i]] <- ggplot(temp)  + geom_rect(mapping=aes(xmin=135000, 
                                                         xmax=194000,
                                                         ymin=min(Nt,LCL,median,UCL),
                                                         ymax=max(Nt,LCL,median,UCL)), 
                                             color="black", fill="grey",size=0.1) + 
    geom_rect(mapping=aes(xmin=18000, 
                                                         xmax=25000,
                                                         ymin=min(Nt,LCL,median,UCL),
                                                         ymax=max(Nt,LCL,median,UCL)), 
                                             color="black", fill="grey",size=0.1) + 
    geom_ribbon(aes(x=time,ymin=LCL,ymax=UCL),
                fill=penguin_colours[i],
                color="black",show.legend = FALSE) +
    ylab("Effective population size") + xlab ("Time (years)") +
    geom_line(aes(x=time,y=Nt),size=0.5) +
    geom_line(aes(x=time,y=median),size=0.5,linetype="dashed") +
    theme(axis.title=element_text(face="bold")) + theme_bw(base_size = 6) +
    facet_grid(~ Species) + scale_y_log10(expand=c(0,0)) + scale_x_reverse(expand=c(0,0))
}
temp <- data %>% filter(Species==(unique(data$Species))[7])
groblist[[7]] <- ggplot(temp) + geom_rect(mapping=aes(xmin=135000, 
                                                      xmax=194000,
                                                      ymin=min(Nt,LCL,median,UCL),
                                                      ymax=max(Nt,LCL,median,UCL)), 
                                          color="black", fill="grey",size=0.1)  + 
  geom_rect(mapping=aes(xmin=18000, 
                                                       xmax=25000,
                                                       ymin=min(Nt,LCL,median,UCL),
                                                       ymax=max(Nt,LCL,median,UCL)), 
                                           color="black", fill="grey",size=0.1) + 
  geom_ribbon(aes(x=time,ymin=LCL,ymax=UCL),
              fill=penguin_colours[7],
              color="black",show.legend = FALSE) +
  ylab("Effective population size") + xlab ("Time (years)") +
  geom_line(aes(x=time,y=Nt),size=0.5) +
  geom_line(aes(x=time,y=median),size=0.5,linetype="dashed") +
  theme(axis.title=element_text(face="bold")) + theme_bw(base_size = 6) +
  facet_grid(~ Species) + scale_y_continuous(labels=scientific,expand=c(0,0))+ 
  theme(axis.text.y = element_text(size=4))  + scale_x_reverse(expand=c(0,0))

plot_to_write <- arrangeGrob(grobs=groblist, ncol=2)
ggsave("facetwrap_diff_log_y_cubSFS_1mill.pdf",plot_to_write,width=8.7,height=17.8,units="cm")

# Same thing as above but also logging the x, so we can see what is going on over recent time scales
rm(groblist)
groblist <- list()

# With thanks to Brian Diggs: https://stackoverflow.com/questions/11053899/how-to-get-a-reversed-log10-scale-in-ggplot2
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

# Modifying time point 0 to 0.5 b/c of logging
data[which(data[,1]==0),1] <- 1

species_order <- c("Macaroni and Royal","Eastern rockhopper","Adélie","Gentoo","Chinstrap","King","Emperor","Northern rockhopper","Western rockhopper","Fiordland crested")
penguin_colours <- penguin_colours[order(factor(names(penguin_colours), levels=c("Macaroni and Royal","Eastern rockhopper","Adélie","Gentoo","Chinstrap","King","Emperor","Northern rockhopper","Western rockhopper","Fiordland crested")))]

for (i in 1:length(species_order)) {
  temp <- data %>% filter(Species==species_order[i])
  groblist[[i]] <- ggplot(temp) + geom_rect(mapping=aes(xmin=135000, 
                                                        xmax=194000,
                                                        ymin=min(Nt,LCL,median,UCL),
                                                        ymax=max(Nt,LCL,median,UCL)), 
                                            color="black", fill="grey",size=0.1)+ 
    geom_rect(mapping=aes(xmin=18000, 
                                                         xmax=25000,
                                                         ymin=min(Nt,LCL,median,UCL),
                                                         ymax=max(Nt,LCL,median,UCL)), 
                                             color="black", fill="grey",size=0.1) + 
    geom_ribbon(aes(x=time,ymin=LCL,ymax=UCL),
                fill=penguin_colours[i],
                color="black",show.legend = FALSE) +
    ylab("Effective population size") + xlab ("Time (years)") +
    geom_line(aes(x=time,y=Nt),size=0.5) +
    geom_line(aes(x=time,y=median),size=0.5,linetype="dashed") +
    theme(axis.title=element_text(face="bold")) + theme_bw(base_size = 6) +
    facet_grid(~ Species) + scale_y_log10(expand=c(0,0)) + scale_x_continuous(trans=reverselog_trans(10),expand=expand_scale(mult=c(0,0.01)))
}

plot_to_write <- arrangeGrob(grobs=groblist, ncol=2)
ggsave("facetwrap_diff_log_x_cubSFS_1mill_figS7.pdf",plot_to_write,width=8.7,height=17.8,units="cm")
