library(rwty)
interspecies <- load.multi("/Users/alanaalexander/Dropbox/seal/Emma/processed_mito_alignment/",format="beast")
interspecies.rwty <- analyze.rwty(interspecies,burnin=500,fill.color = 'posterior')
#Plots looked at to see if convergence between chains achieved
#Average standard deviation of split frequencies
interspecies.rwty$asdsf.plot
#Cumulative change in split frequencies
interspecies.rwty$acsf.cumulative.plot
#Cumulative split frequencies for 20 clades
interspecies.rwty$splitfreqs.cumulative.plot
#Topological autocorelation
interspecies.rwty$autocorr.plot
#Topology Trace
interspecies.rwty$topology.trace.plot$density.plot
#Split frequency comparisons
interspecies.rwty$splitfreq.matrix
#Tree space (100 trees)
interspecies.rwty$treespace.points.plot
