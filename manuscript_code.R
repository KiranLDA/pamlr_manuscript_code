################################
## Box1
################################

library(pamlr)
# Load the data
data("hoopoe")

# make sure the cropping period is in the correct date format
start = as.POSIXct("2016-07-01","%Y-%m-%d", tz="UTC")
end = as.POSIXct("2017-06-01","%Y-%m-%d", tz="UTC")

# Crop the data
PAM_data= create_crop(hoopoe,start,end)
str(PAM_data)

################################
# Box 2
################################

par(mfrow= c(1,5), # number of panels
    oma=c(0,2,0,6), # outer margin around all panels
    mar =  c(4,1,4,1)) # inner margin around individual figure

plot_sensorimage(PAM_data$light$date, ploty=FALSE,
                 log(PAM_data$light$obs+0.01), main = "Light",
                 col=c("black",(viridis::magma(90))), cex=1.2, cex.main = 2)
plot_sensorimage_legend(log(PAM_data$light$obs+0.01), c("black",(viridis::magma(90))), ncol=1, bg="white", inset = 0, x= "bottomright", cex=0.8)

plot_sensorimage(PAM_data$acceleration$date, plotx=TRUE, ploty=FALSE, labely=FALSE,
                 PAM_data$acceleration$act, main = "Activity",
                 col=c("white",rev(viridis::magma(90))), cex=1.2, cex.main = 2)
plot_sensorimage_legend(PAM_data$acceleration$act, c("white",rev(viridis::magma(90))), ncol=1, bg="white", inset = 0, x= "bottomright", cex=0.8)

plot_sensorimage(PAM_data$acceleration$date, plotx=TRUE, ploty=FALSE, labely=FALSE,
                 PAM_data$acceleration$pit,  main="Pitch",
                 col=c("black",(viridis::magma(90))), cex=1.2, cex.main = 2)
plot_sensorimage_legend(PAM_data$acceleration$pit, c("black",(viridis::magma(90))), ncol=1, bg="white", inset = 0, x= "bottomright", cex=0.8)

plot_sensorimage(PAM_data$pressure$date, plotx=TRUE, ploty=FALSE, labely=FALSE,
                 PAM_data$pressure$obs,  main="Pressure",
                 col=c(rev(viridis::magma(90))), cex=1.2, cex.main = 2)
plot_sensorimage_legend(PAM_data$pressure$obs, c(rev(viridis::magma(90))), ncol=1, bg="white", inset = 0, x= "bottomright", cex=0.8)

plot_sensorimage(PAM_data$temperature$date, labely=FALSE,
                 (PAM_data$temperature$obs),  main="Temperature",
                 col=c("black",(viridis::magma(90))), cex=1.2, cex.main = 2)
plot_sensorimage_legend(PAM_data$temperature$obs, c("black",(viridis::magma(90))), ncol=1, bg="white", inset = 0, x= "bottomright", cex=0.8)




################################
# Figure 3 associated analysis
################################

## Ethogram: endurance

par(mfrow= c(1,1), # number of panels
    oma=c(0,0,0,0), # outer margin around all panels
    mar =  c(0,0,0,0)) # inner margin around individual figure
# Classify behaviour
behaviour_flap = classify_flap(dta = PAM_data$acceleration, period = 12)
head(behaviour_flap$timetable)

# Now look at the classification
flap_classification = behaviour_flap$classification

col=c("grey","royalblue4","chartreuse4","gold")
index= 7300:8000
plot(PAM_data$acceleration$date[index],PAM_data$acceleration$act[index],
     type="l", xlab="Date", ylab="Activity")
points(PAM_data$acceleration$date[index],PAM_data$acceleration$act[index],
       col=col[flap_classification+1][index],
       pch=16,)
legend( PAM_data$acceleration$date[index[1]],60 ,
        c("No activity", "Low activity", "High activity", "Migration" ) ,
        col = col[c(behaviour_flap$no_movement, behaviour_flap$low_movement,
                    behaviour_flap$high_movement, behaviour_flap$migration)+1],
        pch=20)



par(mar = c(4,3,4,7))

plot_sensorimage(PAM_data$acceleration$date,
                 flap_classification,
                 main="Classification",
                 col=col,
                 cex=1.2, cex.main = 2)
legend("right",cex=1.2,
       c("Resting", "Active", "Flapping", "Migrating" ) , fill = col, xpd = NA)








################################
# Box 3
################################
# Create a rolling window
to_classify = create_rolling_window(PAM_data,
                                    resolution_out = 30 ,
                                    window = 2*60,
                                    interp = FALSE)

# choose variables of interest
varint = c("sd_pressure",
           "sd_temperature",
           "median_act")

## Plot these variables of interest
# par(mfrow=c(length(varint),1), mar=c(4,4,0.5,0.5))
# for (i in 1:length(varint)){
#   plot(to_classify$date, to_classify[,varint[i]],
#        type="l", xlab="Date", ylab = varint[i])
# }

# remove seed for a real analysis - this is just to make plotting consistent
set.seed(24)

# Activity classification
behaviour_flap = classify_flap(dta = PAM_data$acceleration, period = 12)
flap_classification = behaviour_flap$classification

# pressure classification
behaviour_P = classify_pressurechange(dta = PAM_data$pressure)
P_classification = behaviour_P$classification

## Kmeans
kmean_classification = classify_summary_statistics(to_classify[,varint],
                                             states=2, "kmeans")$cluster

## EMBC example
embc_classification = classify_summary_statistics(to_classify[,varint],
                                             "embc")$cluster
## HMM example
hmm_classification = classify_summary_statistics(dta= to_classify[,varint],
                                                 states=2, "hmm")$cluster


## Changepoint
changepoints = classify_changepoint(to_classify$pressure, cpt.method = "meanvar")




## Plot everything as sensorimages next to each other
par(mfrow= c(1,5), # number of panels
    oma=c(0,2,0,6), # outer margin around all panels
    mar =  c(4,1,4,1)) # inner margin around individual figure

col=c("grey","royalblue4","chartreuse4","gold")
plot_sensorimage(PAM_data$acceleration$date, ploty=FALSE,
          flap_classification,
          main="Activity",
          col=sort(unique(flap_classification))+1,
          cex=1.2, cex.main = 2)

col=c("grey","royalblue4","gold")
plot_sensorimage(PAM_data$pressure$date,
          P_classification,
          main="Pressure",plotx=TRUE, ploty=FALSE, labely=FALSE,
          col=sort(unique(P_classification))+1,
          cex=1.2, cex.main = 2)

col=c("grey","gold")
plot_sensorimage(to_classify$date,
          kmean_classification, plotx=TRUE, ploty=FALSE, labely=FALSE,
          main="k-means",
          col=sort(unique(kmean_classification)),
          cex=1.2, cex.main = 2)
legend("right",cex=1.2,
       legend= sort(unique(embc_classification)), fill=sort(unique(embc_classification)), xpd = NA, title = "Cluster")

col=c("grey","grey","grey","grey","gold","royalblue4","grey","grey")
plot_sensorimage(to_classify$date,
          embc_classification, plotx=TRUE, ploty=FALSE, labely=FALSE,
          main="EMBC",
          col=sort(unique(embc_classification)),
          cex=1.2, cex.main = 2)

plot_sensorimage(to_classify$date,
          hmm_classification,
          main="HMM", labely = FALSE,
          col=sort(unique(hmm_classification)),
          cex=1.2, cex.main = 2)

result_comparison = to_classify[,c("date","pressure","act")]
result_comparison["act_classification"] = flap_classification[PAM_data$acceleration$date %in% to_classify$date]
result_comparison["P_classification"] = P_classification[PAM_data$pressure$date %in% to_classify$date]
result_comparison["kmean_classification"] = kmean_classification
result_comparison["embc_classification"] = embc_classification
result_comparison["hmm_classification"] = hmm_classification


# need to manually set the clusters here because they are randomly allocated
# which cluster is the one representing
result_comparison["act_mig"] = ifelse(result_comparison["act_classification"]==3,1,0)
result_comparison["P_mig"] = ifelse(result_comparison["P_classification"]==2,1,0)
result_comparison["kmean_mig"] = ifelse(result_comparison["kmean_classification"]==1,1,0)
result_comparison["embc_mig"] = ifelse(result_comparison["embc_classification"]==6,1,0)
result_comparison["hmm_mig"] = ifelse(result_comparison["hmm_classification"]==2,1,0)


# plot the selected cluster to make sure they are all corrrect
par(mfrow= c(1,5), # number of panels
    oma=c(0,2,0,6), # outer margin around all panels
    mar =  c(4,1,4,1)) # inner margin around individual figure

plot_sensorimage(result_comparison$date, ploty=FALSE,
          result_comparison$act_mig,
          main="Activity",
          col=sort(unique(hmm_classification)),
          cex=1.2, cex.main = 2)

plot_sensorimage(result_comparison$date,plotx=TRUE, ploty=FALSE, labely=FALSE,
          result_comparison$P_mig,
          main="Pressure",
          col=sort(unique(hmm_classification)),
          cex=1.2, cex.main = 2)

plot_sensorimage(result_comparison$date,plotx=TRUE, ploty=FALSE, labely=FALSE,
          result_comparison$kmean_mig,
          main="kmean",
          col=sort(unique(hmm_classification)),
          cex=1.2, cex.main = 2)

plot_sensorimage(result_comparison$date,plotx=TRUE, ploty=FALSE, labely=FALSE,
          result_comparison$embc_mig,
          main="embc",
          col=sort(unique(hmm_classification)),
          cex=1.2, cex.main = 2)

plot_sensorimage(result_comparison$date, labely = FALSE,
          result_comparison$hmm_mig,
          main="HMM",
          col=sort(unique(hmm_classification)),
          cex=1.2, cex.main = 2)


# calculate the agreement between the different classifications and plot it
# compares the classifications
# result_comparison["agreement"] = rowSums(result_comparison[c("act_mig","P_mig","kmean_mig",
#                                                              "embc_mig","hmm_mig")])
comparison = compare_classifications(date=result_comparison$date,
                                     classifications = result_comparison[c("act_mig","P_mig","kmean_mig", "embc_mig","hmm_mig")])

# plot the comparison/agreement between the different outputs
par(mfrow= c(1,1), # number of panels
    oma=c(0,2,0,6),mar = c(4,3,4,7))
plot_sensorimage(comparison$date,#result_comparison$date,
                 comparison$`1`,#result_comparison$agreement,
                 main="Agreement",
                 col=c("black",viridis::viridis(5)),
                 cex=1.2, cex.main = 2)
legend("right",cex=1.2,legend= 0:5, fill = c("black",viridis::viridis(5)), xpd = NA)


# generate a confusion matrix
compare_confusion_matrix(result_comparison$act_mig, result_comparison$P_mig)



