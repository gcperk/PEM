
# script 2: Run the random forest models based on subset of selected variables: 

## Install packages and check libraries; 
# note this only needs to be run once
# install.packages(c("raster","rgdal","RSAGA", "tidyr","dplyr")
# install.packages("ModelMap",dep = T)

library(tidyr)
library(raster)
library(rgdal)
library(sp)
library(rgdal)
library(ModelMap)
library(dplyr)
library(sf)
library(ggplot2)
library(stringr)

# check the home directory  
getwd()

# Step 1: set up location of drives to input and output
field.data.folder = ("../../Data/Field_data")         # location of full data set 
in.folder = ("../../Analysis/RandomForest/inputs")    # location of attributes 
output.folder = ("../../Analysis/RandomForest/outputs") # location of model outputs 
layer.folder = ("../../Data/Layers")


#######################################

## Step 2: Read in the data file: select more up to date file and read in the datafile
pts.file = "AllDeception_Pts_Consolidated_WHM_BGC.csv"

#pts.file = "AllDeception_Pts_Consolidated_WHM.csv"
#pts.file ="AllDeception_Pts_Consolidated_to_2018-09-23_BGC.csv"
#pts.file ="AllDeception_Pts_Consolidated_to_2018-09-23.csv"

pts.0 = read.csv(paste(field.data.folder,"/",pts.file,sep = ''),stringsAsFactors = FALSE)
      #head(pts.0) ; length(pts.0$Longitude) # error checks

# subset the columns of interest
pts = pts.0 %>% dplyr::select(c(Longitude, Latitude,GlobalID,Biogeoclimatic.Unit,Site.Physiog_5m,Site.Realm_5m,Site.Group_5m,Site.Class_5m,
                                Site.Association_5m,Site.Series.Map.Unit_5m,Site.Var.Phase.Map.Unit_5m,MAP_LABEL,BGC_test,Crew,
        Experience,Random.Point.ID,Certainty,Transition))

head(pts)
unique(pts$Site.Series.Map.Unit_5m)

## data cleaning of file: 
pts <- pts %>% dplyr::filter(Site.Series.Map.Unit_5m != "")# remove rows with no site series data
#length(pts$Longitude) # 2307


## ---------- DECISION 1: HOW TO SUBSAMPLE THE DATA () -------------------

## there are a number of subsets to run for the data to test how the model fits. 
## This includes: 
    ## 1) Crew () 
        #[1] "BJR","BJR,WM","BJR,WM,EC","EAC","EAC,WHM","EBL","KSD"          
        #[9] "PRD","WHM","WHM,AMR","WHM,BJR", "WHM,KSD","WHM,EAC"     
        #[17] "WHM,EAC","WHM,EAC,KSD","HPG"  
    ## 2) Experience (1,2,3),
    ## 3) Random.Point.ID, (number value or NA - highly skewed to NA)
    ## 4) Certainty (1,2,3,4) - number highly skewed to 2
    ## 5) Transition (1,2,3,4,5,7,Null,Blanks) (2 x blank)

# add a new model name with each of the model runs

pts.t = pts# M01_allBGC__map    ## All data
MODELfn <- "M01_allBGC_"
M.descrip <- "All data and BGCs"

#pts.t <- pts %>% dplyr::filter(Experience == 1)   ## Data with experience of 1
#MODELfn <- "M02_allBGC_"
#M.descrip <- "all BGC experience = 1"

#pts.t <- pts %>% dplyr::filter(Experience != 1) 
#MODELfn <- "M03_allBGC_"

#pts.t <- pts %>% dplyr::filter(str_detect(Crew,"WHM")) ; unique(pts.t$Crew)# M03_allBGC__map
#MODELfn <- "M04_allBGC_"
 
      #####################################################
      # Create some data summaries for your interest 
      
      #####################################################
      # create some summary data points to check the raw data 
      #sums <- pts %>% 
      #  dplyr::group_by(Biogeoclimatic.Unit,Site.Series.Map.Unit_5m) %>%
      #  summarise(count = n())
      #  
      #write.csv(sums,paste(field.data.folder,"data_summary_by_BGC.csv",sep = "/")) 
      #ggplot(sums, aes(count)) + geom_histogram()
      
      #sums.essf <- sums %>% filter(Biogeoclimatic.Unit == "ESSF")
      #sums1 <- pts %>% 
      #  group_by(Biogeoclimatic.Unit) %>%
      #  summarise(count = n())
      #####################################################

      # get a quick summary of the types
      #unique(pts$Biogeoclimatic.Unit)
      # "SBSmc2"  "ESSFmc"  "ESSFmcw"        "ESSF"    "SBS"  
      #sort(unique(pts$Site.Series.Map.Unit_5m))
      #unique(pts$MAP_LABEL)
      #[1] SBSmc2 ESSFmc

      
# might need to remove the site series with very low sample points 
          
## ---------- DECISION 2: HOW TO SUBSAMPLE THE DATA BY BGC UNIT () ---------------------
      
## Subset the data per BGC unit: either by field defined BGC or by GIS defined BGC
  ## This includes: 
      ## 1) Biogeoclimatic.Unit
      ## 2) Site.Group_5m : Forest or non_forest 
      ## 3) Site.Series.Map.Unit_5m  () site series within the SS name? 
      ## 4) GIS mapped BGS = (MAP_LABEL),Note use the column BGC_test if you want to check the difference between the field assigned BGC and the GIS defined BGC
      
########################
# Subsample the data by BGC or Mapped BCG
########################

# split out by BGC units (assigned during the survey)
#pts.t <- pts %>% filter(Biogeoclimatic.Unit == "SBS")
#pts.t <- pts %>% filter(Biogeoclimatic.Unit == "ESSF")
#pts.t <- pts %>% filter(Biogeoclimatic.Unit == "ESSFmc")

# split out by BGC units (assigned during the survey)

#pts.t <- pts %>% filter(Biogeoclimatic.Unit == "ESSFmc")
      
# might need to remove the site series with very low sample points 
      
#pts.t = pts    
      
      
      
## ---------- DECISION 3: Select the scale at which the points were extracted (5m,10m,25m) ---------------------
      
##Note these files were extracted in script1: 01_extract_pt_values.R
##  Select the scale at which the points were extracted  
      
att.files = list.files(in.folder) # this provides a list of the csv files generated with attributes 

file1 = att.files[1]            # choose the scale of interest (change the no to select a different scale)
scale.analysis = str_sub(gsub("*D_|_pts.*","", file1),start = -3)   # grab the scale to add to summary table

foi.0 = read.csv(paste(in.folder,file1,sep = "/"),header = TRUE) # read in the file
foi.0 = foi.0 %>%
  select(-c(X,Longitude,Latitude,ObjectID,ID)) 

qdatafn = left_join(pts.t,foi.0, by = "GlobalID") # join the attribute file to the Site series data set (field data)

# tidy up the data set 
qdatafn <- qdatafn %>%
  mutate (ID = seq(1,length(qdatafn$GlobalID),1)) %>%
  select(-c(GlobalID))

# GP Notes to fix error when runing model (if using na.omit) If your subset contains 
#levels(droplevels(qdatafn$SiteSeries))
#qdatafn$SiteSeries <- factor(qdatafn$SiteSeries);
#groupA <- droplevels(dataset2[dataset2$order=="groupA",])

## define the test and training set names # this is not needed as using an out of bag error metric 
# head(qdatafn) ; names(qdatafn)

# create a testing and training data set by random allocation of points 
# randomly seperate into training and testing datasets. 
#get.test(proportion.test=0.2,
#          qdatafn=qdatafn,
#          seed=42,
#          folder=in.folder,
#          qdata.trainfn="qdatafn.train.csv",
#          qdata.testfn="qdatafn.test.csv")
#
#qdatafn <- is.matrix(qdatafn)

#get the working directory and assign it to the variable "folder" so we know where to put the results of the analysis
#this is also the folder that contains the data layers

###############################################################
## SET UP THE MODEL PARAMETERS
###############################################################

#In addition to Random Forests, the program will also do a model based on  Stochastic Gradient Boosting
model.type <-"RF"

# Give a name for the model that will be used to identify the outputs 
#MODELfn <- "M03_allBGC_"
#M_description <- " write in a description here as you want "

#Identify the factors in the csv file that contain data to help predict the individual site series
predList <- c("AnisotropicHeating",
              "GeneralCurvature",
              "Dec_dem_BCALbers_25m",
              "MultiResValleyBottomFlatness",
              "TWI",
              "Slope",
              "Openness_Negative",
              "Openness_Positive",
              "TerrainRuggedness",
              "TopographicPosition")

#If any of the predictors were categorical variables I would identify them with the following command
#except it would be predFactor <- c("List the variable") - Don't forget the c in front of the first parenthesis
#predFactor <- ("BECCode")

predFactor <- FALSE  #I would type this if there were no categorical variables #BGC layer? 

# calcaulte the no of input parameters to be used later on in summary table 
if (predFactor == FALSE){ length.pred = 0}else { length.pred = length(predFactor) }
no.params <-length(predList) + length.pred

#Define the response variable and state whether it is binary (present absent 1/0) or continuous or categorical.  These variables match
#the names I have used in the csv file.

response.name = "Site.Series.Map.Unit_5m"

response.type <- "categorical"

#Not sure what the seed does except starts the model from a given point. Don't change this. 
seed <- 44

#The csv file must have a unique value for each point-the unique rowname is in column "ID"
unique.rowname <- "ID"
#unique.rowname <- "GlobalID"

#This limits the number of rows that the program reads at a given point so you don't blow up the memory.  I am not sure
#how much to put here - this is the value they used in the example
numrows = 400

#as.data.frame(qdatafn)
#This identifies the csv file that is a crosswalk table between the predicted factors (elev, slope, etc.) and the names of the
#raster grids that each one is represented by - you don't need the full path in the name of the file as suggested by the ModelMap pdf file

#MODEL CREATION
#This builds the model for the 103 site series using all of the previous information I have typed in above.
model.obj.ex3 <- model.build(model.type = model.type, 
                             qdata.trainfn = qdatafn,
                             folder = output.folder, unique.rowname= unique.rowname, MODELfn = MODELfn, predList = predList,
                             predFactor = predFactor, response.name = response.name,
                             response.type = response.type, seed = seed, na.action = "na.roughfix")


## Note if you get this error: You need to drop all factors from your subset.  (See code above) 
      #Error in randomForest.default(x, y, mtry = mtryStart, ntree = ntreeTry,  : 
      #Can't have empty classes in y.
## this is also caused when you use na.omit in the model code as it removes NAs and hence you end up with levels that are misssing in your data set that cant be fixed above/


#MODEL DIAGNOSTICS
#This generates several diagnostic graphs of the model including Out-Of_bag model predictions including
#CSv files of presence-absence thresholds optimized by 12 criteria.  It also produces a Variable importance map.
#It also generates a histogram, calibration plot, a ROC plot and AUC plot and error rate

# create a folder with the name of the model outputs

dir.create(model.folder<-file.path(output.folder,MODELfn ), showWarnings = TRUE)


model.pred.ex3 <- model.diagnostics(model.obj = model.obj.ex3,
                                    qdata.trainfn = qdatafn, folder = model.folder, MODELfn = MODELfn,
                                    unique.rowname = unique.rowname, prediction.type = "OOB",
                                    #device.type = c("jpeg", "pdf"), cex = 1.2) # ajust this if you want the outputs in another format 
                                    device.type = "pdf", cex = 1.2)


## Note if you get this error: You need to drop all factors from your subset.  
      #Error in randomForest.default(x, y, mtry = mtryStart, ntree = ntreeTry,  : 
      #Can't have empty classes in y.

#varImpPlot(model.obj.ex3)

## This line is working only for some catergories
#model.interaction.plot(model.obj.ex3, x = "GeneralCurvature", y = "TWI", plot.type= "image", device.type = "pdf", MODELfn = MODELfn, folder = output.folder)


#MODEL FIT METRICS
# calculate the confusion matrix 
pred <- read.table(paste(model.folder,"/",MODELfn,"_pred.csv",sep = ""),header = TRUE, sep = ",", stringsAsFactors = TRUE)

pred$pred<- as.factor(pred$pred) ; pred$obs<- as.factor(pred$obs)
sslevels <- unique(c(levels(pred$pred),levels(pred$obs)))
pred$pred<- factor(pred$pred,levels = sslevels)
pred$obs<- factor(pred$obs,levels = sslevels)
CMX <- table(predicted = pred$pred,observed = pred$obs)
CMX.diag <- diag(CMX)
CMX.OMISSION <- 1-(CMX.diag/apply(CMX,2,sum))
Ave.om = mean(CMX.OMISSION,na.rm = TRUE)
CMX.COMISION <- 1-(CMX.diag/apply(CMX,1,sum))
Ave.com = mean(CMX.COMISION,na.rm = TRUE)
CMX.PCC <- sum(CMX.diag)/sum(CMX)  #Percent Correctly Classified
# estimate Kappa and Kappa (SD)
CMX.KAPPA <-PresenceAbsence::Kappa(CMX)
kap = CMX.KAPPA[1,1]; kap.sd = CMX.KAPPA[1,2]

# create a table with ommission and commision per model 
OM <- cbind(CMX.OMISSION,CMX.COMISION)
OM <- data.frame(OM)
OM <- tibble::rownames_to_column(OM, "SS")
OM <-rbind(c("Model",MODELfn,MODELfn),OM)
#write.csv(OM,paste(output.folder,"/","MODEL_OM_COM.csv",sep = ""),row.names = FALSE) # (add if statement) if running the script from scratch write this line out 

# MAUC # this bit is not quite working yet
#response = pred$obs
#response1 = gsub("/",".",response)
#VOTE = HandTill2001::multcap(response = gsub("/",".",pred$obs),
#                             predicted = as.matrix(pred[,-c(1,2,3)]))
#mauc <- HandTill2001::auc(VOTE)

# output the model output by adding a row to a table with all imputs
sum.data <- read.csv(paste(output.folder,"/","MODEL_OUTPUTS.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)
dline = c(MODELfn,M.descrip, no.params,scale.analysis,CMX.PCC,Ave.om,Ave.com,kap,kap.sd)
sum.data <- rbind(sum.data,dline)
write.csv(sum.data,paste(output.folder,"/","MODEL_OUTPUTS.csv",sep = ""),row.names = FALSE)

# output the model omission/ commission values by adding columnsto a table with all imputs
OM.data <- read.csv(paste(output.folder,"/","MODEL_OM_COM.csv",sep = ""),header = TRUE,stringsAsFactors = FALSE)
OM.data <- left_join(OM.data,OM,by = "SS")  # might need to adjust this one to another join if there is new Site Series classifications
write.csv(OM.data,paste(output.folder,"/","MODEL_OM_COM.csv",sep = ""),row.names = FALSE)


#################################
# MAP PRODUCTION # produce a map for the current model 
#################################
data.folder <- gsub("*D_|_pts.*","", file1)

folder = paste(layer.folder,data.folder,"layers/",sep = "/") # create the filepath where the layers are stored (use the same scale as points extracted from )
list.files(folder)

rastLUTfn <- "ModelMapData_LUT.csv" # need to update this with more layers
rastLUTfn <- read.table(paste(in.folder,rastLUTfn,sep = "/"),
                        header=FALSE,
                        sep=",",
                        stringsAsFactors=FALSE)
rastLUTfn[,1] <- paste(folder,rastLUTfn[,1],sep="/")


#The function model.mapmake() creates an ascii text files and an imangine image file of predictions for each map pixel.

model.mapmake( model.obj=model.obj.ex3,
               folder=model.folder,
               MODELfn=MODELfn,
               rastLUTfn=rastLUTfn,
               na.action="na.omit")#,
               # Mapping arguments
               #map.sd=TRUE)
MODELfn

#############################################################
# Plot 1: 
l <- seq(100,0,length.out=101)
c <- seq(0,100,length.out=101)
col.ramp <- hcl(h = 120, c = c, l = l)

opar <- par(mfrow=c(1,2),mar=c(3,3,2,1),oma=c(0,0,3,4),xpd=NA)
mapgrid.a <- raster(paste(model.folder,"/",MODELfn,"_map.img",sep=""))
#mapgrid.b <- raster(paste(output.folder,"/",MODELfn.b,"_map.img",sep=""))
zlim <- c(0,max(maxValue(mapgrid.a)))
legend.label<-rev(pretty(zlim,n=23))
legend.colors<-col.ramp[trunc((legend.label/max(legend.label))*100)+1]

label.doc = read.csv(paste(model.folder,"/",MODELfn,"_map_key.csv",sep = ""))
category = label.doc$category

legend.label<-paste(legend.label,category,sep="")
image(mapgrid.a,
      col=col.ramp,
      xlab="",ylab="",xaxt="n",yaxt="n",
      zlim=zlim,
      asp=1,bty="n",main="")

mtext(response.name,side=3,line=1,cex=1.2)

legend( x=xmax(mapgrid.a),y=ymax(mapgrid.a),
        legend=legend.label,
        fill=legend.colors,
        bty="n",
        cex=1)
#mtext("Percent Cover",side=3,line=1,cex=1.5,outer=T)
par(opar)

# -------- CALCULATE THE SUMMARY TABLE FOR PROPORTION OF PREDICTED SS --------------------------------------------- ## 

#Extract all the locations within the raster and summary 
freq.ss = data.frame(freq(mapgrid.a))
freq.ss <- merge(label.doc,freq.ss, by.x = "integercode",by.y ="value" )

# all rasters squares (25m)
all.locations.25 <- sum(freq.ss$count)  # note this will change on the scale at which the predictions are being made

freq.ss <- freq.ss %>%
  dplyr:: select(category,count) %>% 
  mutate(pc_predict = round((count/all.locations.25)*100,2))

freq.ss <-rbind(c("Model",MODELfn,MODELfn),freq.ss)

# if exists statement 
map.pred <-read.csv(paste(output.folder,"/","MAP_Predict_pc.csv",sep = ""))
map.pred <- left_join(map.pred,freq.ss,by = "category")

#length(map.pred$category) 
#length(freq.ss$category)

write.csv(freq.ss,paste(output.folder,"/","MAP_Predict_pc.csv",sep = ""),row.names = FALSE)

##xx = data.frame(mapgrid.a)



