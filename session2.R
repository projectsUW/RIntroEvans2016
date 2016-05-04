# 0. Super Basic R

# FIND OUT WHERE YOU ARE (and set it correctly!)

# A. OBJECTS ----
# AN object that stores anything:
x = 32 # x is an object storing the values 32
x

mydearObject = "Hello" # chooses any name for your object!
                       # ...so use a descriptive one.
mydearObject

#delete object from memory:
rm(mydearObject)
mydearObject # gone!

# BECAREFUL WITH:rm(list = ls())

# B. DATA STRUCTURES ----

# B.1. VECTOR: homogeneous elements----
myFirstVectorEver=c(1,2,3)
myFirstVectorEver
mySecondVector=c("a","b","c")
mySecondVector
myThirdVector=c(1,2,m)
myThirdVector #error
myThirdVector=c(1,2,"m")
myThirdVector #error?

# accesing the vector:
myFirstVectorEver[2]
# altering the vector:
myFirstVectorEver[2]=2000
myFirstVectorEver

# B.2. LIST: a flexible container ----
myNewList=list(1,2,"3")
myNewList
myDataInList=list(name="Peter",age=3)
myDataInList
myDataInVector=c(name="Peter",age=3) 
myDataInVector

# accessing the list:
aList=list(1,2,3,c(1,2))
aList[[1]] #notice double bracket
aList[[1]]=11
aList[[4]] # this is a vector
aList[[4]][1] #getting a vector element
myDataInList$name # if list has named fields
myDataInVector$name # not here!!
myDataInVector["name"] # Here it works!!

# B.3. DATAFRAME ----

# it is simple a list of vectors, the most similar to your Excel data: 

names=c("Qing", "Françoise", "Raúl", "Bjork")
ages=c(32,33,28,30)
country=c("China", "Senegal", "Spain", "Norway")
education=as.factor(c("Bach", "Bach", "Master", "PhD"))

students=data.frame(names,ages,country,education,stringsAsFactors=F)
students

# BASIC OPERATIONS ON A DATA FRAME:
summary(students)
mean(students$ages)

students[1] # avoid this
students[,1] # to get columns
students[1,]
students[1,1]
students[2:3,]

# oldest?
students[which.max(ages),] # which.min available!

#younger than average?
average=mean(students$ages)
students[ages<average,]

#from Norway?
students[country!="Norway",] 

#from ...?
DangeourousPlaces=c("Peru", "USA", "Spain")
students[country==DangeourousPlaces,] # not realiable?
students[country %in% DangeourousPlaces,] # better

# C. Importing data:----
library(haven)  # install it first

#get an spss file:
testSpss= read_sav("data/PUMS_527.sav")

is.data.frame(testSpss) # yes
sort(names(testSpss)) # easier to see

# smaller data frame
varsNeeded=c("SEX","PERNP")
subsetSpss=testSpss[,varsNeeded] # rm(testSpss)??

summary(subsetSpss$PERNP) #person earnings
summary(subsetSpss$SEX) #person SEX
summary(as.factor(subsetSpss$SEX)) #person SEX (1 male)
tapply(subsetSpss$PERNP, as.factor(subsetSpss$SEX), mean,na.rm=T)
t.test(subsetSpss$PERNP~ as.factor(subsetSpss$SEX))

#equal means?
t.test(subsetSpss$PERNP~ as.factor(subsetSpss$SEX))$p.value>0.05

testStata=read_dta("data/labdata.dta") # changed to version 13!
names(testStata)
head(testStata)

lm(mpg~weight,data=testStata)

results=lm(mpg~weight,data=testStata)
summary(results)



# I. Data Collection Stage  ----

# R has basic functions, but sometimes needs 
# external functions to achieve a particular goal.
# In this case, we need data from a wikipedia page,
# which is organized in a table.

# I.1. Installing packages  ----
# For that purpose, we will first INSTALL a 'package' that
# that would make the scrapping process easier:
install.packages("rvest")

# I.2. Activating packages  ----
# Installation only means that R has more functions, 
# but they will not work unless you call them:
library(rvest)    

# I.3. Data Source  ----
# The wikipedia page holds the details of the Index of Press Freedom 
# for world countries. First create an 'R Object' that 
# holds the wikipage url: 
pressURL <- "https://en.wikipedia.org/wiki/Press_Freedom_Index"

# I.4. Get the data - dirty  ----
# With the help of rvest functions you will 'read the html'
# from the page, and get a particular piece (node) once you have it.
# As R mostly works sequentially, each step can be chained with the
# symbol "%>%". The object to left of "<-" (or "=") will receive the output.
library(magrittr)

urlTables <- pressURL %>% read_html %>% html_nodes("table")
# equals:
urlTables = pressURL %>% read_html %>% html_nodes("table")
# equals:
urlTables = html_nodes(read_html(pressURL),"table")

# I.5. Cleaning the Data collected (intro)----

# Now we have the tables, but for sure they will NOT be ready for analysis.
# If you see the wikipage, you will see the table needed, 
# but there maybe other information saved as tables:
length(urlTables)  # How many tables?

urlTables # take a look! (wikitable sortable?)


# We need the fourth table, so let's read it:
html_table(urlTables[3])

# then:
dirtyData=html_table(urlTables[3])

# Let's try to see the first rows:
head(dirtyData)

# I.5.1. Getting a data frame ----
# The command "head()" should give you only a few ROWS, 
# but that is not what happened (you got all!). 
# You need to see WHY that happened.
# So, let's check the structure:
str(dirtyData)

# If you scroll to the begining of the result, 
# you will see that you have not a dataframe yet, 
# but a LIST. 
is.data.frame(dirtyData)
is.list(dirtyData)

#The Data frame is the first element in the list:
pressDataDirty=dirtyData[[1]]
is.data.frame(pressDataDirty)

# So now we are better. Let's see what variables (column names) we have:
names(pressDataDirty)


# I.5.2. Subsetting a data frame ----

#Let's practice some code with the country names and last year available (2016), 
# that is, the first and second column:
pressDataDirty2016=pressDataDirty[,c(1,2)]
head(pressDataDirty2016)
 
# The first rows reveal dirty var names, and dirty cells.
# Let's change the var name
names(pressDataDirty2016)[2]=c("press2016")


# I.5.3. Changing values of var ----

# I.5.3.1 EXAMPLE - Changing values of var ----

# I.5.3.1.a. EXAMPLE of SPLITTING - Changing values of var ----
# To change the values in the cells, let's go step by step:
## Understanding how to split a value:
x="hghg\n999"
x
strsplit(x, "\n")[[1]][2] #how many things happened???

# details:
strsplit(x, "\n") 
strsplit(x, "\n")[[1]]
strsplit(x, "\n")[[1]][2]

# I.5.3.1.b. EXAMPLE of FOR-LOOP - Changing values of var ----

## "for" for visiting and showing what are the elements:

for (value in pressDataDirty2016$press2016){
  print (value)
}

## "for" for visiting, transforming and showing:
for (value in pressDataDirty2016$press2016){
  temporal=strsplit(value, "\n")[[1]][2]
  print (temporal) # am I printing numbers??
}

# I.5.3.2 REAL THING - Changing values of var ----

# Now let's clean those cells:
scores=c()  #empty vector!
for (value in pressDataDirty2016$press2016){
  temporal=strsplit(value, "\n")[[1]][2]
  scores=c(scores,temporal) # updating 'scores' vector
}

# we have...
scores


# Now let's create a data frame:

# pressDataDirty2016$Country is a vector:
is.vector(pressDataDirty2016$Country)
# scores is a vector:
is.vector(scores)

# So, data frame is created like this:
press2016=data.frame(country=pressDataDirty2016$Country,press2016=scores)

# are the scores numbers?
head(press2016)
str(press2016)  #NOOOOO!...worse: they are factors!!

# check:
is.numeric(press2016$press2016) 
is.character(press2016$press2016) 
is.factor(press2016$press2016) 

# Which one do you want?
as.numeric(press2016$press2016) # where did the decimals go?
as.character(press2016$press2016) # Can apply arithmetics here?
as.numeric(as.character(press2016$press2016))

# then:
press2016$press2016 = as.numeric(as.character(press2016$press2016))

# I.6. Cleaning the Data collected (NO intro)----

# subsetting:
subsetPress=pressDataDirty[,-c(1)] # all but the first column


# I.6.1. Nested LOOP----

#understand these:
nrow(subsetPress)
ncol(subsetPress)
seq_along(subsetPress)

## Simple loop, this visits column indexes
for (index in seq_along(subsetPress)){
  print (index)
}

## nested loop, it visits the indexes, and
## the current index is used in another FOR:
for (index in seq_along(subsetPress)){
  # index to access a column
  for (value in subsetPress[,index]){
    print(value) #cell for the current column
  }
}

## Now use the previous algorithm

for (index in seq_along(subsetPress)){
  tempScore=c()
  for (value in subsetPress[,index]){
    tempValue= strsplit(value, "\n")[[1]][2]
    tempScore=c(tempScore,tempValue)
  }
   subsetPress[,index]=tempScore
}
# What do we have now?
head(subsetPress)

# ARE we over yet?
## What about the names of the countries
pressDataDirty[,1]


# I.6.2. Text cleaning----

## The leading and trailing 'spaces':

## example:
x <- "  Some text. "
x
trimws(x)

# THEN:
pressDataDirty[118,1]
trimws(pressDataDirty[118,1])  # working?

#install.packages("stringr", dependencies=TRUE)
require(stringr)
str_trim(pressDataDirty[118,1]) # working?
str_trim(pressDataDirty[,1]) # working?


## Extra text: getting rid of the footnote calls:
strsplit(pressDataDirty[,1],"\\[") # try omitting "\\"?



# I.6.7. Making a function----
## REMEMBER?
# scores=c()  #empty vector!
# for (value in pressDataDirty2016$press2016){
#   temporal=strsplit(value, "\n")[[1]][2]
#   scores=c(scores,temporal)
# }

## LET'S make a function!

keepSide=function(aVector, side,splitter){
  require(stringr) # why?
  cleanVector=c()  #empty vector!
  for (value in aVector){
    almostCleanVal=strsplit(value, splitter)[[1]][side]
    CleanVal=str_trim(almostCleanVal)
    cleanVector=c(cleanVector,CleanVal)
  }
  return (cleanVector)
}
##Isn't this nice?:
keepSide(pressDataDirty[,1],1,"\\[")

## Then,
pressDataDirty[,1]=keepSide(pressDataDirty[,1],1,"\\[")

## WE can use that for the ugly names:
names(subsetPress) #dirty

keepSide(names(subsetPress),1,"\n") #clean!

paste0("press",keepSide(names(subsetPress),1,"\n")) # more appropriate!

# changing:       
names(subsetPress)=paste0("press",keepSide(names(subsetPress),1,"\n"))

#clean DF:
pressDataCleanAll=data.frame(country=pressDataDirty[,1],subsetPress,stringsAsFactors = F)
head(pressDataCleanAll)

### Are we over???
str(pressDataCleanAll)

### Let's finish this using "lapply":
pressDataCleanAll[,-c(1)]=lapply(pressDataCleanAll[,-c(1)], as.numeric)

# HERE IT IS:
str(pressDataCleanAll)

# I.7. All in one piece----
library(rvest)  
pressURL = "https://en.wikipedia.org/wiki/Press_Freedom_Index"
#pressData = pressURL %>% read_html %>%html_nodes("table")%>% 
#            extract(3)%>%extract2(1)%>%html_table
pressData = html_table(html_nodes(read_html(pressURL),"table")[3][[1]])

#then
for (index in seq_along(pressData)){
  #pressData[,index]= str_trim(pressData[,index])
  if (index==1){
    pressData[,index]=keepSide(pressData[,index],1,"\\[")
    pressData[,index]=as.character(pressData[,index])
  }
  else{
    pressData[,index]=keepSide(pressData[,index],2,"\n")
    pressData[,index]=as.numeric(pressData[,index])
  }
}
newnames=paste0("press",keepSide(names(pressData)[-c(1)],1,"\n"))
names(pressData)[-c(1)]=newnames

## JUST CONFIRMING!
head(pressData)
str(pressData)

# II. Data Integration Stage  ----

# II.1 Keys for integration  ----

# In order to combine different sources, 
# you need to be sure there is a common column among 
# tables, with unique values.
# It is always better you use an standard "key".
# In this case, instead of country names, we should use 
# an ISO code. 

# I will need another file (external):
isoPress=read.csv("pressKey.csv",na.strings = "",stringsAsFactors=F)

names(isoPress) # what's here?

nrow(isoPress)==nrow(pressData)  #same size??

# integrating:
pressDataISO=merge(pressData,isoPress,
                   by.x="Country", by.y="countryName", 
                   all.x=T)

nrow(pressDataISO)==nrow(pressData)  # as requested in all.x=T

names(pressDataISO) # new column added!

#great now let's keep just 2015:
pressIndex=pressDataISO[,c("iso3","Country","press2015")]
head(pressIndex)

#are there missing values?
nrow(pressIndex[complete.cases(pressIndex),])==nrow(pressIndex)

# what are those missing places?
pressIndex[!complete.cases(pressIndex),]
## ok, let's keep that in mind....

# II.2 Integrating  ----

# In the folder "data" we have two files that also have indexes.
# "idi.csv" has the IT development,and "ief.csv" has the economic freedom.
# the way they wrote country names is different, but fortunately, 
# they have a column with the ISO code.

# II.2.1 Getting multiple files  ----
# let's get the csv files in the folder:

nameOfFilesToIntegrate =list.files("data", pattern="*.csv", full.names=TRUE)
nameOfFilesToIntegrate # the name and the folder (full.names=TRUE)

# apply "read.csv" to the filenames!...
contentsAsList <- lapply(nameOfFilesToIntegrate, read.csv) # this works
# BUT  this is better:
contentsAsList <- lapply(nameOfFilesToIntegrate, read.csv,na.strings = "",stringsAsFactors=F)

#checking:
is.data.frame(contentsAsList)
is.list(contentsAsList) # a list of data frames!


#dataframe 1?
is.data.frame(contentsAsList[[1]])

#check
names(contentsAsList[[1]])
names(contentsAsList[[2]])

# II.2.2 Merging multiple files  ----

# adding one to the list
contentsAsList[[3]]=pressIndex

#just to know the status of files sizes:
lapply(contentsAsList,nrow)

# missingness per DF:
contentsAsList[[1]][!complete.cases(contentsAsList[[1]]),]
contentsAsList[[2]][!complete.cases(contentsAsList[[2]]),]
contentsAsList[[3]][!complete.cases(contentsAsList[[3]]),]


# MUST GET RID OF MISSING IN "KEYS"! 
#remember this?
x="iso3"
contentsAsList[[3]]$x #??

contentsAsList[[3]][,x]

# here is the merging function:

multipleMerger=function(listOfDFs,key){ # set the resulting DF as the first DF.
    resultingDF=listOfDFs[[1]][complete.cases(listOfDFs[[1]][,key]),]
    for (DF in listOfDFs[-1]){ #let's visit the DFs, but not the first one
      DF=DF[complete.cases(DF[,key]),] # make sure the key column is not empty
      resultingDF=merge(resultingDF,DF,all = TRUE)  # all = TRUE
    }
    return (resultingDF)
}

# Let's make it work:
allIndexes=multipleMerger(contentsAsList)
is.data.frame(allIndexes)
nrow(allIndexes)  # remember: lapply(contentsAsList,nrow)


# check missing (verify ISO3):
allIndexes[!complete.cases(allIndexes$iso3),] # 0 rows!!!

#what do we have now?
names(allIndexes)

#country names repeated?
allIndexes[!complete.cases(allIndexes),][,c(2,4,8)] # not really

# maybe we can have one column with all the country names available
allIndexes$country= ifelse(is.na(allIndexes$country),allIndexes$Country.Name,allIndexes$country)
allIndexes$country= ifelse(is.na(allIndexes$country),allIndexes$Country,allIndexes$country)

#did it work?
allIndexes[!complete.cases(allIndexes),]$country  # yes!!

#bye extra columns:
allIndexes=allIndexes[,-c(4,8)]
names(allIndexes)

# write.csv(allIndexes,"data/worldIndexes.csv",row.names=F)


# III. Exploring data ----

# Exploration is different according to data scale (nominal, ratio, etc)
str(allIndexes)

# III.1 Exploring categorical data ----
# Categorical admit count:
table(allIndexes$Region)

# basic plot:
barplot(table(allIndexes$Region))
barplot(sort(table(allIndexes$Region)))
barplot(sort(table(allIndexes$Region)),cex.names=0.3,las=2)

# converting to factor for better plotting:
allIndexes$Region=as.factor(allIndexes$Region)
levels(allIndexes$Region)
levels(allIndexes$Region)=c("Asia-Pacific","Europe","Middle East and \n North Africa",
                            "North America", "South/Central \n America and \n Caribbean",
                            "Sub-Saharan \n Africa" )

barplot(sort(table(allIndexes$Region)),cex.names=0.5,las=2,col="red")

pie(table(allIndexes$Region))

# III.2 Exploring numerical data ----

summary(allIndexes[,c(3,6,7)])

boxplot(allIndexes[,c(3,6,7)])  # different Units!!

library("scales")
allIndexesNUM=lapply(allIndexes[,c(3,6,7)], rescale,to=c(0,1))
boxplot(allIndexesNUM) # normalized

boxplot(scale(allIndexes[,c(3,6,7)])) # standardized

# III.3 Exploring combinations ----

plot(allIndexes$ief2015,allIndexes$idi2015)
plot(scale(allIndexes$ief2015),scale(allIndexes$idi2015))

#setting colors I  WANT to use:
mycols=c("red","lightblue","green","orange","magenta","black")
plot(scale(allIndexes$ief2015),scale(allIndexes$idi2015),col=mycols,pch=16,xlab = "ief",ylab = "ict")
legend(x="topleft", 
       legend = levels(allIndexes$Region), 
       col=mycols, pch=16,cex=0.5)

# time for the grammar of graphics
library(ggplot2)
basicPlot <- ggplot(allIndexes, aes(ief2015, idi2015)) # you say what to use

#see it
basicPlot  + geom_point() # you say HOW to plot

#more variety
basicPlot  + geom_point((aes(colour = Region)))  # give color to point
basicPlot  + geom_point((aes(colour = Region,size = press2015))) # give color and size to point
basicPlot  + geom_point((aes(size = press2015))) + facet_grid(. ~ Region) # size and divide

# version just complete cases (no more warnings)
allIndexesnoNA=allIndexes[complete.cases(allIndexes),]
basicPlotNA <- ggplot(allIndexesnoNA, aes(ief2015, idi2015))
basicPlotNA  + geom_point((aes(size = press2015))) + facet_grid(. ~ Region)


# with grand mean:
basicPlotNA  + geom_point((aes(size = press2015))) +  facet_grid(. ~ Region) + 
               geom_hline(yintercept = mean(na.omit(allIndexes$idi2015))) # this is NEW!!

### more complex
# want to plot the grand mean and the group mean!!

#get mean by group
means_IDI_facets <- tapply(allIndexesnoNA$idi2015,allIndexesnoNA$Region,mean)

#transform as data frame
means_IDI_facets=as.data.frame(means_IDI_facets)
names(means_IDI_facets)=c("idi2015_facets") #change name
means_IDI_facets$Region <- as.factor(rownames(means_IDI_facets)) #index as column
rownames(means_IDI_facets) <- NULL #erase old index

# now plot!
basicPlotNA  + geom_point((aes(size = press2015))) + facet_grid(. ~ Region) + 
  geom_hline(yintercept = mean(allIndexesnoNA$idi2015)) +
  geom_hline(aes(yintercept =idi2015_facets),means_IDI_facets) # this is new

# nicer:
basicPlotNA  + geom_point((aes(size = press2015))) + facet_grid(. ~ Region) + 
  geom_hline(yintercept = mean(allIndexesnoNA$idi2015),col="red") +
  geom_hline(aes(yintercept =idi2015_facets), means_IDI_facets,col="blue")



# IV. Basic Multivariate Analytics

#clusters
allIndexesnoNA[,c(3,6,7)]=scale(allIndexesnoNA[,c(3,6,7)])
clusterResult=kmeans(allIndexesnoNA[,c(3,6,7)],3)
allIndexesnoNA$cluster=clusterResult$cluster # add cluster to DF

# Mapping Data
library(maptools)
map <- readShapeSpatial("data/worldMap/worldMap.shp")
names(map@data) # ISO3 is there!!

# all.x=T is very important. You can not add rows to the map (all.x=T)
all=merge(map,allIndexesnoNA, by.x="ISO3", by.y="iso3",all.x=T)

#version 1
# ugly but informative
quartz()
library(rgeos) # for "polygonsLabel"
plot(all,col=all@data$cluster) #using cluster info to color!!
polygonsLabel(all, labels=all@data$cluster,method = "inpolygon") # before naming cluster

#version 2
colorCluster=c("green", "red", "lightblue")  # my new colors
library(png)
logo <- readPNG("data/logo.png") # just fancy stuff
quartz()
plot(all,col=colorCluster[all@data$cluster],border=NA) 
rasterImage(logo, xleft=-159, ybottom = -49, xright = -80, ytop =-30)
legend(legend = c("green","red","lightblue"), fill = colorCluster, "topright")

#version2a
colorCluster=c("green", "red", "lightblue")
colorClusterlegend=c("green", "lightblue","red") #order wanted in legend
library(png)
logo <- readPNG("data/logo.png")
quartz()
plot(all,col=colorCluster[all@data$cluster],border=NA)
rasterImage(logo, xleft=-159, ybottom = -49, xright = -80, ytop =-30)
legend(legend = colorClusterlegend, fill = colorClusterlegend, "topright")


#version 3
colorCluster=c("green", "red", "lightblue")
colorClusterlegend=c("green", "lightblue","red")
library(png)
logo <- readPNG("data/logo.png")
quartz()
plot(all,col=colorCluster[all@data$cluster],border=NA,main="Evans Categorization of World Development")
rasterImage(logo, xleft=-159, ybottom = -47, xright = -80, ytop =-28)
legend(x= -159, y= -49,legend = colorClusterlegend, fill = colorClusterlegend,cex=0.6)
library(maps)
map.scale(-12,-70,ratio=T, relwidth=0.2,metric=T,cex=0.5)

#subsetting the map
levels(all@data$Region)
CSamerica=all[all@data$Region %in% "South/Central \n America and \n Caribbean",]

plot(CSamerica,col=colorCluster[CSamerica@data$cluster],
     border="grey",
     main="Evans Categorization of World Development")
mtext("Caribbean, Central and South America")
polygonsLabel(CSamerica, labels=CSamerica@data$country,method = "inpolygon",cex=0.5)
legend("left",legend = colorClusterlegend, fill = colorClusterlegend,cex=0.6)
library(maps)
map.scale(ratio=T, relwidth=0.2,metric=T,cex=0.5)
library(GISTools)
north.arrow(xb=-50, yb=-46, len=1, lab="N",col='grey')
detach("package:GISTools", unload=TRUE)

#More modern! - 1
library(leaflet)

# define colors
pal <- colorNumeric(
  palette = "Blues",
  domain = CSamerica@data$idi2015
)
#plot
leaflet(CSamerica) %>%
  addTiles() %>%
  addPolygons(color = ~pal(idi2015),
              stroke = FALSE) 

#More modern! - 2

leaflet(CSamerica) %>%
  addTiles() %>%
  addPolygons(color = ~pal(idi2015),
              stroke = FALSE) %>%
  addLegend("bottomright", pal = pal, values = ~idi2015,
            title = "ICT Index",
            labFormat = labelFormat(suffix = "#"),
            opacity = 1
  )


#More modern! - 3
popup <- paste0("<strong>Country: </strong>", 
                CSamerica@data$country,
                "<br><strong> ICT Index: </strong>", 
                round(CSamerica@data$idi2015,2))

leaflet(CSamerica) %>%
  addTiles() %>%
  addPolygons(color = ~pal(idi2015),
              stroke = FALSE,popup=popup) %>%
  addLegend("bottomright", pal = pal, values = ~idi2015,
            title = "ICT Index",
            labFormat = labelFormat(suffix = "#"),
            opacity = 1
  )


