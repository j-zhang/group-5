  #Program to download
# R-studio v0.97: http://www.rstudio.com/ide/download/desktop
# OR
# R v3.0.2: http://cran.r-project.org/bin/windows/base/

  #Download data on an appropriate folder
OrigData = read.csv(file="~/Documents/Fall 2013/Stats 157/Copy of Stat 157 Questionnaire (Results Redacted) - Form Responses.csv")
library(ggplot)
 
CleanData = function(data){
  
  #Cleaning up Learning Styles Column
  LearnStyle = data[,12]
  Clean = gsub("[^[:alnum:]| ]", " ", LearnStyle)
  Clean = tolower(Clean)
  Clean = gsub("  +", " ", Clean)
  Clean = gsub("[^[[:digit:]]|[[:digit:]][[:digit:]]]"," ", Clean)
  Clean = gsub("  ", " ", Clean)
  Clean = strsplit(Clean," ")
  Clean = sapply(Clean, as.numeric)
  Clean = sapply(1:48, function(x) Clean[[x]][!is.na(Clean[[x]])])
  Clean = sapply(1:48, function(x) if ( length(Clean[[x]]) == 0){
    Clean[[x]] = c(NA,NA,NA,NA)
  } else {Clean[[x]] = Clean[[x]] })
  Clean = t(Clean)
  
  #Adding in Group Roles Data and Naming Columns
  Clean = data.frame(Clean, data[,13], data[,14], data[,15],data[,16])
  colnames(Clean) = c("Visual", "Aural", "Read/Write", "Kinesthetic", "Producer", "Adminstrator", "Entrpreneur", "Integrator")
  
  #Function for adding information from columns about classes students have taken 
  AddData = function(origdata,num, data, type){
    Column = strsplit(tolower(as.character(origdata[,num])), ", ")
    charcVec = unique(unlist(Column))
    Names1 = colnames(data)
    Names2 = sapply(1:length(charcVec), function(x) paste(charcVec[x], ".", type , sep = ""))
    for (i in 1:length(charcVec)){
      a = c()
      a = sapply(1:48, function(x) if (sum(Column[[x]] == charcVec[i])==1){
        a[x]  = 1
      } else  {a[x] = 0})
      data = data.frame(data, a)
    }
    colnames(data) = c(Names1, Names2)
    return(data)
  }
  
  Clean = AddData(data, 3, Clean, "classes")
  Clean = AddData(data, 5, Clean, "personal")
  Clean = AddData(data, 6, Clean, "technical")
  
  #Deleting unnecessary columns and rows
  notna = sapply(1:48, function(x) !is.na(Clean[[x,1]]))
  allData = Clean[notna, ]
  allData = allData[c(-41,-42, -45,-47,-60,-61,-65)]  
}

Final = CleanData(OrigData)

#Dominant learning style 1 to 4 correspond to Visual to Kinesthetic
#Multimodal corresponds to 5 and occurs when the dominantStyle two or more
Styles = Final[,1:4]
learningStyle = rep(0, nrow(Final))
dominantStyle = rep(0, nrow(Final))
for (i in 1: nrow(Styles)) {
  dominantStyle[i] = max(Styles[i,1], Styles[i,2], Styles[i,3], Styles[i,4])
  learningStyle[i] = which(Styles[i,]==dominantStyle[i])
  if (sum(Styles[i,]==dominantStyle[i])>1) {
    
    learningStyle[i] = 5
  }
  else{
    learningStyle[i] = which(Styles[i,]==dominantStyle[i])
  }
}

#Vector of count of skills in numeric format
Classes = Final[, 9:28]
classCount = rowSums(Classes)

Personal = Final[, 29:43]
personalCount = rowSums(Personal)

Technical = Final[, 44:58]
technicalCount = rowSums(Technical)

#Linear regression and correlation
plot(classCount, personalCount)
linearModel = lm(personalCount ~ classCount)
abline(linearModel)
cor(classCount, personalCount)


##Visualization## 
learningStylenName<-c('Visual','Aural','Read/Write','Kinesthetic','Multimodal')
cex <- 4*technicalCount
colour<- as.factor(learningStyle)
plot(classCount, personalCount,pch=16,cex=cex,xlim=c(0,10),ylim=c(0,12),col=adjustcolor(colour,0.6),
     main='Traits of Technical Leads',xlab='Number of Topics (via classes taken)',ylab='Number of Topics (via  personal experience)')
linearModel = lm(personalCount ~ classCount)
abline(linearModel)
legend(x=-.4,y=12.5,legend=learningStylenName,fill=levels(colour),title='legend',cex=0.7)
##cor(classCount, personalCount)##

