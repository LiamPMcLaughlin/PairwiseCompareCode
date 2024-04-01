################################################################################
#Code for Pairwise Ranking comparisons of two data frame column by a grouping variable 
################################################################################
#In order to run code replace these placeholders code with your data's chosen names(this can be done using the control F and replace function in R-studio)
#replace "ChosenDataframe" for your chosen dataframe
#replace "Var1" for name of first comparison column in dataframe
#replace "Var2" for name of second comparison column in dataframe
#replace "ByVar" for name of the column in dataframe containing the groups in which the test will be run (i.e c(carrots,carrots,plums,nectarines))
#results will be a dataframe with in 
#colum 1: the name of each group in ByVar
#column 2: the proportion correctly predicted
#column3: the number of observations for each grouping of ByVar compared
#For a manual use of this pairwise method please consult: Bowen, H. Leamer, E. Sveikauskas, L. (1987) "Multicountry, Multifactor Tests of the Factor Abundance Theory", The American Economic Review, 77(5), pp. 791-809
#Available at: https://www.jstor.org/stable/1810209
################################################################################
#For questions, comments, and support regarding the code please contact liampmclaughlin@gmail.com
################################################################################

results<-c()
for (j in unique(ChosenDataframe$ByVar)){
  holder<-ChosenDataframe[ChosenDataframe$ByVar==j,]
  holder<-holder|>drop_na()
  holder$Porder<- rank(holder$Var1)
  holder$Vorder<- rank(holder$Var2)
  leadedge<-c()
  propbind<-c()
  for (i in 1:length(holder$Porder)) {
    indiv<-(holder$Porder[i]<= holder$Porder & holder$Vorder[i]<=holder$Vorder | holder$Porder[i]>= holder$Porder &  holder$Vorder[i]>=holder$Vorder)
    position<-c(1:length(holder$Porder))
    position<-position[position>i]
    indiv<-indiv[position]
    leadedge<-append(leadedge,indiv)
  }
  proportioncorrect<-sum(leadedge)/length(leadedge)
  name<-c(j)
  number<-c(length(holder$Porder))
  propbind<-data.frame(name,proportioncorrect,number)
  results<-rbind(results,propbind)
}
weighted<-results$proportioncorrect*results$number
name<-c("weighted avg proportion")
proportioncorrect<-sum(weighted)/sum(results$number)
number<-sum(results$number)
weightedbind<-data.frame(name,proportioncorrect,number)
results<-rbind(results,weightedbind)
results <- with(results,  results[order(name) , ])
results
