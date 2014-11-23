## Prerequisites: setwd to directory where data was unzipped
## library(dplyr) library(reshape2)

run_analysis<-function(){
    ## Reading data
    X_Test<-read.table("./test/X_test.txt",sep="")
    Y_Test<-read.table("./test/Y_test.txt",sep="")
    Subj_Test<-read.table("./test/subject_test.txt",sep="")
    
    X_Train<-read.table("./train/X_train.txt",sep="")
    Y_Train<-read.table("./train/Y_train.txt",sep="")
    Subj_Train<-read.table("./train/subject_train.txt",sep="")
    
    act_labels<-read.table("./activity_labels.txt",sep="")
    feat_list<-read.table("./features.txt",sep="")
    
    ## Creating raw dataset
    
    Y_Train<-cbind(Y_Train,Subj_Train)
    Y_Test<-cbind(Y_Test,Subj_Test)
        
    X_Tst<-tbl_df(X_Test)
    X_Trn<-tbl_df(X_Train)
    Y_Tst<-tbl_df(Y_Test)
    Y_Trn<-tbl_df(Y_Train)
    names(Y_Tst)<-c("Activity","Subject")
    names(Y_Trn)<-c("Activity","Subject")
        
    X_Names<-as.vector(feat_list[,2])
    X_Names<-make.names(X_Names,unique=TRUE)
        
    names(X_Tst)<-X_Names
    names(X_Trn)<-X_Names
    
    ## Selecting columns with mean and standard deviation
    
    X_Tst<-select(X_Tst,matches("mean|std)"))
    X_Trn<-select(X_Trn,matches("mean|std)"))
    
    ## Binding data together
    
    X_Tst<-cbind(X_Tst,Y_Tst)
    X_Trn<-cbind(X_Trn,Y_Trn)
    X_All<-rbind(X_Tst,X_Trn)

    ## Make activity names descriptive
    
    names(act_labels)<-c("Activity","Activity_Desc")
    X_All<-left_join(X_All,act_labels,by="Activity")
    X_All<-select(X_All,-Activity)
    
    ## Create tidy data
    melted = melt(X_All, id.var = c("Subject", "Activity_Desc"))
    means = dcast(melted , Subject + Activity_Desc ~ variable,mean)
    
    ## Write tidy data to space separated text file
    
    write.table(means, file="TidyData.txt", sep = " ", row.names = FALSE)
}