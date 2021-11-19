###################Exercise 9##################
#John Kane

#Write a function that takes directory dir as an argument
#Function should read each file from directory dir and calculate the coefficient of
#variation (sd/mean)


CoVar<-function(dir,sep,col){
  #we can define our directory name, the delimiter of the files, and the column we want to look at
  
  #vector to store the output of our function
  CoVarOutput<-numeric(length(files))
  
  for(i in dir){
    #Before calculating coefficients, we want to see if our file has NAs
    isNA<-c(is.na(dir[i]))
    #If the file does contain NA's, we want to have the option to remove them
    if(isNA==TRUE){
      NAerror<-readline(prompt = "Your data contains NAs. Would you like to remove them? Y/N")
      if(NAerror=="Y"){
        y<-x[na.omit(i)] #this will remove NA's
        if (length(y)>49){
          mean<-mean(i)
          stdev<-sd(i)
          coeff<-stdev/mean
          CoVarOutput[i]<-coeff
        }else if(length(y)<50){
          warning<-readline(prompt = "Warning: fewer than 50 observations. Would you like to continue? (Y/N)")
          #They can select to use this file even though it is recommended to have 50 lines
          if (warning=="Y"){
            mean<-mean(i)
            stdev<-sd(i)
            coeff<-stdev/mean
            CoVarOutput[i]<-coeff
            #Or they can select to not run this function
          }else if(warning=="N "){
            return("Function canceled")
            CovarOutput<-"Function not run"
            #Add this in case they don't select Y or N
            #If I didn't add this anything other than Y would default to N
          }else{
            return("Please select either Y or N")
          }
        }
        #We also have the option to leave the NA's in the file
        #We can repeat the function above.
      }if(NAerror==FALSE){
        if (length(y)>49){
          mean<-mean(i)
          stdev<-sd(i)
          coeff<-stdev/mean
          CoVarOutput[i]<-coeff
        }else if(length(y)<50){
          warning<-readline(prompt = "Warning: fewer than 50 observations. Would you like to continue? (Y/N)")
          #They can select to use this file even though it is recommended to have 50 lines
          if (warning=="Y"){
            mean<-mean(i)
            stdev<-sd(i)
            coeff<-stdev/mean
            CoVarOutput[i]<-coeff
          }else if(warning=="N "){
            return("Function canceled")
          }else{
            return("Please select either Y or N")
          }
          
        
    #This is if there are no NAs in the original file  
    }else{
      if (length(y)>49){
        mean<-mean(i)
        stdev<-sd(i)
        coeff<-stdev/mean
        CoVarOutput[i]<-coeff
      }else if(length(y)<50){
        warning<-readline(prompt = "Warning: fewer than 50 observations. Would you like to continue? (Y/N)")
        #They can select to use this file even though it is recommended to have 50 lines
        if (warning=="Y"){
          mean<-mean(i)
          stdev<-sd(i)
          coeff<-stdev/mean
          CoVarOutput[i]<-coeff
          #Or they can select to not run this function
        }else if(warning=="N "){
          return("Function canceled")
          CovarOutput<-"Function not run"
          #Add this in case they don't select Y or N
          #If I didn't add this anything other than Y would default to N
        }else{
          return("Please select either Y or N")
        }
      }
    }
  }
    }}}

#All of the coefficients of variation from the files in dir will be placed in the 
#vector CovarOutput

#This is a lot of repetitive code, but it basically runs through this code 3 times
#Once if there are NAs in the data and you would like to remove them
#Once if there are NAs and you don't want to remove them
#And finally, if the data does not contain NA

#This function can easily be modified because the arguments are dir, sep, and col
#This allows you to specify the directory you want to work in, what is the delimiter of the files,
#and what specific column do you want to look at?
