#has normalization calculations and code to plot mean and best fitness graphs across generations
#
require(nsga2R)

library(readxl)
library(writexl)
#library(ggplot2)

#THE STUFF BELOW WILL ALWAYS HAVE TO BE DONE THE FIRST TIME A MATRIX IS LOADED, THEN NEVER AGAIN, SO UNCOMMENT IF/WHEN NEEDED

prctbl <- as.data.frame(read_xlsx("DemoPrecMat25-1.xlsx", sheet = "Sheet1")) #reading original matrix
prctbl

#
prctbl1 <- data.frame(
  element <- prctbl["element"],
  elemguid<-prctbl["objguid"],
  elemindex <- prctbl["elemindex"],  #indexes here are not necessary bc not case sensitive search
  volelement <- prctbl["volelement"],
  precedent <- prctbl["precedent"],
  precguid<-prctbl["precguid"],
  precindex <- prctbl["precindex"],
  volpreced <- prctbl["volpreced"],
  
  # prodTeam = c (0.3, 0.0000004),
  # costTeam = c(25, 9000000),
  # row.names = c("A","B"),
  stringsAsFactors = FALSE
)
ee<-prctbl1


uniquelementdf <-data.frame(                   #generating case sensitive unique list in r
  uniqueindex <- c(prctbl1[,"elemindex"],prctbl1[,"precindex"]),
  elementguid <- c(prctbl1[,"objguid"],prctbl1[,"precguid"]),
  ifcelement <- c(prctbl1[,"element"],prctbl1[,"precedent"]),
  volelement <- c(prctbl1[,"volelement"],prctbl1[,"volpreced"]),
  
  
  stringsAsFactors = FALSE
)

colnames(uniquelementdf)<-c("uniqueindex", "uniqueguid", "ifcelement", "vol")

uniquelementdf <- uniquelementdf[!duplicated(uniquelementdf$uniqueindex), ]  #remove duplicates (by index) leaving uniques only

# uniquelementdf <- uniquelementdf[!duplicated(uniquelementdf$uniqueguid), ]  #remove duplicates (by guid) leaving uniques only
# #uniquelementdf <- uniquelementdf[order(uniquelementdf$uniqueguid), ]    #sort uniques in order
# uniquelementdf
# 
# write_xlsx(uniquelementdf, "D:\\BIM A+ 2023\\BIM A+ 7\\BIM A+ 7 Thesis\\IFCProblem\\IFCScheduleProject\\uniquelementdf2.xlsx")
# write_xlsx(chrom, "D:\\BIM A+ 2023\\BIM A+ 7\\BIM A+ 7 Thesis\\IFCProblem\\IFCScheduleProject\\debug3.xlsx")
# #writing to new xlsx. after this, use case sensitive vlookup formula in excel 
# # =VLOOKUP(TRUE,CHOOSE({1,2},EXACT(uniqueindex2!$A$2:$A$291,B2),uniqueindex2!$B$2:$B$291),2,FALSE)
# #and don't forget it is an array formula so ctrl+shift+enter
# 
# 
# 
# uniquelementdf <-data.frame(
# elemindex <- c(prctbl1[,"elemindex"],prctbl1[,"precindex"]),
# ifcelement <- c(prctbl1[,"element"],prctbl1[,"precedent"]),
# volelement <- c(prctbl1[,"volelement"],prctbl1[,"volpreced"]),
# 
# stringsAsFactors = FALSE
# ) 
# 
# colnames(uniquelementdf)<-c("uniqueindex", "ifcelement", "vol")
# 
# uniquelementdf <- uniquelementdf[!duplicated(uniquelementdf$uniqueindex), ]  #remove duplicates leaving uniques only
# uniquelementdf <- uniquelementdf[order(uniquelementdf$uniqueindex), ]    #sort uniques in order
# uniquelementdf
# 
# 
# #END OF FIRST TIME RUN STUFF (already fixed the matrix in excel once so don't need to do it again)

# prctbl <- as.data.frame(read_xlsx("demolitionpm.xlsx", sheet = "Matrix2"))  #import xrefed matrix with index back into r
# prctbl


prctbl1 <- data.frame(                       #create final df with indexes and with unecessary stuff removed
  
  elemindex <- prctbl["elemindex"],
  element <- prctbl["element"],
  #elemguid<-prctbl["objguid"],
  volelement <- prctbl["volelement"],
  precindex <- prctbl["precindex"],
  precedent <- prctbl["precedent"],
  #precguid<-prctbl["precguid"],
  volpreced <- prctbl["volpreced"],
  stringsAsFactors = FALSE
)
ee<-prctbl1

# uniquelementdf <- as.data.frame(read_xlsx("demolitionpm.xlsx", sheet = "uniqueindex2"))  
# uniquelementdf

noelem<-length(uniquelementdf[,1])

# t <- data.frame(
#   prodTeam = c (0.3, 0.4, 0.35, 0.5, 0.45, 0.55, 0.42, 0.38, 0.28, 0.48, 0.25, 0.32, 0.3, 0.4, 0.35, 0.5, 0.38, 0.28, 0.48, 0.25 ),
#   costTeam = c(25, 30, 60, 50, 55, 28, 35, 38, 45, 40, 52, 28, 60, 50, 55, 28, 35, 38, 25, 30 ),
#   row.names = c("A","B", "C", "D" , "E", "F","G", "H", "I" , "J", "K" , "L", "M", "N", "O", "P", "Q", "R", "S", "T"),
#   
#   # prodTeam = c (0.3, 0.0000004),
#   # costTeam = c(25, 9000000),
#   # row.names = c("A","B"),
#   stringsAsFactors = FALSE
# )
# t

t<- as.data.frame(read_xlsx("teams.xlsx", sheet = "demoteams4"))
row.names(t)<-c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
t

funct<-function(x){
  chrom <- as.data.frame(round(matrix(x,nrow=(length(uniquelementdf[,1])),ncol=(length(t[,1])))))
  chromunrp2<-chrom   #unrepaired chromosome
  # print(chrom)
  # errorchrom<<-chrom
  # chrom<-as.data.frame(matrix (rep(sample(elemlistindex,length(uniquelementdf$elemindex),replace=FALSE),length(t[,1]))),
  #                             nrow=(length(elemlist)),ncol=length(t[,1]))
  # 
  # chrom<-as.data.frame(matrix(c(sample(elemlistindex,9,replace=FALSE),
  #                               sample(elemlistindex,9,replace=FALSE),
  #                               sample(elemlistindex,9,replace=FALSE),
  #                               sample(elemlistindex,9,replace=FALSE)),
  #                             nrow=(length(elemlist)),ncol=length(t[,1])) )
  #chrom<-chrominit
  # x <- runif(as.numeric((length(uniquelementdf[,1]))* length(t[,1])))
  # x <- runif(as.numeric((length(uniquelementdf[,1]))* length(t[,1])), min = 0, max = length(uniquelementdf[,1]) )
  #print("imetengeneza chrom")
  # chrom <- as.data.frame(round(matrix(x,nrow=length(uniquelementdf$elemindex),ncol=2)))
  #chrom
  
  #built list as vector
  built<-c()
  built
  builtunrp2<-built
  
  #building list as dataframe
  building <- as.data.frame(matrix(ncol=(length(chrom[1,])), nrow = (length(chrom[,1]))))
  building
  
  #remaining list as vector
  remaining<-uniquelementdf[,"uniqueindex"]
  remaining
  remainingunrp2<-remaining
  
  #element construction time as dataframe
  eltime <- as.data.frame(matrix(ncol=(length(chrom[1,])), nrow = (length(chrom[,1])))) #creating a data frame of length (no.teams) to store times
  eltime
  eltimeunrp2<-eltime
  #print("imetengeneza built, remaining na eltime")
  
  ####GENERALIZING LAST PART OF THE FUNCTION FOR +2 TEAMS
  # telltime<-as.data.frame(matrix(ncol=(length(chrom[1,])+1), nrow = (length(chrom[,1]))))
  # builtell<-c()
  
  #for (m in 1:length(telltime[,1])) {
  # if (var(unlist(telltime[i,]))!=0) {      #if all the times in row are not equal (thus variance is not 0)
  #   telltime2<-telltime
  #   telltime2[telltime2==0]<-NA
  #   
  #   if (any((telltime2[i,])>0)) {  #if there is at the very least one non-NA number
  #     p<-(which(telltime2[i,]==min(unlist(telltime2[i,]),na.rm=TRUE),arr.ind=TRUE))[1,2]  #find column position of non-NA minimum in row i
  #     builtell<-append(builtell,telltime[i,p])  #add corresponding chrom to built
  #     for (n in 1:length(telltime[1,])) {
  #       if (telltime[i,n]>0) {   #if eltime value is more than zero
  #         telltime[(i+1),n]<-(telltime[i,n] - telltime[i,p])  #calculate the difference & place all in next eltime row
  #         }  else if (telltime[i,n]==0) {   #if it is zero,
  #       telltime[(i+1),n]<-0      #next row is zero.
  #         }
  #       if (telltime[(i+1),n]==0) {
  #         telltime[(i+1),n]<-NA             #otherwise replace the zero(s) in the next row with a NA
  #       }
  #     }
  #   }
  # }
  
  
  #}
  
  #chrom<-as.data.frame(round(matrix(chrom_error_2,nrow=9,ncol=2)))
  # chrom1<-chrom
  # chrombaddd<-as.data.frame(matrix(c(3,5,7,4,1,8,2,6,9,9,8,1,7,3,6,4,5,2),nrow=9,ncol=2))
  # chrom<-chrombaddd
  
  # chrom<-as.data.frame(round(matrix(chrom_error_1,nrow=9,ncol=2)))
  # as.data.frame(round(matrix(chrom_error_1,nrow=9,ncol=2)))
  # as.data.frame(round(matrix(chrom_error_2,nrow=9,ncol=2)))
  
  
  for (i in 1:(length(chrom[,1]))) {
    for (j in 1:length(chrom[1,])) {
      # i<-3
      # j<-4
      #print(paste("tumeanza for loop", i , "," , j ))
      if  ((!(chrom[i,j] %in% built))&    #element is not in built list
           ((chrom[i,j] %in% remaining))&   #element is in remaining list
           #(is.na(eltime[i,j]))& #time slot in eltime df is empty for element ij
           (((!(chrom[i,j] %in% ee[,"elemindex"])) |    #element has no predecessor OR
             ((chrom[i,j] %in% ee[,"elemindex"]) &       #element has a predecessor AND
              (all(((ee[(which(ee[,"elemindex"] %in% chrom[i,j])),"precindex"]) %in% built)==TRUE)))) &   #predecessor is in built list
            (is.na(eltime[i,j]))) & #time slot in eltime df is empty for element ij
           (((length(elemindex)-length(built))>1) & ((length(setdiff(elemindex,built)))>1)))  {  #more than one element is unbuilt      
        #volume/productivity      #calculate the time it would take to build it (save it in df eltime)
        eltime[i,j] <-(uniquelementdf[(which(uniquelementdf[,"uniqueindex"] %in% chrom[i,j])),"vol"]/t[j,1])  
        #place the gene [i,j] in the building dataframe
        building[i,j]<-chrom[i,j]
        remaining<-remaining[!remaining==chrom[i,j]]    # remove the gene number of mintime from the remaining list
        # for (m in 1:length(eltime[,1])) {
        
        if ((any(chrom[i,]==0)) & ((length(which(chrom[i,]==0)))<=(length(chrom[i,])-1))) { #if there's at least one non-zero in chrom
          for (n in 1:length(chrom[i,])) {
            if (chrom[i,n]==0) {
              eltime[i,n]<-0                            #make eltime to be zero for all the zeros in chrom
            }
          }
        }
        
        
        if (!is.na(sum(eltime[i,]))) {    #when all the slots in the eltime row are full     NEEDS TO HAPPEN ONLY ONCE IN THE FOR n CYCLE
          for (n in 1:length(eltime[1,])) {
            eltime[(i+1),n]<-(eltime[i,n] - (min(eltime[i,][which(eltime[i,]>0)]))) 
            if (eltime[(i+1),n]<0) {
              eltime[(i+1),n]<-0
            }
          } #calculate the difference & place all in next eltime row 
          
          if (var(unlist(eltime[i,]))!=0) {      #if all the times in row are not equal (thus variance is not 0)  
            
            
            q<-(which(eltime[i,]==(min(eltime[i,][which(eltime[i,]>0)])),arr.ind=TRUE))[,2]
            
            for (r in 1:length(q)) {
              built<-append(built,chrom[i,q[r]])          #place the gene number of mintime in the built list
            }
            
            # q<-(which(eltime[i,]==(min(eltime[i,][which(eltime[i,]>0)])),arr.ind=TRUE))[1,2]   #column index of the minimum time in row i   GENERALIZE
            # built<-append(built,chrom[i,q])          #place the gene number of mintime in the built list
            # building[i,q]<-NA                 # remove the gene number of mintime from the building dataframe
            
            
            for (n in 1:length(eltime[1,])) { 
              # remaining<-remaining[!remaining==chrom[p,q]]    # remove the gene number of mintime from the remaining list
              if (eltime[(i+1),n]>0) {         #if next row's j is not zero thus is the element with longer duration(s)
                chrom[i+1,n]<-chrom[i,n]        #in the chromdf replace the gene number below so it's the same with the one above it
                building[i+1,n]<-building[i,n]   #in buildingdf replace the gene number below so it's the same with the one above it
              } else if (eltime[(i+1),n]==0) {
                eltime[(i+1),n]<-NA             #otherwise replace the zero in the next row with a NA
              } else if (eltime[(i+1),n]<0) {
                eltime[(i+1),n]<-NA
              }
            }
          } else if (var(unlist(eltime[i,]))==0){     #if the variance is zero so all the times in the row are equal
            built<-append(built,chrom[i,n])       #place gene numbers of everything in the built list
            building[i,n]<-NA              #remove gene numbers of everything from the building dataframe
            #remaining<-remaining[!remaining==chrom[i,j]]      # remove the gene number of mintime from the remaining list
          }
        }
      } else if ((length(uniquelementdf[,"uniqueindex"])-length(built))==1 & (length(setdiff(uniquelementdf[,"uniqueindex"],built)))==1 & (is.na(eltime[i,j])))  {   #if one element is remaining unbuilt
        #the above two checks ensure there's no doubled element in built list before the final element is added to the built list
        
        if (all(is.na(eltime[i,]))) {                                   #i had to put this here bc sometimes final element was building slowly over phases
          
          chrom[i,j]<-setdiff(uniquelementdf[,"uniqueindex"],built)    #place the final element in chrom[i,j]
          eltime[i,j] <-(uniquelementdf[(which(uniquelementdf[,"uniqueindex"] %in% chrom[i,j])),"vol"]/t[j,1])
          built<-append(built,chrom[i,j])          #place the gene number of mintime in the built list
          remaining<-remaining[!remaining==chrom[i,j]]  
          building[i,j]<-NA
          eltime[is.na(eltime)]<-0   #make the rest of the NAs in eltime zero
          for (i in 1:length(eltime[,1])) {       #make the rest of the genes in chrom zero
            for (j in 1:length(eltime[1,])) {
              if (eltime[i,j]==0) {
                chrom[i,j]<-0
              }
            }
          }
        }
        else {
          chrom[i,(which(!is.na(eltime[i,])))]        #which chrom corresponds to the non-na eltime (final element)  
          built<-append(built,chrom[i,(which(!is.na(eltime[i,])))])
          remaining<-remaining[!remaining==chrom[i,j]]
          eltime[is.na(eltime)]<-0   #make the rest of the NAs in eltime zero
          for (i in 1:length(eltime[,1])) {       #make the rest of the genes in chrom zero
            for (j in 1:length(eltime[1,])) {
              if (eltime[i,j]==0) {
                chrom[i,j]<-0
              }
            }
          }
          
        }
        
        
      } else if ((is.na(eltime[i,j]))) {
        k<-which(remaining %in% chrom[i,j])+1
        ok <- k-1
        
        if ( (length(k)==0) )   {
          k<-1
          ok<-length(remaining)
        }
        
        if ( (k>length(remaining)))   {
          k<-1
          ok<-length(remaining)
        }
        
        condition<-TRUE
        while (condition){   
          if (((!(remaining[k] %in% built))&    #element is not in built list AND            
               ((remaining[k] %in% remaining)) & #element is in remaining list AND
               #(is.na(eltime[i,j])) &     #time slot in eltime df is empty for element ij AND
               (!(remaining[k] %in% ee[,"elemindex"]))) |    #element has no predecessor   OR
              ((!(remaining[k] %in% built))&    #element is not in built list AND            ###ELEMENT IS UNBUILDABLE
               ((remaining[k] %in% remaining)) & #element is in remaining list AND
               #(is.na(eltime[i,j])) &    #time slot in eltime df is empty for element ij AND element's predecessor is in built list
               ((remaining[k] %in% ee[,"elemindex"]) &
                (all(((ee[(which(ee[,"elemindex"] %in% remaining[k])),"precindex"]) %in% built)==TRUE))))) {
            #place the chromossome value with the element being built
            chrom[i,j]<-remaining[k]
            #volume/productivity      #calculate the time it would take to build it (save it in df eltime)
            eltime[i,j] <-(uniquelementdf[(which(uniquelementdf[,"uniqueindex"] %in% chrom[i,j])),"vol"]/t[j,1])
            #remove from remaining
            #remaining <- remaining[-k]
            remaining<-remaining[!remaining==chrom[i,j]]
            
            if ((any(chrom[i,]==0)) & ((length(which(chrom[i,]==0)))<=(length(chrom[i,])-1))) { #if there's at least one non-zero in chrom i #PROBLEM IS HERE
              for (n in 1:length(chrom[i,])) {
                if (chrom[i,n]==0) {
                  eltime[i,n]<-0                            #make eltime to be zero for all the zeros in chrom
                }
              }
            }
            
            #print(paste("chrom","[", i,",",j,"]", "is", chrom[i,j]))
            #print(paste("condition after placing remaining in chrom is",condition))
            
            if (!is.na(sum(eltime[i,]))) {    #when all the slots in the eltime row are full     
              for (n in 1:length(eltime[i,])) {
                eltime[(i+1),n]<-(eltime[i,n] - (min(eltime[i,][which(eltime[i,]>0)]))) #calculate the difference & place all in next eltime row 
                if (eltime[(i+1),n]<0) {
                  eltime[(i+1),n]<-0
                }
              }
              if (var(unlist(eltime[i,]))!=0) {      #if all the times in row are not equal (thus variance is not 0)  
                
                
                
                q<-(which(eltime[i,]==(min(eltime[i,][which(eltime[i,]>0)])),arr.ind=TRUE))[,2]
                
                for (r in 1:length(q)) {
                  built<-append(built,chrom[i,q[r]])          #place the gene number of mintime in the built list
                }
                
                # q<-(which(eltime[i,]==(min(eltime[i,][which(eltime[i,]>0)])),arr.ind=TRUE))[1,2]   #column index of the minimum time in row i  GENERALIZE
                # built<-append(built,chrom[i,q])          #place the gene number of mintime in the built list
                # building[i,q]<-NA                 # remove the gene number of mintime from the building dataframe
                
                
                for (n in 1:length(eltime[i,])) { 
                  # remaining<-remaining[!remaining==chrom[p,q]]    # remove the gene number of mintime from the remaining list
                  if (eltime[(i+1),n]>0) {         #if next row's j is not zero thus is the element with longer duration(s)
                    chrom[i+1,n]<-chrom[i,n]        #in the chromdf replace the gene number below so it's the same with the one above it
                    building[i+1,n]<-building[i,n]   #in buildingdf replace the gene number below so it's the same with the one above it
                  } else if (eltime[(i+1),n]==0) {
                    eltime[(i+1),n]<-NA             #otherwise replace the zero in the next row with a NA
                  } else if (eltime[(i+1),n]<0) {
                    eltime[(i+1),n]<-NA
                  }
                }
              } else if (var(unlist(eltime[i,]))==0){     #if the variance is zero so all the times in the row are equal
                built<-append(built,chrom[i,n])       #place gene numbers of everything in the built list
                building[i,n]<-NA              #remove gene numbers of everything from the building dataframe
                #remaining<-remaining[!remaining==chrom[i,j]]      # remove the gene number of mintime from the remaining list
              }
              #condition<-FALSE
              #print(paste( chrom[i,j],"has been placed in built and condition is", condition))
            }
            
            #print(remaining)
            ###CONDITION == FALSE   there's a problem it's not breaking the condition here as it should with the problem chroms
            #print(paste("the code below literally sets the condition to false"))
            # print(paste("chrom","[", i,",",j,"]", "is", chrom[i,j]))
            # print(paste("k is", k))
            condition <- FALSE 
            break
            # print(paste("this line should not print. the condition after placing chrom in built is",condition))
          } 
          
          if (length(remaining)==0) {   #if no more in remaining, then k = back at original k
            k<-ok
          } else {
            k <- k%%length(remaining)+1 
          }    #cycle through remaining INDEX }
          #print(paste("k is", k))
          #print(paste("remaining [k] is", remaining[k]))
          #print(k)
          #print(paste("this line should only print if condition is true. and condition is",condition))
          if (k==ok)  { 
            
            if (length(remaining)!=0) {
              
              if (((!(remaining[k] %in% built))&    #element is not in built list AND            
                   ((remaining[k] %in% remaining)) & #element is in remaining list AND
                   #(is.na(eltime[i,j])) &     #time slot in eltime df is empty for element ij AND
                   (!(remaining[k] %in% ee[,"elemindex"]))) |    #element has no predecessor   OR
                  ((!(remaining[k] %in% built))&    #element is not in built list AND            ###ELEMENT IS UNBUILDABLE
                   ((remaining[k] %in% remaining)) & #element is in remaining list AND
                   #(is.na(eltime[i,j])) &    #time slot in eltime df is empty for element ij AND element's predecessor is in built list
                   ((remaining[k] %in% ee[,"elemindex"]) &
                    (all(((ee[(which(ee[,"elemindex"] %in% remaining[k])),"precindex"]) %in% built)==TRUE))))) { next
              } 
            }
            
            eltime[i,j]<-0
            chrom[i,j]<-0
            
            
            if (any(is.na(eltime[i,]))) {   #if there are other NAS the next IF won't perform, so turn to zeros (cz unbuildable anyway)
              for (n in 1:length(eltime[i,])) {
                if (is.na(eltime[i,n])) {
                  chrom[i,n]<-0
                  eltime[i,n]<-0
                }
              }
              # chrom[i,(which(is.na(eltime[i,]), arr.ind=TRUE)[1,2])]<-0
              # eltime[i,(which(is.na(eltime[i,]), arr.ind=TRUE)[1,2])]<-0
              #condition = FALSE         #it's breaking for 3+ teams here and FIX THIS
              #break
              # navec<-as.vector(which(is.na(eltime[i,]),arr.ind=TRUE )[,2])  #vector with position of nas in row
              # for (r in length(navec)) {   #for all the indexes of the NAs
              #   eltime[i,navec[r]] <- 0            #zeros in eltime
              #   chrom[i,navec[r]] <- 0             #zeros in chrom
              # }
              #print("this isn't supposed to be here")
            }
            
            
            if (var(unlist(eltime[i,]))!=0) {      #if all the times in row are not equal (thus variance is not 0)
              eltime2<-eltime
              eltime2[eltime2==0]<-NA
              
              
              if (any((eltime[i,])>0)) {  #if there is at the very least one non-zero number
                
                q<-(which(eltime[i,]==(min(eltime[i,][which(eltime[i,]>0)])),arr.ind=TRUE))[,2]
                
                for (r in 1:length(q)) {
                  built<-append(built,chrom[i,q[r]])          #place the gene number of mintime in the built list
                }
                
                # p<-(which(eltime2[i,]==min(unlist(eltime2[i,]),na.rm=TRUE),arr.ind=TRUE))[1,2]  #find column position of non-NA minimum in row i  #WHAT IF THESE ARE MULTIPLE NUMBERS??
                # built<-append(built,chrom[i,p])  #add corresponding chrom to built
                
                for (n in 1:length(eltime[1,])) {
                  if ((eltime[i,n]>0) & (length(as.vector(which(eltime[i,]>0,arr.ind=TRUE)[,2]))>1)) {   #if eltime value is more than zero and the value of non-zeros in row is >1
                    eltime[(i+1),n]<-(eltime[i,n] - (min(eltime[i,][which(eltime[i,]>0)])))  #calculate the difference & place all in next eltime row  CHANGE P
                  }  else if ((eltime[i,n]>0) & (length(as.vector(which(eltime[i,]>0,arr.ind=TRUE)[,2]))==1)) {   #if eltime value is more than zero and the value of non-zeros in row is ==1
                    eltime[(i+1),n]<-0                            #(i.e. it is the only element left in row) next row is zero
                  } else if (eltime[i,n]==0) {   #if it is zero,
                    eltime[(i+1),n]<-0      #next row is zero.
                  }
                  
                  if (eltime[(i+1),n]>0) {         #if the next row's time >0, 
                    chrom[i+1,n]<-chrom[i,n]        #in the chromdf replace the gene number below so it's the same with the one above it
                    building[i+1,n]<-building[i,n]
                  } else if (eltime[(i+1),n]==0) {  #otherwise if the next row's time ==0,
                    eltime[(i+1),n]<-NA             #replace the zero(s) in the next row with a NA
                  }
                  
                }
              }
            } else if (var(unlist(eltime[i,]))==0 & !all(eltime[i,]==0)){     #if the variance is zero so all the times in the row are equal
              
              for (j in 1:length(eltime[i,])) {
                
                built<-append(built,chrom[i,j])       #place gene numbers of everything in the built list
                building[i,j]<-NA  
              }
              
            }
            
            condition <- FALSE
            
          }
          #print(condition) 
        }
      }
    }
  }
  #as.data.frame(round(matrix(chrom_error_2,nrow=9,ncol=2)))
  chrom
  built
  eltime
  remaining
  # print(chrom)
  # write_xlsx(chrom, "D:\\BIM A+ 2023\\BIM A+ 7\\BIM A+ 7 Thesis\\IFCProblem\\IFCScheduleProject\\debug8.xlsx")
  # write_xlsx(as.data.frame(matrix(data=built, nrow=((length(built))/9), ncol=9, byrow=TRUE)),
  #            "D:\\BIM A+ 2023\\BIM A+ 7\\BIM A+ 7 Thesis\\IFCProblem\\IFCScheduleProject\\debug9.xlsx")
  # print(built)
  
  phasetime<-c()
  # print(chrom)
  # errorchromfin<-chrom
  for (m in 1:length(eltime[,1])) {
    
    
    if (all((eltime[m,])==0)) {  #if all zeros
      phasetime<-append(phasetime,0)   #add zero to phase time
    } else { #min(eltime[m,][which(eltime[m,]>0)])
      phasetime<-append(phasetime,min(eltime[m,][which(eltime[m,]>0)]) )
    }
    
    # if (all((eltime[m,])>0)) {      #if no zeros, add minimum phase time to phasetime
    #   phasetime<-append(phasetime,min(eltime[m,]))
    # }
    # else if (any((eltime[m,])==0)) {    #if there is a zero, add non-zero to phasetime
    #   eltime2<-eltime            #copy current eltime into eltime2
    #   eltime2[eltime2==0]<-NA    #turn zeros to NAs in eltime2
    #   if (any((eltime[m,])>0)) {  #if there is at the very least one non-NA number
    #     p<-(which(eltime2[m,]==min(unlist(eltime2[m,]),na.rm=TRUE),arr.ind=TRUE))[1,2]  #find column position of non-NA minimum in row m
    #     phasetime<-append(phasetime,max(eltime2[m,p]))   #add contents of the index to phasetime
    #     
    #   }
    #   
    # }
    # else if (all((eltime[m,])==0)) {  #if all zeros
    #   phasetime<-append(phasetime,0)   #add zero to phase time
    # }
  }
  Xtime<-sum(phasetime)
  
  
  timedf <- as.data.frame(matrix(ncol=(length(chrom[1,])), nrow = (length(chrom[,1])))) #create a df that will have actual time
  timedf[is.na(timedf)] = 0   #this is because the actual time at each phase is the phase time, not the individual element time
  timedf
  
  for (i in 1:(length(timedf[,1]))) {
    for (j in 1:length(timedf[1,])) {
      if (eltime[i,j] > 0) {
        timedf[i,j] <- phasetime[i]
      }
    }}
  
  costvec<-c()
  for (s in 1:length(timedf[1,])) {
    costvec<-append(costvec,((sum(timedf[,s]))*t$costTeam[s]))
  }
  Xcost<-sum(costvec)
  
  
  # costvec2<-c()
  # for (s in 1:length(eltime[1,])) {
  #   costvec2<-append(costvec2,((sum(eltime[,s]))*t$costTeam[s]))
  # }
  # Xcost<-sum(costvec2)
  
  
  
  # #print data for each solution to xlsx
  # print(chrom)
  # fctr=7  #literally any number that you can divide built by to make it look nice on a df for xslx viewing purposes
  # builtdf<-data.frame(matrix(built, ncol=fctr, nrow = noelem/fctr, byrow = TRUE))
  # #write_xlsx(builtdf, "C:\\Users\\melod\\Documents\\School\\BIM A+ 2023\\BIM A+ 7\\BIM A+ 7 Thesis\\IFCProblem\\IFCScheduleProject\\PaperXls\\schedpar27gen100.xlsx")
  # colnames(chromunrp2)<-c("Team1","Team2","Team3","Team4","Team5","Team6","Team7","Team8","Team9","Team10")
  # colnames(chrom)<-c("Team1","Team2","Team3","Team4","Team5","Team6","Team7","Team8","Team9","Team10")
  # colnames(eltime)<-c("Team1","Team2","Team3","Team4","Team5","Team6","Team7","Team8","Team9","Team10")
  # sheets <- list("Cost-Time" = cbind(as.data.frame(Xcost), as.data.frame(Xtime)),
  #                "Schedule" = builtdf, "Chrom-Unrp" = chromunrp2,   "Chrom-Final" = chrom, "ElementTime" = eltime )
  # #write_xlsx(sheets, "C:\\Users\\melod\\Documents\\School\\BIM A+ 2023\\BIM A+ 7\\BIM A+ 7 Thesis\\IFCProblem\\IFCScheduleProject\\PaperXls\\10schedpar14gen100.xlsx")
  # write_xlsx(sheets, "C:\\Users\\DiCE Admin\\Documents\\IFCScheduleProject\\XLs\\4and4" )
  # write_xlsx(sheets, "C:\\Users\\melod\\Documents\\Work\\ISISE work\\Journal Paper\\IFCScheduleProject-20260130T151145Z-3-001\\IFCScheduleProject\\XLs\\costest.xlsx" )
  
  return(rbind(Xtime,Xcost))
  
}

#funct(schedproblem[[100]]$par[27,])

#write_xlsx(as.data.frame(funct(schedproblem[[100]]$par[1,])), "C:\\Users\\melod\\Documents\\School\\BIM A+ 2023\\BIM A+ 7\\BIM A+ 7 Thesis\\IFCProblem\\IFCScheduleProject\\PaperXls\\schedpar1gen100.xlsx")
# built
# eltime
# remaining
# chrom
# which(setdiff(as.vector(unique(ee[,"elemindex"])),as.vector(unique(ee[,"precindex"])))%in%remaining)
# setdiff(as.vector(uniquelementdf[,1]),c(built,remaining))    #find the missing elements
# built[ built %in% built[duplicated(built)] ]      #find duplicated items in vec
# built[duplicated(built) | duplicated(built, fromLast=TRUE)]    #another way to find duplicated items in vec


noelem<-as.numeric(length(uniquelementdf[,1]))
nogenes<-as.numeric((length(uniquelementdf[,1]))* length(t[,1]))

runtime1 <- proc.time()
schedproblem2=nsga2(funct,nogenes,2,#constraints=constrX, cdim=2,
                    lower.bounds = c(rep(0,nogenes)),
                    upper.bounds = c(rep(noelem,nogenes)),
                    popsize = 100, generations = 1:100,vectorized=FALSE)
runtime2<- proc.time()-runtime1
schedproblem2runtime<-runtime2
plot(schedproblem2, xlab="Time", ylab="Cost")

#normalization (value-min)/(max-min)
# six vectors with maxes, means and minimums for cost and times across generations
meantimes<-c()
for (i in 1:100) {
  meantimes <- append (meantimes, mean(schedproblem2[[i]]$value[,1]))
}
meancosts<-c()
for (i in 1:100) {
  meancosts <- append (meancosts, mean(schedproblem2[[i]]$value[,2]))
}
mintimes<-c()
for (i in 1:100) {
  mintimes <- append (mintimes, min(schedproblem2[[i]]$value[,1]))
}
mincosts<-c()
for (i in 1:100) {
  mincosts <- append (mincosts, min(schedproblem2[[i]]$value[,2]))
}
maxtimes<-c()
for (i in 1:100) {
  maxtimes <- append (maxtimes, max(schedproblem2[[i]]$value[,1]))
}
maxcosts<-c()
for (i in 1:100) {
  maxcosts <- append (maxcosts, max(schedproblem2[[i]]$value[,2]))
}
maxmaxcosts<-max(maxcosts)
minmincosts<-min(mincosts)
maxmaxtimes<-max(maxtimes)
minmintimes<-min(mintimes)

normalmincosts<-c()
for(i in 1:100) {
  normalmincosts<- append(normalmincosts, ((mincosts[i]-minmincosts)/(maxmaxcosts-minmincosts)))
}
normalmeancosts<-c()
for(i in 1:100) {
  normalmeancosts<- append(normalmeancosts, ((meancosts[i]-minmincosts)/(maxmaxcosts-minmincosts)))
}
normalmintimes<-c()
for(i in 1:100) {
  normalmintimes<- append(normalmintimes, ((mintimes[i]-minmintimes)/(maxmaxtimes-minmintimes)))
}
normalmeantimes<-c()
for(i in 1:100) {
  normalmeantimes<- append(normalmeantimes, ((meantimes[i]-minmintimes)/(maxmaxtimes-minmintimes)))
}

avgmeans<-c()
for (i in 1:100) {
  avgmeans <- append (avgmeans, mean(c(normalmeancosts[i],normalmeantimes[i])) )
}

avgmins<-c()
for (i in 1:100) {
  avgmins <- append (avgmins, mean(c(normalmincosts[i],normalmintimes[i])) )
}


generations<-c(1:100)

plot(generations,avgmeans, type="l", col="black", lwd=3, ylim=c(-0.01,0.8), xlab="Generations", ylab="Fitness Value (normalized)")
lines(generations,avgmins, col="orange", lwd=3)
title("Mean and best fitness")
legend(50,0.8,c("mean","best"), lwd=c(3,3), col=c("black","orange"), y.intersp=1.5)
#schedproblemruntime<-runtime2

#schedprobb3<-schedproblem2

#Plotting a bunch of generations
#FOR 10-100-100
lengthpar<-length(schedprobb3[[100]]$pareto.optimal[schedprobb3[[100]]$pareto.optimal > 0]) #length of only subsetted pareto optimals
#schedprobb2[[1]]
#plot(schedprobb2[[1]]$value, schedprobb2[[100]]$value, xlab="Time", ylab="Cost", xlim=c(825,855), ylim=c(1350000,2100000) )
#plot(schedprobb3[[1]]$value, col= gray(0.8) , pch = 19, xlab="Time", ylab="Cost", xlim=c(829,853), ylim=c(1350000,2100000), frame.plot = TRUE )
plot(schedprobb3[[1]]$value, col= gray(0.8) , pch = 19, xlab="Time", ylab="Cost", xlim=c(826,837), ylim=c(307300,308800), frame.plot = TRUE )
points(schedprobb3[[5]]$value, col=gray(0.75), pch=19)
points(schedprobb3[[10]]$value, col=gray(0.7), pch=19)
points(schedprobb3[[25]]$value, col=gray(0.6), pch=19)
points(schedprobb3[[50]]$value, col=gray(0.4), pch=19)
points(schedprobb3[[75]]$value, col=gray(0.2), pch=19)
points(schedprobb3[[100]]$value, col='black', pch=19)
points(schedprobb3[[100]]$value[1:lengthpar,], col='red', pch=1)

# Fit a smooth spline to the data (only pareto optimal solutions in final generation, sorted for aesthetics in order of time)
#fit <- smooth.spline(schedprobb3[[100]]$value[1:lengthpar,][order(schedprobb3[[100]]$value[1:lengthpar,][,1], decreasing = FALSE), ])
#fit <- line(schedprobb3[[100]]$value[1:lengthpar,][order(schedprobb3[[100]]$value[1:lengthpar,][,1], decreasing = FALSE), ])
parlines<-(schedprobb3[[100]]$value[1:lengthpar,][order(schedprobb3[[100]]$value[1:lengthpar,][,1], decreasing = FALSE), ])
# Add the smooth curve to the plot (lwd is the line width)
lines(parlines, col = "red", lwd = 2)
legend(833, 308000, legend=c('Gen1', 'Gen10', 'Gen50', 'Gen75', 'Gen100', 'FinalPareto'), pch=c(19, 19, 19, 19, 19,1), 
       col=c(gray(0.8), gray(0.7), gray(0.4), gray(0.2), 'black', 'red'))



#FOR 5-100-100
lengthparr<-length(schedprobb2[[100]]$pareto.optimal[schedprobb2[[100]]$pareto.optimal > 0])

plot(schedprobb2[[1]]$value, col= gray(0.8) , pch = 19, xlab="Time", ylab="Cost", xlim=c(829,853), ylim=c(1350000,2100000), frame.plot = TRUE )
points(schedprobb2[[5]]$value, col=gray(0.75), pch=19)
points(schedprobb2[[10]]$value, col=gray(0.7), pch=19)
points(schedprobb2[[25]]$value, col=gray(0.6), pch=19)
points(schedprobb2[[50]]$value, col=gray(0.4), pch=19)
points(schedprobb2[[75]]$value, col=gray(0.2), pch=19)
points(schedprobb2[[100]]$value, col='black', pch=19)
points(schedprobb2[[100]]$value[1:lengthpar,], col='red', pch=1)

parliness<-(schedprobb2[[100]]$value[1:lengthparr,][order(schedprobb2[[100]]$value[1:lengthparr,][,1], decreasing = FALSE), ])
lines(parliness, col = "red", lwd = 2)
legend(843, 2100000, legend=c('Gen1', 'Gen10', 'Gen50', 'Gen75', 'Gen100', 'FinalPareto'), pch=c(19, 19, 19, 19, 19,1), 
       col=c(gray(0.8), gray(0.7), gray(0.4), gray(0.2), 'black', 'red'))

#for 5-100-100
parlinessdf<-as.data.frame(parliness)
colnames(parlinessdf)<-c("Time","Cost")
#for 10-100-100
parlinesdf<-as.data.frame(parlines)
colnames(parlinesdf)<-c("Time","Cost")

sheets1 <- list("Pareto-5-100-100" = parlinessdf, "Pareto-10-100-100" = parlinesdf)
write_xlsx(sheets1, "C:\\Users\\melod\\Documents\\School\\BIM A+ 2023\\BIM A+ 7\\BIM A+ 7 Thesis\\IFCProblem\\IFCScheduleProject\\PaperXls\\FinalParetoCostsTimes.xlsx")





write_xlsx(uniquelementdf, "C:\\Users\\melod\\Documents\\School\\BIM A+ 2023\\BIM A+ 7\\BIM A+ 7 Thesis\\IFCProblem\\IFCScheduleProject\\PaperXls\\Schedule27withGUIDs-5-100-100.xlsx")



# lines(schedprobb2[[100]]$value[1:lengthpar,][order(schedprobb2[[100]]$value[1:lengthpar,][,1], decreasing = FALSE), ], col='red', lwd = 2)
# 
# schedprobb2[[100]]$pareto.optimal
# 
# lengthpar<-length(schedprobb2[[100]]$pareto.optimal[schedprobb2[[100]]$pareto.optimal > 0])

#only pareto optimal solutions
#schedprobb2[[100]]$value[1:lengthpar,][,1]

#ordered df
#schedprobb2[[100]]$value[1:lengthpar,][order(schedprobb2[[100]]$value[1:lengthpar,][,1], decreasing = FALSE), ]

#new error Error in ov[, 1] : incorrect number of dimensions

#plot(schedprobdemo1, xlab="Time (h)", ylab="Cost (€)")
#
# built<-as.data.frame(matrix (rep(sample(elemlistindex,length(uniquelementdf$elemindex),replace=FALSE),length(t[,1]))),
#                                               nrow=(length(elemlist)),ncol=length(t[,1]))
# matrix(data=built, nrow=((length(built))/9), ncol=9, byrow=TRUE)
# as.data.frame(matrix(data=built, nrow=((length(built))/9), ncol=9, byrow=TRUE))
# as.data.frame(schedprob_demanual52425$value)
# write_xlsx(as.data.frame(schedprob_demanual52425$value),
#            "D:\\BIM A+ 2023\\BIM A+ 7\\BIM A+ 7 Thesis\\IFCProblem\\IFCScheduleProject\\debug7.xlsx")
#write_xlsx(as.data.frame(funct(schedproblem[[100]]$par[1,])), "C:\\Users\\melod\\Documents\\School\\BIM A+ 2023\\BIM A+ 7\\BIM A+ 7 Thesis\\IFCProblem\\IFCScheduleProject\\PaperXls\\schedpar1gen100.xlsx")
# library(writexl)
# sheets <- list("sheet1Name" = sheet1, "sheet2Name" = sheet2) #assume sheet1 and sheet2 are data frames
# write_xlsx(sheets, "path/to/location")

# vv
# as.data.frame.vector(vv)
# fctr=7
# vdf<-data.frame(matrix(ncol=fctr, nrow = noelem/fctr))
# vdf
# vv<-c(1:noelem)
# vv
# vdf<-data.frame(matrix(vv, ncol=fctr, nrow = noelem/fctr, byrow = TRUE))


# data = data.frame(
#   rollno = c(1, 5, 4, 2, 3),
#   subjects = c("java", "python", "php", "sql", "c"))
# 
# print(data)
# 
# print("sort the data in decreasing order based on subjects ")
# print(data[order(data$subjects, decreasing = TRUE), ]   )
# 
# 
# print("sort the data in decreasing order based on rollno ")
# print(data[order(data$rollno, decreasing = FALSE), ]   )


# schedproblemruntime 
# user   system  elapsed 
# 44419.17   194.71 44854.43
