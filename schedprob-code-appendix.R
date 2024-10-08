
require(nsga2R)
library(readxl)
library(writexl)


prctbl <- as.data.frame(read_xlsx("demolitionpm.xlsx", sheet = "Matrix")) #reading original matrix
prctbl
 
prctbl1 <- data.frame(
  element <- prctbl["element"],
  elemguid<-prctbl["objguid"],
  elemindex <- prctbl["elemindex"],  
  volelement <- prctbl["volelement"],
  precedent <- prctbl["precedent"],
  precguid<-prctbl["precguid"],
  precindex <- prctbl["precindex"],
  volpreced <- prctbl["volpreced"],
  stringsAsFactors = FALSE
)
ee<-prctbl1


uniquelementdf <-data.frame(                   #generating case sensitive unique list in r
  elementguid <- c(prctbl1[,"objguid"],prctbl1[,"precguid"]),
  ifcelement <- c(prctbl1[,"element"],prctbl1[,"precedent"]),
  volelement <- c(prctbl1[,"volelement"],prctbl1[,"volpreced"]),

  stringsAsFactors = FALSE
)

colnames(uniquelementdf)<-c("uniqueguid", "ifcelement", "vol")

uniquelementdf <- uniquelementdf[!duplicated(uniquelementdf$uniqueguid), ]  #remove duplicates leaving uniques only
uniquelementdf
 
write_xlsx(uniquelementdf, "Path\\uniquelementdf.xlsx") 

prctbl <- as.data.frame(read_xlsx("demolitionpm.xlsx", sheet = "Matrix2"))  #import xrefed matrix with index back into R
prctbl


prctbl1 <- data.frame(                       #create final df 
  
  elemindex <- prctbl["elemindex"],
  element <- prctbl["element"],
  volelement <- prctbl["volelement"],
  precindex <- prctbl["precindex"],
  precedent <- prctbl["precedent"],
  volpreced <- prctbl["volpreced"],
  stringsAsFactors = FALSE
)
ee<-prctbl1

uniquelementdf <- as.data.frame(read_xlsx("demolitionpm.xlsx", sheet = "uniqueindex2"))  
uniquelementdf

t<- as.data.frame(read_xlsx("teams.xlsx", sheet = "demoteams4"))
row.names(t)<-c("A", "B", "C","D","E","F","G","H","I","J")
t

funct<-function(x){
  #chromosome as data frame
  chrom <- as.data.frame(round(matrix(x,nrow=(length(uniquelementdf[,1])),ncol=(length(t[,1])))))
  
  #built list as vector
  built<-c()
  built
  
  #remaining list as vector
  remaining<-uniquelementdf[,"uniqueindex"]
  remaining
  
  #element construction time as data frame
  eltime <- as.data.frame(matrix(ncol=(length(chrom[1,])), nrow = (length(chrom[,1])))) #creating a data frame of length (no.teams) to store times
  eltime
  
  for (i in 1:(length(chrom[,1]))) {
    for (j in 1:length(chrom[1,])) {
      
      if  ((!(chrom[i,j] %in% built))&    #element is not in built list
           ((chrom[i,j] %in% remaining))&   #element is in remaining list
           (((!(chrom[i,j] %in% ee[,"elemindex"])) |    #element has no predecessor OR
             ((chrom[i,j] %in% ee[,"elemindex"]) &       #element has a predecessor AND
              (all(((ee[(which(ee[,"elemindex"] %in% chrom[i,j])),"precindex"]) %in% built)==TRUE)))) &   #predecessor is in built list
            (is.na(eltime[i,j]))) & #time slot in eltime df is empty for element ij
           (((length(elemlistindex)-length(built))>1) & ((length(setdiff(elemlistindex,built)))>1)))  {  #more than one element is unbuilt      
        #volume/productivity      #calculate the time it would take to build it (save it in df eltime)
        eltime[i,j] <-(uniquelementdf[(which(uniquelementdf[,"uniqueindex"] %in% chrom[i,j])),"vol"]/t[j,1])  
        #place the gene [i,j] in the building dataframe
        building[i,j]<-chrom[i,j]
        remaining<-remaining[!remaining==chrom[i,j]]    # remove the gene number of mintime from the remaining list
        
        if ((any(chrom[i,]==0)) & ((length(which(chrom[i,]==0)))<=(length(chrom[i,])-1))) { #if there's at least one non-zero in chrom
          for (n in 1:length(chrom[i,])) {
            if (chrom[i,n]==0) {
              eltime[i,n]<-0                            #make eltime to be zero for all the zeros in chrom
            }
          }
        }
        
        
        if (!is.na(sum(eltime[i,]))) {    #when all the slots in the eltime row are full     
          for (n in 1:length(eltime[1,])) {
            eltime[(i+1),n]<-(eltime[i,n] - (min(eltime[i,][which(eltime[i,]>0)]))) 
            if (eltime[(i+1),n]<0) {
              eltime[(i+1),n]<-0
            }
          } #calculate the difference & place all in next eltime row 
          
          if (var(unlist(eltime[i,]))!=0) {      #if all the times in row are not equal   
            
            
            q<-(which(eltime[i,]==(min(eltime[i,][which(eltime[i,]>0)])),arr.ind=TRUE))[,2]
            
            for (r in 1:length(q)) {
              built<-append(built,chrom[i,q[r]])          #place the gene number of mintime in the built list
            }
            
            for (n in 1:length(eltime[1,])) { 
              if (eltime[(i+1),n]>0) {         #if next row's j is not zero thus is the element with longer duration(s)
                chrom[i+1,n]<-chrom[i,n]        #in the chromdf replace the gene number below so it's the same with the one above it
                building[i+1,n]<-building[i,n]   
              } else if (eltime[(i+1),n]==0) {
                eltime[(i+1),n]<-NA             #otherwise replace the zero in the next row with a NA
              } else if (eltime[(i+1),n]<0) {
                eltime[(i+1),n]<-NA
              }
            }
          } else if (var(unlist(eltime[i,]))==0){     #if the variance is zero so all the times in the row are equal
            built<-append(built,chrom[i,n])       #place gene numbers of everything in the built list
            building[i,n]<-NA              
          }
        }
      } else if ((length(uniquelementdf[,"uniqueindex"])-length(built))==1 & (length(setdiff(uniquelementdf[,"uniqueindex"],built)))==1 & (is.na(eltime[i,j])))  {   #if one element is remaining unbuilt
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
              ((!(remaining[k] %in% built))&    #element is not in built list AND            
               ((remaining[k] %in% remaining)) & #element is in remaining list AND
               #(is.na(eltime[i,j])) &    #time slot in eltime df is empty for element ij AND element's predecessor is in built list
               ((remaining[k] %in% ee[,"elemindex"]) &
                (all(((ee[(which(ee[,"elemindex"] %in% remaining[k])),"precindex"]) %in% built)==TRUE))))) {
            #place the chromosome value with the element being built
            chrom[i,j]<-remaining[k]
            #volume/productivity      #calculate the time it would take to build it (save it in df eltime)
            eltime[i,j] <-(uniquelementdf[(which(uniquelementdf[,"uniqueindex"] %in% chrom[i,j])),"vol"]/t[j,1])
            #remove from remaining
            remaining<-remaining[!remaining==chrom[i,j]]
            
            if ((any(chrom[i,]==0)) & ((length(which(chrom[i,]==0)))<=(length(chrom[i,])-1))) { #if there's at least one non-zero in chrom i 
              for (n in 1:length(chrom[i,])) {
                if (chrom[i,n]==0) {
                  eltime[i,n]<-0                            #make eltime to be zero for all the zeros in chrom
                }
              }
            }
            
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
                
                for (n in 1:length(eltime[i,])) { 
                  if (eltime[(i+1),n]>0) {         #if next row's j is not zero thus is the element with longer duration(s)
                    chrom[i+1,n]<-chrom[i,n]        #in the chromdf replace the gene number below so it's the same with the one above it
                    building[i+1,n]<-building[i,n]   
                  } else if (eltime[(i+1),n]==0) {
                    eltime[(i+1),n]<-NA             #otherwise replace the zero in the next row with a NA
                  } else if (eltime[(i+1),n]<0) {
                    eltime[(i+1),n]<-NA
                  }
                }
              } else if (var(unlist(eltime[i,]))==0){     #if the variance is zero so all the times in the row are equal
                built<-append(built,chrom[i,n])       #place gene numbers of everything in the built list
                building[i,n]<-NA             
              }
            }
            condition <- FALSE 
            break
            # print(paste("this line should not print. the condition after placing chrom in built is",condition))
          } 
          
          if (length(remaining)==0) {   #if no more in remaining, then k = back at original k
            k<-ok
          } else {
            k <- k%%length(remaining)+1 
          }    
          
          if (k==ok)  { 
            if (length(remaining)!=0) {
              if (((!(remaining[k] %in% built))&    #element is not in built list AND            
                   ((remaining[k] %in% remaining)) & #element is in remaining list AND
                   #(is.na(eltime[i,j])) &     #time slot in eltime df is empty for element ij AND
                   (!(remaining[k] %in% ee[,"elemindex"]))) |    #element has no predecessor   OR
                  ((!(remaining[k] %in% built))&    #element is not in built list AND           
                   ((remaining[k] %in% remaining)) & #element is in remaining list AND
                   #(is.na(eltime[i,j])) &    #time slot in eltime df is empty for element ij AND element's predecessor is in built list
                   ((remaining[k] %in% ee[,"elemindex"]) &
                    (all(((ee[(which(ee[,"elemindex"] %in% remaining[k])),"precindex"]) %in% built)==TRUE))))) { next
              } 
            }
            
            eltime[i,j]<-0
            chrom[i,j]<-0

            if (any(is.na(eltime[i,]))) {   #if there are other NAS turn to zeros, bc unbuildable
              for (n in 1:length(eltime[i,])) {
                if (is.na(eltime[i,n])) {
                  chrom[i,n]<-0
                  eltime[i,n]<-0
                }
              }
            }
            
            
            if (var(unlist(eltime[i,]))!=0) {      #if all the times in row are not equal (thus variance is not 0)
              eltime2<-eltime
              eltime2[eltime2==0]<-NA
              
              if (any((eltime[i,])>0)) {  #if there is at the very least one non-zero number
                
                q<-(which(eltime[i,]==(min(eltime[i,][which(eltime[i,]>0)])),arr.ind=TRUE))[,2]
                
                for (r in 1:length(q)) {
                  built<-append(built,chrom[i,q[r]])          #place the gene number of mintime in the built list
                }
                
                for (n in 1:length(eltime[1,])) {
                  if ((eltime[i,n]>0) & (length(as.vector(which(eltime[i,]>0,arr.ind=TRUE)[,2]))>1)) {   #if eltime value is more than zero and the value of non-zeros in row is >1
                    eltime[(i+1),n]<-(eltime[i,n] - (min(eltime[i,][which(eltime[i,]>0)])))  #calculate the difference & place all in next eltime row 
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
        }
      }
    }
  }
  
  phasetime<-c()

  for (m in 1:length(eltime[,1])) {
    
    if (all((eltime[m,])==0)) {  #if all zeros
      phasetime<-append(phasetime,0)   #add zero to phase time
    } else { 
      phasetime<-append(phasetime,min(eltime[m,][which(eltime[m,]>0)]) )
    }
  }
  Xtime<-sum(phasetime)
  
  costvec<-c()
  for (s in 1:length(eltime[1,])) {
    costvec<-append(costvec,((sum(eltime[,s]))*t$costTeam[s]))
  }
  Xcost<-sum(costvec)
  
  return(rbind(Xtime,Xcost))
}

noelem<-as.numeric(length(uniquelementdf[,1]))
nogenes<-as.numeric((length(uniquelementdf[,1]))* length(t[,1]))

runtime1 <- proc.time()
schedprob=nsga2(funct,nogenes,2,
                lower.bounds = c(rep(0,nogenes)),
                upper.bounds = c(rep(noelem,nogenes)),
                popsize = 48, generations = 50,vectorized=FALSE)
runtime2<- proc.time()-runtime1
plot(schedprob, xlab="Time (h)", ylab="Cost (€)")

