#trying to fix zeros idk

require(nsga2R)

lcolumn1 = 0.3
lcolumn2 = 0.3
lcolumn3 = 0.3
lcolumn4 = 0.3
wcolumn1 = 0.3
wcolumn2 = 0.3
wcolumn3 = 0.3
wcolumn4 = 0.3
hcolumn1 = 3
hcolumn2 = 3
hcolumn3 = 3
hcolumn4 = 3
lbeam1 = 0.3
lbeam2 = 0.3
lbeam3 = 0.3
lbeam4 = 0.3
wbeam1 = 0.4
wbeam2 = 0.4
wbeam3 = 0.4
wbeam4 = 0.4
hbeam1 = 4
hbeam2 = 4
hbeam3 = 4
hbeam4 = 4
lslab = 4
wslab = 4
hslab = 0.2



totallength = c(lcolumn1+lcolumn2+lcolumn3+lcolumn4+lbeam1+lbeam2+lbeam3+lbeam4+lslab) 
totalwidth = c(wcolumn1+wcolumn2+wcolumn3+wcolumn4+wbeam1+wbeam2+wbeam3+wbeam4+wslab)
totalheight = c(hcolumn1+hcolumn2+hcolumn3+hcolumn4+hbeam1+hbeam2+hbeam3+hbeam4+hslab)
totalvol = totallength*totalwidth*totalheight

l = c(lcolumn1,lcolumn2,lcolumn3,lcolumn4,lbeam1,lbeam2,lbeam3,lbeam4,lslab) 
w = c(wcolumn1,wcolumn2,wcolumn3,wcolumn4,wbeam1,wbeam2,wbeam3,wbeam4,wslab)
h = c(hcolumn1,hcolumn2,hcolumn3,hcolumn4,hbeam1,hbeam2,hbeam3,hbeam4,hslab)
vols=l+w+h
vols
totalvol

no.beams = 4
no.columns = 4
no.slabs = 1

elementdtls<-data.frame(
  elementid = c("colm1","colm2","colm3","colm4",
                "bm1","bm2","bm3","bm4","slab"),
  elemindex = c(1:9),             #the index associated with the guid
  vols,
  row.names = c("column1","column2","column3","column4",
                "beam1","beam2","beam3","beam4","slab"),
  stringsAsFactors = FALSE
)
elementdtls

t <- data.frame(
  prodTeam = c (0.3, 0.00004),
  costTeam = c(60, 630000),
  row.names = c("A","B"),
  stringsAsFactors = FALSE
)
t
nrow(t)
ee <- data.frame(
  element = c(rep("bm1",2),rep("bm2",2),rep("bm3",2),rep("bm4",2),rep("slab",4)),
  elemindex = c(rep(5,2),rep(6,2),rep(7,2),rep(8,2),rep(9,4)),                     #the index associated with the guid
  volelement=c(rep((lbeam1*wbeam1*hbeam1),2),rep((lbeam2*wbeam2*hbeam2),2),
               rep((lbeam3*wbeam3*hbeam3),2),rep((lbeam4*wbeam4*hbeam4),2),
               rep((lslab*wslab*hslab),4)),
  precedent = c("colm1",rep("colm2",2),rep("colm3",2),rep("colm4",2),"colm1","bm1","bm2","bm3","bm4"),
  precindex = c(1,rep(2,2),rep(3,2),rep(4,2),1,5,6,7,8),                           #the index associated with the guid
  volpreced = c((lcolumn1*wcolumn1*hcolumn1),rep((lcolumn2*wcolumn2*hcolumn2),2),  
                rep((lcolumn3*wcolumn3*hcolumn3),2),rep((lcolumn4*wcolumn4*hcolumn4),2),
                (lcolumn1*wcolumn1*hcolumn1),(lbeam1*wbeam1*hbeam1),(lbeam2*wbeam2*hbeam2),
                (lbeam3*wbeam3*hbeam3),(lbeam4*wbeam4*hbeam4)),
  stringsAsFactors = FALSE
)
ee
nrow(ee)

#create a dataframe with the unique element indexes and their volumes

uniquelementdf <-data.frame(
  elemindex=c(ee[,"elemindex"],ee[,"precindex"]),
  volelement=c(ee[,"volelement"],ee[,"volpreced"]),
  stringsAsFactors = FALSE
) 

uniquelementdf <- uniquelementdf[!duplicated(uniquelementdf$elemindex), ]  #remove duplicates leaving uniques only
uniquelementdf <- uniquelementdf[order(uniquelementdf$elemindex), ]    #sort uniques in order
uniquelementdf

#create a df with 2 columns of elements' indexes, random & unrepeated

# chrom<-as.data.frame(matrix(c(sample(elemlistindex,9,replace=FALSE),
#                               sample(elemlistindex,9,replace=FALSE)), 
#                             nrow=(length(elemlist)),ncol=length(t[1,])) )
# chrom
# 
# #built list as vector
# built<-c()
# built
# 
# #building list as dataframe
# building <- as.data.frame(matrix(ncol=(length(chrom[1,])), nrow = (length(chrom[,1]))))
# building
# 
# #remaining list as vector
# remaining<-elemlistindex
# remaining
# 
# #element construction time as dataframe
# eltime <- as.data.frame(matrix(ncol=(length(chrom[1,])), nrow = (length(chrom[,1])))) #creating a data frame of length (no.teams) to store times
# eltime

funct<-function(x){
chrom <- as.data.frame(round(matrix(x,nrow=(length(elemlist)),ncol=(length(t[,1])))))
print(chrom)

# chrom<-as.data.frame(matrix (rep(sample(elemlistindex,length(uniquelementdf$elemindex),replace=FALSE),length(t[,1]))),
#                             nrow=(length(elemlist)),ncol=length(t[,1]))
# 
# chrom<-as.data.frame(matrix(c(sample(elemlistindex,9,replace=FALSE),
#                               sample(elemlistindex,9,replace=FALSE),
#                               sample(elemlistindex,9,replace=FALSE),
#                               sample(elemlistindex,9,replace=FALSE)),
#                             nrow=(length(elemlist)),ncol=length(t[,1])) )
#chrom<-chrominit

#print("imetengeneza chrom")
# chrom <- as.data.frame(round(matrix(x,nrow=length(uniquelementdf$elemindex),ncol=2)))
#chrom

#built list as vector
built<-c()
built

#building list as dataframe
building <- as.data.frame(matrix(ncol=(length(chrom[1,])), nrow = (length(chrom[,1]))))
building

#remaining list as vector
remaining<-elemlistindex
remaining

#element construction time as dataframe
eltime <- as.data.frame(matrix(ncol=(length(chrom[1,])), nrow = (length(chrom[,1])))) #creating a data frame of length (no.teams) to store times
eltime

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
    # i<-1
    # j<-2
    #print(paste("tumeanza for loop", i , "," , j ))
    if  ((!(chrom[i,j] %in% built))&    #element is not in built list
         ((chrom[i,j] %in% remaining))&   #element is in remaining list
         #(is.na(eltime[i,j]))& #time slot in eltime df is empty for element ij
         (((!(chrom[i,j] %in% ee[,"elemindex"])) |    #element has no predecessor OR
           ((chrom[i,j] %in% ee[,"elemindex"]) &       #element has a predecessor AND
            (all(((ee[(which(ee[,"elemindex"] %in% chrom[i,j])),"precindex"]) %in% built)==TRUE)))) &   #predecessor is in built list
          (is.na(eltime[i,j]))) & #time slot in eltime df is empty for element ij
         (((length(elemlistindex)-length(built))>1) & ((length(setdiff(elemlistindex,built)))>1)))  {  #more than one element is unbuilt      
      #volume/productivity      #calculate the time it would take to build it (save it in df eltime)
      eltime[i,j] <-(uniquelementdf[(which(uniquelementdf[,"elemindex"] %in% chrom[i,j])),"volelement"]/t[j,1])  
      #place the gene [i,j] in the building dataframe
      building[i,j]<-chrom[i,j]
      remaining<-remaining[!remaining==chrom[i,j]]    # remove the gene number of mintime from the remaining list
      # for (m in 1:length(eltime[,1])) {
      # 
      if (!is.na(sum(eltime[i,]))) {    #when all the slots in the eltime row are full     NEEDS TO HAPPEN ONLY ONCE IN THE FOR n CYCLE
        for (n in 1:length(eltime[1,])) {
          eltime[(i+1),n]<-(eltime[i,n] - min(which(eltime[i,] > 0))) } #calculate the difference & place all in next eltime row 
        
        if (var(unlist(eltime[i,]))!=0) {      #if all the times in row are not equal (thus variance is not 0)  
          
          q<-(which(eltime[i,]==min(which(eltime[i,] > 0)),arr.ind=TRUE))[1,2]   #column index of the minimum time in row i   GENERALIZE
          built<-append(built,chrom[i,q])          #place the gene number of mintime in the built list
          building[i,q]<-NA                 # remove the gene number of mintime from the building dataframe
          for (n in 1:length(eltime[1,])) { 
            # remaining<-remaining[!remaining==chrom[p,q]]    # remove the gene number of mintime from the remaining list
            if (eltime[(i+1),n]>0) {         #if next row's j is not zero thus is the element with longer duration(s)
              chrom[i+1,n]<-chrom[i,n]        #in the chromdf replace the gene number below so it's the same with the one above it
              building[i+1,n]<-building[i,n]   #in buildingdf replace the gene number below so it's the same with the one above it
            } else if (eltime[(i+1),n]==0) {
              eltime[(i+1),n]<-NA             #otherwise replace the zero in the next row with a NA
            }
          }
        } else if (var(unlist(eltime[i,]))==0){     #if the variance is zero so all the times in the row are equal
          built<-append(built,chrom[i,n])       #place gene numbers of everything in the built list
          building[i,n]<-NA              #remove gene numbers of everything from the building dataframe
          #remaining<-remaining[!remaining==chrom[i,j]]      # remove the gene number of mintime from the remaining list
        }
      }
    } else if ((length(elemlistindex)-length(built))==1 & (length(setdiff(elemlistindex,built)))==1 & (is.na(eltime[i,j])))  {   #if one element is remaining unbuilt
      #the above two checks ensure there's no doubled element in built list before the final element is added to the built list
      chrom[i,j]<-setdiff(elemlistindex,built)    #place the final element in chrom[i,j]
      eltime[i,j] <-(uniquelementdf[(which(uniquelementdf[,"elemindex"] %in% chrom[i,j])),"volelement"]/t[j,1])
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
      
      if ((chrom[i,j]==0) & (sum(chrom[i,])>0)) {
        eltime[i,j] <- 0
        if (!is.na(sum(eltime[i,]))) {    #when all the slots in the eltime row are full     
          for (n in 1:length(eltime[i,])) {
            eltime[(i+1),n]<-(eltime[i,n] - min(which(eltime[i,] > 0))) #calculate the difference & place all in next eltime row 
          }
          if (var(unlist(eltime[i,]))!=0) {      #if all the times in row are not equal (thus variance is not 0)  
            
            q<-(which(eltime[i,]==min(which(eltime[i,] > 0)),arr.ind=TRUE))[1,2]   #column index of the minimum time in row i  GENERALIZE
            built<-append(built,chrom[i,q])          #place the gene number of mintime in the built list
            building[i,q]<-NA                 # remove the gene number of mintime from the building dataframe
            for (n in 1:length(eltime[i,])) { 
              # remaining<-remaining[!remaining==chrom[p,q]]    # remove the gene number of mintime from the remaining list
              if (eltime[(i+1),n]>0) {         #if next row's j is not zero thus is the element with longer duration(s)
                chrom[i+1,n]<-chrom[i,n]        #in the chromdf replace the gene number below so it's the same with the one above it
                building[i+1,n]<-building[i,n]   #in buildingdf replace the gene number below so it's the same with the one above it
              } else if (eltime[(i+1),n]==0) {
                eltime[(i+1),n]<-NA             #otherwise replace the zero in the next row with a NA
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
        
        
        
        condition <- FALSE
        break
      } 
      
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
          eltime[i,j] <-(uniquelementdf[(which(uniquelementdf[,"elemindex"] %in% chrom[i,j])),"volelement"]/t[j,1])
          #remove from remaining
          #remaining <- remaining[-k]
          remaining<-remaining[!remaining==chrom[i,j]]
          
          #print(paste("chrom","[", i,",",j,"]", "is", chrom[i,j]))
          #print(paste("condition after placing remaining in chrom is",condition))
          
          if (!is.na(sum(eltime[i,]))) {    #when all the slots in the eltime row are full     
            for (n in 1:length(eltime[i,])) {
              eltime[(i+1),n]<-(eltime[i,n] - min(which(eltime[i,] > 0))) #calculate the difference & place all in next eltime row 
            }
            if (var(unlist(eltime[i,]))!=0) {      #if all the times in row are not equal (thus variance is not 0)  
              
              q<-(which(eltime[i,]==min(which(eltime[i,] > 0)),arr.ind=TRUE))[1,2]   #column index of the minimum time in row i  GENERALIZE
              built<-append(built,chrom[i,q])          #place the gene number of mintime in the built list
              building[i,q]<-NA                 # remove the gene number of mintime from the building dataframe
              for (n in 1:length(eltime[i,])) { 
                # remaining<-remaining[!remaining==chrom[p,q]]    # remove the gene number of mintime from the remaining list
                if (eltime[(i+1),n]>0) {         #if next row's j is not zero thus is the element with longer duration(s)
                  chrom[i+1,n]<-chrom[i,n]        #in the chromdf replace the gene number below so it's the same with the one above it
                  building[i+1,n]<-building[i,n]   #in buildingdf replace the gene number below so it's the same with the one above it
                } else if (eltime[(i+1),n]==0) {
                  eltime[(i+1),n]<-NA             #otherwise replace the zero in the next row with a NA
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
          eltime[i,j]<-0
          chrom[i,j]<-0
          
          
          if (any(is.na(eltime[i,]))) {   #if there are other NAS the next IF won't perform, so turn to zeros (cz unbuildable anyway)
            
            condition = FALSE
            break
            # navec<-as.vector(which(is.na(eltime[i,]),arr.ind=TRUE )[,2])  #vector with position of nas in row
            # for (r in length(navec)) {   #for all the indexes of the NAs
            #   eltime[i,navec[r]] <- 0            #zeros in eltime
            #   chrom[i,navec[r]] <- 0             #zeros in chrom
            # }
            print("this isn't supposed to be here")
          }
          
          if (var(unlist(eltime[i,]))!=0) {      #if all the times in row are not equal (thus variance is not 0)
            eltime2<-eltime
            eltime2[eltime2==0]<-NA
            
            
            if (any((eltime[i,])>0)) {  #if there is at the very least one non-zero number
              p<-(which(eltime2[i,]==min(unlist(eltime2[i,]),na.rm=TRUE),arr.ind=TRUE))[1,2]  #find column position of non-NA minimum in row i  #WHAT IF THESE ARE MULTIPLE NUMBERS??
              built<-append(built,chrom[i,p])  #add corresponding chrom to built
              for (n in 1:length(eltime[1,])) {
                if ((eltime[i,n]>0) & (length(as.vector(which(eltime[i,]>0,arr.ind=TRUE)[,2]))>1)) {   #if eltime value is more than zero and the value of non-zeros in row is >1
                  eltime[(i+1),n]<-(eltime[i,n] - eltime[i,p])  #calculate the difference & place all in next eltime row
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
          } else if (var(unlist(eltime[i,]))==0){     #if the variance is zero so all the times in the row are equal
            built<-append(built,chrom[i,j])       #place gene numbers of everything in the built list
            building[i,j]<-NA  }
          
          condition <- FALSE }
        #print(condition) 
      }
    }
  }
}
#as.data.frame(round(matrix(chrom_error_2,nrow=9,ncol=2)))
print(chrom)
print(built)
# eltime
# remaining
# print(chrom)
# print(built)

phasetime<-c()
for (m in 1:length(eltime[,1])) {
  if (all((eltime[m,])>0)) {      #if no zeros, add minimum phase time to phasetime
    phasetime<-append(phasetime,min(eltime[m,]))
  }
  else if (any((eltime[m,])==0)) {    #if there is a zero, add non-zero to phasetime
    eltime2<-eltime            #copy current eltime into eltime2
    eltime2[eltime2==0]<-NA    #turn zeros to NAs in eltime2
    if (any((eltime[m,])>0)) {  #if there is at the very least one non-NA number
      p<-(which(eltime2[m,]==min(unlist(eltime2[m,]),na.rm=TRUE),arr.ind=TRUE))[1,2]  #find column position of non-NA minimum in row m
      phasetime<-append(phasetime,max(eltime2[m,p]))   #add contents of the index to phasetime
      
    }
    
  }
  else if (all((eltime[m,])==0)) {  #if all zeros
    phasetime<-append(phasetime,0)   #add zero to phase time
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

# built
# eltime
# remaining
# chrom
# 

noelem<-as.numeric(length(elemlist))
nogenes<-as.numeric((length(elemlist)* length(t[,1])))
schedprob=nsga2(funct,nogenes,2,#constraints=constrX, cdim=2,
                lower.bounds = c(rep(0,nogenes)),
                upper.bounds = c(rep(noelem,nogenes)),
                popsize = 12, generations = 12,vectorized=FALSE)

plot(schedprob, xlab="Xtime", ylab="Xcost")

# schedprob=nsga2(funct,18,2,#constraints=constrX, cdim=2,
#                 lower.bounds = c(rep(0,18)),
#                 upper.bounds = c(rep(9,18)),
#                 popsize = 500, generations = 500,vectorized=FALSE)




# noelem<-length(elemlist)
# nogenes<-(length(elemlist)* length(t[,1]))
# schedprob=nsga2(funct,nogenes,2,#constraints=constrX, cdim=2,
#                 lower.bounds = c(rep(0,nogenes)),
#                 upper.bounds = c(rep(noelem,nogenes)),
#                 popsize = 12, generations = 12,vectorized=FALSE)
# Error in nsga2(funct, nogenes, 2, lower.bounds = c(rep(0, nogenes)), upper.bounds = c(rep(noelem,  : 
#                                                                                             Argument 's_upper_bound' is not a real vector.


# schedprob=nsga2(funct,(length(elemlist)* length(t[,1])),2,#constraints=constrX, cdim=2,
#                 lower.bounds = c(rep(0,(length(elemlist)* length(t[,1])))),
#                 upper.bounds = c(rep(length(elemlist),(length(elemlist)* length(t[,1])))),
#                 popsize = 12, generations = 12,vectorized=FALSE)



# inputvars<-nrow(chrom)*ncol(chrom)
# 
# schedprobb<-nsga2R(fn=funct, varNo=inputvars, objDim=2, lowerBounds = rep(0, varNo), upperBounds = rep(1, varNo),
#                    popSize = 12, tourSize = 2, generations = 12, cprob = 0.7, XoverDistIdx = 5,
#                    mprob = 0.2, MuDistIdx = 10)
