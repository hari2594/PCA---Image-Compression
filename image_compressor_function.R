#-------------------------------------------------------------------
#FUNCTION 1
#-------------------------------------------------------------------
imagecompress=function(image_path,saveas,calarity)
{
  library(jpeg)
  image=readJPEG(image_path)
  
  r <- image[,,1]
  g <- image[,,2]
  b <- image[,,3]
  
  image.r.pca <- prcomp(r, center = FALSE)
  image.g.pca <- prcomp(g, center = FALSE)
  image.b.pca <- prcomp(b, center = FALSE)
  
  #Collect the PCA objects into a list.
  rgb.pca <- list(image.r.pca, image.g.pca, image.b.pca)
  length(rgb.pca)
  
  
  # Selecting the ncomp value
  propvar_r=cumsum(image.r.pca$sdev^2)/sum(image.r.pca$sdev^2)*100
  propvar_b=cumsum(image.b.pca$sdev^2)/sum(image.b.pca$sdev^2)*100
  propvar_g=cumsum(image.g.pca$sdev^2)/sum(image.g.pca$sdev^2)*100
  
  count_r=0
  count_b=0
  count_g=0
  
  for (i in propvar_r) {
    if (i<=calarity){ 
      count_r <- count_r +1
    } else {
      break
    }
  }
  
  for (i in propvar_b) {
    if (i<=calarity){ 
      count_b <- count_b +1
    } else {
      break
    }
  }
  
  for (i in propvar_g) {
    if (i<=calarity){ 
      count_g <- count_g +1
    } else {
      break
    }
  }
  
  ncomp=max(count_b,count_g,count_r)
  #print(i," - ",ncomp)
  print(paste(image_path,ncomp,sep= "-"))
  
  #Reconstruct
  
  R = image.r.pca$x[,1:ncomp]%*%t(image.r.pca$rotation[,1:ncomp])
  B = image.b.pca$x[,1:ncomp]%*%t(image.b.pca$rotation[,1:ncomp])
  G = image.g.pca$x[,1:ncomp]%*%t(image.g.pca$rotation[,1:ncomp])
  
  R = ifelse(R>1,1,R)
  G = ifelse(G>1,1,G)
  B = ifelse(B>1,1,B)
  
  R = ifelse(R<0,0,R)
  G = ifelse(G<0,0,G)
  B = ifelse(B<0,0,B)
  
  img=array(c(R,G,B),dim=c(dim(image)))
  
  fil=paste(getwd(),"/",saveas,sep= "")     # renaming the file
  writeJPEG(img,fil)                    # saving the compressed file
  
}


#-------------------------------------------------------------------
#FUNCTION 2
#-------------------------------------------------------------------
compressorg=function(loc,calarity,delorginal=F)
{
  default=getwd()

  # Extracting only the image file
  files=list.files(path = loc,all.files = T)
  img.files = files[grep(".jpg", files, fixed=T)]
  
  
  # getting the path of all the image files
  p=c()
  for(i in img.files)
  {
    path=paste(loc,"\\",i,sep= "")
    p=c(p,path)
  }
  
  # for testing--------------------
  # p=p[c(2,1)]
  count=0
  
  for (i in p)
    {
    library(jpeg)
    image=readJPEG(i)
    print=i
    r <- image[,,1]
    g <- image[,,2]
    b <- image[,,3]
    
    image.r.pca <- prcomp(r, center = FALSE)
    image.g.pca <- prcomp(g, center = FALSE)
    image.b.pca <- prcomp(b, center = FALSE)
    
    #Collect the PCA objects into a list.
    rgb.pca <- list(image.r.pca, image.g.pca, image.b.pca)
    length(rgb.pca)
    
    
    # Selecting the ncomp value
    propvar_r=cumsum(image.r.pca$sdev^2)/sum(image.r.pca$sdev^2)*100
    propvar_b=cumsum(image.b.pca$sdev^2)/sum(image.b.pca$sdev^2)*100
    propvar_g=cumsum(image.g.pca$sdev^2)/sum(image.g.pca$sdev^2)*100
    
    count_r=0
    count_b=0
    count_g=0
    
    for (i in propvar_r) {
      if (i<=calarity){ 
        count_r <- count_r +1
      } else {
        break
      }
    }
    
    for (i in propvar_b) {
      if (i<=calarity){ 
        count_b <- count_b +1
      } else {
        break
      }
    }
    
    for (i in propvar_g) {
      if (i<=calarity){ 
        count_g <- count_g +1
      } else {
        break
      }
    }
    
    ncomp=max(count_b,count_g,count_r)
    #print(i," - ",ncomp)
    print(paste(print,ncomp,sep= "-"))
    
    #Reconstruct
    
    R = image.r.pca$x[,1:ncomp]%*%t(image.r.pca$rotation[,1:ncomp])
    B = image.b.pca$x[,1:ncomp]%*%t(image.b.pca$rotation[,1:ncomp])
    G = image.g.pca$x[,1:ncomp]%*%t(image.g.pca$rotation[,1:ncomp])
    
    R = ifelse(R>1,1,R)
    G = ifelse(G>1,1,G)
    B = ifelse(B>1,1,B)
    
    R = ifelse(R<0,0,R)
    G = ifelse(G<0,0,G)
    B = ifelse(B<0,0,B)
    
    img=array(c(R,G,B),dim=c(dim(image)))
    library(stringr)
    # spltting the file name for writing compressed image
    count=count+1
    c=str_split(img.files[count], "\\.")[[1]][1]
    fil=paste(c,"_compress.jpg",sep= "")  # renaming the file
    writeJPEG(img,fil)                    # saving the compressed file
  }
  files=list.files(path = loc,all.files = T)
  #img.files1 = files[grep(".jpg", files, fixed=T)]
  img.files2 = files[grep("compress.jpg", files, fixed=T)]
  
  # getting the path of all the actual image files
  p1=c()
  for(i in img.files)
  {
    path=paste(loc,"\\",i,sep= "")
    p1=c(p1,path)
  }
  
  # getting the path of all the compressed image files
  p2=c()
  for(i in img.files2)
  {
    path=paste(loc,"\\",i,sep= "")
    p2=c(p2,path)
  }
  
  # creating a folder for the compressed images
  dir.create(file.path(loc,"compressed_image"), showWarnings = FALSE)
  setwd(file.path(loc,"compressed_image"))
  
  # copying the compressed images to the created folder
  for (i in p2)
  {
    file.copy(from=i, to=getwd(), 
              overwrite = TRUE, recursive = FALSE, 
              copy.mode = TRUE)
    file.remove(i)
  }
  
  #check for the condition to delete the orginal file - if "yes" copy the orginal files
  #by creating a folder and deleteing from the source destination
  if (delorginal == T){ 
    dir.create(file.path(loc,"Orginal_image"), showWarnings = FALSE)
    setwd(file.path(loc,"Orginal_image"))
    for (i in p1)
    {
      file.copy(from=i, to=getwd(), 
                overwrite = TRUE, recursive = FALSE, 
                copy.mode = TRUE)
      file.remove(i)
    }
  }
  setwd(default)
}



#-------------------------------------------------------------------
#TESTING THE FUNCTIONS
#-------------------------------------------------------------------

#Function1:

setwd("C:\\Users\\hi\\Desktop\\Programming\\R Programming\\sir\\image")
imagecompress(image_path="C:\\Users\\hi\\Desktop\\Programming\\R Programming\\sir\\image\\katia.jpg",
              saveas="katia_compr9925.jpg",calarity=99.25)

#Function2:
compressorg(loc="C:\\Users\\hi\\Desktop\\Programming\\R Programming\\sir\\image",
            calarity = 99.5,delorginal=F)





