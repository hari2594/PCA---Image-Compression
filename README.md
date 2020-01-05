# PCA---Image-Compression
The goal of this R Function is to compress arbitrary images using numerical principal component analysis techniques to obtain the most visually appealing compressed image. 

**Image compression with principal component analysis is a dimension reduction technique.** 

PCA is a statistical method that uses orthogonal transformations to turn a potentially correlated set of
data into a linearly uncorrelated set of data which contain principal components. The number of principal components will be less than or equal to the total number of variables in the original dataset. Furthermore, the principal components are sorted in a way so that the first component contains the largest
possible variance in the data, and each succeeding component has the next highest variance.

**The `objective pursued by the analysis of principal components` is the representation of the numerical measurements of 
several variables in a space of few dimensions, where our senses can perceive relationships that would otherwise remain 
hidden in higher dimensions.**

An image is a matrix of pixels represented by RGB color values. Thus, principal component analysis can be used to reduce the
dimensions of the matrix (image) and project those new dimensions to reform the image that retains its qualities but is 
smaller in k-weight. We will use `PCA` to compress the image. As the number of principal components used to 
project the new data increases, the quality and representation compared to the original image improve.
---
#### Code Structure:
```R
setwd("C:\\Users\\hi\\Desktop\\Programming\\R Programming\\sir\\image")
imagecompress(image_path="C:\\Users\\hi\\Desktop\\Programming\\R Programming\\sir\\image\\katia.jpg",
              saveas="katia_compr9925.jpg",calarity=99.25)

#Function2:
compressorg(loc="C:\\Users\\hi\\Desktop\\Programming\\R Programming\\sir\\image",
            calarity = 99.5,delorginal=F)
  .........
```
