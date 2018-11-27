#input data dari excel ke R
tengah <-read.csv('C:/Users/user/Desktop/Alfon/TA/nu rm/tengah1.csv', header = TRUE)
dim(tengah)

#plot data                    
library(ggplot2)
Kelas_Lahan_05 <-as.factor(tengah$kl_2005)
Kelas_Lahan_10 <- as.factor(tengah$kl_2010)
levels(Kelas_Lahan_05) <- list("Hutan"=1,"Ladang"=2,"Perkebunan"=3,
                            "Kawasan Terbangun"=4,
                            "Sawah"=5,"Semak"=6,"Perairan"=7,"Tambak"=8)
levels(Kelas_Lahan_10) <- list("Hutan"=1,"Ladang"=2,"Perkebunan"=3,
                            "Kawasan Terbangun"=4,
                            "Sawah"=5,
                            "Semak"=6,"Perairan"=7,"Tambak"=8)
#hasil plot lahan tahun 2005
qplot(tengah$x, tengah$y, data = tengah, colour = Kelas_Lahan_05) +
  scale_color_brewer(palette = "Paired") 
+ xlab("X (m)") + ylab("Y (m)") 
#hasil plot lahan tahun 2010
qplot(tengah$x, tengah$y, data = tengah, colour = Kelas_Lahan_10) +
  scale_color_brewer(palette = "Paired") 
+ xlab("X (m)") + ylab("Y (m)")

#menyusun data ke dalam bentuk matriks ukuran 321 x 321
t<-matrix(tengah[,4], nrow = 321, ncol = 321, byrow = FALSE)
t
brs<-nrow(t)
klm<-ncol(t)
length(t)

#iterasi utk menyusun data
for (i in 1:brs) {
  if (i %% 2 == 0) {
    for (j in 1:(brs/2)) {
      tmp = t[j,i]
      t[j,i] = t[klm-j+1,i ]
      t[klm-j+1, i] = tmp
    }
  }
}

#definisikan nilai awal
satusatu<-0
satudua<-0
satutiga<-0
satuempat<-0
satulima<-0
satuenam<-0
satutujuh<-0
totalsatu<-0

#iterasi untuk menghitung banyak perubahan lahan 
#dari suatu lahan menuju lahan lain
for(i in 1:length(t)) {
  if (t[i] == 5 & t[i+2] == 1) {
    satusatu <- satusatu +1
    totalsatu<- totalsatu+1
  }
  else if (t[i] == 5 & t[i+2] == 2) {
    satudua <- satudua +1
    totalsatu<- totalsatu+1
  }
  else if (t[i] == 5 & t[i+2] == 3) {
    satutiga <- satutiga+1
    totalsatu<- totalsatu+1
  }
  else if (t[i] == 5 & t[i+2] == 4) {
    satuempat <- satuempat +1
    totalsatu<- totalsatu+1
  }
  else if (t[i] == 5 & t[i+2] == 5) {
    satulima <- satulima +1
    totalsatu<- totalsatu+1
  }
  else if (t[i] == 5 & t[i+2] == 6) {
    satuenam <- satuenam +1
    totalsatu<-totalsatu+1
  } else if (t[i] == 5 & t[i+2] == 7) {
    satutujuh<-satutujuh+1
    totalsatu<- totalsatu+1}
}

#panggil hasil
satusatu
satudua
satutiga
satuempat
satulima
satuenam
satutujuh
totalsatu

# perhitungan melibatkan matriks peluang transisi untuk data tahun 2010
library('markovchain')
perubahanlahan <- c("Hutan", "Ladang", "Perkebunan", "Kawasan Terbangun", 
                    "Sawah", "Semak", "Perairan")
byRow  <- TRUE
matrixlahanu <- matrix(data = c(0.9554, 0.0369, 0.0022, 0.0003, 0.0022, 0.0019, 0.0011,
                                0.0115, 0.9565, 0.0103, 0.0037, 0.0088, 0.0058, 0.0034,
                                0.0034, 0.0529, 0.9272, 0.002, 0.007, 0.0066, 0.0009, 
                                0.0049, 0.1699, 0.0184, 0.7692, 0.0304, 0,0.0072,
                                0.0045, 0.0602, 0.0091, 0.0043, 0.8978, 0.0029, 0.0212,
                                0.0062,0.0602, 0.014, 0, 0.0043, 0.9153, 0,
                                0.0416, 0.3351, 0.0156, 0.0139, 0.316, 0, 0.2778),byrow=byRow,nrow = 7,
                       dimnames = list(perubahanlahan,perubahanlahan))

rowSums(matrixlahanu)
mclahanu <- new("markovchain", states = perubahanlahan, byrow=byRow, 
                transitionMatrix = matrixlahanu,name="lahan")
summary(mclahanu)
library(igraph)
library(RColorBrewer)
mclahanu.igraph<-as(mclahanu,"igraph")
mclahanu.igraph
SCC <- clusters(mclahanu.igraph, mode="strong")
V(mclahanu.igraph)$color <- rainbow(SCC$no)[SCC$membership]
plot(mclahanu.igraph, mark.groups = split(1:vcount(mclahanu.igraph), 
                                          SCC$membership), 
     vertex.label.color = "black", vertex.color = "red", edge.color = "orange",
     edge.arrow.size = 0.5, label.font = 0.5)
library(expm)
lem <- matrix(data = c(0.3067, 0.1872, 0.1063, 0.0144, 0.3841, 0.0008, 0.0004),
              nrow = 1, ncol = 7, byrow = TRUE)
lem <- matrix(data = c(0.057, 0.672, 0.049, 0.055, 0.065, 0.067, 0.034),
              nrow = 1, ncol = 7, byrow = TRUE)
nlangkah<-lem%*%(matrixlahanu%^%100)
mel<- t(lem)
nlangkah <-matrixlahanu%^%4
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
specify_decimal(nlangkah, 4)


#perhitungan melibatkan matriks peluang transisi untuk data tahun 2010
matrixlahanu <- matrix(data = c(0.9527, 0.0365, 0.0038, 0.0004, 0.0055, 0.0001, 0.001,
                                0.0108, 0.9541, 0.0118, 0.0068, 0.0117, 0.0013, 0.0035,
                                0.0049, 0.05, 0.9327, 0.0016, 0.0096, 0.0005, 0.0007, 
                                0, 0, 0, 1, 0 ,0 ,0,
                                0.0136, 0.1078, 0.0204, 0.0097, 0.8245, 0.0001, 0.0239,
                                0.0012,0.0985, 0.0099, 0, 0.0012, 0.8892, 0,
                                0, 0, 0, 0, 0, 0, 1),byrow=byRow,nrow = 7,
                       dimnames = list(perubahanlahan,perubahanlahan))
rowSums(matrixlahanu)
mclahanu <- new("markovchain", states = perubahanlahan, byrow=byRow, 
                transitionMatrix = matrixlahanu,name="lahan")

#plot diagram transisi keadaan
library(igraph)
library(RColorBrewer)
mclahanu.igraph<-as(mclahanu,"igraph")
mclahanu.igraph
SCC <- clusters(mclahanu.igraph, mode="strong")
V(mclahanu.igraph)$color <- rainbow(SCC$no)[SCC$membership]
plot(mclahanu.igraph, mark.groups = split(1:vcount(mclahanu.igraph), 
                                          SCC$membership), 
     vertex.label.color = "black", vertex.color = "red", 
     edge.color = "orange",edge.arrow.size = 0.5, label.font = 0.5)

library(expm)
#hitung matriks peluang transisi untuk 2 langkah
nlangkah <-matrixlahanu%^%2  
#pembulatan kedalam 4 angka dibelakang koma
specify_decimal(nlangkah, 4) 

