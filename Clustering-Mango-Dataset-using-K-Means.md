Clustering Mango Dataset using K-Means
================
Muhammad Athanabil Andi Fawazdhia
2022-12-16

# Preliminary

K-Means is one of many algorithms that can do clustering or unsupervised
task. K-Means find label using metrics distances, widely using Euclidean
distances. In this projects, i want to find label (large/medium/small)
from Mango dataset using k-means

# Library Used

``` r
library(factoextra)
library(tidyverse)
library(cluster) 
library(ggplot2) 
library(dplyr) 
library(broom) 
library(ggdendro) 
library(readxl)
library(fmsb)
library(NbClust)
library(RColorBrewer)
library(gridExtra)
```

# Data Used

``` r
data <- read_excel("C:/Users/ASUS/Downloads/Nabil/Projek/Dataset_Mango Classification.xlsx")
data <- as.data.frame(data)
rownames(data) <- data$Mango_Types
data <- data[,-1:-2]
head(data)
```

    ##                     Mango_Shape Mango_LeafShape Mango_Color Mango_Weight
    ## Madu Anggur Mango         Round        Crinkled       Green        342.5
    ## Gedong Mango              Round            Flat      Yellow        352.1
    ## Kemang Mango               Oval        Crinkled       Green        571.9
    ## Chokanan Mango    Kidney Shaped            Flat      Yellow        450.8
    ## Garifta Mango     Kidney Shaped            Flat         Red        445.6
    ## Pakel Mango               Round            Flat       Green        348.5
    ##                   Mango_Length Mango_Circumference
    ## Madu Anggur Mango         10.0                24.3
    ## Gedong Mango              11.5                24.4
    ## Kemang Mango              15.4                29.9
    ## Chokanan Mango            14.0                25.1
    ## Garifta Mango             13.5                26.8
    ## Pakel Mango               11.7                23.3

# Exploratory Data Analyst

## Check Dimension Data

``` r
dim(data)
```

    ## [1] 15  6

Data have 15 rows and 6 columns

## Check Type Each Variable

``` r
str(data)
```

    ## 'data.frame':    15 obs. of  6 variables:
    ##  $ Mango_Shape        : chr  "Round" "Round" "Oval" "Kidney Shaped" ...
    ##  $ Mango_LeafShape    : chr  "Crinkled" "Flat" "Crinkled" "Flat" ...
    ##  $ Mango_Color        : chr  "Green" "Yellow" "Green" "Yellow" ...
    ##  $ Mango_Weight       : num  342 352 572 451 446 ...
    ##  $ Mango_Length       : num  10 11.5 15.4 14 13.5 11.7 12.6 14.3 12.3 14.7 ...
    ##  $ Mango_Circumference: num  24.3 24.4 29.9 25.1 26.8 23.3 24.8 27.6 25.8 27 ...

Type variable Mango_Sahpe, Mango_LeafShape and Mango_Color as characther
and the rest are numeric

## Making Histogram for Each Numeric Variabel

``` r
par(mfrow=c(2,2))
hist(data[,4], col="yellow", main = "Mango_Weight")
hist(data[,5], col="red", main = "Mango_Length")
hist(data[,6], col="cyan", main = "Mango_Circumference")
```

![](Clustering-Mango-Dataset-using-K-Means_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Making Plot for Numeric Variable

``` r
plot(data, col="cyan")
```

![](Clustering-Mango-Dataset-using-K-Means_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Making Boxplot for Numeric Variable

``` r
boxplot(data[,4:6])
```

![](Clustering-Mango-Dataset-using-K-Means_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

There are one outlier in Mango_Circumference

# Preprocessing

## Label Encoding

``` r
data$Mango_Shape <- as.numeric(factor(data$Mango_Shape))
data$Mango_LeafShape <- as.numeric(factor(data$Mango_LeafShape))
data$Mango_Color <- as.numeric(factor(data$Mango_Color))
head(data)
```

    ##                   Mango_Shape Mango_LeafShape Mango_Color Mango_Weight
    ## Madu Anggur Mango           3               1           1        342.5
    ## Gedong Mango                3               2           3        352.1
    ## Kemang Mango                2               1           1        571.9
    ## Chokanan Mango              1               2           3        450.8
    ## Garifta Mango               1               2           2        445.6
    ## Pakel Mango                 3               2           1        348.5
    ##                   Mango_Length Mango_Circumference
    ## Madu Anggur Mango         10.0                24.3
    ## Gedong Mango              11.5                24.4
    ## Kemang Mango              15.4                29.9
    ## Chokanan Mango            14.0                25.1
    ## Garifta Mango             13.5                26.8
    ## Pakel Mango               11.7                23.3

Label Encoding have a goal to transform categorical data to numeric.

## Normalization

``` r
data_norm <- scale(data)
head(data_norm)
```

    ##                   Mango_Shape Mango_LeafShape Mango_Color Mango_Weight
    ## Madu Anggur Mango   1.3592235      -1.1660918  -0.7245688   -1.0897888
    ## Gedong Mango        1.3592235       0.1793987   1.6906606   -0.9575039
    ## Kemang Mango        0.1599086      -1.1660918  -0.7245688    2.0712694
    ## Chokanan Mango     -1.0394062       0.1793987   1.6906606    0.4025503
    ## Garifta Mango      -1.0394062       0.1793987   0.4830459    0.3308960
    ## Pakel Mango         1.3592235       0.1793987  -0.7245688   -1.0071108
    ##                   Mango_Length Mango_Circumference
    ## Madu Anggur Mango   -1.9229867          -0.7558234
    ## Gedong Mango        -0.9073247          -0.7005193
    ## Kemang Mango         1.7333965           2.3412092
    ## Chokanan Mango       0.7854453          -0.3133902
    ## Garifta Mango        0.4468913           0.6267804
    ## Pakel Mango         -0.7719031          -1.3088650

scale() is a function to do Normalization in R Studio, the goal is to
change the scala data for example 1000,10,100 to range -3 to 3 using Z
Score formula.

# K-Means Clustering

``` r
set.seed(123)
#making prediction label using kmeans
km_label = kmeans(data_norm, 3)
```

## Making a Plot Clustering

``` r
#making plot cluster
fviz_cluster(km_label, data = data)
```

![](Clustering-Mango-Dataset-using-K-Means_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Plot result that k-means succes making 3 clusters from dataset.

## Making a New DataFrame

``` r
#making new dataframe
df <- cbind(data, km_label$cluster)
df_final <- data.frame(df)
df_final
```

    ##                   Mango_Shape Mango_LeafShape Mango_Color Mango_Weight
    ## Madu Anggur Mango           3               1           1        342.5
    ## Gedong Mango                3               2           3        352.1
    ## Kemang Mango                2               1           1        571.9
    ## Chokanan Mango              1               2           3        450.8
    ## Garifta Mango               1               2           2        445.6
    ## Pakel Mango                 3               2           1        348.5
    ## Kweni Mango                 3               3           1        368.6
    ## Manalagi Mango              1               2           1        440.2
    ## Dodol Mango                 2               2           1        402.2
    ## Lalijiwo Mango              2               1           1        507.4
    ## Malibu Mango                1               1           2        416.4
    ## Arumanis Mango              1               3           1        298.1
    ## Legong Mango                2               3           2        465.3
    ## Gandaria Mango              1               2           3        420.5
    ## Golek Mango                 2               1           1        493.7
    ##                   Mango_Length Mango_Circumference km_label.cluster
    ## Madu Anggur Mango         10.0                24.3                1
    ## Gedong Mango              11.5                24.4                1
    ## Kemang Mango              15.4                29.9                2
    ## Chokanan Mango            14.0                25.1                3
    ## Garifta Mango             13.5                26.8                3
    ## Pakel Mango               11.7                23.3                1
    ## Kweni Mango               12.6                24.8                1
    ## Manalagi Mango            14.3                27.6                2
    ## Dodol Mango               12.3                25.8                1
    ## Lalijiwo Mango            14.7                27.0                2
    ## Malibu Mango              12.5                25.5                3
    ## Arumanis Mango            11.3                22.5                1
    ## Legong Mango              14.2                25.5                3
    ## Gandaria Mango            12.6                26.0                3
    ## Golek Mango               12.0                26.5                2

## Making polygon plot

``` r
new <- aggregate(df[,-ncol(df)], list(df[,ncol(df)]), mean)
new
```

    ##   Group.1 Mango_Shape Mango_LeafShape Mango_Color Mango_Weight Mango_Length
    ## 1       1        2.50        2.166667    1.333333       352.00     11.56667
    ## 2       2        1.75        1.250000    1.000000       503.30     14.10000
    ## 3       3        1.20        2.000000    2.400000       439.72     13.36000
    ##   Mango_Circumference
    ## 1            24.18333
    ## 2            27.75000
    ## 3            25.78000

``` r
# Set graphic colors
library(RColorBrewer)
coul <- brewer.pal(3, 'Dark2')
colors_border <- coul
library(scales)
colors_in <- alpha(coul, 0.3)

radarchart( new[,-1], axistype=0 , maxmin=F,
    #custom polygon
    pcol=colors_border, pfcol=colors_in, plwd=4, plty=1,
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
    #custom labels
    vlcex=0.8 
    )

# Add a legend
legend(x=1.7, y=1, legend = new$Group.1, bty = "n", pch=20, col=colors_in, text.col = "grey", cex=1.2, pt.cex=3)
```

![](Clustering-Mango-Dataset-using-K-Means_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## Conclusion

Members of Cluster 2 show dominant value in Weight, length &
circumference so we can conclude that mango in cluster 2 have large
size, then followed up by cluster 3 as medium size, and cluster 1 as
small size.
