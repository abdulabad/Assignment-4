# Assignment-4
### Author: Abdul Abad
### Date: November 11, 2018

Principle Component Analysis
================

Data
----

The data you will be using comes from the Assistments online intelligent tutoring system (<https://www.assistments.org/>). It describes students working through online math problems. Each student has the following data associated with them:

-   id
-   prior\_prob\_count: How many problems a student has answered in the system prior to this session
-   prior\_percent\_correct: The percentage of problems a student has answered correctly prior to this session
-   problems\_attempted: The number of problems the student has attempted in the current session
-   mean\_correct: The average number of correct answers a student made on their first attempt at problems in the current session
-   mean\_hint: The average number of hints a student asked for in the current session
-   mean\_attempt: The average number of attempts a student took to answer a problem in the current session
-   mean\_confidence: The average confidence each student has in their ability to answer the problems in the current session

Start by uploading the data
---------------------------

``` r
D1 <- read.csv("Assistments-confidence.csv", header = TRUE)
D1$id <- NULL

  #We won't need the id variable, so remove that.
```

Create a correlation matrix of the relationships between the variables, including correlation coefficients for each pair of variables/features.
-----------------------------------------------------------------------------------------------------------------------------------------------

``` r
#You can install the corrplot package to plot some pretty correlation matrices (sometimes called correlograms)

library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
#Generate pairwise correlations
COR <- cor(D1)

corrplot(COR, order="AOE", method="circle", tl.pos="lt", type="upper",        
tl.col="black", tl.cex=0.6, tl.srt=45, 
        addCoef.col="black", addCoefasPercent = TRUE,
        sig.level=0.50, insig = "blank")
```

![](Abdul_Abad_Assignment_4_Final_Submission_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
#Study your correlogram image and save it, you will need it later
```

Create a new data frame with the mean\_correct variables removed
----------------------------------------------------------------

``` r
D1$mean_correct <- NULL
D2 <- D1
#The, scale and center your data for easier interpretation
D2 <- scale(D2, center = TRUE)
```

Now run the PCA on the new data frame
-------------------------------------

``` r
pca <- prcomp(D2, scale = TRUE)
```

Although princomp does not generate the eigenvalues directly for us, we can print a list of the standard deviation of the variance accounted for by each component.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
pca$sdev
```

    ## [1] 1.2825140 1.0543565 1.0245688 0.9621486 0.8556715 0.7320146

``` r
#To convert this into variance accounted for we can square it, these numbers are proportional to the eigenvalue

pca$sdev^2
```

    ## [1] 1.6448423 1.1116675 1.0497412 0.9257299 0.7321737 0.5358454

``` r
#A summary of our pca will give us the proportion of variance accounted for by each component

summary(pca)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3    PC4    PC5     PC6
    ## Standard deviation     1.2825 1.0544 1.0246 0.9621 0.8557 0.73201
    ## Proportion of Variance 0.2741 0.1853 0.1750 0.1543 0.1220 0.08931
    ## Cumulative Proportion  0.2741 0.4594 0.6344 0.7887 0.9107 1.00000

``` r
#We can look at this to get an idea of which components we should keep and which we should drop

plot(pca, type = "lines")
```

![](Abdul_Abad_Assignment_4_Final_Submission_files/figure-markdown_github/unnamed-chunk-5-1.png)

Think about which components you would drop and make a decision
---------------------------------------------------------------

Part II
-------

``` r
#Now, create a data frame of the transformed data from your pca.

D1 <- read.csv("Assistments-confidence.csv", header = TRUE)

D3 <- as.data.frame(pca$x)

#Attach the variable "mean_correct" from your original data frame to D3.


D4 <- cbind(D3, as.data.frame(D1$mean_correct))

#Now re-run your scatterplots and correlations between the transformed data and mean_correct. If you had dropped some components would you have lost important infomation about mean_correct?

COR2 <- cor(D4)
```

Now print out the eigenvectors (often called loadings) for the components you generated:
----------------------------------------------------------------------------------------

``` r
pca$rotation
```

    ##                               PC1         PC2         PC3        PC4
    ## prior_prob_count      -0.26034140  0.45818753 -0.40090679 -0.6897642
    ## prior_percent_correct  0.16840319  0.81617867  0.09267306  0.2640040
    ## problems_attempted    -0.45568733  0.31685183  0.36387724  0.3168141
    ## mean_hint             -0.63337594 -0.12501620 -0.08008842 -0.1122586
    ## mean_attempt          -0.54200011 -0.08510858 -0.04585364  0.3108682
    ## mean_confidence        0.03581325  0.02547483 -0.83051917  0.4948890
    ##                                PC5         PC6
    ## prior_prob_count      -0.007142834 -0.29280482
    ## prior_percent_correct  0.298843852  0.37134715
    ## problems_attempted    -0.592336569 -0.32911025
    ## mean_hint             -0.102302115  0.74412634
    ## mean_attempt           0.697232132 -0.33781385
    ## mean_confidence       -0.251357022 -0.01452143

``` r
#Examine the eigenvectors, notice that they are a little difficult to interpret. It is much easier to make sense of them if we make them proportional within each component

loadings <- abs(pca$rotation) #abs() will make all eigenvectors positive

sweep(loadings, 2, colSums(loadings), "/") #sweep() computes each row as a proportion of the column. (There must be a way to do this with dplyr()?)
```

    ##                              PC1        PC2        PC3        PC4
    ## prior_prob_count      0.12423113 0.25081186 0.22101700 0.31516257
    ## prior_percent_correct 0.08035956 0.44677621 0.05108998 0.12062699
    ## problems_attempted    0.21744737 0.17344469 0.20060288 0.14475664
    ## mean_hint             0.30223780 0.06843387 0.04415217 0.05129246
    ## mean_attempt          0.25863458 0.04658844 0.02527878 0.14203987
    ## mean_confidence       0.01708956 0.01394492 0.45785919 0.22612148
    ##                               PC5        PC6
    ## prior_prob_count      0.003664468 0.14011651
    ## prior_percent_correct 0.153315014 0.17770154
    ## problems_attempted    0.303884750 0.15748983
    ## mean_hint             0.052483764 0.35608836
    ## mean_attempt          0.357699023 0.16165478
    ## mean_confidence       0.128952980 0.00694897

``` r
#Now examine your components and try to come up with substantive descriptions of what some might represent?

#You can generate a biplot to help you, though these can be a bit confusing. They plot the transformed data by the first two components. Therefore, the axes represent the direction of maximum variance. Then mapped onto this point cloud are the original directions of the variables, depicted as red arrows. It is supposed to provide a visualization of which variables "go together". Variables that possibly represent the same underlying construct point in the same direction.  

biplot(pca)
```

![](Abdul_Abad_Assignment_4_Final_Submission_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
#Calculate values for each student that represent these your composite variables and then create a new correlogram showing their relationship to mean_correct.
```

Part III
========

Also in this repository is a data set and codebook from Rod Martin, Patricia Puhlik-Doris, Gwen Larsen, Jeanette Gray, Kelly Weir at the University of Western Ontario about people's sense of humor. Can you perform a PCA on this data?
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
D1 <- read.csv("humor_data.csv", header = TRUE)

#Plot correlation matrices like we did in previous parts

library(corrplot)

#Generate pairwise correlations
COR <- cor(D1)

corrplot(COR, order="AOE", method="circle", tl.pos="lt", type="upper",        
tl.col="black", tl.cex=0.6, tl.srt=45, 
        addCoef.col="black", addCoefasPercent = TRUE,
        sig.level=0.50, insig = "blank")
```

![](Abdul_Abad_Assignment_4_Final_Submission_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
#Study your correlogram image and save it, you will need it later
```

``` r
D1$mean_correct <- NULL
D2 <- D1
#The, scale and center your data for easier interpretation
D2 <- scale(D2, center = TRUE)
```

``` r
pca <- prcomp(D2, scale = TRUE)
```

``` r
pca$sdev
```

    ##  [1] 2.77741205 1.99741106 1.82727783 1.60136384 1.41994758 1.08471580
    ##  [7] 1.05831097 1.01722610 0.98473607 0.96930324 0.94652821 0.92110808
    ## [13] 0.90289108 0.88599396 0.86076682 0.84486247 0.83509363 0.79956676
    ## [19] 0.78074605 0.76943615 0.75814138 0.74351947 0.74010850 0.72710613
    ## [25] 0.71879884 0.70969332 0.69294580 0.68753963 0.65853463 0.65579228
    ## [31] 0.64351843 0.61413918 0.60177193 0.56000492 0.52627934 0.09014409
    ## [37] 0.04631333 0.04194096 0.03965788

``` r
#To convert this into variance accounted for we can square it, these numbers are proportional to the eigenvalue

pca$sdev^2
```

    ##  [1] 7.714017706 3.989650939 3.338944253 2.564366147 2.016251126
    ##  [6] 1.176608357 1.120022107 1.034748940 0.969705120 0.939548776
    ## [11] 0.895915650 0.848440100 0.815212296 0.784985298 0.740919512
    ## [16] 0.713792588 0.697381379 0.639306999 0.609564388 0.592031995
    ## [21] 0.574778350 0.552821200 0.547760591 0.528683322 0.516671774
    ## [26] 0.503664605 0.480173888 0.472710740 0.433667859 0.430063512
    ## [31] 0.414115970 0.377166933 0.362129458 0.313605509 0.276969940
    ## [36] 0.008125957 0.002144925 0.001759044 0.001572748

``` r
#A summary of our pca will give us the proportion of variance accounted for by each component

summary(pca)
```

    ## Importance of components:
    ##                           PC1    PC2     PC3     PC4    PC5     PC6
    ## Standard deviation     2.7774 1.9974 1.82728 1.60136 1.4199 1.08472
    ## Proportion of Variance 0.1978 0.1023 0.08561 0.06575 0.0517 0.03017
    ## Cumulative Proportion  0.1978 0.3001 0.38571 0.45146 0.5032 0.53333
    ##                            PC7     PC8     PC9    PC10    PC11    PC12
    ## Standard deviation     1.05831 1.01723 0.98474 0.96930 0.94653 0.92111
    ## Proportion of Variance 0.02872 0.02653 0.02486 0.02409 0.02297 0.02175
    ## Cumulative Proportion  0.56205 0.58858 0.61344 0.63753 0.66051 0.68226
    ##                          PC13    PC14   PC15   PC16    PC17    PC18
    ## Standard deviation     0.9029 0.88599 0.8608 0.8449 0.83509 0.79957
    ## Proportion of Variance 0.0209 0.02013 0.0190 0.0183 0.01788 0.01639
    ## Cumulative Proportion  0.7032 0.72329 0.7423 0.7606 0.77847 0.79487
    ##                           PC19    PC20    PC21    PC22    PC23    PC24
    ## Standard deviation     0.78075 0.76944 0.75814 0.74352 0.74011 0.72711
    ## Proportion of Variance 0.01563 0.01518 0.01474 0.01417 0.01405 0.01356
    ## Cumulative Proportion  0.81050 0.82568 0.84042 0.85459 0.86864 0.88219
    ##                           PC25    PC26    PC27    PC28    PC29    PC30
    ## Standard deviation     0.71880 0.70969 0.69295 0.68754 0.65853 0.65579
    ## Proportion of Variance 0.01325 0.01291 0.01231 0.01212 0.01112 0.01103
    ## Cumulative Proportion  0.89544 0.90835 0.92067 0.93279 0.94391 0.95493
    ##                           PC31    PC32    PC33    PC34   PC35    PC36
    ## Standard deviation     0.64352 0.61414 0.60177 0.56000 0.5263 0.09014
    ## Proportion of Variance 0.01062 0.00967 0.00929 0.00804 0.0071 0.00021
    ## Cumulative Proportion  0.96555 0.97522 0.98451 0.99255 0.9997 0.99986
    ##                           PC37    PC38    PC39
    ## Standard deviation     0.04631 0.04194 0.03966
    ## Proportion of Variance 0.00005 0.00005 0.00004
    ## Cumulative Proportion  0.99991 0.99996 1.00000

``` r
#We can look at this to get an idea of which components we should keep and which we should drop

plot(pca, type = "lines")
```

![](Abdul_Abad_Assignment_4_Final_Submission_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
#Now, create a data frame of the transformed data from your pca.

D1 <- read.csv("humor_data.csv", header = TRUE)

D3 <- as.data.frame(pca$x)

#Attach the variable "mean_correct" from your original data frame to D3.



#Now re-run your scatterplots and correlations between the transformed data and mean_correct. If you had dropped some components would you have lost important infomation about mean_correct?

COR2 <- cor(D4)
```
