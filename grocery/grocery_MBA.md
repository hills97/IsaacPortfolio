---
title: "grocery MBA"
author: "Donggyu Isaac Hills"
output: 
  html_document:
    keep_md: true
---



#### Cleaning and exploring 

```r
sum(is.na(grocery))
```

```
## [1] 0
```

```r
str(grocery)
```

```
## 'data.frame':	38765 obs. of  3 variables:
##  $ Member_number  : int  1808 2552 2300 1187 3037 4941 4501 3803 2762 4119 ...
##  $ Date           : chr  "21-07-2015" "05-01-2015" "19-09-2015" "12-12-2015" ...
##  $ itemDescription: chr  "tropical fruit" "whole milk" "pip fruit" "other vegetables" ...
```

```r
head(grocery)
```

```
##   Member_number       Date  itemDescription
## 1          1808 21-07-2015   tropical fruit
## 2          2552 05-01-2015       whole milk
## 3          2300 19-09-2015        pip fruit
## 4          1187 12-12-2015 other vegetables
## 5          3037 01-02-2015       whole milk
## 6          4941 14-02-2015       rolls/buns
```

```r
sorted <- grocery[order(grocery$Member_number),]
```

#### Including Plots




