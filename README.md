Multiple Linear Regression and Hypothesis testing
================
Seren Dances
5/24/2022

The working Problem:

Using the data of gas vapor, found on page 183 of the *Linear Models in
Statistics 2nd Edition (Rencher),* and with the assumption that ![y
\\sim N(X\\beta, \\sigma^2
I),](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;y%20%5Csim%20N%28X%5Cbeta%2C%20%5Csigma%5E2%20I%29%2C
"y \\sim N(X\\beta, \\sigma^2 I),") where ![\\pmb{\\beta} =
(\\beta\_0,\\beta\_1,\\beta\_2,\\beta\_3,\\beta\_4)'](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cpmb%7B%5Cbeta%7D%20%3D%20%28%5Cbeta_0%2C%5Cbeta_1%2C%5Cbeta_2%2C%5Cbeta_3%2C%5Cbeta_4%29%27
"\\pmb{\\beta} = (\\beta_0,\\beta_1,\\beta_2,\\beta_3,\\beta_4)'"). When
gasoline is pumped into the tank of a car, vapors are vented into the
atmosphere. An experiment was conducted to determine whether y, the
amount of vapor, can be predicted using the following four variables
based on initial conditions of the tank and the dispensed gasoline:

![x\_1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x_1
"x_1") = tank temperature
(![^\\circ](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5E%5Ccirc
"^\\circ")F)

![x\_2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x_2
"x_2") = gasoline temperature
(![^\\circ](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5E%5Ccirc
"^\\circ")F)

![x\_3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x_3
"x_3") = vapor pressure in tank (psi)

![x\_4](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x_4
"x_4") = vapor pressure of gasoline (psi)

1.(a) Test ![H\_0: \\beta\_2 = \\beta\_4
= 0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;H_0%3A%20%5Cbeta_2%20%3D%20%5Cbeta_4%20%3D%200
"H_0: \\beta_2 = \\beta_4 = 0"):

Loading Data…

``` r
X <- read.csv('443dancesdata.csv')
X <- unlist(X)
X <- array(X, dim = c(32,5))
y = X[,1]
```

The data set where the first col is y and the rest are the x values:

``` 
       y x1 x2   x3   x4
 [1,] 29 33 53 3.32 3.42
 [2,] 24 31 36 3.10 3.26
 [3,] 26 33 51 3.18 3.18
 [4,] 22 37 51 3.39 3.08
 [5,] 27 36 54 3.20 3.41
 [6,] 21 35 35 3.03 3.03
 [7,] 33 59 56 4.78 4.57
 [8,] 34 60 60 4.72 4.72
 [9,] 32 59 60 4.60 4.41
[10,] 34 60 60 4.53 4.53
[11,] 20 34 35 2.90 2.95
[12,] 36 60 59 4.40 4.36
[13,] 34 60 62 4.31 4.42
[14,] 23 60 36 4.27 3.94
[15,] 24 62 38 4.41 3.49
[16,] 32 62 61 4.39 4.39
[17,] 40 90 64 7.32 6.70
[18,] 46 90 60 7.32 7.20
[19,] 55 92 92 7.45 7.45
[20,] 52 91 92 7.27 7.26
[21,] 29 61 62 3.91 4.08
[22,] 22 59 42 3.75 3.45
[23,] 31 88 65 6.48 5.80
[24,] 45 91 89 6.70 6.60
[25,] 37 63 62 4.30 4.30
[26,] 37 60 61 4.02 4.10
[27,] 33 60 62 4.02 3.89
[28,] 27 59 62 3.98 4.02
[29,] 34 59 62 4.39 4.53
[30,] 19 37 35 2.75 2.64
[31,] 16 35 35 2.59 2.59
[32,] 22 37 37 2.73 2.59
```

Creating the design matrix.

``` r
design = X[,c(2:5)]
j = rep(1,32)

#unrestricted design matrix
design <- cbind(j, design)

#restricted design matrix
design.R <- cbind(design[,c(1:2)],design[,5])
```

To find the coefficient estimates for the unrestricted
![\\beta's](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta%27s
"\\beta's"). Find ![(X'X)^{-1}
X'y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%28X%27X%29%5E%7B-1%7D%20X%27y
"(X'X)^{-1} X'y").

``` r
#UR for unrestricted
coef.UR <- solve(t(design) %*% design) %*% t(design) %*% y
coef.UR
```

``` 
          [,1]
j   1.01501756
x1 -0.02860886
x2  0.21581693
x3 -4.32005167
x4  8.97488928
```

To find the coefficient estimates for the restricted model, Find
![(X'\_1 X\_1)^{-1} X'\_1
y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%28X%27_1%20X_1%29%5E%7B-1%7D%20X%27_1%20y
"(X'_1 X_1)^{-1} X'_1 y").

``` r
coef.R <- solve(t(design.R) %*% design.R) %*% t(design.R) %*% y

coef.R
```

``` 
         [,1]
j   4.4730173
x1 -0.1284081
    7.8838074
```

To find the F statistic use the following formula:
![\\frac{\\frac{\\hat{\\beta'}X'y-\\hat{\\beta'^\*\_1
X'\_1y}}{q}}{\\frac{y'y-\\hat{\\beta'}X'y}{(n-k-1)}}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cfrac%7B%5Cfrac%7B%5Chat%7B%5Cbeta%27%7DX%27y-%5Chat%7B%5Cbeta%27%5E%2A_1%20X%27_1y%7D%7D%7Bq%7D%7D%7B%5Cfrac%7By%27y-%5Chat%7B%5Cbeta%27%7DX%27y%7D%7B%28n-k-1%29%7D%7D
"\\frac{\\frac{\\hat{\\beta'}X'y-\\hat{\\beta'^*_1 X'_1y}}{q}}{\\frac{y'y-\\hat{\\beta'}X'y}{(n-k-1)}}")

``` r
#degrees of freedom
h = 2
df = (32-4-1)

#F statistic

numerator = ((t(coef.UR) %*% t(design) %*% y) - (t(coef.R) %*% t(design.R) %*% y))/h

denomenator = (((t(y) %*% y)-(t(coef.UR) %*% t(design) %*% y))/ df)

F = numerator/denomenator

#Critical value
critF = qf(.95,h,df)
```

The F statistic is:

    [1] 12.42426

And the critical value is:

    [1] 3.354131

Based on the F statistic, reject
![H\_0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;H_0
"H_0"). Conclude that the selected values have a significant effect on
the dependent variable y.

2)  Construct a 95% CI for
    ![\\beta\_2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta_2
    "\\beta_2")

To find the 95% CI for
![\\beta\_2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta_2
"\\beta_2"), we must first find some values.

``` r
SSE = (t(y) %*% y - (t(coef.UR) %*% t(design) %*%y))
s = sqrt(SSE/(32-5-1))
```

From
![(X'X)^{-1}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%28X%27X%29%5E%7B-1%7D
"(X'X)^{-1}") we can find
![g\_{jj}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;g_%7Bjj%7D
"g_{jj}").

``` r
XXinv = solve((t(X) %*% X))
XXinv
```

``` 
               y            x1            x2           x3           x4
y   0.0049153593  0.0001614946 -0.0011527478  0.019561241 -0.042570540
x1  0.0001614946  0.0010985764 -0.0002604604 -0.020579143  0.008512408
x2 -0.0011527478 -0.0002604604  0.0007278156  0.006481214 -0.004138046
x3  0.0195612413 -0.0205791434  0.0064812143  1.116142039 -1.087828015
x4 -0.0425705398  0.0085124083 -0.0041380464 -1.087828015  1.355635986
```

So ![g\_{jj}
= 0.0007278156](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;g_%7Bjj%7D%20%3D%200.0007278156
"g_{jj} = 0.0007278156").From the coefficients of the unrestricted model
we can find
![\\hat{\\beta\_2}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Chat%7B%5Cbeta_2%7D
"\\hat{\\beta_2}").

``` r
print(coef.UR[3,])
```

``` 
       x2 
0.2158169 
```

Now to plug and chug using ![\\hat{\\beta\_1} \\pm
t\_{0,025,(32-4-1)}s\\sqrt(g\_{jj})](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Chat%7B%5Cbeta_1%7D%20%5Cpm%20t_%7B0%2C025%2C%2832-4-1%29%7Ds%5Csqrt%28g_%7Bjj%7D%29
"\\hat{\\beta_1} \\pm t_{0,025,(32-4-1)}s\\sqrt(g_{jj})")

``` r
tval = pt(0.025,27)
UpperCI = (coef.UR[3,]) + tval*s*sqrt(0.0007278156)
LowerCI = (coef.UR[3,]) - tval*s*sqrt(0.0007278156)
```

So the 95% CI for
![\\beta\_2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbeta_2
"\\beta_2") is, (0.1775489, 0.254085).

3)  Construct a 90% CI for the *mean* amount of vapor when the tank
    temperature is
    ![85^\\circ](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;85%5E%5Ccirc
    "85^\\circ")F, the fasoline temperature is
    ![78^\\circ](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;78%5E%5Ccirc
    "78^\\circ")F, the vapor pressure in tank is 5.5 psi, and the vapor
    pressure in gasoline is 5.2 psi.

Establish the vector for
![x\_0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x_0
"x_0"):

``` r
xNaught = c(1,85,78,5.5,5.2)
```

Then the expected value of y at
![x\_0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x_0
"x_0"):

``` r
expectedValueY <- (t(xNaught) %*% coef.UR)
```

Now to build the CI:

``` r
#t-value
tval = pt(0.05,27)
#preforming matrix multiplication 
m <- t(xNaught) %*% XXinv %*% xNaught
#calculating bounds
nintyUpper = expectedValueY + tval*s*sqrt(m)
nintyLower = expectedValueY - tval*s*sqrt(m)
```

So the 90% CI for the mean using the given parameter values of
(1,85,78,5.5,5.2) is (34.44082, 42.21143). The interval found represents
the theorized range of the mean amount of vapor at the given parameters.

4)  Under the same conditions as the previous step, find the 90%
    prediction interval for the amount of vapor.

Many of the same variables are used as in the previous step.

``` r
predUpper = expectedValueY + tval*s*sqrt((1+m))
predLower = expectedValueY - tval*s*sqrt((1+m))
```

The result of the 90% PI for amount of vapor is (34.18048, 42.47177).
This interval is very similar to the CI found in the last step however
it is slightly wider.

5)  Interpretation of the prediction interval vs the confidence
    interval:

As stated in part d, the prediction interval is wider. This makes sense
as it is because the
![(X'X)^{-1}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%28X%27X%29%5E%7B-1%7D
"(X'X)^{-1}") value is smaller than 1, and therefore the addition of 1
under the square root sign increaded the magnitude of the interval.
