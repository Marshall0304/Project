library(forecast)

#读入数据
#shangzheng <- read.table('E:/study_material/master_slides/data_mining/课程设计/sherlock/000001_output.txt')
shangzheng <- read.table('E:/study_material/master_slides/data_mining/课程设计/sherlock/arima/inputShang6_train.txt')
shangzheng <- shangzheng[5]
shangzhengTest <- read.table('E:/study_material/master_slides/data_mining/课程设计/sherlock/arima/inputShang6_test.txt')
shangzhengTest <- shangzhengTest [5]
#训练模型
arimaModel<-auto.arima(shangzheng,trace=T)
orign <- arimaModel$x
pre <- predict(arimaModel,n.ahead = 1,shangzhengTest)
pre <- fitted(arimaModel)
write.table(orign, file = 'orign.txt', row.names = F, quote = F)
write.table(pre, file = 'pre.txt', row.names = F, quote = F)

#画图
plot(orign,col='red')
lines(pre,col='blue')

#预测1天数据
preResult<-forecast(arimaModel,1)
#plot(preResult)


##
> arimaModel
Series: shangzheng 
ARIMA(3,1,3)                    

Coefficients:
         ar1      ar2     ar3      ma1     ma2      ma3
      0.6139  -0.7892  0.4380  -0.5741  0.7329  -0.3450
s.e.  0.1219   0.0843  0.1066   0.1259  0.0924   0.1074

sigma^2 estimated as 1770:  log likelihood=-32107.43
AIC=64228.85   AICc=64228.87   BIC=64276.01
##

##
划分训练集和测试集
> arimaModel
Series: shangzheng 
ARIMA(4,1,4) with drift         

Coefficients:
         ar1      ar2     ar3      ar4      ma1     ma2      ma3     ma4
      0.1223  -0.0525  0.2138  -0.8299  -0.1191  0.0619  -0.1990  0.8951
s.e.  0.0334   0.0303  0.0421   0.0461   0.0284  0.0238   0.0348  0.0400
       drift
      0.5148
s.e.  0.6130

sigma^2 estimated as 1594:  log likelihood=-24268.77
AIC=48557.54   AICc=48557.59   BIC=48622.21
##

## fitted.arima
function (object, biasadj = FALSE, ...) 
{
    x <- getResponse(object)
    if (!is.null(object$fitted)) {
        return(object$fitted)
    }
    if (is.null(x)) {
        return(NULL)
    }
    if (is.null(object$lambda)) {
        return(x - object$residuals)
    }
    else {
        fits <- InvBoxCox(BoxCox(x, object$lambda) - object$residuals, 
            object$lambda)
        if (biasadj) {
            return(InvBoxCoxf(x = fits, fvar = var(object$residuals), 
                lambda = object$lambda))
        }
        else {
            return(fits)
        }
    }
}

##
##
> getS3method("predict", "Arima")
function (object, n.ahead = 1L, newxreg = NULL, se.fit = TRUE, 
    ...) 
{
    myNCOL <- function(x) if (is.null(x)) 
        0
    else NCOL(x)
    rsd <- object$residuals
    xr <- object$call$xreg
    xreg <- if (!is.null(xr)) 
        eval.parent(xr)
    else NULL
    ncxreg <- myNCOL(xreg)
    if (myNCOL(newxreg) != ncxreg) 
        stop("'xreg' and 'newxreg' have different numbers of columns")
    class(xreg) <- NULL
    xtsp <- tsp(rsd)
    n <- length(rsd)
    arma <- object$arma
    coefs <- object$coef
    narma <- sum(arma[1L:4L])
    if (length(coefs) > narma) {
        if (names(coefs)[narma + 1L] == "intercept") {
            xreg <- cbind(intercept = rep(1, n), xreg)
            newxreg <- cbind(intercept = rep(1, n.ahead), newxreg)
            ncxreg <- ncxreg + 1L
        }
        xm <- if (narma == 0) 
            drop(as.matrix(newxreg) %*% coefs)
        else drop(as.matrix(newxreg) %*% coefs[-(1L:narma)])
    }
    else xm <- 0
    if (arma[2L] > 0L) {
        ma <- coefs[arma[1L] + 1L:arma[2L]]
        if (any(Mod(polyroot(c(1, ma))) < 1)) 
            warning("MA part of model is not invertible")
    }
    if (arma[4L] > 0L) {
        ma <- coefs[sum(arma[1L:3L]) + 1L:arma[4L]]
        if (any(Mod(polyroot(c(1, ma))) < 1)) 
            warning("seasonal MA part of model is not invertible")
    }
    z <- KalmanForecast(n.ahead, object$model)
    pred <- ts(z[[1L]] + xm, start = xtsp[2L] + deltat(rsd), 
        frequency = xtsp[3L])
    if (se.fit) {
        se <- ts(sqrt(z[[2L]] * object$sigma2), start = xtsp[2L] + 
            deltat(rsd), frequency = xtsp[3L])
        list(pred = pred, se = se)
    }
    else pred
}
<bytecode: 0x0000000005966c68>
<environment: namespace:stats>

##