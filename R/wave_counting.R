library(dplyr)

CubicInterpSplineAsPiecePoly <- function (x, y, method = c("fmm", "natural", "periodic", "hyman")) {
  ## method validation
  if (!(method %in% c("fmm", "natural", "periodic", "hyman")))
    mystop("'method' must be one of the following: 'fmm', 'natural', 'periodic', 'hyman'!")
  ## use `splinefun` for cubic spline interpolation
  CubicInterpSpline <- stats::splinefun(x, y, method)
  ## extract construction info
  construction_info <- environment(CubicInterpSpline)$z
  ## export as an "PiecePoly" object
  pieces <- seq_len(length(construction_info$x) - 1L)
  PiecePolyCoef <- with(construction_info, rbind(y[pieces], b[pieces], c[pieces], d[pieces], deparse.level = 0L))
  structure(list(PiecePoly = list(coef = PiecePolyCoef, shift = TRUE),
                 knots = construction_info$x), method = method,
            class = c("PiecePoly", "CubicInterpSpline"))
}

## represent a fitted smoothing spline as an interpolation spline
SmoothSplineAsPiecePoly <- function (SmoothSpline) {
  ## input validation
  if (!inherits(SmoothSpline, "smooth.spline"))
    mystop("This function only works with models that inherit 'smooth.spline' class!")
  ## knots of the smoothing spline
  kx <- with(SmoothSpline$fit, knot * range + min)
  kx <- kx[4:(length(kx) - 3)]
  ky <- predict(SmoothSpline, kx, 0L)[[2]]  ## deriv = 0L
  ## natural cubic spline interpolation over the knots
  CubicInterpSplineAsPiecePoly(kx, ky, method = "natural")
}

predict.PiecePoly <- function (object, newx, deriv = 0L, ...) {
  ## change symbol
  PiecePolyObject <- object
  ## extract piecewise polynomial coefficients
  PiecePolyCoef <- PiecePolyObject$PiecePoly$coef
  shift <- PiecePolyObject$PiecePoly$shift
  ## get degree
  degree <- dim(PiecePolyCoef)[1L] - 1L
  ## deriv validation
  if (deriv > degree) return(numeric(length(newx)))
  ## get power
  power <- 0:(degree - deriv)
  ## extract knots
  x <- PiecePolyObject$knots
  ## which piece?
  piece_id <- findInterval(newx, x, TRUE)
  ind <- split.default(seq_len(length(newx)), piece_id)
  unique_piece_id <- as.integer(names(ind))
  n_pieces <- length(unique_piece_id)
  ## loop through pieces
  y <- numeric(length(newx))
  i <- 1L
  while (i <= n_pieces) {
    ii <- unique_piece_id[i]
    xi <- newx[ind[[i]]] - shift * x[ii]
    pc <- PiecePolyCoef[, ii]
    if (deriv > 0) pc <- pc[-seq_len(deriv)] * choose(deriv:degree, deriv) * factorial(deriv)
    y[ind[[i]]] <- c(outer(xi, power, "^") %*% pc)
    i <- i + 1L
  }
  y
}

## `solve` method for "PiecePoly"
##    solve
##    function (a, b, ...)
##  1. backsolve 'x' value given a 'y' value on the spline
##  2. find extrema of the spline
solve.PiecePoly <- function (a, b = 0, deriv = 0L, ...) {
  ## change symbol
  PiecePolyObject <- a
  y <- b
  ## helpful message (in case someone used `y = y0` than `b = y0` to give RHS which returns misleading results)
  cat(sprintf("solving equation for RHS value %.7g\n", y))
  ## extract piecewise polynomial coefficients
  PiecePolyCoef <- PiecePolyObject$PiecePoly$coef
  shift <- PiecePolyObject$PiecePoly$shift
  n_pieces <- dim(PiecePolyCoef)[2L]
  ## get degree
  degree <- dim(PiecePolyCoef)[1L] - 1L
  ## extract knots
  x <- PiecePolyObject$knots
  ## deriv validation
  if (deriv >= degree) mystop("'deriv' can not exceed 'degree'!")
  ## list of roots on each piece
  xr <- vector("list", n_pieces)
  ## loop through pieces
  i <- 1L
  while (i <= n_pieces) {
    ## polynomial coefficient
    pc <- PiecePolyCoef[, i]
    ## take derivative
    if (deriv > 0) pc <- pc[-seq_len(deriv)] * choose(deriv:degree, deriv) * factorial(deriv)
    pc[1] <- pc[1] - y
    ## complex roots
    croots <- base::polyroot(pc)
    ## real roots (be careful when testing 0 for floating point numbers)
    rroots <- Re(croots)[round(Im(croots), 10) == 0]
    ## is shifting needed?
    if (shift) rroots <- rroots + x[i]
    ## real roots in (x[i], x[i + 1])
    xr[[i]] <- rroots[(rroots >= x[i]) & (rroots <= x[i + 1])]
    ## next piece
    i <- i + 1L
  }
  ## collapse list to atomic vector and return
  unlist(xr)
}

splines_method <- function(y, df=50) {
  # Cálculo das splines cúbicas
  spline_fit <- smooth.spline(y, df=df)
  poly <- SmoothSplineAsPiecePoly(spline_fit)
  # Maximos e minimos das splines
  zeros  <- solve(poly, deriv=1)
  minimos_ <- zeros[predict(poly, zeros, deriv=2)>0]
  minimos <- unique(c(1, minimos_))
  # if (length(minimos_) > 0) {
  #   for (i in 1:(length(minimos_) - 1)) {
  #     if(minimos_[i+1] - minimos_[i] > 100/7) {
  #       minimos <- c(minimos, minimos_[i])
  #     }
  #   }
  # }
  
  return(list(minimos=minimos, obj=spline_fit))
}


Calculate.BF <- function(data.sub, n.wind=14, n.boot=1000){
  # Data Sub
  # data.sub <- data[which(data$Country_code==cid), ]
  # Initialize objects
  df <- list()
  coef.lin <- numeric()
  coef.exp <- numeric()
  rsq.lin <- numeric()
  rsq.exp <- numeric()
  BIC.lin <- numeric()
  BIC.exp <- numeric()
  BF <- numeric()
  bs.results <- list()
  BF.ci.ll <- numeric()
  BF.ci.ul <- numeric()
  # Loop across time points
  for (i in n.wind:nrow(data.sub)){
    df[[i]] <- data.frame(time = c(0:(n.wind-1)),
                          raw.cases = data.sub$y[(i-(n.wind-1)):i],
                          log.raw.cases = log(data.sub$y[(i-(n.wind-
                                                               1)):i]))
    df[[i]][is.na(df[[i]])] <- 0
    # Recode -Inf into missing
    df[[i]]$log.raw.cases[df[[i]]$log.raw.cases==-Inf] <- NA
    # Calculate estimates
    if (sum(df[[i]]$raw.cases==0)<(n.wind*.3)){
      mod.lin <- lm(df[[i]]$raw.cases ~ df[[i]]$time)
      mod.exp <- lm(df[[i]]$log.raw.cases ~ df[[i]]$time)
      coef.lin[i] <- as.numeric(mod.lin$coef[2])
      coef.exp[i] <- as.numeric(exp(mod.exp$coef[2]))
      rsq.lin[i] <- summary(mod.lin)$r.squared
      rsq.exp[i] <- summary(mod.exp)$r.squared
      BIC.lin[i] <- length(na.omit(df[[i]]$raw.cases))*log(1-rsq.lin[i]) +
        log(length(na.omit(df[[i]]$raw.cases)))
      BIC.exp[i] <- length(na.omit(df[[i]]$log.raw.cases))*log(1-
                                                                 rsq.exp[i]) +
        log(length(na.omit(df[[i]]$log.raw.cases)))
      BF[i] <- exp((BIC.lin[i]-BIC.exp[i])/2)
    } else {
      rsq.lin[i] <- NA
      rsq.exp[i] <- NA
      BIC.lin[i] <- NA
      BIC.exp[i] <- NA
      BF[i] <- NA
    }
    # Delete values which indicate decline
    BF[coef.exp<=1] <- NA
    # Bootstrap BF confidence interval
    if (n.boot > 0) {
      bs <- function(data, indices){
        d <- data[indices,]
        if (sum(d$raw.cases==0)<=(n.wind/3)){
          rsq.lin <- summary(lm(d$raw.cases ~ d$time))$r.squared
          rsq.exp <- summary(lm(d$log.raw.cases ~ d$time))$r.squared
          BIC.lin <- length(na.omit(d$raw.cases))*log(1-rsq.lin) +
            log(length(na.omit(d$raw.cases)))
          BIC.exp <- length(na.omit(d$log.raw.cases))*log(1-rsq.exp) +
            log(length(na.omit(d$log.raw.cases)))
          BF <- exp((BIC.lin-BIC.exp)/2)
        } else {
          BF <- NA
        }
        return(BF)
      }
      bs.results[[i]] <- boot(data=df[[i]],
                              statistic=bs,
                              R=n.boot)
      if (!is.na(BF[i])){
        BF.ci.ll[i] <- boot.ci(bs.results[[i]], type="perc")$perc[4]
        BF.ci.ul[i] <- boot.ci(bs.results[[i]], type="perc")$perc[5]
      } else {
        BF.ci.ll[i] <- NA
        BF.ci.ul[i] <- NA
      }
    } else {
      BF.ci.ll[i] <- NA
      BF.ci.ul[i] <- NA
    }
  }
  return(data.frame(cases=data.sub$y,
                    # coef.lin,
                    # coef.exp,
                    rsq.lin,
                    rsq.exp,
                    BIC.lin,
                    BIC.exp,
                    BF,
                    BF.ci.ll,
                    BF.ci.ul)
  )
}

kriston_method <- function(data, n.wind=84) {
  BF.out.3 <- Calculate.BF(data.frame(y=data), n.wind=n.wind, n.boot=0)
  if (length(na.omit(BF.out.3$BF)) == 0) {
    return(list(minimos=c(1), obj=BF.out.3))
  }
  wave_start <- c(n.wind, which(BF.out.3$BF > 3))
  unique_waves <- c(wave_start[1])
  if (length(wave_start) > 1) {
    for (i in 2:length(wave_start)) {
      if(wave_start[i] - wave_start[i-1] > 1/3) {
        unique_waves <- c(unique_waves, wave_start[i])
      }
    }
  }
  unified_waves <- c(unique_waves[1])
  if (length(unique_waves) > 1) {
    for (i in 2:length(unique_waves)) {
      bfs <- BF.out.3$BF[unified_waves[length(unified_waves)]:unique_waves[i]]
      if (sum(is.na(bfs)) > 0 || sum(bfs < 1) > 0) {
        unified_waves <- c(unified_waves, unique_waves[i])
      }
    }
  }
  
  unified_waves <- unified_waves - n.wind + 1
  return(list(minimos=unified_waves, obj=BF.out.3))
}


wave_count <- function(method, y, param, weekly, date) {
  if (weekly) {
    df <- tibble(y=y, date=date, i=1:length(y))
    df <- df %>%
      mutate(week = (as.numeric(date) %/% 7) - (as.numeric(min(date)) %/% 7)) %>%
      arrange(date)
    df_gpd <- df %>%
      group_by(week) %>%
      summarize(y=min(y))
    wave_start <- do.call(method, list(diff(df_gpd$y), param))
    
    semanas <- df_gpd$week[wave_start$minimos]
    minimos <-  (df[df$week %in% semanas,] %>%
                   group_by(week) %>%
                   summarize(dia=min(i)))$dia
    obj <- wave_start$obj
    return(list(minimos=minimos, obj=obj))
  }
  do.call(method, list(diff(y), param))
}

remove_empty_waves <- function(y, minimum) {
  y_min <- c(y[minimum], max(y))
  y_min_lag <- lag(y_min)
  minimum[!((y_min_lag == y_min)[2:length(y_min)])]
}