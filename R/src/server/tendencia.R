
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


grafico_series <- function(datas, series, titulo_grafico, eixo_x, eixo_y) {
  tendencias <- estima_tendencia(series)
  p <- as.data.frame(cbind(serie=series)) %>%
    slice(-1) %>%
    mutate(serie_mutada = diff(series)) %>%
    ggplot(aes(x = datas[2:length(datas)], y = serie_mutada)) +
    geom_line(color = "blue") +
    labs(title = titulo_grafico, x = eixo_x, y = eixo_y) +
    theme_minimal() +
    scale_x_date(date_breaks = "4 months", date_labels = "%b-%Y") +
    geom_line(aes(x=datas[2:length(datas)], y=diff(tendencias)), color='red')
  
  fig <- ggplotly(p)
  return(fig)
  
}



estima_tendencia <- function(serie) {
  print(serie)
  serie[is.na(serie)] <- 0
  # cálculo da série diária
  y <- c(1, diff(serie))
  
  # Cálculo das splines cúbicas
  spline_fit <- smooth.spline(y, df=40)
  poly <- SmoothSplineAsPiecePoly(spline_fit)
  # Maximos e minimos das splines
  zeros  <- solve(poly, deriv=1)
  minimos_ <- zeros[predict(poly, zeros, deriv=2)>0]
  minimos <- c()
  for (i in 1:(length(minimos_) - 1)) {
    if(minimos_[i+1] - minimos_[i] > 90) {
      minimos <- c(minimos, minimos_[i])
    }
  }
  
  N <- length(minimos)
  factors <- sapply(1:N, FUN=function(i){paste('I(d',i,'/(1+exp(b',i,'*(log(x)-e',i,'))))', sep='')})
  model_formula <- reformulate(termlabels = factors, response = 'y')
  t <- 1:length(serie)
  ip <- c()
  print(minimos)
  for (i in 1:N) {
    if(i == N) {
      yi <- serie[as.integer(minimos[i]):length(serie)] - serie[as.integer(minimos[i])-1]
      ti <- as.integer(minimos[i]):length(serie)
    } else {
      yi <- serie[as.integer(minimos[i]):as.integer(minimos[i+1]+1)]- serie[as.integer(minimos[i])-1]
      ti <- as.integer(minimos[i]):as.integer(minimos[i+1]+1)
    }
    modeloi <- drm(yi~ti, fct=LL2.3())
    ipi <- t(as.data.frame(modeloi$coefficients))
    colnames(ipi) <- paste(substr(colnames(ipi),1,1), i, sep='')
    ip <- cbind(ip, ipi)
  }
  dados <- data.frame(y=serie, x=t)
  fit <- nls(model_formula, dados, start=ip[1,])
  
  return(predict(fit))
}

