#Algorithms & functions NB : We assumed volat = cst


PriceCall.Volat = function (S0, K, date0, maturity, r, volat_vector, type = "Call")
{
  format = "%d/%m/%Y"
  spread_date = as.numeric(as.Date(maturity, format) - as.Date(date0, format))/365
  price_call = rep(NA, length(volat_vector))
  for (i in 1:length(volat_vector))
  {
    price_call[i] = BS.OptionPricerEU(S0, K, spread_date, 0, volat_vector[i], "Call")
  }
  return (price_call)
}

pf.delta_hedging = function (delta_serie, price_option, stock_price)
{
  portfolio = rep(NA, length(delta_serie))
  #V0 = C0
  j = 1
  if (is.na(price_option[1]))
  {
    while (is.na(price_option[j]))
    {
      j = j + 1
    }
    portfolio[j] = price_option[j]
  }
  else 
  {
    portfolio[1] = price_option[1]
  }
  for (i in (j+1):length(portfolio))
  {
    portfolio[i] = portfolio[i - 1] + delta_serie[i - 1] * (stock_price[i] - stock_price[i - 1])
  }
  return (portfolio)
}

Vector.DeltaOption = function (vector_d1)
{
  vector_delta <- rep(NA,length(vector_d1))
  for (i in 1:length(vector_d1))
  {
    #delta[i] = N(d1[i])
    vector_delta[i] = pnorm(vector_d1[i])
  }
  return (vector_delta)
}

Vector.d1 = function (S, K, r, volat, T_vector)
{
  vector_d1 <- rep(NA, length(T_vector))
  for (i in 1:length(T_vector))
  {
    vector_d1[i] = (log(S[i]/K) + (r + 0.5*volat^2)*T_vector[i]) / (volat*sqrt(T_vector[i]))
  }
  return (vector_d1)
}

Vector.d1.VolatVary = function (S, K, r, volat, T_vector)
{
  vector_d1 <- rep(NA, length(volat))
  for (i in 1:length(volat))
  {
    if (!is.na(volat[i]))
    {
      vector_d1[i] = (log(S[i]/K) + (r + 0.5*volat[i]^2)*T_vector[i]) / (volat[i]*sqrt(T_vector[i]))
    }
  }
  return (vector_d1)
}

Vector.PriceOption = function (stock_price, K, T_vector, r, volat, type = "Call")
{
  price_option = rep(NA, length(T_vector))
  for (i in 1:length(T_vector))
  {
    price_option[i] = BS.OptionPricerEU(stock_price[i], K, T_vector[i], 0, volat, "Call")
  }
  return (price_option)
}

maturity.vector = function (date_serie, T)
{
  format = "%d/%m/%Y"
  date = rep(NA, length(date_serie))
  for (i in 1:length(date_serie))
  {
    date[i] = as.numeric(as.Date(T, format) - as.Date(date_serie[i], format))/365
  }
  return (date)
}

BS.OptionPricerEU <-function(S, K, T, r,volat, type="Call")
{
  d1 <- (log(S/K) + (r + 0.5*volat^2)*T) / (volat*sqrt(T))
  d2 <- d1 - volat*sqrt(T)
  if(type=="Call")
  {
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  }
  if(type=="Put")
  {
    value <- K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
  }
  return(value)
}

