data = pfmcdatalist[[1]]$pfmcchange.patient3




lo.p1 = loess(p1 ~ rectime, data = data, span = 0.1)
plot(data$rectime, data$p1, type = "l")
lines(predict(lo.p1), 
      x = data$rectime, col = "red", lwd = 2)

tidydata = tidy(data)
smoothingSpline = smooth.spline(tidydata$rectime, tidydata$pressure, spar = 0.35)
plot(tidydata$rectime, tidydata$pressure, pch = 21, cex = 0.5)
lines(smoothingSpline, col = "red", lwd = 2)

tidydata %>%
  ggplot(aes(x = rectime, y = pressure, color = sensor)) +
  geom_point() +
  geom_line(smoothingSpline)



lo.whole = loess(pressure ~ rectime, data = tidydata, span = 0.1)
plot(tidydata$rectime, tidydata$pressure)

lines(predict(lo.whole), 
      x = tidydata$rectime, col = "red", lwd = 2)
