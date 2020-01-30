# Find all data folders
data_folders = list.files(pattern = "_csv")

# Folder that contains patient3 data s0007
s0007.folder = data_folders[1]

# Load in all the helper functions
source("Functions.R")

# Process folder and get the change dataset for patient 3
data = process(s0007.folder)$pfmcchange.patient3

# Try smooth sensor 1
# Fit a Local Polynomial Regression with 10% span 
# lo.p1 = loess(p1 ~ rectime, data = data, span = 0.1)

# Plot sensor 1 vs time and the smoothed line
# plot(data$rectime, data$p1, type = "l")
# lines(predict(lo.p1), x = data$rectime, col = "red", lwd = 2)

# # Trial 1.Smooth all data using smooth.spline
tidydata = tidy(data)
# 
# smoothingSpline = smooth.spline(tidydata$rectime, tidydata$pressure, spar = 0.5)
# 
# plot(tidydata$rectime, tidydata$pressure, pch = 21, cex = 0.5, col = "grey")
# lines(smoothingSpline, col = "green", lwd = 2)
# 

# Trial 2. Smooth all data using Local Polynomial Regression
# After trying out all span values, 0.1 works the best
lo.whole = loess(pressure ~ rectime, data = tidydata, span = 0.1)
# plot(tidydata$rectime, tidydata$pressure, pch = 21, cex = 0.5)

# lines(tidydata$rectime[order(tidydata$rectime)], 
#       predict(lo.whole)[order(tidydata$rectime)], 
#       col = "red", lwd = 2)

smoothed.df = unique(data.frame(rectime = tidydata$rectime[order(tidydata$rectime)], 
                            pressure = predict(lo.whole)[order(tidydata$rectime)]))

# Can I use ggplot  ???????
g.smoothed = ggplot(data = tidydata) +
  geom_line(aes(x = rectime, y = pressure, color = sensor)) +
  geom_line(data = smoothed.df, aes(x = rectime, y = pressure), size = 1) +
  labs(x = "Recording Time (in s)", y = "Pressure Change (in mm Hg)",
       title = "Pressure trace from all sensors with smoothed line") +
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave("Colored Pressure traces with Smoothed Line.png", width = 10, height = 4)
