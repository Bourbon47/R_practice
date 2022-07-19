# scatterplots for geyser data

library(MASS)
data(geyser)
attach(geyser)
plot(duration~waiting)
x11()
symbols(duration[2:299]~waiting[2:299], circles=duration[1:298], inches=0.2)
x11()
plot(waiting[2:299] ~ duration[1:298])
x11()
plot(duration[2:299] ~ duration[1:298])

# end