library(tidyverse)

powerDf <-
  expand.grid(
    sampSizePerGroup = ( 12 * 2^(0:7) ),
    effectSize = c(.2, .5, .8),
    alpha = c(0.005, 0.05)
  ) %>%
  tidyr::expand(effectSize, sampSizePerGroup, alpha) %>%
  group_by(effectSize, sampSizePerGroup, alpha)

runPowerSim <- function(df, nsims = 1000) {
  p <- array(NA, dim = nsims)
  for (s in 1:nsims) {
    data <- data.frame(
      y = rnorm(df$sampSizePerGroup * 2),
      group = array(0, dim = df$sampSizePerGroup * 2)
    )
    
    data$group[1:df$sampSizePerGroup] <- 1
    data$y[data$group == 1] <- data$y[data$group == 1] + df$effectSize
    tt <- t.test(y ~ group, data = data)
    p[s] <- tt$p.value
  }
  return(data.frame(power = mean(p < df$alpha)))
}

# run the simulation
powerSimResults <- powerDf %>%
  do(runPowerSim(.))

powerplot <- ggplot(powerSimResults,
       aes(sampSizePerGroup,power,color=as.factor(effectSize),linetype=as.factor(alpha))) +
  geom_line(size=1) +
  annotate('segment',x=0,xend=max(powerDf$sampSizePerGroup),
           y=0.8,yend=0.8,linetype='dotted',size=.5) +
  scale_x_continuous( breaks=unique(powerDf$sampSizePerGroup)) +
  labs(
    color = "Effect size",
    x = "Sample size",
    y = "Power",
    linetype = "alpha"
  )

ggplot2::ggsave(filename = "power-plot.png", plot = powerplot, path = "~/BrownLabNotebook/analyzing-lit/images", dpi = 600, width = 6000, height = 3000, units = "px")
