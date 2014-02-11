#loess call

loess.control(surface = c("interpolate", "direct"),
              statistics = c("approximate", "exact"),
              trace.hat = c("exact", "approximate"),
              cell = 0.2, iterations = 4, ...)

# Direct method allows extrapolation
lcontrol<-loess.control(surface = c("direct"),
              statistics = c("exact"),
              trace.hat = c("exact"),
              cell = 0.2, iterations = 5)

# Uses interpolation on a kd-tree, will give NA outside of the bounding box
lcontrol<-loess.control(surface = c("interpolate"),
                        statistics = c("exact"),
                        trace.hat = c("exact"),
                        cell = 0.2, iterations = 5)

loess(formula, data, weights, subset, na.action, model = FALSE,
      span = 0.25, degree = 2,
      normalize = FALSE,
      family = c("gaussian", "symmetric"),
      method = c("loess"),
      control = lcontrol)