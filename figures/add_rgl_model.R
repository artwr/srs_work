add_rgl_model <- function(fdata){
  
  ## takes a model in long form, in the format
  ## 1st column x
  ## 2nd is y,
  ## 3rd is z (height)
  ## and draws an rgl model
  
  ##
  # note that x has to be ascending, followed by y
  print(head(fdata))
  
  fdata <- fdata[order(fdata[, 1], fdata[, 2]), ]
  
  print(head(fdata))
  ##
  require(reshape2)
  require(rgl)
  orig_names <- colnames(fdata)
  
  #print(head(fdata))
  colnames(fdata) <- c("x", "y", "z")
  fdata <- as.data.frame(fdata)
  
  ## work out the min and max of x,y,z
  xlimits <- c(min(fdata$x, na.rm = T), max(fdata$x, na.rm = T))
  ylimits <- c(min(fdata$y, na.rm = T), max(fdata$y, na.rm = T))
  zlimits <- c(min(fdata$z, na.rm = T), max(fdata$z, na.rm = T))
  l <- list (x = xlimits, y = ylimits, z = zlimits)
  xyz <- do.call(expand.grid, l)
  #print(xyz)
  x_boundaries <- xyz$x
  #print(class(xyz$x))
  y_boundaries <- xyz$y
  #print(class(xyz$y))
  z_boundaries <- xyz$z
  #print(class(xyz$z))
  
  # now turn fdata into a wide format for use with the rgl.surface
  fdata[, 2] <- as.character(fdata[, 2])
  fdata[, 3] <- as.character(fdata[, 3])
  #print(class(fdata[, 2]))
  wide_form <- dcast(fdata, y ~ x, value_var = "z")
  print(head(wide_form))
  wide_form_values <- as.matrix(wide_form[, 2:ncol(wide_form)])  
  x_values <- as.numeric(colnames(wide_form[2:ncol(wide_form)]))
  y_values <- as.numeric(wide_form[, 1])
  print(x_values)
  print(y_values)
  wide_form_values <- wide_form_values[order(y_values), order(x_values)]
  x_values <- x_values[order(x_values)]
  y_values <- y_values[order(y_values)]
  print(x_values)
  print(y_values)
  
  print(dim(wide_form_values))
  print(length(x_values))
  print(length(y_values))
  
  rgl.surface(z = x_values,  ## these are all different because
              x = y_values,  ## of the confusing way that 
              y = wide_form_values,  ## rgl.surface works!
              coords = c(2,3,1),
              alpha = .8)
  # plot points in red just to be on the safe side!
  points3d(fdata, col = "red")
}