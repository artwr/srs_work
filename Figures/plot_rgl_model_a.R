plot_rgl_model_a <- function(fdata, plot_contour = T, plot_points = T, 
                             verbose = F, colour = "rainbow", smoother = F){
  ## takes a model in long form, in the format
  ## 1st column x
  ## 2nd is y,
  ## 3rd is z (height)
  ## and draws an rgl model
  
  ## includes a contour plot below and plots the points in blue
  ## if these are set to TRUE
  
  # note that x has to be ascending, followed by y
  if (verbose) print(head(fdata))
  
  fdata <- fdata[order(fdata[, 1], fdata[, 2]), ]
  if (verbose) print(head(fdata))
  ##
  require(reshape2)
  require(rgl)
  orig_names <- colnames(fdata)
  colnames(fdata) <- c("x", "y", "z")
  fdata <- as.data.frame(fdata)
  
  ## work out the min and max of x,y,z
  xlimits <- c(min(fdata$x, na.rm = T), max(fdata$x, na.rm = T))
  ylimits <- c(min(fdata$y, na.rm = T), max(fdata$y, na.rm = T))
  zlimits <- c(min(fdata$z, na.rm = T), max(fdata$z, na.rm = T))
  l <- list (x = xlimits, y = ylimits, z = zlimits)
  xyz <- do.call(expand.grid, l)
  if (verbose) print(xyz)
  x_boundaries <- xyz$x
  if (verbose) print(class(xyz$x))
  y_boundaries <- xyz$y
  if (verbose) print(class(xyz$y))
  z_boundaries <- xyz$z
  if (verbose) print(class(xyz$z))
  if (verbose) print(paste(x_boundaries, y_boundaries, z_boundaries, sep = ";"))
  
  # now turn fdata into a wide format for use with the rgl.surface
  fdata[, 2] <- as.character(fdata[, 2])
  fdata[, 3] <- as.character(fdata[, 3])
  #if (verbose) print(class(fdata[, 2]))
  wide_form <- dcast(fdata, y ~ x, value.var = "z")
  if (verbose) print(head(wide_form))
  wide_form_values <- as.matrix(wide_form[, 2:ncol(wide_form)])  
  if (verbose) print(wide_form_values)
  dimwideformvalues<-dim(wide_form_values)
  if (verbose) print(dimwideformvalues)
  x_values <- as.numeric(colnames(wide_form[2:ncol(wide_form)]))
  y_values <- as.numeric(wide_form[, 1])
  if (verbose) print(x_values)
  if (verbose) print(y_values)
  wide_form_values <- wide_form_values[order(y_values), order(x_values)]
  wide_form_values <- as.numeric(wide_form_values)
  x_values <- x_values[order(x_values)]
  y_values <- y_values[order(y_values)]
  if (verbose) print(x_values)
  if (verbose) print(y_values)
  
  if (verbose) print(dimwideformvalues)
  if (verbose) print(length(x_values))
  if (verbose) print(length(y_values))
  
  zlim <- range(wide_form_values, na.rm =T)
  if (verbose) print(zlim)
  zlen <- zlim[2] - zlim[1] + 1
  if (verbose) print(zlen)
  
  if (colour == "rainbow"){
    colourut <- rainbow(zlen, alpha = 0)
    if (verbose) print(colourut)
    col <- colourut[ wide_form_values - zlim[1] + 1]
    # if (verbose) print(col)
  } else {
    col <- "grey"
    if (verbose) print(table(col2))
  }
  
  
  open3d()
  plot3d(x_boundaries, y_boundaries, z_boundaries, 
         box = T, col = "black",  xlab = orig_names[1], 
         ylab = orig_names[2], zlab = orig_names[3])
  
  rgl.surface(z = x_values,  ## these are all different because
              x = y_values,  ## of the confusing way that 
              y = wide_form_values,  ## rgl.surface works! - y is the height!
              coords = c(2,3,1),
              color = col,
              alpha = 1.0,
              lit = F,
              smooth = smoother)
  
  if (plot_points){
    # plot points in red just to be on the safe side!
    points3d(fdata, col = "blue")
  }
  
  if (plot_contour){
    # plot the plane underneath
    flat_matrix <- wide_form_values
    if (verbose) print(flat_matrix)
    y_intercept <- (zlim[2] - zlim[1]) * (-2/3) # put the flat matrix 1/2 the distance below the lower height 
    flat_matrix[which(flat_matrix != y_intercept)] <- y_intercept
    if (verbose) print(flat_matrix)
    
    rgl.surface(z = x_values,  ## these are all different because
                x = y_values,  ## of the confusing way that 
                y = flat_matrix,  ## rgl.surface works! - y is the height!
                coords = c(2,3,1),
                color = col,
                alpha = 1.0,
                smooth = smoother)
  }
}