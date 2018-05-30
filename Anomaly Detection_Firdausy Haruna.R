library(ggplot2)
library(grid)
library(readr)
library(h2o)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, 
                     max_mem_size = '2g')
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, 
                    max_mem_size = '2g',    
                    nthreads = 3)
train = read.csv("mnist_train.csv")
test = read.csv("mnist_test.csv")
test_label <- (read.csv("test_label.csv")) 
train[,1] = as.factor(train[,1]) # convert digit labels to factor for classification
train_hex = as.h2o(train)
test_hex = as.h2o(test)

predictors = c(2:785)
resp = 1

ae_model <- h2o.deeplearning(x = predictors,
            #y = 42, #response (ignored - pick any non-constant column)
            training_frame = train_hex, #CAVEAT data is deprecated
            activation = "Tanh",
            autoencoder = T,
            hidden = c(50),
            ignore_const_cols = F,
            epochs = 5)

test_rec_error <- as.data.frame(h2o.anomaly(ae_model, test_hex))
test_rec_error
summary(test_rec_error)

test_features_deep <- h2o.deepfeatures(ae_model, test_hex, layer = 1)
summary(test_features_deep)

#=========================VISUALIZING OUTLIERS===================================================
te <- cbind(test_label,test,test_rec_error)
new_te <- te[order(te$Reconstruction.MSE), c(1:785)]
write.csv(new_te,file = "new_te.csv", row.names = T)
new_te <- data.frame(read.csv("new_te.csv"))
new_te <- new_te[-1]
labels   <- new_te[,1]
features <- new_te[,-1]


#==============================GOOD HANDWRITTING=================================================
rowsToPlot <- c(1:28)

rowToMatrix <- function(row) {
  intensity <- as.numeric(row)/max(as.numeric(row))
  return(t(matrix((rgb(intensity, intensity, intensity)), 28, 28)))
}

geom_digit <- function(digits) 
{
  layer(geom = GeomRasterDigit, stat = "identity", position = "identity", data = NULL, 
        params = list(digits=digits))  
}

GeomRasterDigit <- ggproto("GeomRasterDigit", 
                           ggplot2::GeomRaster, 
                           draw_panel = function(data, panel_scales, coordinates, digits = digits) {
                             if (!inherits(coordinates, "CoordCartesian")) {
                               stop("geom_digit only works with Cartesian coordinates",
                                    call. = FALSE)
                             }
                             corners <- data.frame(x = c(-Inf, Inf), y = c(-Inf, Inf))
                             bounds <- coordinates$transform(corners, panel_scales)
                             x_rng <- range(bounds$x, na.rm = TRUE)
                             y_rng <- range(bounds$y, na.rm = TRUE)
                             rasterGrob(as.raster(rowToMatrix(digits[data$rows,])), 
                                        x = mean(x_rng), y = mean(y_rng), 
                                        default.units = "native", just = c("center","center"), 
                                        interpolate = FALSE)
                           }) 

p <- ggplot(data.frame(rows=rowsToPlot, labels=labels[rowsToPlot]), 
            aes(x=0.1, y=.9, rows=rows, label=labels)) + 
       geom_blank() + xlim(0,1) + ylim(0,1) + xlab("") + ylab("") + 
       facet_wrap(~ rows, ncol=7) +
       geom_digit(features) +
       geom_text(colour="#53cfff") +
       theme(panel.background = element_rect(fill = 'black'),
             panel.border = element_rect(fill = NA, colour = "#cfff53"),
             panel.grid = element_blank(),
             strip.background = element_blank(),
             strip.text.x = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks = element_blank(),
             axis.line = element_blank()) +
       ggtitle("Good Examples")

plot(p)
# =====================TERRIBLE HANDWRITTINGS ====================================================
rowsToPlot <- c(9973:10000)

rowToMatrix <- function(row) {
  intensity <- as.numeric(row)/max(as.numeric(row))
  return(t(matrix((rgb(intensity, intensity, intensity)), 28, 28)))
}

geom_digit <- function(digits) 
{
  layer(geom = GeomRasterDigit, stat = "identity", position = "identity", data = NULL, 
        params = list(digits=digits))  
}

GeomRasterDigit <- ggproto("GeomRasterDigit", 
                           ggplot2::GeomRaster, 
                           draw_panel = function(data, panel_scales, coordinates, digits = digits) {
                             if (!inherits(coordinates, "CoordCartesian")) {
                               stop("geom_digit only works with Cartesian coordinates",
                                    call. = FALSE)
                             }
                             corners <- data.frame(x = c(-Inf, Inf), y = c(-Inf, Inf))
                             bounds <- coordinates$transform(corners, panel_scales)
                             x_rng <- range(bounds$x, na.rm = TRUE)
                             y_rng <- range(bounds$y, na.rm = TRUE)
                             rasterGrob(as.raster(rowToMatrix(digits[data$rows,])), 
                                        x = mean(x_rng), y = mean(y_rng), 
                                        default.units = "native", just = c("center","center"), 
                                        interpolate = FALSE)
                           }) 

p <- ggplot(data.frame(rows=rowsToPlot, labels=labels[rowsToPlot]), 
            aes(x=0.1, y=.9, rows=rows, label=labels)) + 
       geom_blank() + xlim(0,1) + ylim(0,1) + xlab("") + ylab("") + 
       facet_wrap(~ rows, ncol=7) +
       geom_digit(features) +
       geom_text(colour="#53cfff") +
       theme(panel.background = element_rect(fill = 'black'),
             panel.border = element_rect(fill = NA, colour = "#cfff53"),
             panel.grid = element_blank(),
             strip.background = element_blank(),
             strip.text.x = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks = element_blank(),
             axis.line = element_blank()) +
       ggtitle("Terrible Examples")

plot(p)
