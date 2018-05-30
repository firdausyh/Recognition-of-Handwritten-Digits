library(ggplot2)
library(grid)
library(readr)
train <- data.frame(read.csv("mnist_train.csv"))
test <- data.frame(read.csv("mnist_test.csv"))
test_label <- (read.csv("test_label.csv")) 
labels   <- train[,1]
features <- train[,-1]



rowsToPlot <- sample(1:nrow(train), 49)

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
       ggtitle("Example Handwritten Digits")

plot(p)

#ggsave("example_digits.png", p, width=10, height=10)

#===============================================================================================================================================================
library(h2o)
localH2O = h2o.init(max_mem_size = '2g', nthreads = -1)
train[,1] = as.factor(train[,1]) # convert digit labels to factor for classification
train_h2o = as.h2o(train)
test_h2o = as.h2o(test)
duration <- proc.time()

## train model
model =
  h2o.deeplearning(x = 2:785,  # column numbers for predictors
                   y = 1,   # column number for label
                   training_frame = train_h2o, # data in H2O format
                   activation = "RectifierWithDropout", # algorithm
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5), # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(800,800), # two layers of 100 nodes
                   momentum_stable = 0.99,
                   nesterov_accelerated_gradient = T, # use it for speed
                   epochs = 15) # no. of epochs
plot(model)
h2o.confusionMatrix(model)
duration - proc.time()

h2o_y_test <- h2o.predict(model, test_h2o)

## convert H2O format into data frame and  save as csv
df_y_test = as.data.frame(h2o_y_test)
df_y_test = data.frame( Label = df_y_test$predict)
write.csv(df_y_test, file = "prediction.csv", row.names=F)

prediction<-read.csv("prediction.csv")
prop.table(table(prediction == test_label))

#Confusion Matrix 
x <- as.factor(prediction$Label)
y <- as.factor(test_label$Label)
table(pred = x, true = y)


h2o.shutdown(prompt = F)
