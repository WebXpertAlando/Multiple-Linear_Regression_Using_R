library(caTools)

auto_mobile<-read.csv("C:/Users/WebXpert/automobileEDA.csv")

head(auto_mobile)

summary(auto_mobile)
 
# Scatter plot for Engine Size vs Price

plot(auto_mobile$engine.size, auto_mobile$price,
     xlab = "Engine Size",
     ylab = "price",
     main = "Scatter plot for Engine Size Price")



#Box plot for Engine Size vs Price

boxplot(auto_mobile$engine.size, auto_mobile$price,
        xlab = "Engine Size",
        ylab = "Price",
        
        main = "Box plot for Engine Size Vs Price")
        
#Box plot for Drive -Wheels vs Price        
boxplot(auto_mobile$number.of.doors, auto_mobile$price,
        xlab = "Number of Doors",
        ylab = "Price",
        
        main = "Box plot for Number  of Door Vs Price")



# Encoding categorical data Body-style
auto_mobile$body.style = factor(auto_mobile$body.style,
                       levels = c('sedan','wagon','hatchback','convertible','hardtop'),
                       labels = c(1, 2, 3, 4, 5))

auto_mobile$body.style

# Encoding categorical data drive-system
auto_mobile$drive.wheels = factor(auto_mobile$drive.wheels,
                                levels = c('rwd','fwd','4wd'),
                                labels = c(1, 2, 3))

auto_mobile$drive.wheels

# Encoding categorical data make
auto_mobile$make = factor(auto_mobile$make,
                                  levels = c('alfa-romero','toyota','audi','bmw','chevrolet','dodge','honda','volvo','isuzu','mazda','mitsubishi','nissan','plymouth','jaguar','mercedes','peugot','saab','porsche','renault','subaru'),
                                  labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
auto_mobile$make


# Encoding categorical data engine location
auto_mobile$engine.location = factor(auto_mobile$engine.location,
                                  levels = c('rear','front'),
                                  labels = c(1, 2))

auto_mobile$engine.location






# Encoding categorical data number of doors
auto_mobile$num.of.doors = factor(auto_mobile$num.of.doors,
                                  levels = c('two','four'),
                                  labels = c(1, 2))

auto_mobile$num.of.doors


# Encoding categorical data Horse Power
auto_mobile$horsepower.binned = factor(auto_mobile$horsepower.binned,
                                  levels = c('Medium','High','Low'),
                                  labels = c(1, 2, 3))

auto_mobile$horsepower.binned

# Encoding categorical data num of cylinders
auto_mobile$num.of.cylinders = factor(auto_mobile$num.of.cylinders,
                                  levels = c('two','three','four','five','six','eight','twelve'),
                                  labels = c(2, 3, 4, 5, 6, 8, 12))
auto_mobile$num.of.cylinders


# Encoding categorical data engine type
auto_mobile$engine.type = factor(auto_mobile$engine.type,
                                  levels = c('dohc','ohc','ohcv','ohcf','rotor'),
                                  labels = c(1, 2, 3, 4, 5))
auto_mobile$engine.type



# Encoding categorical data aspiration
auto_mobile$aspiration = factor(auto_mobile$aspiration,
                                  levels = c('std','turbo'),
                                  labels = c(1, 2))

auto_mobile$aspiration






# Split the data into train and Test Sets

set.seed(123)
split = sample.split(auto_mobile$gas, SplitRatio=0.80)

train_set<- subset(auto_mobile, split == "TRUE")

test_set<- subset(auto_mobile, split=="FALSE")

train_set

test_set


# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = price ~ .,
               data = train_set)


summary(regressor)

# Predicting the Test set results
y_pred<-predict(regressor, data = test_set)

y_pred



# Actual values
actual <- test_set$price

# Calculate metrics
mse <- mean((y_pred - actual)^2)
rmse <- sqrt(mse)
mae <- mean(abs(y_pred - actual))
r_squared <- cor(actual, y_pred)^2

# Print metrics
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", r_squared, "\n")



library(ggplot2)


# Generate the plot
ggplot() + geom_point(aes(x = train_set$engine.size, 
                          y = train_set$price), colour = 'red') +
  geom_line(aes(x = train_set$engine.size,
                y = predict(regressor, data = train_set)), colour = 'blue') +
  
  ggtitle('Selling Price vs Year (Training set)') +
  xlab('Engine Size') + ylab("Price")
