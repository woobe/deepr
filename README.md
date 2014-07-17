deepr
=====

An R package to streamline the training, fine-tuning and predicting processes for deep learning. It aims to further simplify the functions in packages such as 'deepnet' and 'darch'.
<br>

### Installation
```
library(devtools)
install_github("woobe/deepr")
```
<br>

### Example 1

Extracting hidden features from original predictors (x) and creating new predictors (x_new).
<br>

#### 1.1 Load Library
```
library(deepr)
```

#### 1.2 Generate random numbers as original predictors (x)
```
x <- matrix(rnorm(1000000), nrow = 5000)
dim(x)
[1] 5000  200
```

#### 1.3 Train a RBM model with 100 hidden features
```
model_rbm <- train_rbm(x, n_features = 100)

=====================================================================
[deepr]: Training a Restricted Boltzmann Machine
=====================================================================
[deepr]: Removing variables with near zero variance ...
[deepr]: Normalising x to values between 0 and 1 ...
[deepr]: Training a RBM Layer of 10 Hidden Features ...
[deepr]: Returning the RBM Model ...
[deepr]: All Done! Total Duration: 474 sec.
=====================================================================
```

#### 1.4 Transform original x into new x
```
x_new <- transform_x(model_rbm, x)

=====================================================================
[deepr]: Transforming Predictors Using a Trained RBM
=====================================================================
[deepr]: Pre-processing predictors (x) based on the RBM object ...
[deepr]: Converting original predictors (x) into new ones (x_new) ...
[deepr]: Returning new predictors (x_new) ...
[deepr]: All Done! Total Duration: 1 sec.
=====================================================================

dim(x_new)
[1] 5000   100
```
