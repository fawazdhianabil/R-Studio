#import data
res <- read.csv(file = 'C:/Users/ASUS/Downloads/Nabil/Data Klaster.csv')


#membagi data menjadi training dan testing
#install.packages('caTools')
library(caTools)

res$klaster <- factor(res$klaster)
res$klaster
set.seed(123)
split = sample.split(res$klaster, SplitRatio = 0.8)

training_set = subset(res, split == TRUE)
test_set = subset(res, split == FALSE)

#Support Vector Machine
#install.packages('e1071')
library(e1071)

classifier = svm(formula = factor(klaster) ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial') #kernel rbf(radial basis function)

#membuat prediksi
y_pred = predict(classifier, newdata = test_set)
y_pred


#membuat confusion matrix
cm = table(test_set$klaster, y_pred)
cm

#mencari metrics accuracy, precision, recall dan f1
n = sum(cm) #jumlah prediksi
nc = nrow(cm) #jumlah prediksi baris
diag = diag(cm)  #jumlah diagonal
rowsums = apply(cm, 1, sum) #jumlah baris
colsums = apply(cm, 2, sum) #jumlah kolom
accuracy = sum(diag) / n #akurasi
precision = diag / colsums #presisi
recall = diag / rowsums #recall
f1 = 2 * precision * recall / (precision + recall) #f1 
metrics_svm <- data.frame(accuracy,precision, recall, f1) 
metrics_svm #metrics

#membuat average metrics
macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)
macro_metrics_svm <-  data.frame(accuracy,macroPrecision, macroRecall, macroF1)
macro_metrics_svm #average metrics svm

plot(classifier, test_set, UNEM_RATE~MORTGAGE)

#Decision Tree
#install.packages(rpart)
#install.packages(rpart.plot)
library(rpart)
library(rpart.plot)

#melatih decision tree
dct <- rpart(factor(klaster)~., 
             data = training_set, 
             method = 'class')

#membuat plot decision tree
rpart.plot(dct)

#prediksi dengan decision tree
y_pred_dct = predict(dct, test_set, type = 'class')
y_pred_dct

#membuat confusion matrix decision tree
cm_dct = table(test_set$klaster, y_pred_dct)
cm_dct

#membuat metrics decision tree
n_dct= sum(cm_dct) 
nc_dct = nrow(cm_dct) 
diag_dct = diag(cm_dct)  
rowsums_dct= apply(cm_dct, 1, sum) 
colsums_dct = apply(cm_dct, 2, sum) 
accuracy_dct = sum(diag_dct) / n_dct 
precision_dct = diag_dct / colsums_dct 
recall_dct = diag_dct / rowsums_dct 
f1_dct = 2 * precision_dct * recall_dct / (precision_dct + recall_dct) 
metrics_dct <- data.frame(accuracy_dct,precision_dct, recall_dct, f1_dct) 
metrics_dct

#membuat average decision tree
macroPrecision_dct = mean(precision_dct)
macroRecall_dct = mean(recall_dct)
macroF1_dct = mean(f1_dct)
macro_metrics_dct <-  data.frame(accuracy_dct,macroPrecision_dct, 
                                 macroRecall_dct, macroF1_dct)
macro_metrics_dct

#GridSearch SVM
library(caret)
svm_grid <- train(form = factor(klaster) ~ .,
                  data = training_set,
                  method = 'svmRadial') #kernel RBF
svm_grid$bestTune

y_pred_grid = predict(svm_grid, newdata = test_set)
y_pred_grid


#membuat confusion matrix
cm = table(test_set$klaster, y_pred_grid)
cm

#mencari metrics accuracy, precision, recall dan f1
n = sum(cm) #jumlah prediksi
nc = nrow(cm) #jumlah prediksi baris
diag = diag(cm)  #jumlah diagonal
rowsums = apply(cm, 1, sum) #jumlah baris
colsums = apply(cm, 2, sum) #jumlah kolom
accuracy = sum(diag) / n #akurasi
precision = diag / colsums #presisi
recall = diag / rowsums #recall
f1 = 2 * precision * recall / (precision + recall) #f1 
metrics_svm <- data.frame(accuracy,precision, recall, f1) 
metrics_svm #metrics

#membuat average metrics
macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)
macro_metrics_svm <-  data.frame(accuracy,macroPrecision, macroRecall, macroF1)
macro_metrics_svm #average metrics svm

#GridSearch tree
library(caret)
tree_grid <- train(form = factor(klaster) ~ .,
                   data = training_set,
                   method = 'ctree') #decision tree
tree_grid$bestTune

y_pred_tree_grid = predict(tree_grid, newdata = test_set)
y_pred_tree_grid


#membuat confusion matrix
cm = table(test_set$klaster, y_pred_tree_grid)
cm

#mencari metrics accuracy, precision, recall dan f1
n = sum(cm) #jumlah prediksi
nc = nrow(cm) #jumlah prediksi baris
diag = diag(cm)  #jumlah diagonal
rowsums = apply(cm, 1, sum) #jumlah baris
colsums = apply(cm, 2, sum) #jumlah kolom
accuracy = sum(diag) / n #akurasi
precision = diag / colsums #presisi
recall = diag / rowsums #recall
f1 = 2 * precision * recall / (precision + recall) #f1 
metrics_tree <- data.frame(accuracy,precision, recall, f1) 
metrics_tree #metrics

#membuat average metrics
macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)
macro_metrics_tree <-  data.frame(accuracy,macroPrecision, macroRecall, macroF1)
macro_metrics_tree #average metrics tree

#K-Fold SVM
library(caret)
set.seed(123)

train_control <- trainControl(method = "cv",
                              number = 10)

model <- train(factor(klaster)~., data = training_set,
               trControl = train_control,
               method = "svmRadial")
print(model)


#K-Fold DCT
library(caret)
set.seed(123)

train_control <- trainControl(method = "cv",
                              number = 10)

model_tree <- train(factor(klaster)~., data = training_set,
                    trControl = train_control,
                    method = "ctree")
print(model_tree)

