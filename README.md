# Suicide Risk After Receiving Treatments of Mental Illness

Suicide, as a serious public health issue, that highly related with mental illness has attract people’s attention for more than a half century. With plentiful studies on suicide, scholars determine that even after receiving treatment of mental health, the risk of suicide still exists. Therefore, the research aims to first explore the influencing factors of suicide risk and find out if there is any relationship between two influencing factors, and then build a model to effectively classify the suicide risk which can be used for improving the suicide prevention project.


### Code File
Suicide_Risk Report.Rmd: an R markdown file with cleaned code and comments <br />
Suicide_Risk.R: unorganized code attempt for the whole research


### R Packages
R version 4.2.2

dplyr (version 1.0.10): data cleaning <br />
caret (version 6.0-93): used for training Knn, decision tree, random forest models <br />
jtools (version 2.2.0): used for plotting interactive effects among categorical variables; calculating F1-score <br />
interactions (version 1.1.0): used for plotting interactive effects among continuous variables <br />
keras (version 2.9.0): building and training neural network model <br />
tensorflow (version 2.9.0): training neural network <br />
rpart.plot (version 3.1.1): visualizing decision tree <br />



### Data Files
The three NSDUH datasets, from 2018 to 2020, used in the research are provided in this link. https://drive.google.com/file/d/1sRAQ4q9rTf-fSdWHYsF79wanOg3dZZW6/view?usp=share_link

The NSDUH datasets are also available in its website https://www.samhsa.gov/data/data-we-collect/nsduh-national-survey-drug-use-and-health![image](https://user-images.githubusercontent.com/97774448/202839317-16f53179-78e9-41f4-9b5e-20d7e36bdbc6.png)
