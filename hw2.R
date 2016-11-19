# Lalith Devireddy
# Stat 534 Homework 2

# Problem 1
# uses glm to fit logistic regression and returns the AIC = deviance + 2*NumberOfCoefficients
getLogisticAIC <- function(response,explanatory,data)
{
  #check if the regression has no explanatory variable
  if(length(explanatory)==0)
  {
    # regression with no explanatory variables
    deviance = glm(data[,response] ~ 1,family=binomial(link=logit))$deviance;
  }
  else
  {
    # regression with at least one explanatory variable
    deviance = glm(data[,response] ~ as.matrix(data[,as.numeric(explanatory)]),family=binomial(link=logit))$deviance;
  }
  return(deviance+2*(1+length(explanatory)));
}

# Problem 2
# Finds a good logistic model using the Akaike criterion and iteratively adds the best variable
# (to minimize AIC) using a greedy approach
forwardSearchAIC<- function(response,data,lastPredictor)
{
  bestRegression = NULL;
  bestAIC = getLogisticAIC(response,bestRegression,data);
  VariablesNotInModel = 1:lastPredictor;
  changed = T;
  while(length(VariablesNotInModel) > 0 && changed)
  {
    currentBest = bestRegression;
    currentBestAIC = bestAIC;
    changed = F;
    for(num in VariablesNotInModel)
    {
      newRegression = union(bestRegression,num);
      newAIC = getLogisticAIC(response,newRegression,data);
      if(newAIC < currentBestAIC)
      {
        changed = T;
        currentBestAIC = newAIC;
        currentBest = newRegression;
      }
    }
    bestRegression = currentBest;
    bestAIC = currentBestAIC;
  }
  return(list(aic=bestAIC,reg=bestRegression));
}

# Problem 3
# Finds a good logistic model using the Akaike criterion and iteratively removes the worst variable
# (in order to minimize AIC) using a greedy approach
backwardSearchAIC <- function(response,data,lastPredictor)
{
  bestRegression = 1:lastPredictor;
  bestAIC = getLogisticAIC(response,bestRegression,data);
  changed = T;
  while(length(bestRegression) > 0 && changed)
  {
    currentBest = bestRegression;
    currentBestAIC = bestAIC;
    changed = F;
    for(i in 1:length(bestRegression)){
      newRegression = bestRegression[-i];
      newAIC = getLogisticAIC(response,newRegression,data);
      if(newAIC < currentBestAIC)
      {
        changed = T;
        currentBestAIC = newAIC;
        currentBest = newRegression;
      }
    }
    bestRegression = currentBest;
    bestAIC = currentBestAIC;
  }
  return(list(aic=bestAIC,reg=bestRegression));
}

# Problem 4 Code
getLogisticBIC <- function(response,explanatory,data)
{
  #check if the regression has no explanatory variables
  if(0==length(explanatory))
  {
    #regression with no explanatory variables
    deviance = glm(data[,response] ~ 1,family=binomial(link=logit))$deviance;
  }
  else
  {
    #regression with at least one explanatory variable
    deviance = glm(data[,response] ~ as.matrix(data[,as.numeric(explanatory)]),family=binomial(link=logit))$deviance;
  }
  return(deviance+log(nrow(data))*(1+length(explanatory)));
}

forwardSearchBIC<- function(response,data,lastPredictor)
{
  bestRegression = NULL;
  bestBIC = getLogisticBIC(response,bestRegression,data);
  VariablesNotInModel = 1:lastPredictor;
  changed = T;
  while(length(VariablesNotInModel) > 0 && changed)
  {
    currentBest = bestRegression;
    currentBestBIC = bestBIC;
    changed = F;
    for(num in VariablesNotInModel)
    {
      newRegression = union(bestRegression,num);
      newBIC = getLogisticBIC(response,newRegression,data);
      if(newBIC < currentBestBIC)
      {
        changed = T;
        currentBestBIC = newBIC;
        currentBest = newRegression;
      }
    }
    bestRegression = currentBest;
    bestBIC = currentBestBIC;
  }
  return(list(bic=bestBIC,reg=bestRegression));
}

backwardSearchBIC <- function(response,data,lastPredictor)
{
  bestRegression = 1:lastPredictor;
  bestBIC = getLogisticBIC(response,bestRegression,data);
  changed = T;
  while(length(bestRegression) > 0 && changed)
  {
    currentBest = bestRegression;
    currentBestBIC = bestBIC;
    changed = F;
    for(i in 1:length(bestRegression)){
      newRegression = bestRegression[-i];
      newBIC = getLogisticBIC(response,newRegression,data);
      if(newBIC < currentBestBIC)
      {
        changed = T;
        currentBestBIC = newBIC;
        currentBest = newRegression;
      }
    }
    bestRegression = currentBest;
    bestBIC = currentBestBIC;
  }
  return(list(bic=bestBIC,reg=bestRegression));
}

# Main execution function, parameter is file to read
main <- function(file)
{
  data <- read.table(file,header=F);
  response = ncol(data);
  lastPredictor = ncol(data)-1;
  results = NULL;
  # prints best AIC for glm fit and regression variables by greedily adding variables from glm
  results = c(results, forwardSearchAIC(response,data,lastPredictor));
  # prints best AIC for glm fit and regression variables by greedily removing variables from glm
  results = c(results, backwardSearchAIC(response,data,lastPredictor));
  
  # For Problem 4:
  # prints best BIC for glm fit and regression variables by greedily adding variables from glm
  results = c(results, forwardSearchBIC(response,data,lastPredictor));
  # prints best BIC for glm fit and regression variables by greedily removing variables from glm
  results = c(results, backwardSearchBIC(response,data,lastPredictor));
  print(results);
}

main("534binarydata.txt");

# Problem 4
# forward and backward did give the same AIC value (22) with 10 explanatory variables each, but the
# variables were not all the same. The forward AIC algorithm generated a model using the following
# variables: c(23, 3, 22, 21, 49, 4, 20, 34, 53, 46). The backward AIC algorithm generated a model
# using the following variables: c(1, 3, 9, 12, 20, 23, 25, 34, 41, 46). The BIC forward algorithm 
# gave a BIC of 54.97 with the explanatory variables c(23, 3, 22, 21, 49, 4, 20, 34, 53, 46) and
# the BIC backward algorithm found the same BIC value with explanatory variables
# c(1, 3, 9, 12, 20, 23, 25, 34, 41, 46).