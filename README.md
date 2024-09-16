# LINEAR_METHODS_ASSIGNMENTS

<br>

## DEFINITION

- Linear models are a class of statistical models used to describe the relationship between a dependent variable and one or more independent variables. The simplest form is simple linear regression, which represents the relationship between two variables using a straight line. The general formula for a linear model is:

$$
Y = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \ldots + \beta_n X_n + \epsilon
$$

Where:

- $Y$ is the dependent variable,
- $\beta_0$ is the intercept (value of $Y$ when all $X$ are 0),
- $\beta_1$, $\beta_2$, $\ldots$, $\beta_n$ are the coefficients representing the influence of each independent variable $X_1$, $X_2$, $\ldots$, $X_n$
- $\epsilon$ is the error term representing the variability in $Y$ not explained by the model.

<br>

## CHARACTERISTICS OF LINEAR MODELS

- ***Linearity:*** The relationship between the dependent and independent variables is assumed to be linear.
- ***Simplicity and Interpretation:*** The coefficients $\beta$ indicate the magnitude and direction of the relationship between each independent variable and the dependent variable.
- ***Assumptions:*** These include linearity, independence, homoscedasticity (constant variance of errors), and normality of errors.

<br>

## EXAMPLES OF LINEAR MODELS

1. ***Simple Linear Regression:*** Relationship between two variables. For example, predicting the price of a house $Y$ based on its size in square meters $X$:

$$
Price = \beta_0 + \beta_1*Size + \epsilon
$$

2. ***Multiple Linear Regression:*** Relationship between a dependent variable and multiple independent variables. For example, predicting the price of a house based on its size, number of rooms, and age of the house:

$$
Price = \beta_0 + \beta_1*Size + \beta_2*Rooms + \beta_3*Age + \epsilon
$$

3. ***Analysis of Variance (ANOVA):*** A type of linear model used to compare means between different groups. Although it doesn't have an explicit formula, it can be considered an extension of linear regression to analyze differences between groups.

4. ***Logistic Regression (an extension):*** Although not linear in terms of output, logistic regression is linear in how it models the relationship between independent variables and the probability of a binary outcome.

<br>

## USE OF LINEAR MODELS

- Linear models are widely used in fields such as economics, medicine, social sciences, and engineering to understand relationships, make predictions, and make informed decisions. They are a fundamental tool in data analysis due to their simplicity and ease of interpretation.