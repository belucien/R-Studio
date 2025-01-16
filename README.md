# R-Studio NAVLE Predictive Model
# A predictive model based on veterinary medicine students' GPA and ACT scores designed to predict their performance and outcome on the board exam.

NAVLE Predictive Model - Use Case

The NAVLE Predictive Model utilizes logistic and linear regression techniques to predict student success on the NAVLE exam. Key variables include semester GPAs and cumulative scores in core subjects such as Anatomy, Pharmacology, and Pathology. The Logistic Regression Selected Variables Model (Logit_VS) achieved higher performance metrics, with an Area Under Curve (AUC) of 0.8833, a sensitivity of 0.7537, and a specificity of 0.8833. The Linear Regression Selected Variables Model reported an R² of 0.5272 on the test data, slightly outperforming the full variable model. Selected variables for both models were optimized for data availability and predictive accuracy.

To calculate the probability of passing the NAVLE exam using the logistic regression model:

𝑃
=
1
1
+
exp
⁡
(
−
(
Intercept
+
Coefficients × Variables
)
)
P= 
1+exp(−(Intercept+Coefficients × Variables))
1
​

Example variables include SEM4SPRINGGPA, SEM5FALLGPA, and core subject cumulative scores. For implementation, gather Semester 5 and 6 GPAs (adjusted for repeated courses), plug variables into the formula, and use coefficients derived from the regression output. This model provides actionable insights for identifying at-risk students and tailoring academic support.

A Final Score of >= 525 is passing while <= 524 is considered fail

Conclusion:

Are sensitivity and specificity good? 
  
  Yes, especially given the high AUC (0.8833). The model is well-balanced in distinguishing between passing and failing candidates.

Does R² align with these metrics? 
  
  While R² is moderate, it is reasonable for this type of application. The logistic regression model's performance metrics (sensitivity, specificity, and AUC) complement the R² and indicate that the model is suitable for        prediction.


# IDs have been anonymized to protect student confidentiality.
