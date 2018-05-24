## Text Prediction

The overall process for building a text predictor changed several times. At first I was too focused on accuracy and had to focus more on speed. My initial method was to read in the entire training dataset, which can be downloaded by clicking here:  
https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip  
Once the text was cleaned I ran a loop that analyzed small pieces at a time and once a predicted word reached a certain level of confidence, the loop would break. See code in the "Exploratory_Work" folder [here](https://github.com/AlexPouletsos/Text_Prediction/blob/master/Exploratory_Work/Accurate%20%26%20Slow%20Algorithm.R).
This was proving to be an effective method if time was no issue. However, it was quite slow and would be even slower if the algorithm was to return the top 3 predictions. The idea was abandoned once I realized it can never be used in a practical application.

The final method for building a text predictor involved building a prediction table. The first step is to clean the data, then build 2 to 5 word n-grams. The next step is to separate the n-grams into two columns, "base" and "predicted", then only keep the top three predictions for each unique base. Now you have a manageable table for the text prediction algorithm to reference that will still be quite accurate.  

I first built the Prediction Table using 20% of the training dataset, but I found the prediction algorithm took too long to process. Using 10% of the training dataset seems to be the most I can use for the prediction algorithm to run at a comfortable speed. It doesn't seem much accuracy was sacrificed either. The experience could be much different for faster computers, but I found this to be the best option using a machine with 4 GB of RAM.  

When the user enters a sentence, any extra spaces or capitalization will not effect the output. All letters will change to lower case, extra spaces between words will be reduced to one and any spaces at the end will be removed before the algorithm evaluates the string. Then it looks for an exact match in the 5-grams list and returns the 3 predictions. If there are less than 3 matches, it looks for 4-gram, then 3-gram, etc. In order to prevent repeats, there is a condition that if the model moves to the next set of n-grams then it won't return any predicted words already used in previous n-grams. 



Link to Text Predictor Shiny App:  
https://alexpouletsos.shinyapps.io/Text_Predictor/
