## Text Prediction

Many of us have experienced how typing on mobile devices can be slow and frustrating. A way to ease this frustration without carrying around a keyboard is to use text prediction where an application will accurately guess what you want to type next before you type it.

The goal of this project is to build a predictive text model and produce an application that uses it. The application will accept a partial sentence that the user will type, then return 3 options to choose from.

In order to build a predictive text model, we need data. The dataset must contain written speech that can be used to calculate the most frequent occurrences of ngrams. The dataset used for this project is a series of blog posts, news articles and twitter feeds, which can be downloaded by clicking here:  
https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip    

The overall process for building a text predictor changed several times. At first I was too focused on accuracy and had to focus more on speed. My initial method involved working with the entire training dataset. I ran a loop that cleaned and analyzed small pieces at a time and once a predicted word reached a certain level of confidence, the loop would break. See code in the "Exploratory_Work" folder [here](https://github.com/AlexPouletsos/Text_Prediction/blob/master/Exploratory_Work/Accurate%20%26%20Slow%20Algorithm.R).
This was proving to be an effective method if time was no issue. However, it was quite slow and would be even slower if the algorithm was to return the top 3 predictions. The idea was abandoned once I realized it can never be used in a practical application.

The final method for building a text predictor involved building a prediction table. The first step is to clean the data by removing punctuation and symbols, then build 2 to 5 word n-grams. The next step is to separate the n-grams into two columns, "base" and "predicted", then only keep the top three predictions for each unique base. Now you have a manageable table for the text prediction algorithm to reference that will still be quite accurate.  

I first built the Prediction Table using a random sample of 20% of the training dataset, but I found the prediction algorithm took too long to process. Using a random sample of 10% of the training dataset seems to be the most I can use for the prediction algorithm to run at a comfortable speed. It doesn't seem much accuracy was sacrificed either. To see the R code that was used to build the prediction table, click [here](https://github.com/AlexPouletsos/Text_Prediction/blob/master/Prediction%20Table.R).

When the user enters a sentence, any extra spaces, commas or capitalization will not effect the output. All letters will change to lower case, extra spaces between words will be reduced to one and any spaces at the end will be removed before the algorithm evaluates the string. Regardless of how many words are typed, only the last 4 are evaluated. It looks for an exact match in the 4-grams list and returns the 3 predictions. If there are less than 3 predictions available for the 4-gram, it will look for the 3-gram predictions, then 2-gram predictions and finally the predictions for the last word in the sentence. If at last there are no matches at all, the words "and", "to", or "the" will be returned. In order to prevent repeats, there is a condition that if the model moves to the next set of n-grams then it won't return any predicted words already used. The app will also recognize a period, question mark or exclamation point at the end of a sentence and will suggest "I", "In", and "The", which are 3 of the most common beginnings to a sentence according to this dataset. To see the R code that was used to build the Shiny app, click [here](https://github.com/AlexPouletsos/Text_Prediction/blob/master/Shiny_App_Code/app.R).

The Shiny app is inspired by the SwiftKey application where users are given 3 suggested words to choose from to go next in their sentence. As you type your sentence, you will see the words on the three buttons change. You have the option to continue typing or click on one of the three words which will then be added to your existing sentence. Try it out!

Link to Text Predictor Shiny App:  
https://alexpouletsos.shinyapps.io/Text_Predictor/
