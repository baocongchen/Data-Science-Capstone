# Data Science Capstone: [Text Wizard](https://petertran.shinyapps.io/Text-wizard/ "Text Wizard")
## Description of The Algorithm: choosing the right techniques
The app uses `shiny`, `shinythemes`, `tm`, `RWeka`, `dplyr` libraries to import, clean, tokenize and summarize the data. The app applies n-gram model to predict the next word. N-gram model isn’t a perfect solution for natural language processing; however in practice, it has been widely used due to its effectiveness in modeling language data. I created n-grams of size 1 to size 5 and calculate their frequencies by which their indices are ordered . Furthermore, N-gram of bigger size receives higher priority to be chosen as the next word.

## Description of The Algorithm: discussing the cons of the applied algorithm
Despite its practicality, the deployed algorithm has its own weaknesses; for example , it does not take into consideration the linguistic knowledge such as semantics, phonetics etc. Its n-grams has the max size of 5, which means the range of dependency is 4; this makes it hard to make an accurate prediction for long range text. In fact, natural languages incorporate so many cases of unbounded dependencies that my n-gram model fails to capture.

## Description of The Function of The App
The app is built by using the shiny package and uploaded on shiny web service. It can predict your next input based on what you already input. Your input can be anything you want; the built-in processes will take care of your input. They will clean the input data to make it ready for the prediction algorithm. 

## How to Use
Just enter a few words or a sentence and click the submit button, and the app will show you 8 words that are most likely to come after your input text. Depending on your input, you may have to wait for a few seconds to get the result.


