# eBay Auctions


## Data description
The file eBayAuctions.csv contains information on 1972 auctions transacted on eBay.com during May-June 2004. The goal is to use these data to build a model that will classify competitive auctions from non-competitive ones. A competitive auction is defined as an auction with at least two bids placed on the item auctioned. The data include variables that describe the item (auction category), the seller (his/her eBay rating), and the auction terms that the seller selected (auction duration, opening price, currency). In addition, we have the price at which the auction closed. The goal is to predict whether or not the auction will be competitive.

## Questions we will be answering in this project:
1. Split the data into training and validation datasets using a 65% to 35% ratio, with a seed of 29.

2. Fit a classification tree using all predictors. Use the default tree generated by rpart(). Apply this default tree on the validation set and write down the confusion matrix. How can you compute the accuracy based on the confusion matrix?

3. Find the accuracy of this model for training data.

4. Is this model practical for predicting the outcome of a new auction? (HINT: Consider the availability of information about the predictors at the time of prediction.)

5. Fit another default classification tree. This time only use the predictors that can be used for predicting the outcome of a new auction. Describe the resulting tree in terms of rules.

6. Use the classification tree we built in the previous step to predict whether the following auction is competitive or not.

&nbsp;&nbsp;&nbsp;  ![image](https://github.com/Moh-Nafi/Business_Data_Analysis/assets/133475571/d46aedf7-c2cb-4723-9db1-546ea83ec1e9)

7. Based on this last tree, what can you conclude from these data about the chances of an auction obtaining at least two bids and its relationship to the auction settings set by the seller (duration, opening price, currency)? What would you recommend for a seller as the strategy that will most likely lead to a competitive auction?

## Project Outcome

### Output result of question 2
![image](https://github.com/Moh-Nafi/Business_Data_Analysis/assets/133475571/33febdec-4f7b-46b2-9efd-0ad934c0daa5)

### Output result of question 5
![image](https://github.com/Moh-Nafi/Business_Data_Analysis/assets/133475571/dcb28626-3d6a-4853-b303-c2eb89e95774)

