# Player Marketvalue Prediction App

## Objective
This repository aims to provide a simple, yet effective approach to predict the market value of a soccer player. The goal is to implement a short application that allows to enter attribute values of a player and predicts a market value in Euro based on these values.

## Method
In general several machine learning algorithms are implemented to predict the market value of a soccer player based on market values present in the video game FIFA. It uses player attributes as defined by the videogame and estimates the parameters of the player market value and these attributes.



## Data
For model training the data on the players from the Electronic Arts Videogame FIFA 21 has been used, collected by Stefano Leone and published on [Kaggle.com]([url](https://www.kaggle.com/datasets/stefanoleone992/fifa-21-complete-player-dataset)).

## In-Production App (interactive)
An implementation in R-Shiny allows a quick and easy approach to generate a GUI for the attribute value input. In the following the GUI is presented with some example values for a player:
![image](https://github.com/COrthey93/Soccer-Player-Marketvalue-Prediction/assets/69873793/7ffd1476-c392-4309-964b-beeff7322745)

The attributes are described as follows:
| Player Attribute  | Description |
| ------------- | ------------- |
| Age  | Reflects player's experience and potential  |
| Potential (in %)  | Reflects a player's possible development threshold |
| International Reputation | The reputation reflects if a player is part of a national team |
| Wage | Reflects the valuation of another team for the player |
| Stamina (in %) | Reflects the player's physical state and endurance |
| Finishing Rate (in %) | Reflects a player's ability to score a goal from a chance |
| Short Passing Rate (in %) | Reflects a player's ability to play precise short passes |
| Dribbling Ability (in %) | Reflects a player's ability to move the ball forward across the pitch |
| Movement Reactions (in %) | Reflects a player's ability to react on opponent's movements and movents of direction change |

This allows a manager of a football team to assess a player's value directly by comparing the training performance of a given player to the performance of other players. For example:
- in a long run a given player of ten total players gets exhausted second-last of all players (for example measured by some sort of fitness device) -> Stamina (in %) = 9/10 = 90% as the player 'finished 2nd place'.
- in a training situation a player scores 8 of 10 shots on goal -> Finishing Rate (in %) = 80%
