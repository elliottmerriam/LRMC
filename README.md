# LRMC Model

R implementation of Logistic Regression/Markov Chain (LRMC) model originally developed by Joel Sokol and Paul Kvam at Georgia Tech (see http://www2.isye.gatech.edu/~jsokol/lrmc/about_lrmc.html).  This script builds off of a version originally provided by Richard W. Sharp.

The LRMC model is an NCAA basketball ranking system designed to predict a winner for each game of the NCAA tournament ("March Madness").  NCAA tournament games are played on a "neutral" court, whereas regular season games are played on one of the teams' home courts.  The model predicts the winner of each tournament game based on logistic regression analysis of regular season games between the two teams.  The model is based on just two input parameters for each game:  the margin of victory in their regular season games, and whose court the teams played on for those games.

This R script builds an LRMC model for the 2014 men's NCAA basketball tournament, and compares the accuracy of its predictions to those of models based on Strength of Schedule (SOS) and Rating Percentage Index (RPI), or a simple average of the three.
