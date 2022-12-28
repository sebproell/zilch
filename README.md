# Zilch solver

A small project mainly written to experiment with Haskell.
This program calculates probabilities for the game [Zilch](https://zilch.playr.co.uk/rules.php).

Interesting features in this code:

- Infinite recursive data structure (cyclic graph of all game states)
- Memoization of certain expensive function calls


## How to run

In project root:
```
cabal run
```

Sample input/output:

```
Number of dice: 
5
Score: 
3000
Rolling up to 1 times (when reasonable) never gives more points than you already have --> Stop
Rolling up to 2 times (when reasonable) gives 3007.0478576320074 on average --> Take another turn
Rolling up to 3 times (when reasonable) gives 3008.4009896776324 on average --> Take another turn
Rolling up to 4 times (when reasonable) gives 3008.544725154699 on average --> Take another turn
Number of dice: 
1
Score: 
400
Rolling up to 1 times (when reasonable) never gives more points than you already have --> Stop
Rolling up to 2 times (when reasonable) never gives more points than you already have --> Stop
Rolling up to 3 times (when reasonable) never gives more points than you already have --> Stop
Rolling up to 4 times (when reasonable) never gives more points than you already have --> Stop
Number of dice: 
1
Score: 
300
Rolling up to 1 times (when reasonable) never gives more points than you already have --> Stop
Rolling up to 2 times (when reasonable) never gives more points than you already have --> Stop
Rolling up to 3 times (when reasonable) gives 302.4439674615217 on average --> Take another turn
Rolling up to 4 times (when reasonable) gives 310.6836511557179 on average --> Take another turn
```
