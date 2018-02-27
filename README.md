# Countdown-Pearl

A terminal programme for the numbers game based on Graham Hutton's Functional Pearl "The countdown problem" ported to Scala

Example usage: sbt "run 867 [6, 1, 1, 5, 5, 10]"

Output: 

Chosen: List(10, 6, 5, 5, 1, 1) for a Target of: 867

Elapsed time: 1s

0 solutions

If you do no provide a target it will show all possible targets under 1000.

Example usage: sbt "run [50, 100, 4, 2, 2, 4]"
Chosen: List(100, 50, 4, 4, 2, 2)
797 possible Targets