# Rush-hour
Haskell program which solves the board game Rush Hour. Solution is brute-force but fast; doesn't work when board has no solution

To run:

Load/compile rush_hr.hs

Then type rush_hour <Your board configuration here>

Board configuration should be written in the following format:

["--B---","--B-DD","XX----","--AA--","ZZ----","--YY--"],

where any letters other than 'X' represent other cars/trucks.

The special car must be labeled "XX", and must be placed in the third row.

This solver currently does not work on boards larger/smaller than 6x6.
