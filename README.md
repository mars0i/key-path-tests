# key-path-tests

Speed tests of Clojure functions that return sequences of sequences of
keys representing paths through embedded maps.

keypaths.clj contains the functions I tested, some testing code, and a
few other functions that I didn't test because they do something
slightly different.

keypathsCriteriumTimes.txt contains timing info for the functions.

The functions are by various authors.  The original versions--none of
which are mine--can be found at:

http://stackoverflow.com/questions/21768802/how-can-i-get-the-nested-keys-of-a-map-in-clojure:

http://stackoverflow.com/questions/25268818/get-key-chains-of-a-tree-in-clojure

https://clojurians.slack.com/archives/C0FVDQLQ5/p1489779215484550
