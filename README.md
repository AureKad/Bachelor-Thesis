# Domain-Specific-Language-for-Recreational-Mathematics
This is the Code for the Bachelor thesis. 

The DLX Algorithm from Chapter 3.1 and 4.1 is implemented in dlx.hs and dllArray.hs. 
Lazy Functional State Threads are also used in dlx.hs and dllArray.hs (but mostly only in dllArray.hs).

Some examples for how to solve exact cover problems with DLX are shown in dlx.hs (for the really basic probles), queens.hs, and sudoku.hs. The non dlx.hs modules, in this case, only create the format needed for the DLX algorithm to work, so dlx.hs imports both modules and solves the problems using their functions. 

Chapter 5 modules are dsl.hs and dlxFormt.hs. The dsl.hs defines the data structures needed to create the exact cover problems, while dlxFormat.hs creates the format based on the defined data structure. 

connectedInputs.hs is the last module created and is actually just Fillomino. It is implemented and rewritten from the works of Donald Knuth. I wanted to generalize my dsl even more then I started implementing this module, however, because of time issues (and my nerves :c ) I just copied it and used it as a way to show that the dsl is "expandable". Of course i know that how I did it was not that clean but I could not muster up the energy to do it better :D 