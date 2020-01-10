# PFGA [*P*arallel *F*unctional *G*enetic *A*lgorithm]

Completed for [COMS-4005-003](http://www.cs.columbia.edu/~sedwards/classes/2019/4995-fall/index.html), Parallel Functional Programming taught by Dr. Stephen Edwards at Columbia University.  Attempted parallel implementation of genetic algorithm to evolve "Robbie the Robot," from Melanie Mitchell's Introduction to Complexity.  (Sequential implementation is successful; the use of ParIO Monad less so).   

## Usage:

Will `stack run`; usage error message will suggest following arguments. They must all be provided:
 - population size
 - dimension of the GridWorlds to be searched
 - steps per evaluative simulation
 - number of generations
 - density of garbage in GridWorld
 - mutation rate
 - crossover rate
 - frequency of logging
 - path to logfile (appended)
 - whether to be --par
