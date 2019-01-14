# Genetic-Algoritm

This implementation of Genetic Algorithm is based on the ideas from the article:
[Genetig Algorithm on Polynomials](https://www.researchgate.net/publication/269132907_Optimal_Polynomial_Regression_Models_by_using_a_Genetic_Algorithm)

### Usage:

```
stack install
Genetic-Algoritm-exe <Num of GEnerations> <Size of population> <Threshold fitness> <elite size>
```
Currently the code tries to find Rosenbrock function from the above Article.

Notice that if the Algirithm reaches the threshold before the final genretation it will return the output and stop. For example this is a test run from my PC (the +RTS flag uses to work on 8 cores):

```
» Genetic-Algoritm-exe 500 20 0.01 10 +RTS -N8                                                                                             orenm@OrenPC

No. of generatoins: 500
Population Size: 20
Score of goal: 0.01
Elite size (N-elit): 10

Progress:
[===================================================================================================================>.............................................................................................]
 200.012 * x1^2 * x2 + 100.488 * x1^4 + (-2.669) * x1 + 99.998 * x2^2 + 0.891 * x1^(-1) * x2^(-3)
Fitness of final Polynomial is: 5.796397023532152e-3

Attention: component with coefficient smaller tan 0.001 are rounded to nothing
------------------------------------------------------------
» 
```

#### Contact me
If you have any questions or problems, or if you want to take this project a step forward

orenm13@gmail.com
