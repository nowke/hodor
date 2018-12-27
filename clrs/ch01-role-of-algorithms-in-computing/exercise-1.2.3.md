What is the smallest value of $n$ such that an algorithm whose running time is $100n^2$ runs faster than an algorithm whose running time is $2^n$ on the same machine?

To find $n$,

$$100n^2 < 2^n$$

Trying values for $n$,
$$n = 14 \Rightarrow 100 \cdot 14^2 = 19600 > 2^{14} $$
$$n = 15 \Rightarrow 100 \cdot 15^2 = 22500 < 2^{15} $$

Hence, smallest value of $n$ is $n=15$
