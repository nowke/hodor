Suppose we are comparing implementations of insertion sort and merge sort on the same machine. For inputs of size $n$, insertion sort runs in $8n^2$ steps, while merge sort runs in $64n \cdot \log_{2}n$ steps. For which values of $n$ does insertion sort beat merge sort?

Insertion sort is slower than merge sort for values $n \mid 8n^2 < 64n \cdot log_2 n$. To find $n$,

$$ 8n^2 < 64n \cdot log_2 n $$
$$ n < 8 \cdot log_2 n $$
$$ 2^{\frac {n}{8}} < n$$

Trying values for $n$,

$$n=48 \Rightarrow 2^{\frac {48}{8}} = 64 > 48$$
$$n=44 \Rightarrow 2^{\frac {44}{8}} = 45.2548 > 44$$
$$n=43 \Rightarrow 2^{\frac {43}{8}} = 41.5 < 43$$

Hence, for $n<=43$, insertion sort beats merge sort