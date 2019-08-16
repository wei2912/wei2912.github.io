---
title: A Primer to Analytic Combinatorics
category: Algorithms
date: 2019-08-17
katex: true
---

The material below is my attempt to condense [Analysis of Algorithms
(AofA)][aofa], an excellent course by [Prof. Robert Sedgewick of Princeton
University][rs], into something more digestable and easily visualised. (Even [a
self-proclaimed gentle introduction to analytic combinatorics][gentle-intro]
and [a series of condensed slides on the applications of analytic
combinatorics][apps], both which inspired this article, deal in a lot of
advanced math which could be hard to follow.)

[TODO: insert motivation for study]

[aofa]: https://www.coursera.org/learn/analysis-of-algorithms
[rs]: https://www.cs.princeton.edu/~rs/
[gentle-intro]: https://lipn.univ-paris13.fr/~nicodeme/nablus14/nafiles/gentle.pdf
[apps]: http://www.dim.uchile.cl/~school2012/martinez/latinschool.pdf

We assume knowledge of some calculus, discrete math and probability in the rest
of this article, as well as basic asymptotics. [TODO: link to extra resources
for supplement]

# Part I. Generating Functions

> A generating function is a clothesline on which we hang up a sequence of
numbers for display.

--- *Herbert S. Wilf in Generatingfunctionology, Chapter 1*.

Take any sequence of numbers that has a recognizable pattern to it, say the
*Fibonacci sequence*: $0, 1, 1, 2, 3, 5, \ldots$. While we can count through
the terms of each sequence and consider things like the "0.5th term" to be
alien, it is possible to represent sequences using analytical techniques
involving real or complex numbers, *which are certainly not in the realm of
discrete mathematics*. A sequence such as the following,

$$1, 1, 1, \ldots$$

can be turned into a **power series**,

$$1 + x + x^2 + \ldots = \frac{1}{1 - x}$$

by putting the coefficients of the power series as each individual term of a
sequence. Such a power series is what we call a **generating function**.

---

**Definition.** The function

$$A(z) = \sum_{k \geq 0} a_k z^k$$

is the **Ordinary Generating Function (OGF)**^[We sometimes refer to the OGF as
simply a generating function, but other types of generating functions exist
too. These are outside the scope of this article.] of the sequence $a_0, a_1,
a_2, \ldots, a_k, \ldots$. We denote $[z^N]A(z)$ to be the coefficient of $z^N$
in $A(z)$.

---

Addition and subtraction of generating functions are very natural and intuitive
operations. Similarly, scaling from some sequence $x_0, x_1, x_2, \ldots$ to
$x_0, kx_1, k^2x_2, \ldots$ for some constant $k$ is not too difficult either
--- just replace $x$ with $kx$ in the generating function. We could also
differentiate and integrate these sequences without much trouble.

### From Recurrences to Generating Functions

What we've seen above works nicely if we are looking at a bunch of standard
series, but that isn't very interesting. Back to our example of the Fibonacci
sequence:

$$a_n = a_{n-1} + a_{n-2},
\quad a_0 = 0, a_1 = 1.$$

A closed-form solution to this isn't trivial, but we could convert this
recurrence relation into a generating function. First, with the help of the
**Kronecker delta**, a nifty notation that defines $\delta_{ij} = 1$ for $i =
j$ and $\delta_{ij} = 0$ otherwise, we simplify the initial conditions of this
recurrence relation:

$$a_n = a_{n-1} + a_{n-2} + \delta_{n1},
\quad a_n = 0 \text{ if } n < 0.$$

Let $A(z) = \sum_n a_nz^n$, or the OGF of $\{a_n\}_{n \geq 0}$. We slap on a
$z^n$ term to the whole equation and sum on $n \geq 0$:

$$\sum_n a_nz^n = \sum_n (a_{n-1}z^n + a_{n-2}z^n + \delta_{n1}z^n);$$

with a bit of thought, we could rewrite this into

$$\sum_n a_nz^n = z\sum_n a_{n-1}z^{n-1} + z^2\sum_n a_{n-2}z^{n-2} +
\sum_n \delta_{n1}z^n$$

which simplifies very nicely to $$A(z) = zA(z) + z^2A(z) + z.$$

This yields $A(z) = z/(1 - z - z^2)$, a very neat formula that
*condenses* all the intricacies of the Fibonacci sequence into a simple
algebraic function. Certainly not what you'd imagine the Fibonacci sequence to
look like.

Our aim now is to obtain the coefficient of the $n$th term. I won't elaborate
further, but [Austin Rochford's blog post](https://austinrochford.com/posts/2013-11-01-generating-functions-and-fibonacci-numbers.html)
sums up the proof of *Binet's formula* well, and we get $$F_n =
\frac{\phi^n - \psi^n}{\phi - \psi}$$ with $\phi = \frac{1 + \sqrt{5}}{2}, \psi
= \frac{1 - \sqrt{5}}{2}$.

### Convolution

Things go iffy when we try to multiply two generating functions together. Say
we used the two generating functions $1 + x + x^2 + x^3 + \ldots = 1/(1 - x)$
and $1 + 2x + 4x^2 + 8x^3 + \ldots = 1/(1 - 2x)$, and multiplied them together.
While we *know* what the generating function looks like, we don't know much
about the sequence it represents. An attempt to figure out the power series
could yield this:

$$\begin{aligned}
  &(1 + x + x^2 + x^3 + \ldots)(1 + 2x + 4x^2 + 8x^3 + \ldots) \\
  &= 1 + (1 \times 2 + 1 \times 1)x + (1 \times 4 + 1 \times 2 + 1 \times 1)x^2
  \\
  &\quad + (1 \times 8 + 1 \times 4 + 1 \times 2 + 1 \times 1)x^3 \\
  &= 1 + 3x + 7x^2 + 15x^3 + \ldots \\
\end{aligned}$$

With a way to formalise our intuition of generating functions and some
intuition on how multiplication of power series would work, we could derive a
lemma.

---

**Lemma.** If $A(z) = \sum_{k \geq 0} a_k z^k$ is the OGF of $a_0, a_1, a_2,
\ldots, a_k, \ldots$, and $B(z) = \sum_{k \geq 0} b_k z^k$ is the OGF of $b_0,
b_1, b_2, \ldots, b_k, \ldots$, then $A(z)B(z)$ is the OGF of $a_0b_0, a_0b_1 +
a_1b_0, a_0b_2 + a_1b_1 + a_2b_0, \ldots$.

**Proof.** Multiplying two sums and rewriting the terms, $$\begin{aligned}
  A(z)B(z) &= \sum_{k \geq 0} a_k z^k \sum_{n \geq 0} b_n z^n \\
    &= \sum_{k \geq 0} \sum_{n \geq 0} a_k b_n z^{n+k} \\
    &= \sum_{k \geq 0} \sum_{n \geq k} a_k b_{n-k} z^n \\
    &= \sum_{n \geq 0} \left(\sum_{0 \leq k \leq n} a_kb_{n-k}\right) z^n.
  &&\blacksquare
\end{aligned}$$

---

From this, one could manipulate the OGF $A(z)$ of $a_0, a_1, a_2, \ldots,
a_k, \ldots$ to obtain $a_0, a_0 + a_1, a_0 + a_1 + a_2, \ldots, \sum^k_{i = 0}
a_i, \ldots$, simply by considering $\frac{1}{1 - z}A(z)$ as we just did in
the example above.

# Part II. The Symbolic Method

This part explores the construction of many common recursive data structures,
and takes a look at how transfer theorems can help us create generating
functions from these data structures.

# Part III. The Analysis

Beyond counting how many data structures we could construct of some size, we
would of course want to analyse certain properties of these data structures and
algorithms.

