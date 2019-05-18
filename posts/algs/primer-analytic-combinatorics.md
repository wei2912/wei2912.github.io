---
title: A Primer to Analytic Combinatorics
category: Algorithms
date: 2019-01-11
katex: true
---

The material below is my attempt to condense [Analysis of Algorithms
(AofA)][aofa], an excellent course by [Prof. Robert Sedgewick of Princeton
University][rs], into something more digestable and easily visualised. (Even [a
self-proclaimed gentle introduction to analytic combinatorics][gentle-intro],
which also inspired this article, deals in a lot of fairly advanced math for
undergraduates.)

The course contains a fair bit of particularly elegant math, worth studying
even for those who shy away from advanced math. A basic understanding of
analytic combinatorics can broaden the way one interprets and analyses
algorithms.

[aofa]: https://www.coursera.org/learn/analysis-of-algorithms
[rs]: https://www.cs.princeton.edu/~rs/
[gentle-intro]: https://lipn.univ-paris13.fr/~nicodeme/nablus14/nafiles/gentle.pdf

# Part I. Mathematical Foundations

This part gives a brief overview of the mathematical foundations required to
understand the content later on.

## Recurrences

[explain basic recurrences]

## Generating Functions and Power Series

While the terms of every sequence are countable, it is possible to represent
sequences using techniques involving real or complex numbers which are
uncountably infinite in nature. A sequence such as the following,

$$1, 1, 1, \ldots$$

can be turned into a **power series** (a polynomial but with infinitely many
terms),

$$1 + x + x^2 + \ldots = \frac{1}{1 - x}$$

by putting the coefficients of the power series as each individual term of a
sequence. Such a power series is what we call a **generating function**.

**Definition.** The function

$$A(z) = \sum_{k \geq 0} a_k z^k$$

is the **Ordinary Generating Function (OGF)**^[We sometimes refer to the OGF as
simply a generating function. In the scope of this article, Exponential
Generating Functions (EGFs) have been excluded.] of the sequence $a_0, a_1,
a_2, \ldots, a_k, \ldots$. We denote $[z^N]A(z)$ to be the coefficient of $z^N$
in $A(z)$.

Addition and subtraction of generating functions are very natural and intuitive
operations. Similarly, scaling from some sequence $x_0, x_1, x_2, \ldots$ to
$x_0, kx_1, k^2x_2, \ldots$ for some constant $k$ is not too difficult either -
just replace $x$ with $kx$ in the generating function.

### Convolution

Things go iffy when we try to multiply two generating functions together. Say
we used the two generating functions $1 + x + x^2 + x^3 + \ldots = 1/(1 - x)$
and $1 + 2x + 4x^2 + 8x^3 + \ldots = 1/(1 - 2x)$, and multiplied them together.
While we *know* what the generating function looks like, we don't know much
about the sequence it represents. An attempt to figure out the power series
could yield this:

$$\begin{aligned}
   & (1 + x + x^2 + x^3 + \ldots)(1 + 2x + 4x^2 + 8x^3 + \ldots) \\
  =& 1 + (1 \cdot 2 + 1 \cdot 1)x + (1 \cdot 4 + 1 \cdot 2 + 1 \cdot 1)x^2 + (1
    \cdot 8 + 1 \cdot 4 + 1 \cdot 2 + 1 \cdot 1)x^3 \\
  =& 1 + 3x + 7x^2 + 15x^3 + \ldots \\
\end{aligned}$$

With a way to formalise our intuition of generating functions and some
intuition on how multiplication of power series would work, we could derive a
lemma.

**Lemma.** If $A(z) = \sum_{k \geq 0} a_k z^k$ is the OGF of $a_0, a_1, a_2,
\ldots, a_k, \ldots$, and $B(z) = \sum_{k \geq 0} b_k z^k$ is the OGF of $b_0,
b_1, b_2, \ldots, b_k, \ldots$, then $A(z)B(z)$ is the OGF of $a_0b_0, a_0b_1 +
a_1b_0, a_0b_2 + a_1b_1 + a_2b_0, \ldots$.

**Proof.** $$\begin{aligned}
  A(z)B(z) &= \sum_{k \geq 0} a_k z^k \sum_{n \geq 0} b_n z^n \\
    &= \sum_{k \geq 0} \sum_{n \geq 0} a_k b_n z^{n+k} \\
    &= \sum_{k \geq 0} \sum_{n \geq k} a_k b_{n-k} z^n \\
    &= \sum_{n \geq 0} \left(\sum_{0 \leq k \leq n} a_kb_{n-k}\right) z^n
\end{aligned}$$

From this, one could manipulate the OGF $A(z)$ of $a_0, a_1, a_2, \ldots,
a_k, \ldots$ to obtain $a_0, a_0 + a_1, a_0 + a_1 + a_2, \ldots, \sum^k_{i = 0}
a_i, \ldots$, simply by considering $\frac{1}{1 - z}A(z)$ as we just did in
the example above.

## Asymptotics

[a brief explanation of the big O/Theta/Omega functions and the approximation
of functions should suffice]

# Part II. The Symbolic Method

This part explores the construction of many common recursive data structures,
and takes a look at how transfer theorems can help us create generating
functions from these data structures.

# Part III. The Analysis

Beyond counting how many data structures we could construct of some size, we
would of course want to analyse certain properties of these data structures and
algorithms.

