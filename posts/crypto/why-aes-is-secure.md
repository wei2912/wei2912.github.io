---
title: Why AES Is Secure
date: 2015-10-07
---

The [Advanced Encryption Standard, otherwise known as Rijndael (which was its
original name)](https://en.wikipedia.org/wiki/Advanced_Encryption_Standard), is
one of the most widely used ciphers for the encryption of data. It's been
approved by the US government to protect classified data. In this article, I'll
explain how AES works and why it's secure.

This article is written for anyone who has knowledge of basic number theory and
computer knowledge. No prior knowledge of cryptography is required.

## Galois field theory

AES makes extensive use of Galois field Theory, which I'm going to give a very
brief introduction to. This section explains [this brief introduction to Galois
Field Theory in cryptography](http://www.math.washington.edu/~morrow/336_12/papers/juan.pdf).

A [Galois field](https://en.wikipedia.org/wiki/Finite_field) is a
[field](https://en.wikipedia.org/wiki/Field_%28mathematics%29) containing a
finite number of elements. A field is a set on which the operations of addition,
multiplication, subtraction and division meet certain rules. We'll refer to
these rules as they come in useful -- don't worry too much about them.

There's the field of real numbers and the field of rational numbers. What makes
a Galois field different from these fields, is that it contains a finite number
of elements. On the other hand, there is an infinite number of real numbers and
rational numbers.

A Galois field is denoted as $GF(p^n)$ where $p \in \mathbb{P}$ and $n \in \mathbb{Z^+}$.
We call $p^n$ the order of the field. The elements of a Galois field are the set
of elements $\{0, 1, 2, \ldots, p^n - 1\}$

In AES, where we want to manipulate bytes, $GF(2^8)$ is used. $GF(2^8)$ has a
set of elements which represent all possible values of a byte. The fact that the
addition and multiplication operations are closed (that is, applying addition
and multiplication to any two elements will always return an element that is in
the set of elements of that field) makes the Galois field very convenient for
manipulating bytes, as operations can be done on any two bytes to get a new
byte.

### Galois field arithmetic

Firstly, let's take a look at $GF(p)$. Addition and multiplication is done
similarly to integers, followed by modulo $p$. For example, in $GF(5)$, $4 + 3 = 7$
is reduced to $2$, and $4 \times 2 = 8$ is reduced to $3$.

Elements of $GF(p^n)$ can be represented as polynomials of degree less than $n$.
In addition, we continue to perform modulo $p$. For example, in $GF(3^3)$, the
polynomial $x^2 + 2$ would represent 11 and the polynomial $2x^2 + x + 1$ would
represent 22. How these polynomials are created is similar to how you would
represent a base 10 number in base 3, with $x = 3$. Adding up the two
polynomials gives us $3x^2 + x + 3$, which is reduced to $x + 3$ or 4.

However, things get more complicated when it comes to multiplication. Take the
polynomials $2x^2 + 1$ and $x + 1$. Multiplying these two would give us $(2x^2 + 1)(x + 1) = 2x^3 + 2x^2 + x + 1$.
At this point, we've exceeded the order of the field! The way to resolve this is
to modulo a suitable polynomial, by long division. There are certain rules for
selecting this polynomial, which I will not discuss.

### Use of Galois field theory in AES

In AES, we use the Galois field $GF(2^8)$ as its set of elements is the set of
all possible values of a byte. Each element can be represented by a binary
number. It turns out that addition of two elements in $GF(2^8)$ is simply XOR of
both elements.

On the other hand, multiplication is more complex. AES uses the polynomial
$x^8 + x^4 + x^3 + x + 1$.

The exact uses of Galois field theory will be covered in later sections.

## Overview of AES

Now that we're done with basic Galois field theory, it's time to take a look at
how AES is implemented.

AES can be represented by two functions, $E(k, p)$ and $D(k, c)$ where $k$ is
the key, $p$ is the plaintext and $c$ is the ciphertext. The former represents
encryption, and the latter represents decryption. A valid encryption scheme
would ensure that $D(k, E(k, p)) = p$ for any $k$ and $p$.

AES has 3 key sizes: 128, 192 and 256 bits. It is known as a block cipher; that
means it encrypts plaintext in seperate blocks of fixed size, as opposed to a
stream cipher which encrypts plaintext bit by bit. Its block size is 128 bit.

![](https://i.stack.imgur.com/SnHH2.png)

Credits to [mikeazo's answer](http://crypto.stackexchange.com/a/8044) for the
image.

AES can be broken down into 4 steps:

1. `AddRoundKey`
2. `SubBytes`
3. `ShiftRows`
4. `MixColumns`

These 4 steps make up a round. Before the first round, `AddRoundKey` is
performed, and in the last round, `MixColumns` is omitted.

Depending on the key size, AES has different number of rounds. The reason is
that given more key bits, there is a need for more rounds so as to ensure that
there is *[confusion](https://en.wikipedia.org/wiki/Confusion_and_diffusion)*,
or that each character of the plaintext depends on several parts of the key.
Confusion is a key property of a secure cipher; the other property is diffusion,
which means that if we change a character of the plaintext, then several
characters of the ciphertext should change. We'll see how these two properties
are used in the design of AES.

## Key Schedule

Each round has a certain key of its own, derived from the key given. The
algorithm to obtain the round keys is called the key schedule.

