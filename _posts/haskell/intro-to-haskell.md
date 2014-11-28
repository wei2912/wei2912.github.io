---
title: Introduction to Haskell
date: 2014-11-27
---

[Haskell](https://www.haskell.org/platform/) is a pure functional programming language that comes with lazy evaluation. Its elegance is why many programmers like writing code in Haskell.

This post assumes that the reader has at least an intermediate knowledge of an imperative programming language, but does not assume knowledge of functional programming.

Also, this post is not intended to teach Haskell. This post is intended to explain the code just briefly, such that the reader understands what the code is doing.

If you wish to try out the code, download [main.hs](/res/intro-to-haskell/main.hs) and run `ghci main.hs`. You'll need GHC installed. Then, you can evaluate stuff and see what you get.

## Powers of a number

Haskell lets us do quite a lot of elegant stuff. Here's one that I find particularly elegant.

```haskell
pows :: Integer -> [Integer]
pows pow = iterate (pow *) 1
```

In here, we define a function that takes in an integer and returns a list of integers (that's what the first line shows: a type signature).

In Haskell, parameters are separated by spaces (not enclosed in brackets and seperated by commas, for those of you familiar with C-style languages). The first and only parameter we take in is `pow`.

Here, we return the result of `iterate (pow *) 1`. What does that function do? Let's look at the definition of `iterate`.

```haskell
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)
```

The type signature on the first line uses what we call polymorphic types, which allow for any type. A function which takes in one parameter and returns something is denoted by `(a -> a)`. As you can see from the use of "a" as both input and output, the two types have to be the same.

Here, `iterate` takes in a function, an element, and returns a list of elements. All of these have to be the same type.

The `:` operator is known as the cons operator. It appends an element to a list of the same type. In here, we prepend `x` to `iterate f (f x)`.

If you were paying close attention, you may realize that I appear to have made a typo in `pow *`. This is not the case. Haskell allows the use of [partially applied functions](https://en.wikipedia.org/wiki/Partial_application).

If you don't understand what that is, don't worry. Just take it as a function that takes in a number and multiplies it by 3.

It's hard to see what this function does if you're not familiar with recursion. Let's look at a trace.

      iterate (3 *) 1
    = 1 : iterate (3 *) (3 * 1)
    = 1 : 3 : iterate (3 *) (3 * 3)
    = 1 : 3 : 9 : iterate (3 *) (3 * 9)
    = ...

We can see that we'll get a list of powers of 3.

But wait! Wouldn't that go on infinitely? In Haskell, executing `pows 3` would have given us an infinite list of the powers of 3 (try it and see what happens!). Notice that the list will keep on growing. That looks like a horrible thing.

But we can still do stuff with an infinite list, thanks to laziness. We need not evaluate the whole list. We only need to evaluate the items that we need to. That is the crux of laziness.

That means that from our infinite list:

    [1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683, ...]

We can do stuff like get the first 5 powers of 3:

```haskell
take 5 (pows 3)
```

and get:

    [1, 3, 9, 27, 81]

or maybe get all powers of 2 below 10:

```haskell
takeWhile (< 100) (pows 2)
```

and get:

    [1, 2, 4, 8, 16, 32, 64]

The elegance of this solution is the fact that we easily did this with recursion, in a rather "mathematical" way.

Furthermore, we need not repeat the same code multiple times so as to create variations of the code that filtered elements the way we did. That would have been a rather hackish solution. This ability to reuse functions like this grants us a lot of power in Haskell and allows us to reason about code in a very powerful way.

## Fibonacci Numbers

Fibonacci numbers start with 0 and 1. Each number is the sum of the previous two numbers. In this example, we'll handle only positive integers.

Here's how the sequence looks like:

    0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 ...

As you can see, the third number, 1, is the sum of 0 and 1. The fourth number, 2 is the sum of 1 and 1. This sequence goes on forever.

On to the code:

```haskell
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

This time round, we define `fibs` as a list of integers. We then prepend `0` and `1` to `zipWith (+) fibs (tail fibs)`.

What does `zipWith` do? It takes a function and applies them to the values of both of the lists. The implementation of this is out of the scope of this post (but is quite trivial for any beginner in Haskell to implement).

Take a look at this trace:

    zipWith (+) [1, 2, 3] [4, 5, 6] -- 1 + 4
    = 5 : zipWith (+) [2, 3] [5, 6] -- 2 + 5
    = 5 : 7 : zipWith (+) [3] [6]   -- 3 + 6
    = 5 : 7 : 9 : zipwith (+) [] []
    = [5, 7, 9]

Now that `zipWith` is sorted, we see that we pass in `fibs` and `tail fibs`. What's `tail fibs`? Take a look at this graphical representation:

        +--+--+--+--+--+--+--+--+
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
     ^

The top line denotes the elements that make up the tail of a list while the bottom carat denotes the head. To put it in words, the head is the first element of the list, while the tail is everything after the head.

`zipWith (+) fibs (tail fibs)` is still a mystery to us. What could it possibly mean? Let's look at `fibs` and `tail fibs` first (with the values that we already know):

      0 1 ...
    + 1 ...
    ---------
      1 

The first line is `fibs` and the second line is `tail fibs`. Adding up the numbers on the first column, 0 and 1, gives us 1, our third number. We append this to the list.

When we evaluate the fourth number, here's what our list looks like:

      0 1 1 ...
    + 1 1 ...
    ---------
      1 2

We get the fourth number, 2, and it's appended to the list. Now, for the fifth number:

      0 1 1 2 ...
    + 1 1 2 ...
    ---------
      1 2 3

We get 3. This continues, eventually building up a list of Fibonacci numbers.

This algorithm, while a bit hard to comprehend, is inherently elegant and simple.

## Conclusion

I hope this has showed you how elegant Haskell could be and why many programmers like working in it.

There's a lot more to Haskell than this (such as Monads) which make solving other real-life problems quite elegant as well.

These 2 readings here explain very well why Haskell matters, and why many programmers like Haskell:

* [Why Haskell matters](https://www.haskell.org/haskellwiki/Why_Haskell_matters)
* [Why Functional Programming matters](http://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf)

You may also wish to follow [bitemyapp's guide](https://github.com/bitemyapp/learnhaskell) to learn Haskell.

Feel free to contact me (look to the left for my contact details) if you have any questions or suggestions about this post. Alternatively, you can find me at [#haskell@Freenode](irc://chat.freenode.net/#haskell) under the nick "wei2912".
