---
title: "Regularisation & Redundancy: Examining Generalisation in Deep Learning"
category: Machine Learning Theory
date: 2022-12-20
katex: true
---

The past decade has seen deep learning models grow massively in size, as researchers
gain access to greater levels of processing power. These models enter the regime of
[_overparameterisation_][topml], where the number of learnable parameters far exceed the
number of training examples[^op-fn], and yet achieve surprisingly high performance on
unseen data in very complex tasks (just give the [ImageNet challenge][ilsvrc] a try!).

Rather than explain existing theories of generalisation, which at present are still very
diverse and complex, this article seeks to examine some recent fascinating empirical
findings in Convolutional Neural Networks (CNNs) from two different perspectives:

1. Performing **regularisation**, which encourages "simpler" or more "structured" models
   in order to improve generalisation; and
2. Reducing **redundancy**, which compresses models to reduce memory requirements.

These two directions in deep learning development revolve around a central question:
**what makes massive neural network architectures so effective, and can smaller or
simpler architectures do as well?** Even partial answers to this question could have
implications for the design of deep learning architectures.

[topml]: https://arxiv.org/abs/2109.02355 "A Farewell to the Bias-Variance Tradeoff? An
Overview of the Theory of Overparameterized Machine Learning"
[ilsvrc]: https://cs.stanford.edu/people/karpathy/ilsvrc/

[^op-fn]:
    The number of model parameters may not be a good proxy for model complexity in
    deep learning, and what constitutes a good definition of model complexity remains an
    open question. See [Dar et al. (2021)][topml].

## Convolutional Neural Networks (CNNs)

The effectiveness of CNNs on image tasks is often attributed to its resemblance with the
[visual cortex][vis-cor].

![CNN Architecture (Credits: [Wikimedia](https://commons.wikimedia.org/wiki/File:Typical_cnn.png))][cnn]

[vis-cor]: https://en.wikipedia.org/wiki/Visual_cortex "Visual cortex"
[cnn]: https://upload.wikimedia.org/wikipedia/commons/6/63/Typical_cnn.png "File:Typical cnn.png"

## Regularisation

### Bias-Variance Tradeoff

### Double-Descent Phenomenon

## Redundancy

### Network Sparsity

### Quantisation

## Implications
