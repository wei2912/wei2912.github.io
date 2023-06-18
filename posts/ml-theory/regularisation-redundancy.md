---
title: "Regularisation & Redundancy: Examining Generalisation in Deep Learning"
category: ml-theory
date: 2023-06-19
katex: true
toc: true
---

The past decade has seen deep learning models grow massively in size, as
researchers gain access to greater levels of processing power. These models
enter the regime of [overparameterisation][topml], where the number of learnable
parameters far exceed the number of training examples[^op-fn], and yet achieve
surprisingly high performance on unseen data in complex tasks.

Rather than explain existing theories of generalisation, which at present are
still very diverse and complex, this article seeks to examine some recent
fascinating empirical findings in [Convolutional Neural Networks (CNNs)][cnn]
from two different perspectives:

1. Performing **regularisation**, which encourages "simpler" or more
   "structured" models in order to improve generalisation; and
2. Reducing **redundancy**, which compresses models to reduce memory
   requirements.

These two perspectives revolve around a central question in the design of deep
learning architectures: **what makes massive neural network architectures so
effective**, and **can smaller or simpler architectures do as well?**

[topml]:
    https://arxiv.org/abs/2109.02355
    "A Farewell to the Bias-Variance Tradeoff? An
    Overview of the Theory of Overparameterized Machine Learning"
[cnn]:
    https://en.wikipedia.org/wiki/Convolutional_neural_network
    "Convolutional neural network"

[^op-fn]:
    The number of model parameters may not be a good proxy for model complexity
    in deep learning, and what constitutes a good definition of model complexity
    remains an open question. See [Dar et al. (2021)][topml].

## Simple Models?

[TODO: add FNN diagram]

The [universal approximation theorem][ua-thm] is often cited as the underlying
reason behind the expressivity of neural networks - essentially, under _certain
conditions_, a [Feedforward Network (FNN)][fnn] can approximate any continuous
function:

1. **(Arbitrary width)** [Kurt et al. (1989)][ua-thm-w] proved for a FNN with _a
   single hidden layer of arbitrary width_ and the [tanh][tanh] activation
   function.
2. **(Arbitrary depth)** [Lu et al. (2017)][ua-thm-d] proved for a FNN with
   _arbitrarily many $(n+4)$-width hidden layers_ and the [ReLU][relu]
   activation function.

[ua-thm]:
    https://en.wikipedia.org/wiki/Universal_approximation_theorem
    "Universal approximation theorem"
[fnn]:
    https://en.wikipedia.org/wiki/Feedforward_neural_network
    "Feedforward neural network"
[ua-thm-w]:
    https://dl.acm.org/doi/abs/10.5555/70405.70408
    "Multilayer feedforward networks are universal approximators"
[tanh]:
    https://en.wikipedia.org/wiki/Hyperbolic_functions
    "Hyperbolic functions"
[ua-thm-d]:
    https://proceedings.neurips.cc/paper/2017/hash/32cbf687880eb1674a07bf717761dd3a-Abstract.html
    "The Expressive Power of Neural Networks: A View from the Width"
[relu]:
    https://en.wikipedia.org/wiki/Rectifier_(neural_networks)
    "Rectifier (neural networks)"

## Regularisation

Regularisation is often seen as imposing [Occam's razor][occam]: between two
different models which are similarly feasible, we should prefer the "simpler"
model. Simplicity can be viewed in various ways:

1. a model with lower "flexibility" may better represent the test population,
   such as in the classic example of [polynomial regression][uf-of];
2. models [that can be "compressed" more][mdl] could be considered simpler;
3. for models with many parameters, [a penalty term is often added to the loss
   function][ridge] to achieve higher sparsity (i.e. having more parameters
   close to zero);
4. certain restrictions could be imposed on the model structure, such as [the
   use of Convolutional Neural Network (CNN) architctures][cnn].

Intuitively, simpler models work well in simpler tasks, especially if we know
how the solution should look like; for e.g., learning a polynomial function is
straightforward even if the degree of the polynomial is unknown. This is not the
case with complex tasks such as image classification, where it is unclear why
one should prefer "simpler" model architectures over more sophisticated or
"flexible" ones. **Understanding regularisation in complex tasks requires us to
revisit _the bias-variance tradeoff_**.

[occam]: https://en.wikipedia.org/wiki/Occam%27s_razor "Occam's razor"
[uf-of]:
    https://scikit-learn.org/stable/auto_examples/model_selection/plot_underfitting_overfitting.html
    "Underfitting vs. Overfitting"
[mdl]:
    https://en.wikipedia.org/wiki/Minimum_description_length
    "Minimum description length"
[ridge]: https://en.wikipedia.org/wiki/Ridge_regression "Ridge regression"

### Bias-Variance Tradeoff

Often used to justify regularisation in introductory courses, [TODO]

### Double-Descent Phenomenon

The effectiveness of CNNs on image tasks is often attributed to its resemblance
with the [visual cortex][vis-cor].

![CNN Architecture (Credits: [Wikimedia](https://commons.wikimedia.org/wiki/File:Typical_cnn.png))][cnn-arch]

[vis-cor]: https://en.wikipedia.org/wiki/Visual_cortex "Visual cortex"
[cnn-arch]: /public/ml-theory/regularisation-redundancy/cnn-arch.png

## Redundancy

### Network Sparsity

### Quantisation

## Implications
