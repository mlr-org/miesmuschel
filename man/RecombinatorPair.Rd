% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Recombinator.R
\name{RecombinatorPair}
\alias{RecombinatorPair}
\title{Pair Recombinator Base Class}
\description{
Base class for recombination that covers the common case of combining two individuals, where two (typically complementary) child individuals
could be taken as the result, such as \link[=RecombinatorCrossoverUniform]{bitwise crossover} or \link[=RecombinatorSimulatedBinaryCrossover]{SBX crossover}.

This is a relatively lightweight class, it adds the \code{keep_complement} active binding and sets \verb{$n_indivs_in} and \verb{$n_indivs_out} appropriately.
}
\section{Inheriting}{

\code{RecombinatorPair} is an abstract base class and should be inherited from. Inheriting classes should implement the private
\verb{$.recombine_pair()} function. During \verb{$operate()}, the \verb{$.recombine_pair()} function is called with the same input as the \verb{$.recombine()} function
of the \code{\link{Recombinator}} class. It should return a \code{data.table} of two individuals.

Constructors of inheriting  classes should have a \code{keep_complement} argument.
}

\seealso{
Other base classes: 
\code{\link{Filtor}},
\code{\link{FiltorSurrogate}},
\code{\link{MiesOperator}},
\code{\link{Mutator}},
\code{\link{MutatorDiscrete}},
\code{\link{MutatorNumeric}},
\code{\link{OperatorCombination}},
\code{\link{Recombinator}},
\code{\link{Scalor}},
\code{\link{Selector}},
\code{\link{SelectorScalar}}

Other recombinators: 
\code{\link{OperatorCombination}},
\code{\link{Recombinator}},
\code{\link{dict_recombinators_cmpmaybe}},
\code{\link{dict_recombinators_convex}},
\code{\link{dict_recombinators_cvxpair}},
\code{\link{dict_recombinators_maybe}},
\code{\link{dict_recombinators_null}},
\code{\link{dict_recombinators_proxy}},
\code{\link{dict_recombinators_sbx}},
\code{\link{dict_recombinators_sequential}},
\code{\link{dict_recombinators_swap}},
\code{\link{dict_recombinators_xonary}},
\code{\link{dict_recombinators_xounif}}
}
\concept{base classes}
\concept{recombinators}
\section{Super classes}{
\code{\link[miesmuschel:MiesOperator]{miesmuschel::MiesOperator}} -> \code{\link[miesmuschel:Recombinator]{miesmuschel::Recombinator}} -> \code{RecombinatorPair}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{keep_complement}}{(\code{logical(1)})\cr
Whether the operation keeps both resulting individuals of the operation or discards the complement.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-RecombinatorPair-new}{\code{RecombinatorPair$new()}}
\item \href{#method-RecombinatorPair-clone}{\code{RecombinatorPair$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="miesmuschel" data-topic="MiesOperator" data-id="help"><a href='../../miesmuschel/html/MiesOperator.html#method-MiesOperator-help'><code>miesmuschel::MiesOperator$help()</code></a></span></li>
<li><span class="pkg-link" data-pkg="miesmuschel" data-topic="MiesOperator" data-id="operate"><a href='../../miesmuschel/html/MiesOperator.html#method-MiesOperator-operate'><code>miesmuschel::MiesOperator$operate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="miesmuschel" data-topic="MiesOperator" data-id="prime"><a href='../../miesmuschel/html/MiesOperator.html#method-MiesOperator-prime'><code>miesmuschel::MiesOperator$prime()</code></a></span></li>
<li><span class="pkg-link" data-pkg="miesmuschel" data-topic="MiesOperator" data-id="print"><a href='../../miesmuschel/html/MiesOperator.html#method-MiesOperator-print'><code>miesmuschel::MiesOperator$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="miesmuschel" data-topic="MiesOperator" data-id="repr"><a href='../../miesmuschel/html/MiesOperator.html#method-MiesOperator-repr'><code>miesmuschel::MiesOperator$repr()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RecombinatorPair-new"></a>}}
\if{latex}{\out{\hypertarget{method-RecombinatorPair-new}{}}}
\subsection{Method \code{new()}}{
Initialize base class components of the \code{RecombinatorPair}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RecombinatorPair$new(
  keep_complement = TRUE,
  param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
  param_set = ps(),
  packages = character(0),
  dict_entry = NULL,
  own_param_set = quote(self$param_set)
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{keep_complement}}{(\code{logical(1)})\cr
Whether the operation should keep both resulting individuals (\code{TRUE}), or only the first and discard
the complement (\code{FALSE}). Default \code{TRUE}.
The \verb{$keep_complement} field will reflect this value.}

\item{\code{param_classes}}{(\code{character})\cr
Classes of parameters that the operator can handle. May contain any of \code{"ParamLgl"}, \code{"ParamInt"}, \code{"ParamDbl"}, \code{"ParamFct"}.
Default is all of them.\cr
The \verb{$param_classes} field will reflect this value.}

\item{\code{param_set}}{(\code{\link[paradox:ParamSet]{ParamSet}} | \code{list} of \code{expression})\cr
Strategy parameters of the operator. This should be created by the subclass and given to \code{super$initialize()}.
If this is a \code{\link[paradox:ParamSet]{ParamSet}}, it is used as the \code{MiesOperator}'s \code{\link[paradox:ParamSet]{ParamSet}}
directly. Otherwise it must be a \code{list} of expressions e.g. created by \code{alist()} that evaluate to \code{\link[paradox:ParamSet]{ParamSet}}s,
possibly referencing \code{self} and \code{private}.
These \code{\link[paradox:ParamSet]{ParamSet}} are then combined using a \code{\link[paradox:ParamSetCollection]{ParamSetCollection}}.
Default is the empty \code{\link[paradox:ParamSet]{ParamSet}}.\cr
The \verb{$param_set} field will reflect this value.}

\item{\code{packages}}{(\code{character})
Packages that need to be loaded for the operator to function. This should
be declared so these packages can be loaded when operators run on parallel
instances. Default is \code{character(0)}.\cr
The \verb{$packages} field will reflect this values.}

\item{\code{dict_entry}}{(\code{character(1)} | \code{NULL})\cr
Key of the class inside the \code{\link[mlr3misc:Dictionary]{Dictionary}} (usually one of
\code{\link{dict_mutators}}, \code{\link{dict_recombinators}}, \code{\link{dict_selectors}}), where it can
be retrieved using a \link[=mut]{short access function}. May be \code{NULL} if the operator
is not entered in a dictionary.\cr
The \verb{$dict_entry} field will reflect this value.}

\item{\code{own_param_set}}{(\code{language})\cr
An expression that evaluates to a \code{\link[paradox:ParamSet]{ParamSet}} indicating the configuration parameters that are entirely owned by
this operator class (and not proxied from a construction argument object). This should be \code{quote(self$param_set)} (the default) when
the \code{param_set} argument is not a list of expressions.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RecombinatorPair-clone"></a>}}
\if{latex}{\out{\hypertarget{method-RecombinatorPair-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RecombinatorPair$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
