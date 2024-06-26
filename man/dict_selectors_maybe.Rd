% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SelectorMaybe.R
\name{dict_selectors_maybe}
\alias{dict_selectors_maybe}
\alias{SelectorMaybe}
\title{Selector-Combination that Selects According to Two Selectors}
\description{
\code{\link{Selector}} that wraps two other \code{\link{Selector}}s given during construction and uses both for selection proportionally.
Each of the resulting \code{n_select} individuals is chosen either from \verb{$selector}, or from \verb{$selector_not}.

This makes it possible to implement selection methods such as random interleaving, where only a fraction of \code{p}
individuals were selected by a criterion, while the others are taken randomly.
}
\section{Algorithm}{

To perform selection, \code{n_selector_in} rows of \code{values} are given to \verb{$selector}, and the remaining
\code{nrow(values) - n_selector_in} rows are given to \verb{$selector_not}. Both selectors are used to generate
a subset of selected individuals: \verb{$selector} generates \code{n_selector_out} individuals, and \verb{$selector_not} generates
\code{n_select - n_selector_out} individuals.

\code{n_selector_in} is either set to
\code{round(nrow(values) * p_in)} when \code{proportion_in} is \code{"exact"}, or to \code{rbinom(1, nrow(values), p_in)} when \code{proportion_in} is \code{"random"}.

\code{n_selector_out} is set to \code{round(n_select * p_out)} when \code{proportion_out} is \code{"exact"}, or to \code{rbinom(1, n_select, p_out)} when \code{proportion_out} is \code{"random"}.

When \code{odds_correction} is \code{TRUE}, then \code{p_out} is adjusted depending on the used \code{n_selector_in} value before being applied. Let \code{odds(p) = p/(1-p)}.
Then the effective \code{p_out} is set such that \verb{odds(effective p_out) = odds(p_out) * n_selector_in / (nrow(values) - n_selector_in) / odds(p_in)}.
This corrects for the discrepancy between the chosen \code{p_in} and the effective proportion of \code{n_selector_in / nrow(values)} caused either by rounding
errors or when \code{proportion_in} is \code{"random"}.

When \code{p_in} is exactly 1 or exactly 0, and \code{p_out} is not equal to \code{p_in}, then an error is given.

If \code{nrow(values)} is 1, then this individuum is returned and \verb{$selector} / \verb{$selector_not} are not called.

If \code{try_unique} is \code{TRUE}, then \code{n_selector_out} is set to at most \code{n_selector_in} and at least \code{n_select - nrow(values) + n_selector_in},
and an error is generated when \code{nrow(values)} is less than \code{n_select}.

If \code{try_unique} is \code{FALSE} and \code{odds_correction} is \code{TRUE} and \code{n_selector_in} is either 0 or \code{nrow(values)}, then \verb{$p_out} is set to either 0 or 1, respectively.

If \code{try_unique} is \code{FALSE} and \code{odds_correction} is \code{FALSE} and \code{n_selector_in} is either 0 or \code{nrow(values)}, and \code{n_selector_out} is not equal
to 0 or \code{n_select}, respectively, then
\code{n_selector_in} is increased / decreased by 1 to give \verb{$selector_not} / \verb{$selector} at least one individuum to choose from. While this behaviour
may seem pathological, it is to ensure continuity with sampled values of \code{n_selector_in} that are close to 0 or \code{n_select}.

If \code{n_selector_out} is \code{n_select} or 0, or if \code{n_selector_in} is \code{nrows(values) - 1} or 1,
then only \verb{$selector} / \verb{$selector_not} is executed, respectively; possibly with a subset
of \code{values} if \code{n_selector_in} differs from \code{nrow(values)} / 0.
}

\section{Configuration Parameters}{

This operator has the configuration parameters of the \code{\link{Selector}}s that it wraps: The configuration parameters of the operator given to the \code{selector} construction argument
are prefixed with \code{"maybe."}, the configuration parameters of the operator given to the \code{selector_not} construction argument are prefixed with \code{"maybe_not."}.

Additional configuration parameters:
\itemize{
\item \code{p_in} :: \code{numeric(1)} \cr
Probability per individual (when \code{random_choise} is \code{TRUE}), or fraction of individuals (when \code{random_choice} is \code{FALSE}),
that are given to \verb{$selector} instead of \verb{$selector_not}. This may be overriden when \code{try_unique} is \code{TRUE}, in which
case at least as many rows are given to \verb{$selector} and \verb{$selector_not} as they are generating output values respectively.
When this is exactly 1 or exactly 0, then \code{p_out} must be equal to \code{p_in}.
Must be set by the user.
\item \code{p_out} :: \code{numeric(1)} \cr
Probability per output value (when \code{random_choise} is \code{TRUE}), or fraction of output values (when \code{random_choice} is \code{FALSE}),
that are generated by \verb{$selector} instead of \verb{$selector_not}. When this values is not given, it defaults to \code{p_in}.
\item \code{shuffle_input} :: \code{logical(1)} \cr
Whether to distribute input values randomly to \verb{$selector} / \verb{$selector_not}. If \code{FALSE}, then the first part of \code{values}
is given to \verb{$selector}. This only randomizes \emph{which} lines of \code{values} are given to \verb{$selector} / \verb{$selector_not}, but it
does not necessarily reorder the lines of values given to each. In particular, if \code{p_out} is 0 or 1, then no shuffling takes place. Initialized to \code{TRUE}.
\item \code{proportion_in} :: \code{character(1)} \cr
When set to \code{"random"}, sample the number of individuals given to \verb{$selector} according to \code{rbinom(1, nrow(values), p_in)}.
When set to \code{"exact"}, give \verb{$selector} \code{round(nrow(values) * p_in)} individuals. Initialized to \code{"exact"}.
\item \code{proportion_out} :: \code{character(1)} \cr
When set to \code{"random"}, sample the number of individuals generated by \verb{$selector} according to \code{rbinom(1, n_select, p_out)}.
When set to \code{"exact"}, have \verb{$selector} generate \code{round(n_select * p_out)} individuals.
\item \code{odds_correction} :: \code{logical(1)}\cr
When set, the effectively used value of \code{p_out} is set to
\code{1 / (1 + ((nrow(values) - n_selector_in) * p_in * (1 - p_out)) / (n_selector_in * p_out * (1 - p_in)))}, see the \strong{Algorithm} section.
Initialized to \code{FALSE}.
\item \code{try_unique} :: \code{logical(1)}\cr
Whether to give at least as many rows of \code{values} to each of \verb{$selector} and \verb{$selector_not} as they are generating output
values. This should be set to \code{TRUE} whenever \code{SelectorMaybe} is used to select unique values, and can be set to
\code{FALSE} when selecting values multiple times is acceptable. When this is \code{TRUE}, then having \code{n_select > nrow(values)}
generates an error. Initialized to \code{TRUE}.
}
}

\section{Supported Operand Types}{


Supported \code{\link[paradox:Domain]{Domain}} classes are the set intersection of supported classes of \code{selector} and \code{selector_not}.
}

\section{Dictionary}{

This \code{\link{Filtor}} can be created with the short access form \code{\link[=ftr]{ftr()}}
(\code{\link[=ftrs]{ftrs()}} to get a list), or through the the \link[mlr3misc:Dictionary]{dictionary}
\code{\link{dict_filtors}} in the following way:

\if{html}{\out{<div class="sourceCode">}}\preformatted{# preferred:
ftr("maybe", <selector> [, <selector_not>])
ftrs("maybe", <selector> [, <selector_not>])  # takes vector IDs, returns list of Filtors

# long form:
dict_filtors$get("maybe", <selector> [, <selector_not>])
}\if{html}{\out{</div>}}
}

\seealso{
Other selectors: 
\code{\link{Selector}},
\code{\link{SelectorScalar}},
\code{\link{dict_selectors_best}},
\code{\link{dict_selectors_null}},
\code{\link{dict_selectors_proxy}},
\code{\link{dict_selectors_random}},
\code{\link{dict_selectors_sequential}},
\code{\link{dict_selectors_tournament}}

Other selector wrappers: 
\code{\link{dict_selectors_proxy}},
\code{\link{dict_selectors_sequential}}
}
\concept{selector wrappers}
\concept{selectors}
\section{Super classes}{
\code{\link[miesmuschel:MiesOperator]{miesmuschel::MiesOperator}} -> \code{\link[miesmuschel:Selector]{miesmuschel::Selector}} -> \code{SelectorMaybe}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{selector}}{(\code{\link{Selector}})\cr
\code{\link{Selector}} being wrapped. This operator gets run with probability / proportion \code{p_in} and generates
output with probability / proportion \code{p_out} (configuration parameters).}

\item{\code{selector_not}}{(\code{\link{Selector}})\cr
Alternative \code{\link{Selector}} being wrapped. This operator gets run with probability / proportion \code{1 - p_in}
and generates output with probability / proportion \code{1 - p_out} (configuration parameters).}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SelectorMaybe-new}{\code{SelectorMaybe$new()}}
\item \href{#method-SelectorMaybe-prime}{\code{SelectorMaybe$prime()}}
\item \href{#method-SelectorMaybe-clone}{\code{SelectorMaybe$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="miesmuschel" data-topic="MiesOperator" data-id="help"><a href='../../miesmuschel/html/MiesOperator.html#method-MiesOperator-help'><code>miesmuschel::MiesOperator$help()</code></a></span></li>
<li><span class="pkg-link" data-pkg="miesmuschel" data-topic="MiesOperator" data-id="operate"><a href='../../miesmuschel/html/MiesOperator.html#method-MiesOperator-operate'><code>miesmuschel::MiesOperator$operate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="miesmuschel" data-topic="MiesOperator" data-id="print"><a href='../../miesmuschel/html/MiesOperator.html#method-MiesOperator-print'><code>miesmuschel::MiesOperator$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="miesmuschel" data-topic="MiesOperator" data-id="repr"><a href='../../miesmuschel/html/MiesOperator.html#method-MiesOperator-repr'><code>miesmuschel::MiesOperator$repr()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SelectorMaybe-new"></a>}}
\if{latex}{\out{\hypertarget{method-SelectorMaybe-new}{}}}
\subsection{Method \code{new()}}{
Initialize the \code{SelectorMaybe} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SelectorMaybe$new(selector, selector_not = SelectorRandom$new())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{selector}}{(\code{\link{Selector}})\cr
\code{\link{Selector}} to wrap. This operator gets run with probability / fraction \code{p_in} (Configuration parameter).\cr
The constructed object gets a \emph{clone} of this argument.
The \verb{$selector} field will reflect this value.}

\item{\code{selector_not}}{(\code{\link{Selector}})\cr
Another \code{\link{Selector}} to wrap. This operator runs when \code{selector} is not chosen. By
default, this is \code{\link{SelectorRandom}}, i.e. selecting randomly.\cr
The constructed object gets a \emph{clone} of this argument.
The \verb{$selector_not} field will reflect this value.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SelectorMaybe-prime"></a>}}
\if{latex}{\out{\hypertarget{method-SelectorMaybe-prime}{}}}
\subsection{Method \code{prime()}}{
See \code{\link{MiesOperator}} method. Primes both this operator, as well as the wrapped operators
given to \code{selector} and \code{selector_not} during construction.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SelectorMaybe$prime(param_set)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{param_set}}{(\code{\link[paradox:ParamSet]{ParamSet}})\cr
Passed to \code{\link{MiesOperator}}\verb{$prime()}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\link{invisible} \code{self}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SelectorMaybe-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SelectorMaybe-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SelectorMaybe$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
