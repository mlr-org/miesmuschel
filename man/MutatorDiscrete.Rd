% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Mutator.R
\name{MutatorDiscrete}
\alias{MutatorDiscrete}
\title{Discrete Mutator Base Class}
\description{
Base class for mutation operations on discrete individuals, inheriting from \code{\link{Mutator}}.

\code{MutatorDiscrete} operators perform mutation on discrete (logical and factor valued) individuals. Inheriting
operators implement the private \verb{$.mutate_discrete()} function that is called once for each individual
and is given a character vector.
}
\section{Inheriting}{

\code{MutatorDiscrete} is an abstract base class and should be inherited from. Inheriting classes should implement the private \verb{$.mutate_discrete()}
function. During \verb{$operate()}, the \verb{$.mutate_discrete()} function is called once for each individual, with the parameters \code{values} (the
individual as a single \code{character} vector), and \code{levels} (a list of \code{character} containing the possible values for each element of \code{values}). Typically,
\verb{$initialize()} should also be overloaded.
}

\seealso{
Other base classes: 
\code{\link{Filtor}},
\code{\link{MiesOperator}},
\code{\link{MutatorNumeric}},
\code{\link{Mutator}},
\code{\link{OperatorCombination}},
\code{\link{Recombinator}},
\code{\link{Selector}}

Other mutators: 
\code{\link{MutatorNumeric}},
\code{\link{Mutator}},
\code{\link{OperatorCombination}},
\code{\link{dict_mutators_cmpmaybe}},
\code{\link{dict_mutators_erase}},
\code{\link{dict_mutators_gauss}},
\code{\link{dict_mutators_maybe}},
\code{\link{dict_mutators_null}},
\code{\link{dict_mutators_proxy}},
\code{\link{dict_mutators_unif}}
}
\concept{base classes}
\concept{mutators}
\section{Super classes}{
\code{\link[miesmuschel:MiesOperator]{miesmuschel::MiesOperator}} -> \code{\link[miesmuschel:Mutator]{miesmuschel::Mutator}} -> \code{MutatorDiscrete}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{MutatorDiscrete$new()}}
\item \href{#method-clone}{\code{MutatorDiscrete$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="miesmuschel" data-topic="MiesOperator" data-id="operate">}\href{../../miesmuschel/html/MiesOperator.html#method-operate}{\code{miesmuschel::MiesOperator$operate()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="miesmuschel" data-topic="MiesOperator" data-id="prime">}\href{../../miesmuschel/html/MiesOperator.html#method-prime}{\code{miesmuschel::MiesOperator$prime()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Initialize base class components of the \code{MutatorNumeric}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MutatorDiscrete$new(
  param_classes = c("ParamLgl", "ParamFct"),
  param_set = ps()
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{param_classes}}{(\code{character})\cr
Classes of parameters that the operator can handle. May contain any of \code{"ParamLgl"}, \code{"ParamFct"}.
Default is both of them.\cr
The \verb{$param_classes} field will reflect this value.}

\item{\code{param_set}}{(\code{\link[paradox:ParamSet]{ParamSet}} | \code{list} of \code{expression})\cr
Strategy parameters of the operator. This should be created by the subclass and given to \code{super$initialize()}.
If this is a \code{\link[paradox:ParamSet]{ParamSet}}, it is used as the \code{MiesOperator}'s \code{\link[paradox:ParamSet]{ParamSet}}
directly. Otherwise it must be a \code{list} of expressions e.g. created by \code{alist()} that evaluate to \code{\link[paradox:ParamSet]{ParamSet}}s,
possibly referencing \code{self} and \code{private}.
These \code{\link[paradox:ParamSet]{ParamSet}} are then combined using a \code{\link[paradox:ParamSetCollection]{ParamSetCollection}}.
Default is the empty \code{\link[paradox:ParamSet]{ParamSet}}.\cr
The \verb{$param_set} field will reflect this value.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MutatorDiscrete$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
