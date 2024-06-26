% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ScalorDomcount.R
\name{dict_scalors_domcount}
\alias{dict_scalors_domcount}
\alias{ScalorDomcount}
\title{Scalor Counting Dominating Individuals}
\description{
\code{\link{Scalor}} that returns a the number of (weakly, epsilon-) dominated or dominating individuals for each individuum.
}
\section{Configuration Parameters}{

\itemize{
\item \code{output} :: \code{character(1)}\cr
What to count: individuals that are being dominated by the point under consideration(\code{"count_dominated"}),
or individuals that do not dominate the point under consideration (\code{"count_not_dominating"}).
In both cases, a larger output means the individual is "better", in some way, according to the fitness values.
Initialized with \code{"count_not_dominating"}.
\item \code{epsilon} :: \code{numeric}\cr
Epsilon-value for non-dominance, as used by \code{\link{rank_nondominated}}. Initialized to \code{0}.
\item \code{jitter} :: \code{logical(1)}\cr
Whether to add random jitter to points, with magnitude \code{sqrt(.Machine$double.eps)} relative to fitness values.
This is used to effectively break ties.
\item \code{scale_output} :: \code{logical(1)}\cr
Whether to scale output by the total numberof individuals, giving output between \code{0} and \code{1} (inclusive) when \code{TRUE}
or integer outputs ranging from 0 and \code{nrow(fitnesses)} (inclusive) when \code{FALSE}. Initialized to \code{TRUE}.
}
}

\section{Supported Operand Types}{


Supported \code{\link[paradox:Domain]{Domain}} classes are: \code{\link[paradox:Domain]{p_lgl}} ('ParamLgl'), \code{\link[paradox:Domain]{p_int}} ('ParamInt'), \code{\link[paradox:Domain]{p_dbl}} ('ParamDbl'), \code{\link[paradox:Domain]{p_fct}} ('ParamFct')
}

\section{Dictionary}{

This \code{\link{Scalor}} can be created with the short access form \code{\link[=scl]{scl()}}
(\code{\link[=scls]{scls()}} to get a list), or through the the \link[mlr3misc:Dictionary]{dictionary}
\code{\link{dict_scalors}} in the following way:

\if{html}{\out{<div class="sourceCode">}}\preformatted{# preferred:
scl("domcount")
scls("domcount")  # takes vector IDs, returns list of Scalors

# long form:
dict_scalors$get("domcount")
}\if{html}{\out{</div>}}
}

\examples{
p = ps(x = p_dbl(-5, 5))
data = data.frame(x = rep(0, 5))

sd = scl("domcount")
sd$prime(p)

(fitnesses = matrix(c(1, 5, 2, 3, 0, 3, 1, 0, 10, 8), ncol = 2))

# to see the fitness matrix, use:
## plot(fitnesses, pch = as.character(1:5))

# note that for both 2 and 4, all points do not dominate them
# their value is therefore 1
sd$operate(data, fitnesses)

sd$param_set$values$scale_output = FALSE
sd$operate(data, fitnesses)

sd$param_set$values$output = "count_dominated"
# point 4 dominates three other points, point 2 only one other point.
sd$operate(data, fitnesses)
}
\seealso{
Other scalors: 
\code{\link{Scalor}},
\code{\link{dict_scalors_aggregate}},
\code{\link{dict_scalors_fixedprojection}},
\code{\link{dict_scalors_hypervolume}},
\code{\link{dict_scalors_nondom}},
\code{\link{dict_scalors_one}},
\code{\link{dict_scalors_proxy}},
\code{\link{dict_scalors_single}}
}
\concept{scalors}
\section{Super classes}{
\code{\link[miesmuschel:MiesOperator]{miesmuschel::MiesOperator}} -> \code{\link[miesmuschel:Scalor]{miesmuschel::Scalor}} -> \code{ScalorDomcount}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ScalorDomcount-new}{\code{ScalorDomcount$new()}}
\item \href{#method-ScalorDomcount-clone}{\code{ScalorDomcount$clone()}}
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
\if{html}{\out{<a id="method-ScalorDomcount-new"></a>}}
\if{latex}{\out{\hypertarget{method-ScalorDomcount-new}{}}}
\subsection{Method \code{new()}}{
Initialize the \code{ScalorNondom} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ScalorDomcount$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ScalorDomcount-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ScalorDomcount-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ScalorDomcount$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
