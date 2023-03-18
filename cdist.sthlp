{smcl}
{* 18mar2023}{...}
{hi:help cdist}{...}
{right:{browse "http://github.com/benjann/cdist/"}}
{hline}

{title:Title}

{pstd}{hi:cdist} {hline 2} Counterfactual distribution estimation


{title:Syntax}

{pstd}
    Estimation

{p 12 19 2}
    {cmd:cdist} [{cmdab:est:imate}] {depvar} [{indepvars}] {ifin} {weight}{cmd:,}
    {opt by(groupvar)}
    [
    {help cdist##opt:{it:options}}
    ]

{pmore}
    {it:indepvars} may contain factor variables; see {help fvvarlist}.
    {p_end}
{pmore}
    {cmd:pweight}s, {cmd:iweight}s and {cmd:fweight}s are allowed; see help {help weight}.

{pstd}
    Report decomposition after estimation

{p 12 19 2}
    {cmd:cdist decomp}
    [{cmd:,} {help cdist##decompopt:{it:decomp_options}}
    {help cdist##repopts:{it:reporting_options}} ]

{marker explist}{...}
{pstd}
    Report linear combinations of results after estimation

{p 12 19 2}
    {cmd:cdist lincom} {it:explist}
    [{cmd:,} {help cdist##repopts:{it:reporting_options}} ]

{pmore}
    where {it:explist} is

{p 12 19 2}
    {cmd:(}[{it:name}{cmd::}]{it:exp}{cmd:)} [{cmd:(}[{it:name}{cmd::}]{it:exp}{cmd:)} ... ]

{pmore}
    with {it:exp} as a linear combination of equation names and {it:name} as an
    optional name for the result to be used in the output ({it:exp} will
    be used as name if {it:name} is omitted). Parentheses can be omitted if there are no
    spaces in a specification. Unless restricted by {cmd:keep()} or
    {cmd:drop()}, the equation names that can be used in
    {it:exp} are as follows:

{p2colset 13 20 22 2}{...}
{p2col:{cmd:obs0}}observed distribution of group 0
    {p_end}
{p2col:{cmd:fit0}}fitted distribution of group 0
    {p_end}
{p2col:{cmd:adj0}}composition-adjusted distribution of group 0
    {p_end}
{p2col:{cmd:loc0}}composition-adjusted and location-shifted distribution of group 0; only available if
    {cmd:jmp} has been specified
    {p_end}
{p2col:{cmd:obs1}}observed distribution of group 1
    {p_end}
{p2col:{cmd:fit1}}fitted distribution of group 1
    {p_end}
{p2col:{cmd:adj1}}composition-adjusted distribution of group 1
    {p_end}
{p2col:{cmd:loc0}}composition-adjusted and location-shifted distribution of group 1; only available if
    {cmd:jmp} has been specified
    {p_end}

{pmore}
    For example, type

{p 12 19 2}
    {com}. cdist lincom (Delta: fit0 - fit1) (Char: fit0 - adj0) (Coef: adj0 - fit1){txt}

{pmore}
    to decompose the group difference ({cmd:Delta}) into a part due to differences in
    characteristics ({cmd:Char}) and a part due to differences in coefficients
    ({cmd:Coef}). This is equivalent to the default decomposition obtained by
    {cmd:cdist decomp}.


{synoptset 20 tabbed}{...}
{marker opt}{...}
{p2coldent :{help cdist##options:{it:options}}}Description{p_end}
{synoptline}
{syntab :Main}
{synopt :{opt by(groupvar)}}specifies two groups; {cmd:by()} is required
    {p_end}
{synopt :{opt swap}}swap the groups
    {p_end}
{synopt :{opt pool:ed}}adjust to the pooled characteristics distribution
    {p_end}
{synopt :{opt jmp}[{cmd:(}{help cdist##jmp:{it:type}}{cmd:)}]}include location-shifted
    counterfactuals
    {p_end}

{syntab :Target statistics}
{synopt :{cmdab:s:tatistics(}{help cdist##stats:stats}{cmd:)}}statistics to be calculated
    {p_end}
{synopt :{opt p:ercentile}[{cmd:(}{help cdist##percentile:{it:spec}}{cmd:)}]}calculate
    percentiles; alternative to {cmd:statistics()}
    {p_end}
{synopt :{opt q:uantile}[{cmd:(}{help cdist##quantile:{it:spec}}{cmd:)}]}calculate
    quantiles; alternative to {cmd:statistics()}
    {p_end}
{synopt :{opt pdf}[{cmd:(}{help cdist##pdf:{it:spec}}{cmd:)}]}calculate
    density function; alternative to {cmd:statistics()}
    {p_end}
{synopt :{opt cdf}[{cmd:(}{help cdist##cdf:{it:spec}}{cmd:)}]}calculate
    distribution function; alternative to {cmd:statistics()}
    {p_end}
{synopt :{opt qdef(#)}}quantile definition; # in {0,..,11}
    {p_end}

{syntab :Results}
{synopt :{cmd:decomp}[{cmd:(}{help cdist##decompopt:{it:options}}{cmd:)}]}report decomposition
    {p_end}
{synopt :{cmd:lincom(}{help cdist##explist:{it:explist}}{cmd:)}}report specified
    linear combinations; see {cmd:cdist lincom} above
    {p_end}
{synopt :{opt keep(eqlist)}}keep specified equations; may use {cmd:*} and {cmd:?} wildcard characters
    {p_end}
{synopt :{opt drop(eqlist)}}drop specified equations; may use {cmd:*} and {cmd:?} wildcard characters
    {p_end}

{syntab :Estimation method}
{synopt :{cmdab:m:ethod(}{it:{help cdist##method:method}}{cmd:)}}estimation
    method; can be {cmdab:l:ogit} (distribution regression)
    or {cmd:qr} (quantile regression process)
    {p_end}
{synopt :{opt g:size(#)}}size of evaluation grid (number of models)
    {p_end}
{synopt :{opt noint:egrate}}do not integrate predictions; only relevant if {cmd:method(qr)}
    {p_end}
{synopt :{opt int:egrate(# [#])}}details of integration; only relevant if {cmd:method(qr)}
    {p_end}
{synopt :{opt nodot:s}}suppress progress dots
    {p_end}

{syntab :SE/VCE}
{synopt :{opt vce(vcetype)}}variance estimation method; {it:vcetype} may
    {cmd:bootstrap} or {cmd:jackknife}; see help {it:{help vce_option}}
    {p_end}

{syntab :Generate}
{synopt :{cmdab:gen:erate}[{cmd:(}{it:{help cdist##generate:spec}}{cmd:)}]}store
    fitted and counterfactual distributions as variables
    {p_end}
{synopt :{cmd:replace}}allow replacing existing variables
    {p_end}

{marker repopts}{...}
{syntab :Reporting}
{synopt :{opt level(#)}}set confidence level; default is {cmd:level(95)}
    {p_end}
{synopt :[{ul:{cmd:no}}]{opt head:er}}whether to display the table header
    {p_end}
{synopt :{opt noleg:end}}suppress lincom legend
    {p_end}
{synopt :{opt notab:le}}suppress table of results
    {p_end}
{synopt :{it:{help estimation_options##display_options:display_options}}}standard display options
    {p_end}
{synoptline}

{marker decompopt}{...}
{p2coldent :{help cdist##decompoptions:{it:decomp_options}}}Description{p_end}
{synoptline}
{synopt :{opt rev:erse}}report reverse decomposition
    {p_end}
{synopt :{opt ave:rage}}report averaged decomposition
    {p_end}
{synopt :{opt three:fold}}report threefold decomposition
    {p_end}
{synoptline}


{title:Description}

{pstd}
    {cmd:cdist} estimates counterfactual distributions using methods suggested
    by Chernozhukov et al. (2013). The unconditional (counterfactual) distributions
    are either obtained by distribution regression using {helpb logit} models
    or by a linear quantile regression process (using {helpb mf_mm_qr:mm_qr()} from
    the {helpb moremata} package).

{pstd}
    For an alternative implementation of these (and related) methods
    see package {helpb counterfactual} by Blaise Melly (type
    {stata "net from https://raw.githubusercontent.com/bmelly/Stata/main/"}).


{title:Dependencies}

{pstd}
    {cmd:cdist} requires {helpb moremata}. Type

        {com}. {net "describe moremata, from(http://fmwww.bc.edu/repec/bocode/m/)":ssc describe moremata}{txt}


{marker options}{...}
{title:Options}

{dlgtab:Main}

{phang}
    {opt by(groupvar)} specifies the variable that identifies the two groups to be compared. Option {cmd:by()}
    is required.

{phang}
    {opt swap} swaps the groups identified by {it:groupvar}. By default, the lower value
    of {it:groupvar} identifies group 0 and the higher value identifies group 1. Specify
    {cmd:swap} to use the lower value for group 1 and the higher value for group 0.

{phang}
    {opt pooled} uses the distribution of characteristics in the pooled sample across
    both groups when generating counterfactual predictions. The default is to use the
    distribution of the opposite group in each case. Specify {cmd:pooled} if you
    want to standardize both groups to the same overall distribution. {opt pooled}
    is not allowed together with {cmd:jmp}.

{marker jmp}{...}
{phang}
    {opt jmp}[{cmd:(}{it:type}{cmd:)}] causes composition-adjusted and
    location-shifted counterfactual distributions to be computed in addition to
    the composition-adjusted counterfactual distributions. These additional
    distributions are needed for decompositions in the style of Juhn et
    al. (1993), where the unexplained part is subdivided into a part due to
    differences in (conditional) location and a part due to differences in
    (conditional) residual distribution. If {cmd:jmp} is has been applied,
    {cmd:cdist decomp} will report such a decomposition. Argument {it:type} can
    be {opt med:ian} to use median regression for the location model (the
    default), or {opt mean} to use least-squares regression for the location
    model. {cmd:jmp} is not allowed together with {opt pooled}.

{dlgtab:Target statistics}

{marker stats}{...}
{phang}
    {opt statistics(stats)} specifies the statistics to be reported. {it:stats} is
    a space-separated list of a selection of the following statistics:

{p2colset 13 28 30 2}{...}
{p2col:{opt m:ean}}arithmetic mean{p_end}
{p2col:{opt v:ariance}}variance{p_end}
{p2col:{opt sd}}standard deviation{p_end}
{p2col:{opt med:ian}}median{p_end}
{p2col:{opt iqr}[{opt (lo up)}]}inter quantile range, i.e. {opt p(up)}-{opt p(lo)}; default is {cmd:iqr(25 75)} (inter quartile range){p_end}
{p2col:{opt g:ini}}Gini coefficient{p_end}
{p2col:{opt mld}}mean log deviation{p_end}
{p2col:{opt theil}}Theil index{p_end}
{p2col:{opt cv}}coefficient of variation{p_end}
{p2col:{opt vlog}}variance of logarithms{p_end}
{p2col:{opt sdlog}}standard deviation of logarithms{p_end}
{p2col:{opt p(#)}}quantile (percentile) at percentage {it:#} in [0,100]{p_end}
{p2col:{opt q(#)}}quantile at proportion {it:#} in [0,1]; {cmd:q(}{it:#}{cmd:)} = {cmd:p(}{it:#}{cmd:*100)}{p_end}
{p2col:{opt qr:atio(lo up)}}quantile ratio, i.e. {opt p(up)}/{opt p(lo)}; e.g., type {cmd:qratio(10 90)} for the D9/D1 ratio{p_end}

{pmore}
    Only one of {cmd:statistics()}, {opt percentile()}, {opt quantile()}, {opt pdf()}, and {opt cdf()}
    is allowed. The default is {cmd:statistics(mean)}.

{marker percentile}{...}
{phang}
    {opt percentile}[{cmd:(#}{it:#}|{it:numlist}{cmd:)}] reports quantiles (percentiles), either
    at {it:#} equally-spaced percentages between 0 and 100, or at the percentages specified in {it:{help numlist}}. Specifying
    {cmd:percentile} without argument is equivalent to {cmd:percentile(#9)}.

{marker quantile}{...}
{phang}
    {opt quantile}[{cmd:(#}{it:#}|{it:numlist}{cmd:)}] reports quantiles, either
    at {it:#} equally-spaced proportions between 0 and 1, or at the proportions specified in {it:{help numlist}}. Specifying
    {cmd:quantile} without argument is equivalent to {cmd:quantile(#9)}.

{marker pdf}{...}
{phang}
    {opt pdf}[{cmd:(#}{it:#}|{it:numlist}[{cmd:,} {it:pdfopts}]{cmd:)}] reports density estimates, either
    at {it:#} equally-spaced outcome values across the range of {it:depvar}, or at the outcome values
    specified in {it:{help numlist}}. Specifying {cmd:pdf} without argument is equivalent to
    {cmd:pdf(#9)}. {it:pdfopts} are as follows (see {helpb mf_mm_density:mm_density()} for more information).

{phang2}
    {cmdab:bw:idth(}{it:#}|{it:method}{cmd:)} sets the bandwidth to {it:#} or, alternatively,
    specifies the type of automatic bandwidth selector to be used. {it:method} can be
    {cmdab:s:ilverman} (optimal of Silverman),
    {cmdab:o:versmoothed} (oversmoothed rule),
    {opt sj:pi} (Sheather-Jones solve-the-equation plug-in),
    {cmdab:d:pi}[{cmd:(}{it:#}{cmd:)}] (Sheather-Jones direct plug-in,
        where {it:#} specifies the number of stages of functional estimation;
        default is {cmd:2}), or
    {opt isj} (diffusion estimator bandwidth). The default is {cmd:bwidth(dpi(2))}

{phang2}
    {opt k:ernel(kernel)} specifies the kernel function. {it:kernel} may
    be {opt e:panechnikov}, {opt epan2} (alternative Epanechnikov kernel
    function), {opt b:iweight}, {opt triw:eight}, {opt c:osine},
    {opt g:aussian}, {opt p:arzen}, {opt r:ectangle} or {opt t:riangle}. The default
    is {cmd:kernel(gaussian)}.

{phang2}
    {opt adapt:ive(#)} specifies the number of iterations used by the adaptive
    kernel density estimator. The default is {cmd:adaptive(0)} (non-adaptive
    density estimator).

{phang2}
    {opt ll(#)} specifies the lower boundary of the support of data and causes
    boundary-correction to be applied to the density estimate. Error will be
    returned if the data contains values smaller than {it:#}.

{phang2}
    {opt ul(#)} specifies the upper boundary of the support of data and causes
    boundary-correction to be applied to the density estimate. Error will be
    returned if the data contains values larger than {it:#}.

{phang2}
    {opt bo:undary(method)} sets the type of boundary correction. Choices are
    {opt ren:orm} (renormalization method; the default), {opt refl:ect} (reflection method), or
    {opt lc} (linear combination technique). This is only relevant if {cmd:ll()} or {cmd:ul()}
    has been specified.

{marker cdf}{...}
{phang}
    {opt cdf}[{cmd:(#}{it:#}|{it:numlist}{cmd:)}] reports the cumulative distribution function, either
    at {it:#} equally-spaced outcome values across the range of {it:depvar}, or at the outcome values
    specified in {it:{help numlist}}. Specifying {cmd:cdf} without argument is equivalent to
    {cmd:cdf(#9)}.

{phang}
    {opt qdef(#)} sets the quantile definition to be used when computing
    quantiles, with {it:#} in {c -(}0,...,11{c )-}. The default is
    {cmd:qdef(2)} (same as, e.g. {helpb summarize}). See
    {helpb mf_mm_quantile:mm_quantile()} for information on the
    different definitions.

{dlgtab:Results}

{phang}
    {cmd:decomp}[{cmd:(}{help cdist##decompoptions:{it:decomp_options}}{cmd:)}]
    reports the decomposition of the gap between the two groups, rather than
    observed and counterfactual values for each group. {help cdist##decompoptions:{it:decomp_options}} select
    the type of decomposition; see below.

{phang}
    {cmd:lincom(}{help cdist##explist:{it:explist}}{cmd:)} reports specified
    linear combinations of results; see {helpb cdist##explist:cdist lincom} above

{phang}
    {opt keep(eqlist)} keeps the specified equations. Use this option to
    save computer time if you are only interested in a selection of the results that are
    returned by {cmd:cdist} by default. {it:eqlist} is a space-separated
    list of equation names, possibly containing {cmd:*} and {cmd:?} wildcard characters. See
    {helpb cdist##explist:cdist lincom} above for the list of equation names used by {cmd:cdist}.

{phang}
    {opt drop(eqlist)} drops the specified equations. See {cmd:keep()} above.

{dlgtab:Estimation method}

{marker method}{...}
{phang}
    {opt method(method)} selects the method by which the counterfactual
    distributions are estimated. {it:method} can be {cmdab:l:ogit}
    to use distribution regression based on logistic regression,
    or {cmd:qr} to obtain counterfactual distributions based on
    series of quantile regressions.

{phang}
    {opt gsize(#)} sets the size of the evaluation grid for distribution regression
    or the quantile regression process (number of models estimated in each group). The default
    is {cmd:gsize(100)}. In case of {cmd:method(logit)}, the realized evaluation grid
    may be smaller than {cmd:gsize()} depending on the data.

{phang}
    {opt nointegrate} requests that the target statistics are computed from the
    raw predictions rather than the distribution function of the
    predictions. This is only relevant if {cmd:method(qr)} has been specified.

{phang}
    {opt integrate(# [#])} set the size (number of points) and padding percentage of the approximation grid
    used to compute the distribution function of predictions. The padding percentage
    determines how much the approximation grid extends beyond the observed minimum and maximum
    of {it:depvar}. The default is {cmd:integrate(1000 5)} (1000 evaluation points;
    5 percent padding on each side). This is
    only relevant if {cmd:method(qr)} has been specified.

{phang}
    {opt nodots} suppresses the progress dots in the results window.

{dlgtab:SE/VCE}

{phang}
    {opt vce(vcetype)} selects the variance estimation method. {it:vcetype} may
    {cmd:bootstrap} or {cmd:jackknife}; see help {it:{help vce_option}}. By default
    variance estimation is omitted.

{dlgtab:Generate}

{marker generate}{...}
{phang}
    {cmdab:generate}[{cmd:(}{it:spec}{cmd:)}] stores the fitted and counterfactual
    distributions as variables. Two variables are generated per distribution: the
    outcome values and the weights. {it:spec} is either an explicit list of variable
    names or {it:stub}{cmd:*} to generate automatic names with a common prefix. Specifying
    {cmd:generate} without argument is equivalent to
    {cmd:generate(_dstat_*)}.

{phang}
    {cmd:replace} allows replacing existing variables.

{dlgtab:Reporting}

{phang}
    {opt level(#)} specifies the confidence level, as a percentage, for
    confidence intervals. The default is {cmd:level(95)} or as set by {helpb
    set level}. This is only relevant if option {cmd:vce()} has been specified.

{phang}
    [{cmd:no}]{opt header} decides whether the table header is displayed or not.

{phang}
    {opt nolegend} suppresses legend providing information on the reported
    linear combinations. This is only relevant if {cmd:decomp()} or {cmd:lincom}
    has been specified or when displaying results from {cmd:cdist decomp} or
    {cmd:cdist lincom}.

{phang}
    {opt notable} suppresses the output table containing the estimated results.

{phang}
    {it:display_options} are standard reporting options such as
    {cmd:cformat()} or {cmd:coeflegend}; see
    {helpb estimation_options##display_options:[R] Estimation options}.

{marker decompoptions}{...}
{it:{dlgtab:decomp_options}}

{phang}
    {opt reverse} reports the reverse decomposition. This is not allowed if
    option {cmd:pooled} has been applied.

{phang}
    {opt average} reports the averaged decomposition. This is not allowed if
    option {cmd:pooled} has been applied. Only one of {cmd:average} and
    {cmd:threefold} is allowed.

{phang}
    {opt threefold} report the threefold decomposition. This is not allowed if option {cmd:jmp} or
    option {cmd:pooled} has been applied. Only one of {cmd:average} and
    {cmd:threefold} is allowed.


{title:Examples}

{pstd}
    Twofold decomposition using distribution regression:

        . {stata sysuse nlsw88, clear}
        . {stata generate lnwage = ln(wage)}
        . {stata cdist lnwage tenure ttl_exp grade, by(union) statistics(mean variance iqr(10 90))}
        . {stata cdist decomp}
        . {stata cdist decomp, reverse}
        . {stata cdist decomp, average}

{pstd}
    JMP type decomposition using quantile regressions:

        . {stata cdist lnwage tenure ttl_exp grade, by(union) method(qr) jmp statistics(mean variance iqr(10 90))}
        . {stata cdist decomp}
        . {stata cdist decomp, reverse}
        . {stata cdist decomp, average}


{title:Returned results}

{pstd}
    {cmd:cdist} stores the following results in {cmd:e()}.

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Scalars}{p_end}
{synopt:{cmd:e(N)}}number of observations{p_end}
{synopt:{cmd:e(N0)}}number of observations in group 0{p_end}
{synopt:{cmd:e(N1)}}number of observations in group 1{p_end}
{synopt:{cmd:e(by0)}}value identifying group 0{p_end}
{synopt:{cmd:e(by1)}}value identifying group 1{p_end}
{synopt:{cmd:e(gsize)}}target grid size (number of regressions){p_end}
{synopt:{cmd:e(gsize0)}}realized grid size in group 0{p_end}
{synopt:{cmd:e(gsize1)}}realized grid size in group 1{p_end}
{synopt:{cmd:e(qdef)}}quantile definition{p_end}
{synopt:{cmd:e(pdf_bwidth)}}bandwidth used for density estimation (if relevant){p_end}
{synopt:{cmd:e(pdf_adaptive)}}number of iterations of adaptive density estimator (if relevant){p_end}
{synopt:{cmd:e(pdf_ll)}}lower boundary of support for density estimation (if relevant){p_end}
{synopt:{cmd:e(pdf_ul)}}upper boundary of support for density estimation (if relevant){p_end}
{synopt:{cmd:e(integrate)}}number of integration points (if relevant){p_end}
{synopt:{cmd:e(integ_pad)}}integration padding percentage (if relevant){p_end}
{synopt:{cmd:e(k)}}number of coefficients per equation in {cmd:e(b)}{p_end}
{synopt:{cmd:e(k_eq)}}number of equations in {cmd:e(b)}{p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Macros}{p_end}
{synopt:{cmd:e(cmd)}}{cmd:cdist}{p_end}
{synopt:{cmd:e(cmdline)}}command as typed{p_end}
{synopt:{cmd:e(depvar)}}name of outcome variable{p_end}
{synopt:{cmd:e(indepvars)}}names of covariates{p_end}
{synopt:{cmd:e(by)}}name of group variable{p_end}
{synopt:{cmd:e(pooled)}}{cmd:pooled} or empty{p_end}
{synopt:{cmd:e(jmp)}}{cmd:median}, {cmd:mean}, or empty{p_end}
{synopt:{cmd:e(statistics)}}specification from {cmd:statistics()}{p_end}
{synopt:{cmd:e(coeftype)}}{cmd:quantile}, {cmd:percentile}, {cmd:pdf}, {cmd:cdf}, or empty{p_end}
{synopt:{cmd:e(pdf_kernel)}}kernel used for density estimation (if relevant){p_end}
{synopt:{cmd:e(pdf_bwmethod)}}bandwidth selector used for density estimation (if relevant){p_end}
{synopt:{cmd:e(pdf_boundary)}}boundary correction method used for density estimation (if relevant){p_end}
{synopt:{cmd:e(eqnames)}}names of equations in {cmd:e(b)}{p_end}
{synopt:{cmd:e(generate)}}names of generated variables{p_end}
{synopt:{cmd:e(wtype)}}weight type{p_end}
{synopt:{cmd:e(wexp)}}weight expression{p_end}
{synopt:{cmd:e(title)}}title in estimation output{p_end}
{synopt:{cmd:e(properties)}}{cmd:b} or {cmd:b V}{p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Matrices}{p_end}
{synopt:{cmd:e(b)}}estimates{p_end}
{synopt:{cmd:e(V)}}variance matrix of estimates (if VCE is applied){p_end}
{synopt:{cmd:e(at)}}evaluation points of estimates (if relevant){p_end}
{synopt:{cmd:e(B0)}}coefficients individual regressions in group 0 (if estimated){p_end}
{synopt:{cmd:e(AT0)}}evaluation points of regressions in {cmd:e(B0)}{p_end}
{synopt:{cmd:e(B1)}}coefficients individual regressions in group 1 (if estimated){p_end}
{synopt:{cmd:e(AT1)}}evaluation points of regressions in {cmd:e(B1)}{p_end}
{synopt:{cmd:e(B0loc)}}coefficients of location regression in group 0 (if estimated){p_end}
{synopt:{cmd:e(B1loc)}}coefficients of location regression in group 1 (if estimated){p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Functions}{p_end}
{synopt:{cmd:e(sample)}}estimation sample{p_end}
{p2colreset}{...}

{pstd}
    If option {cmd:decomp()} or option {cmd:lincom()} is specified or, equivalently,
    if post-estimation command {cmd:cstat decomp} or {cmd:cstat lincom} is applied,
    the following results are added or updated.

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Scalars}{p_end}
{synopt:{cmd:e(k_eq)}}number of equations in {cmd:e(b)}{p_end}
{synopt:{cmd:e(k_eq_0)}}number of equations in {cmd:e(b_0)}{p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Macros}{p_end}
{synopt:{cmd:e(lincom)}}the transformation specification{p_end}
{synopt:{cmd:e(eqnames)}}names of equations in {cmd:e(b)}{p_end}
{synopt:{cmd:e(eqnames_0)}}names of equations in {cmd:e(b_0)}{p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Matrices}{p_end}
{synopt:{cmd:e(b)}}transformed estimates{p_end}
{synopt:{cmd:e(b_0)}}untransformed estimates{p_end}
{synopt:{cmd:e(V)}}variance matrix of transformed estimates (if VCE applied){p_end}
{synopt:{cmd:e(V_0)}}variance matrix of untransformed estimates (if VCE applied){p_end}

{pstd}
    If {cmd:vce(bootstrap)} or {cmd:vce(jackknife)} is specified, additional
    results are stored in {cmd:e()}; see {helpb bootstrap} and
    {helpb jackknife}, respectively.


{title:References}

{phang}
    Chernozhukov, Victor, Iv{c a'}n Fern{c a'}ndez-Val, Blaise Melly (2013). Inference on
    Counterfactual Distributions. Econometrica 81(6):2205–2268.
    {p_end}
{phang}
    Juhn, Chinhui, Kevin M. Murphy, Brooks Pierce (1993). Wage Inequality and the Rise in Returns to Skill. Journal of
    Political Economy 101(3): 410-442.
    {p_end}


{title:Author}

{pstd}
    Ben Jann, University of Bern, ben.jann@unibe.ch

{pstd}
    Thanks for citing this software as follows:

{pmore}
    Jann, B. (2023). cdist: Stata module for counterfactual distribution estimation. Available from
    {browse "http://github.com/benjann/cdist/"}.


{title:Also see}

{psee}
    Online:  help for
    {helpb logit}, {helpb qreg}, {helpb counterfactual} (if installed), {helpb oaxaca} (if installed), {helpb moremata} (if installed)
