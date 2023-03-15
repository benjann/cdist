{smcl}
{* 15mar2023}{...}
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
    optional name to be used in the output ({it:exp} will be used as name if
    {it:name} is omitted). Parentheses can be omitted if there are no
    spaces in the specification. Unless restricted by {cmd:keep()} or
    {cmd:drop()}, the equation names that can be used in
    {it:exp} are as follows:

{p2colset 13 20 22 2}{...}
{p2col:{cmd:obs0}}observed distribution of group 0
    {p_end}
{p2col:{cmd:fit0}}fitted distribution of group 0
    {p_end}
{p2col:{cmd:adj0}}composition-adjusted distribution of group 0
    {p_end}
{p2col:{cmd:loc0}}composition-adjusted and location-shifted distribution of group 0; only if
    {cmd:jmp} has been specified
    {p_end}
{p2col:{cmd:obs1}}observed distribution of group 1
    {p_end}
{p2col:{cmd:fit1}}fitted distribution of group 1
    {p_end}
{p2col:{cmd:adj1}}composition-adjusted distribution of group 1
    {p_end}
{p2col:{cmd:loc0}}composition-adjusted and location-shifted distribution of group 1; only if
    {cmd:jmp} has been specified
    {p_end}

{pmore}
    For example, type

            {com}. cdist lincom (Delta:fit0-fit1) (Char:fit0-adj0) (Coef:adj0-fit1){txt}

{pmore}
    to decompose the group difference ({cmd:Delta}) into a part due to differences in
    characteristics ({cmd:Char}) and a part due to differences in coefficients
    ({cmd:Coef}). This is equivalent to the default decomposition obtained by
    {cmd:cdist decomp}.


{synoptset 22 tabbed}{...}
{marker opt}{synopthdr:options}
{synoptline}
{syntab :Main}
{synopt :{opt by(groupvar)}}specifies two groups; {cmd:by()} is required
    {p_end}
{synopt :{opt swap}}swap the groups
    {p_end}
{synopt :{opt pool:ed}}adjust to pooled characteristics distribution; the default is to adjust to characteristics distribution of the opposite group
    {p_end}
{synopt :{opt jmp}[{cmd:(}{it:type}{cmd:)}]}also obtain location-shifted counterfactuals {cmd:loc0} and {cmd:loc1}; only allowed with {cmd:method(qr)};
    not allowed with {cmd:pooled}; {it:type} may be {opt med:ian} (default) or {opt mean}
    {p_end}

{syntab :Target statistics}
{synopt :{opt s:tatistics(stats)}}space separated list of statistics to be calculated; available statistics are
    {cmdab:m:ean}, {cmdab:v:ariance}, {cmd:sd}, {cmdab:med:ian}, {cmd:iqr}[{opt (# #)}], {cmdab:g:ini}, {cmd:mld}, {cmd:theil}, {cmd:cv},
    {cmd:vlog}, {cmd:sdlog}, {opt q(#)}, {opt p(#)}, {opt qr:atio(# #)}; default is {cmd:statistics(mean)}
    {p_end}
{synopt :{opt p:ercentile}[{cmd:(#}{it:#}{cmd:)}]}calculate {it:#} equally spaced percentiles; default is {cmd:#9}
    {p_end}
{synopt :{opth p:ercentile(numlist)}}calculate percentiles corresponding to the specified percentages
    {p_end}
{synopt :{opt pdf}[{cmd:(#}{it:#}{cmd:)}]}calculate density at {it:#} equally spaced outcome values; default is {cmd:#9}
    {p_end}
{synopt :{opth pdf(numlist)}}calculate density at the specified outcome values
    {p_end}
{synopt :{opt cdf}[{cmd:(#}{it:#}{cmd:)}]}calculate distribution function at {it:#} equally spaced outcome values; default is {cmd:#9}
    {p_end}
{synopt :{opth cdf(numlist)}}calculate distribution function at the specified outcome values
    {p_end}
{synopt :{opth qdef(#)}}quantile definition to be used when computing percentiles; # in {0,..,11}, default is {cmd:2}
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
{synopt :{opt m:ethod(method)}}estimation method; can be {cmdab:l:ogit} (distribution regression based on
    logit models; the default) or {cmd:qr} (quantile regression process)
    {p_end}
{synopt :{opt g:size(#)}}size of evaluation grid (number of models); default
    is {cmd:gsize(100)}; with method {cmd:logit}, the used evaluation grid
    may be smaller than {cmd:gsize()} depending on the data
    {p_end}
{synopt :{opt noint:egrate}}compute target statistics from the raw predictions
    of the quantile regressions rather than from the integrated distribution of
    predictions; this is only relevant with {cmd:method(qr)}
    {p_end}
{synopt :{opt int:egrate(#)}}number of evaluation points for the integration of
    the quantile regression predictions; default is {cmd:integrate(1000)};
    this is only relevant with {cmd:method(qr)}
    {p_end}
{synopt :{opt nodot:s}}suppress progress dots
    {p_end}

{syntab :SE/VCE}
{synopt :{opt vce(vcetype)}}variance estimation method; {it:vcetype} may
    {cmd:bootstrap} or {cmd:jackknife}; see help {it:{help vce_option}}
    {p_end}

{syntab :Generate}
{synopt :{cmdab:gen:erate}[{cmd:(}{it:spec}{cmd:)}]}store fitted and counterfactual distributions
    as variables (two variables are stored per distribution: outcome values and weights); {it:spec}
    can be an explicit list of variable names, or {it:stub}{cmd:*} to specify a common prefix for the
    variable names (default is {cmd:_dstat_*})
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
{synopt :{it:{help display_options}}}standard display option
    {p_end}
{synopt :{opt coefl:egend}}display legend instead of statistics
    {p_end}
{synoptline}

{synoptset 22 tabbed}{...}
{marker decompopt}{synopthdr:decomp_options}
{synoptline}
{synopt :{opt rev:erse}}report reversed decomposition; not allowed if
    {cmd:pooled} has been applied
    {p_end}
{synopt :{opt ave:rage}}report averaged decomposition; not allowed if
    {cmd:pooled} has been applied; not allowed together with {cmd:threefold}
    {p_end}
{synopt :{opt three:fold}}report threefold decomposition; not allowed if {cmd:jmp} or
    {cmd:pooled} have been applied; not allowed together with {cmd:average}
    {p_end}
{synoptline}


{title:Description}

{pstd}
    {cmd:cdist} estimates counterfactual distributions using methods suggested
    by Chernozhukov et al. (2013). The unconditional (counterfactual) distributions
    are either obtained my distribution regression using {helpb logit} models
    or by a linear quantile regression process (using {helpb mf_mm_qr:mm_qr()} from
    the {helpb moremata} package).

{pstd}
    For an alternative implementation of these (and related) methods
    see package {helpb counterfactual} by Blaise Melly.


{title:Dependencies}

{pstd}
    {cmd:cdist} requires {helpb moremata}. Type

        {com}. {net "describe moremata, from(http://fmwww.bc.edu/repec/bocode/m/)":ssc describe moremata}{txt}


{title:Options}

{dlgtab:Main}

{phang}
    ?


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

{pstd} Scalars:

{p2colset 5 20 20 2}{...}
{p2col : {cmd:e(?)}} ?
    {p_end}

{pstd} Macros:

{p2col : {cmd:e(?)}} ?
    {p_end}

{pstd} Matrices:

{p2col : {cmd:e(?)}} ?
    {p_end}


{title:References}

{phang}
    Chernozhukov, Victor, Iv{c a'}n Fern{c a'}ndez-Val, Blaise Melly (2013). Inference on
    Counterfactual Distributions. Econometrica 81(6):2205–2268.


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
