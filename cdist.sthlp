{smcl}
{* 11mar2023}{...}
{hi:help cdist}{...}
{right:{browse "http://github.com/benjann/cdist/"}}
{hline}

{title:Title}

{pstd}{hi:cdist} {hline 2} Counterfactual distribution estimation


{title:Syntax}

{pstd}
    Estimation

{p 8 15 2}
    {cmd:cdist} [{cmdab:est:imate}] {depvar} [{indepvars}] {ifin} {weight}{cmd:,}
    {opt by(groupvar)}
    [
    {help cdist##opt:{it:options}}
    ]

{marker explist}{...}
{pstd}
    Report linear combinations of results after estimation

{p 8 15 2}
    {cmd:cdist} {cmd:lincom} {it:explist}
    [{cmd:,} {cmd:post} {help cdist##repopts:{it:reporting_options}}
    ]

{pmore}
    where {it:explist} is

            {cmd:(}[{it:name}{cmd::}]{it:exp}{cmd:)} [{cmd:(}[{it:name}{cmd::}]{it:exp}{cmd:)} ... ]

{pmore}
    with {it:exp} as a linear combination of equation names and {it:name} as an
    optional name to be used in the output ({it:exp} will be used as name if
    {it:name} is omitted). Parentheses can be omitted if there are no
    spaces in the specification. The equation names that can be used in
    {it:exp} are those listed in the output of {cmd:cdist}, which depends on
    context. The default set of equations is:

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

            {com}. cdist lincom (Diff:fit1-fit0) (Char:adj0-fit0) (Coef:fit1-adj0){txt}

{pmore}
    to decompose the group difference ({cmd:Diff}) into a part due to differences in
    characteristics ({cmd:Char}) and a part due to differences in coefficients
    ({cmd:Coef}).

{pmore}
    Option {cmd:post} requests that the transformed results be stored in {cmd:e()}, 
    replacing the original results from {cmd:cdist estimate}. By default,
    {cmd:cdist lincom} returns the transformed results in {cmd:r()} and does
    not modify {cmd:e()}.


{synoptset 22 tabbed}{...}
{marker opt}{synopthdr:options}
{synoptline}
{syntab :Main}
{synopt :{opt by(groupvar)}}specifies two groups; {cmd:by()} is required
    {p_end}
{synopt :{opt swap}}swap the groups
    {p_end}
{synopt :{opt pool:ed}}adjust to pooled X distribution; the default is to adjust to X distribution of other group
    {p_end}
{synopt :{opt jmp}}also obtain location-shifted counterfactuals; only allowed with {cmd:method(qr)}; not allowed with
    {cmd:pooled}
    {p_end}

{syntab :Target statistics}
{synopt :{opt nq:uantiles(#)}}number of quantiles to be reported; default is {cmd:nquantiles(9)}
    {p_end}
{synopt :{opth p:ercentiles(numlist)}}calculate percentiles corresponding to the specified percentages
    {p_end}
{synopt :{opt s:tatistics(stats)}}calculate listed statistics; available statistics are
    {cmdab:m:ean}, {cmdab:v:ariance}, {cmd:sd}, {cmdab:med:ian}, {cmd:iqr}[{cmd:(# #)}], {cmdab:g:ini}, {cmd:mld}, {cmd:theil}, {cmd:cv},
    {cmd:vlog}, {cmd:sdlog}, {cmd:q(#)}, {cmd:p(#)}, {opt qr:atio(# #)}
    {p_end}
{synopt :{opth qdef(#)}}quantile definition to be used when taking quantiles; # in {0,..,11}, default is 2
    {p_end}

{syntab :Equations}
{synopt :{cmd:lincom(}{help cdist##explist:{it:explist}}{cmd:)}}report specified
    linear combinations; {it:explist} is as described above
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

{syntab :SE/VCE}
{synopt :{opt vce(vcetype)}}variance estimation method; {it:vcetype} may 
    {cmd:bootstrap} or {cmd:jackknife}; see help {it:{help vce_option}}
    {p_end}

{marker repopts}{...}
{syntab :Reporting}
{synopt :{opt level(#)}}set confidence level; default is {cmd:level(95)}
    {p_end}
{synopt :[{ul:{cmd:no}}]{opt head:er}}whether to display the table header
    {p_end}
{synopt :{opt notab:le}}suppress table of results
    {p_end}
{synopt :{it:{help display_options}}}standard display option
    {p_end}
{synopt :{opt coefl:egend}}display legend instead of statistics
    {p_end}
{synoptline}
{pstd}
    {it:indepvars} may contain factor variables; see {help fvvarlist}.
    {p_end}
{pstd}
    {cmd:pweight}s, {cmd:iweight}s and {cmd:fweight}s are allowed; see help {help weight}.


{title:Description}

{pstd}
    {cmd:cdist} estimates counterfactual distributions using methods suggested
    by Chernozhukov et al. (2013). The unconditional (counterfactual) distributions
    are either obtained my distribution regression using {helpb logit} models
    or by a linear quantile regression process using {helpb mf_mm_qr:mm_qr()} from 
    the {helpb moremata} package.

{pstd}
    For an alternative implementation of these (and additional) methods
    see package {helpb counterfactual} by Blaise Melly.


{title:Dependencies}

{pstd}
    {cmd:cdist} requires {cmd:moremata}. Type

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
        . {stata "cdist lincom (Diff:fit0-fit1) (Char:fit0-adj0) (Coef:adj0-fit1)"}

{pstd}
    JMP type decomposition using quantile regressions:

        . {stata cdist lnwage tenure ttl_exp grade, by(union) method(qr) jmp statistics(mean variance iqr(10 90))}
        . {stata "cdist lincom (Diff:fit0-fit1) (Char:fit0-adj0) (Coef:adj0-loc0) (Resid:loc0-fit1)"}


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
    {helpb logit}, {helpb qreg}, {helpb counterfactual} (if installed), {helpb oaxaca} (if installed)
