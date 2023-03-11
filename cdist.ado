*! version 0.0.2  11mar2023  Ben Jann

capt mata: assert(mm_version()>=201)
if _rc {
    di as error "{bf:moremata} version 2.0.1 or newer is required; " _c
    di as error "type {stata ssc install moremata, replace}"
    error 499
}

program cdist, eclass
    version 14
    if replay() {
        Display `0'
        exit
    }
    gettoken subcmd 00 : 0, parse(" ,")
    if `"`subcmd'"'=="lincom"  {
        Lincom `00'
        exit
    }
    local version : di "version " string(_caller()) ":"
    `version' _vce_parserun cdist, wtypes(pw iw) bootopts(force) jkopts(force): `0'
    if "`s(exit)'" == "" {
        if `"`subcmd'"'!=substr("estimate",1,max(3,strlen(`"`subcmd'"'))) {
            local 00 `0'
        }
        Estimate `00' // returns diopts, results
        if `"`lincom'"'!="" {
            _Lincom `lincom', post
        }
        Display, `diopts'
    }
    ereturn local cmdline `"cdist `0'"'
end

program Display, rclass
    if `"`e(cmd)'"'!="cdist" {
        di as err "last cdist results not found"
        exit 301
    }
    syntax [, noHEADer noTABle vsquish * ]
    local options vsquish `options'
    if "`header'"=="" {
        local hflex 1
        if      c(stata_version)<17            local hflex 0
        else if d(`c(born_date)')<d(13jul2021) local hflex 0
        local w1 17
        local c1 49
        local c2 = `c1' + `w1' + 1
        local w2 10
        local c3 = `c2' + 2
        if `hflex' local headopts head2left(`w1') head2right(`w2')
        else       local headopts
        _coef_table_header, `headopts'
        if `hflex' { // if _coef_table_header used more space than allocated
            local offset1 = max(0, `s(head2_left)' - `w1')
            local offset2 = max(0, `s(head2_right)' - `w2')
            local c1 = `c1' - `offset1' - `offset2'
            local c2 = `c2' - `offset2'
        }
        if `"`e(pooled)'"'!="" local pooled "yes"
        else                   local pooled "no"
        di as txt _col(`c1') "Pooled" _col(`c2') "=" _col(`c3')/*
            */ as res %`w2's "`pooled'"
        di as txt "Group 0: `e(by)' = " as res e(by0) /*
            */ as txt _col(`c1') "N of obs 0" _col(`c2') "=" _col(`c3')/*
            */ as res %`w2'.0gc e(N0)
        di as txt "Group 0: `e(by)' = " as res e(by1) /*
            */ as txt _col(`c1') "N of obs 1" _col(`c2') "=" _col(`c3')/*
            */ as res %`w2'.0gc e(N1)
        di as txt _col(`c1') "Estimation method" _col(`c2') "=" _col(`c3')/*
            */ as res %`w2's e(method)
        if e(gsize0)==e(gsize1) {
            local gsize = e(gsize0)
            if `gsize'<. local gsize %`w2'.0g `gsize'
            else         local gsize %`w2's "(none)"
            di as txt _col(`c1') "Grid size" _col(`c2') "=" _col(`c3') /*
                */ as res `gsize'
        }
        else {
            forv i=0/1 {
                local gsize = e(gsize`i')
                if `gsize'<. local gsize %`w2'.0g `gsize'
                else         local gsize %`w2's "(none)"
                di as txt _col(`c1') "Grid size `i'" _col(`c2') "=" /*
                    */ _col(`c3') as res `gsize'
            }
        }
        di ""
    }
    if "`table'"=="" {
        eret display, `options'
        return add
        local xvars `"`e(indepvars)'"'
        if `"`xvars'"'!="" {
            if (strlen(`"`xvars'"')>66) {
                local xvars = substr(`"`xvars'"',1,63) + "..."
            }
        }
        else local xvars "(none)"
        di as txt "covariates: " as res `"`xvars'"'
    }
end

program Estimate, eclass
    // syntax
    syntax varlist(fv) [if] [in] [pw iw fw], by(varname) [ swap POOled /*
        */ NQuantiles(numlist max=1 int >0) Percentiles(numlist >=0 <=100) /*
        */ Statistics(str) qdef(numlist max=1 int >=0 <=11) /*
        */ lincom(str) keep(str) drop(str) /*
        */ Method(str) Gsize(int 100) jmp /*
        */ noHEADer noTABle * ]
    if "`qdef'"=="" local qdef 2
    _get_diopts diopts, `options'
    c_local diopts `header' `table' `diopts'
    if `"`statistics'"'!="" {
        if "`percentiles'"!="" {
            di as err "only one of statistics() and percentiles() allowed"
            exit 198
        }
        if "`nquantiles'"!="" {
            di as err "only one of statistics() and nquantiles() allowed"
            exit 198
        }
        _parse_statistics `statistics'
    }
    else if "`percentiles'"!="" {
        if "`nquantiles'"!="" {
            di as err "only one of percentiles() and nquantiles() allowed"
            exit 198
        }
    }
    else if "`nquantiles'"=="" local nquantiles 9
    _parse_method, `method'
    if "`jmp'"!="" {
        if !inlist("`method'","qr","qr0") {
            di as error "jmp requires method(qr)"
            exit 198
        }
        if "`pooled'"!="" {
            di as error "jmp and pooled not both allowed"
            exit 198
        }
    }
    
    // lincom()/keep()/drop()
    local eqs "obs0 fit0 adj0"
    if "`jmp'"!="" local eqs "`eqs' loc0"
    local eqs = "`eqs' " + subinstr("`eqs'","0","1",.)
    if `"`keep'"'!="" {
        if `"`drop'"'!="" {
            di as err "keep() and drop() not both allowed"
            exit 108
        }
        if `"`lincom'"'!="" {
            di as err "keep() and lincom() not both allowed"
            exit 108
        }
        mata: _parse_keep()
        if "`eqs'"=="" {
            di as err "invalid keep(): no equations selected"
            exit 198
        }
    }
    else if `"`drop'"'!="" {
        if `"`lincom'"'!="" {
            di as err "drop() and lincom() not both allowed"
            exit 108
        }
        mata: _parse_drop()
        if "`eqs'"=="" {
            di as err "invalid drop(): all equations dropped"
            exit 198
        }
    }
    else if `"`lincom'"'!="" {
        _parse_keep_lincom "`eqs'" `"`lincom'"' // returns eqs
        c_local lincom `"`lincom'"'
    }
    
    // sample
    marksample touse
    markout `touse' `by'
    local wgt [`weight'`exp']
    _nobs `touse' `wgt'
    local N = r(N)
    qui levelsof `by' if `touse'
    if r(r)!=2 {
        di as err "by() must identify two groups"
        exit 498
    }
    if "`swap'"!="" {
        local by0: word 2 of `r(levels)'
        local by1: word 1 of `r(levels)'
    }
    else {
        local by0: word 1 of `r(levels)'
        local by1: word 2 of `r(levels)'
    }
    forv i = 0/1 {
        tempname touse`i'
        qui gen byte `touse`i'' = `by'==`by`i'' & `touse'
        _nobs `touse`i'' `wgt'
        local N`i' = r(N)
    }
    
    // weights: generate variable
    if "`weight'"!="" {
        local wvar = substr(`"`exp'"', 3, .) // strip "= "
        capt confirm var `wvar'
        if _rc {
            tempname wvar
            qui gen double `wvar' `exp'
        }
    }
    
    // estimate
    tempname b
    gettoken depvar xvars : varlist
    _fv_check_depvar `depvar'
    local xvars `xvars'
    fvrevar `xvars' if `touse' // create tempvars for factor variable terms
    local XVARS `r(varlist)'
    mata: cdist()
    
    // results
    eret post `b' `wgt', depname(`depvar') obs(`N') esample(`touse')
    eret local title         "Counterfactual distribution estimation"
    eret local cmd           "cdist"
    eret local method        "`method'"
    eret local indepvars     "`xvars'"
    eret local by            "`by'"
    eret local pooled        "`pooled'"
    eret local jmp           "`jmp'"
    eret local qdef          "`qdef'"
    eret local statistics    "`statistics'"
    eret local percentiles   "`percentiles'"
    eret local eqnames       "`eqs'"
    eret scalar N0           = `N0'
    eret scalar N1           = `N1'
    eret scalar by0          = `by0'
    eret scalar by1          = `by1'
    eret scalar gsize        = `gsize'
    eret scalar gsize0       = `gsize0'
    eret scalar gsize1       = `gsize1'
    eret scalar k            = `k'
    eret scalar k_eq         = `: list sizeof eqs'
end

program _parse_method
    cap n syntax [, qr qr0 Logit ]
    if _rc {
        di as err "error in {bf:method()}"
        exit 198
    }
    
    local method `qr' `qr0' `logit'
    if `:list sizeof method'>1 {
        di as err "only one method allowed"
        exit 198
    }
    if "`method'"=="" local method logit
    c_local method `method'
end

program _parse_statistics
    local S
    while (`"`0'"'!="") {
        gettoken s 0 : 0, bind
        __parse_statistics, `s'
        local S `S' `s'
    }
    c_local statistics `S'
end

program __parse_statistics
    local stats Mean Variance sd MEDian iqr Gini mld theil cv vlog sdlog
    cap n syntax [, `stats' /*
        */ IQR2(numlist min=2 max=2 >=0 <=100) /*
        */ QRatio(numlist min=2 max=2 >=0 <=100) /*
        */ Q(numlist max=1 >=0 <=100) /*
        */ P(numlist max=1 >=0 <=100) /*
        */ ]
    if _rc {
        di as err "error in {bf:statistics()}"
        exit 198
    }
    local s
    else {
        local stats = strlower("`stats'")
        foreach stat of local stats {
            local s `s' ``stat''
        }
    }
    if "`iqr2'"!="" {
        local iqr2: subinstr local iqr2 " " ","
        local s `s' iqr(`iqr2')
    }
    if "`qratio'"!="" {
        local qratio: subinstr local qratio " " ","
        local s `s' qratio(`qratio')
    }
    if "`q'"!="" {
        local s `s' q(`q')
    }
    if "`p'"!="" {
        local s `s' p(`p')
    }
    c_local s `s'
end

program _parse_keep_lincom
    args eqs lincom
    // create e(b) and e(V) using eqs as colnames
    tempname ecurrent
    _estimates hold `ecurrent', restore nullok
    _Lincom_make_bV `eqs'
    // apply test and collect matched names
    tempname R
    _Lincom_get_R `R' `"`lincom'"'
    mata: st_local("eqs", invtokens( ///
        select(tokens(st_local("eqs")), colsum(st_matrix("`R'"):!=0))))
    c_local eqs "`eqs'"
end

program Lincom, rclass
    if `"`e(cmd)'"'!="cdist" {
        di as err "last cdist results not found"
        exit 301
    }
    _parse comma lincom 0 : 0
    syntax [, post HEADer noTABle * ]
    if "`header'"=="" local header noheader
    _get_diopts diopts, `options'
    local diopts `header' `table' `diopts'
    _Lincom `lincom', `post'
    if "`post'"!="" {
        Display, `diopts'
        return add
        exit
    }
    tempname b
    mat `b' = r(b)
    capt confirm matrix r(V)
    if _rc==0 {
        tempname V
        mat `V' = r(V)
    }
    tempname ecurrent rcurrent
    _return hold `rcurrent'
    _estimates hold `ecurrent', restore copy
    _Lincom_e `b' `V'
    Display, `diopts'
    capt confirm matrix r(table)
    if _rc==0 {
        tempname T
        matrix `T' = r(table)
    }
    _return restore `rcurrent'
    return add
    if "`T'"!="" {
        return matrix table = `T'
    }
end

program _Lincom
    _parse comma lincom 0 : 0
    syntax [, post ]
    // determine transformation matrix
    local eqs `"`e(eqnames)'"'
    tempname ecurrent
    _estimates hold `ecurrent', restore
    _Lincom_make_bV `eqs'
    tempname R
    _Lincom_get_R `R' `"`lincom'"'
    _estimates unhold `ecurrent'
    // apply transformations
    tempname b
    capt confirm matrix e(V)
    if _rc==0 tempname V
    mata: _Lincom_apply()
    // return results
    if "`post'"!="" {
        _Lincom_e `b' `V'
    }
    else {
        _Lincom_r `b' `V'
    }
end

program _Lincom_e, eclass
    args b V
    local eqs: coleq `b', quoted
    local eqs: list uniq eqs
    local eqs: list clean eqs
    if "`V'"!="" local V "V = `V'"
    eret repost b = `b' `V', resize
    eret local eqnames `"`eqs'"'
    eret scalar k_eq = `: list sizeof eqs'
end

program _Lincom_r, rclass
    args b V
    return matrix b = `b'
    if "`V'"!="" {
        return matrix V = `V'
    }
end

program _Lincom_make_bV
    tempname b V
    local k: list sizeof 0
    mat `b' = J(1,`k',0)
    mat coln `b' = `0'
    mat `V' = `b''*`b'
    eret post `b' `V'
end

program _Lincom_get_R
    args R lincom
    if `"`lincom'"'=="" local lincom "()"
    tempname r
    while (`"`lincom'"'!="") {
        gettoken exp lincom : lincom, match(par)
        capt _on_colon_parse `exp'
        if _rc==0 {
            local nm = strtrim(`"`s(before)'"')
            local exp = strtrim(`"`s(after)'"')
        }
        else local nm ""
        if `"`nm'"'=="" local nm `"`exp'"'
        if `"`exp'"'=="" {
            di as err "{it:exp} is missing"
            di as err "error in lincom()"
            exit 198
        }
        capt n qui _test `exp' = 0 // evaluate exp
        if _rc {
            di as err "error in lincom()"
            exit 198
        }
        mat `r' = get(Rr)
        capt n qui mat roweq `r' = `"`nm'"' // check name
        if _rc {
            di as err "error in lincom()"
            exit 198
        }
        mat `R' = nullmat(`R') \ `r'
    }
    mat `R' = `R'[1...,1..colsof(`R')-1]
end


version 14
mata:
mata set matastrict on

void _parse_keep()
{
    real scalar      n, i
    string rowvector keep, eqs
    real rowvector   idx, match, matched
    
    keep = tokens(st_local("keep"))
    n = length(keep)
    if (!n) return // nothing to do
    eqs = tokens(st_local("eqs"))
    matched = J(1, length(eqs), 0)
    idx = J(1, 0, .)
    for (i=1;i<=n;i++) {
         match = strmatch(eqs, keep[i]) :& !matched
         idx = idx, selectindex(match)
         matched = matched + match
    }
    st_local("eqs", invtokens(eqs[idx]))
}

void _parse_drop()
{
    real scalar      n, i
    string rowvector drop, eqs
    real rowvector   matched
    
    drop = tokens(st_local("drop"))
    n = length(drop)
    if (!n) return // nothing to do
    eqs = tokens(st_local("eqs"))
    matched = J(1, length(eqs), 0)
    for (i=1;i<=n;i++) matched = matched + strmatch(eqs, drop[i])
    st_local("eqs", invtokens(eqs[selectindex(matched:==0)]))
}

void _Lincom_apply()
{
    real scalar    k, i, j, i0, i1, j0, j1
    real matrix    R, r
    string matrix  cstripe
    
    // expand r
    k = st_numscalar("e(k)")
    r = st_matrix(st_local("R"))
    i = rows(r)
    j = cols(r)
    R = J(i*k, j*k, .)
    i0 = i*k + 1
    for (;i;i--) {
        i1 = i0 - 1
        i0 = i0 - k
        j = cols(r)
        j0 = j*k + 1
        for (;j;j--) {
            j1 = j0 - 1
            j0 = j0 - k
            R[|i0,j0 \ i1,j1|] = I(k) * r[i,j]
        }
    }
    // evaluate
    cstripe = mm_expand(st_matrixrowstripe(st_local("R"))[,1], k, 1, 1), 
              J(rows(r), 1, st_matrixcolstripe("e(b)")[|1,2\k,2|])
    st_matrix(st_local("b"), st_matrix("e(b)") * R')
    st_matrixcolstripe(st_local("b"), cstripe)
    if (st_local("V")!="") {
        st_matrix(st_local("V"), R * st_matrix("e(V)") * R')
        st_matrixcolstripe(st_local("V"), cstripe)
        st_matrixrowstripe(st_local("V"), cstripe)
    }
}

struct CDIST {
    string scalar          method,  // estimation method
                           touse,   // varname of sample identifier
                           xvars,   // varnames of covariates
                           wvar     // varname of weights
    real scalar            g,       // target size of approximation grid
                           k,       // number of statistics
                           pooled,  // used pooled X distribution
                           qdef     // quantile definition
    transmorphic colvector s        // statistics
    string rowvector       eqs      // equations to be included in results
    real colvector         w        // pooled weights
    real matrix            X        // pooled X
}

struct CDIST_G {
    string scalar          touse    // varname of group identifier
    real scalar            g,       // size of approximation grid
                           p,       // evaluation grid (probabilities)
                           l        // evaluation grid (levels)
    real colvector         y,       // depvar
                           w        // weights
    real matrix            X        // covariates
    real matrix            b        // regression coefficients
    real colvector         b50      // coefficients of median regression
    real colvector         obs,     // results: observed
                           fit,     // results: fitted
                           adj,     // results: X adjusted
                           loc      // results: X adjusted and location shifted
}

void cdist()
{
    real scalar           j, i0, i1
    string colvector      s
    real colvector        b
    pointer scalar        f
    struct CDIST   scalar S
    struct CDIST_G scalar G0, G1
    
    // setup S
    S.method = st_local("method")
    S.pooled = st_local("pooled")!=""
    S.qdef   = strtoreal(st_local("qdef"))
    S.g      = strtoreal(st_local("gsize"))
    if (st_local("statistics")!="") {
        S.s = s = tokens(st_local("statistics"))'
        S.k = length(s)
    }
    else if (st_local("percentiles")!="") {
        s   = tokens(st_local("percentiles"))'
        S.s = strtoreal(s) / 100
        S.k = length(s)
    }
    else {
        S.k = strtoreal(st_local("nquantiles"))
        S.s = (1::S.k)/(S.k+1)
        s   = strofreal(S.s * 100)
        st_local("percentiles", invtokens(s'))
    }
    S.eqs = tokens(st_local("eqs"))
    S.touse = st_local("touse")
    S.xvars = st_local("XVARS")
    S.wvar  = st_local("wvar")
    // setup G0 and G1
    G0.touse = st_local("touse0")
    G1.touse = st_local("touse1")
    st_view(G0.y, ., st_local("depvar"), G0.touse)
    st_view(G1.y, ., st_local("depvar"), G1.touse)
    if (S.xvars!="") {
        st_view(G0.X=., ., S.xvars, G0.touse)
        st_view(G1.X=., ., S.xvars, G1.touse)
        if (S.pooled) st_view(S.X=., ., S.xvars, S.touse)
    }
    else {
        G0.X = J(rows(G0.y), 0, .)
        G1.X = J(rows(G1.y), 0, .)
        if (S.pooled) S.X = J(rows(st_data(., S.touse, S.touse)), 0, .)
    }
    if (S.wvar!="") {
        st_view(G0.w, ., S.wvar, G0.touse)
        st_view(G1.w, ., S.wvar, G1.touse)
        if (S.pooled) st_view(S.w, ., S.wvar, S.touse)
    }
    else {
        G0.w = G1.w = 1
        if (S.pooled) S.w = 1
    }
    // loc0/loc1: need to obtain median fits
    if (anyof(S.eqs,"loc0") | anyof(S.eqs,"loc1")) {
        G0.b50 = mm_qrfit(G0.y, G0.X, G0.w, 0.5)
        G1.b50 = mm_qrfit(G1.y, G1.X, G1.w, 0.5)
    }
    // compute results
    if      (S.method=="logit") f = &_cdist_dr()  // distribution regression
    else                        f = &_cdist_qr()  // quantile regression
    if (anyof(S.eqs,"obs0")) G0.obs = _cdist_stats(G0.y, G0.w, S.s, S.qdef)
    (*f)(S, G0, G1, (anyof(S.eqs,"fit0"), anyof(S.eqs,"adj0"), anyof(S.eqs,"loc0")))
    if (anyof(S.eqs,"obs1")) G1.obs = _cdist_stats(G1.y, G1.w, S.s, S.qdef)
    (*f)(S, G1, G0, (anyof(S.eqs,"fit1"), anyof(S.eqs,"adj1"), anyof(S.eqs,"loc1")))
    // return results
    j = length(S.eqs)
    b = J(j*S.k, 1, .)
    i0 = j * S.k + 1
    for (;j;j--) {
        i1 = i0 - 1
        i0 = i0 - S.k
        if      (S.eqs[j]=="obs0") b[|i0 \ i1|] = G0.obs
        else if (S.eqs[j]=="fit0") b[|i0 \ i1|] = G0.fit
        else if (S.eqs[j]=="adj0") b[|i0 \ i1|] = G0.adj
        else if (S.eqs[j]=="loc0") b[|i0 \ i1|] = G0.loc
        else if (S.eqs[j]=="obs1") b[|i0 \ i1|] = G1.obs
        else if (S.eqs[j]=="fit1") b[|i0 \ i1|] = G1.fit
        else if (S.eqs[j]=="adj1") b[|i0 \ i1|] = G1.adj
        else if (S.eqs[j]=="loc1") b[|i0 \ i1|] = G1.loc
    }
    st_matrix(st_local("b"), b')
    st_matrixcolstripe(st_local("b"), (mm_expand(S.eqs', S.k, 1, 1),
        J(length(S.eqs),1,s)))
    st_local("gsize0", strofreal(G0.g))
    st_local("gsize1", strofreal(G1.g))
    st_local("k",      strofreal(S.k))
}

// subroutine to compute results based on quantile regression
void _cdist_qr(struct CDIST scalar S, struct CDIST_G scalar G,
    struct CDIST_G scalar G1, real rowvector todo)
{
    // anything to do?
    if (!any(todo)) return
    // determine evaluation points (regular grid)
    G.g = S.g
    G.p = (1::G.g)/G.g :- .5/G.g
    // estimate coefficients
    G.b = J(cols(G.X) + 1, G.g, .)
    if (S.method=="qr0") _cdist_qr_b0(G)
    else                 _cdist_qr_b(G)
    // obtain fit
    if (todo[1]) {
        G.fit = _cdist_stats(
                vec(_cdist_xb(G.X, G.b)),         // stacked predictions
                S.wvar=="" ? 1 : J(G.g, 1, G.w),  // stacked weights
                S.s, S.qdef)
    }
    // obtain adj
    if (todo[2]) {
        if (S.pooled) G.adj = _cdist_stats(vec(_cdist_xb(S.X, G.b)),
            S.wvar=="" ? 1 : J(G.g, 1, S.w), S.s, S.qdef)
        else G.adj = _cdist_stats(vec(_cdist_xb(G1.X, G.b)),
            S.wvar=="" ? 1 : J(G.g, 1, G1.w), S.s, S.qdef)
    }
    // obtain loc
    if (todo[3]) {
        G.loc = _cdist_stats(vec(_cdist_xb(G1.X, (G.b:-(G.b50-G1.b50)))),
            S.wvar=="" ? 1 : J(G.g, 1, G1.w), S.s, S.qdef)
    }
}

// obtains quantile regressions
void _cdist_qr_b0(struct CDIST_G scalar G)
{
    real scalar i
    
    for (i=1; i<=G.g; i++) G.b[,i] = mm_qrfit(G.y, G.X, G.w, G.p[i])
}

// obtains quantile regressions; somewhat faster than _cdist_qr_b0()
void _cdist_qr_b(struct CDIST_G scalar G)
{
    real scalar        i
    real colvector     b_mid, b_i
    class mm_qr scalar Q

    // get 1st estimate in middle 
    Q.data(G.y, G.X, G.w)
    i = ceil(G.g/2)
    Q.p(G.p[i])
    G.b[,i] = b_mid = b_i = Q.b()
    i++
    // move up
    for (; i<=G.g; i++) {
        Q.p(G.p[i])
        Q.b_init(b_i)
        G.b[,i] = b_i = Q.b()
    }
    // move down
    i = ceil(G.g/2) - 1
    b_i = b_mid
    for (; i; i--) {
        Q.p(G.p[i])
        Q.b_init(b_i)
        G.b[,i] = b_i = Q.b()
    }
}

// subroutine to compute results based on distribution regression
void _cdist_dr(struct CDIST scalar S, struct CDIST_G scalar G,
    struct CDIST_G scalar G1, real rowvector todo)
{
    real scalar    ymax // maximum of depvar (within group)
    real colvector F    // distribution function
    
    // anything to do?
    if (!any(todo)) return
    // determine evaluation points
    if (mm_nunique(G.y)<=S.g) G.l = mm_unique(G.y) // use observed values
    else {
        G.p = (1::S.g)/S.g:-.5/S.g
        G.l = mm_unique(mm_quantile(G.y, G.w, G.p \ 1, 1/*low quantile*/))
    }
    G.g = rows(G.l)
    ymax = G.l[G.g]
    if (G.g>1) G.l = G.l[|1 \ G.g-1|] // remove maximum
    else             G.l = J(0,1,.)
    G.g = rows(G.l)
    // estimate coefficients
    G.b = J(cols(G.X) + 1, G.g, .)
    _cdist_dr_logit(S, G)
    assert(!hasmissing(G.b))
    // obtain fit
    if (todo[1]) {
        F = mean(invlogit(_cdist_xb(G.X, G.b)), G.w)' \ 1
        _sort(F, 1) // rearrange if there are crossings
        G.fit = _cdist_stats(G.l \ ymax, mm_diff(0\F), S.s, S.qdef)
    }
    // obtain adj
    if (todo[2]) {
        if (S.pooled) F = mean(invlogit(_cdist_xb(S.X, G.b)), S.w)' \ 1
        else          F = mean(invlogit(_cdist_xb(G1.X, G.b)), G1.w)' \ 1
        _sort(F, 1) // rearrange if there are crossings
        G.adj = _cdist_stats(G.l \ ymax, mm_diff(0\F), S.s, S.qdef)
    }
}

// subroutine to fit the logit models in Stata (should maybe port to moptimize())
void _cdist_dr_logit(struct CDIST scalar S, struct CDIST_G scalar G)
{
    real scalar   i, Y
    string scalar Ynm, cmd
    
    Ynm = st_tempname()
    Y   = st_addvar("byte", Ynm)
    cmd = "version 10: logit " + // use old logit version; less overhead
          Ynm +  " " + S.xvars + " " +
          (S.wvar!="" ? "[iw=" + S.wvar + "] " : "") +
          "if " + G.touse + ", asis"
    for (i=1; i<=G.g; i++) {
        st_store(., Y, G.touse, G.y:<=G.l[i])
        stata(cmd, 1)
        G.b[,i] = st_matrix("e(b)")'
    }
    st_dropvar(Ynm)
}

// subroutine to generate predictions
real matrix _cdist_xb(real matrix X, real matrix B)
{
    real scalar    i, k
    real colvector b
    real matrix    XB
    
    k = cols(X)
    i = cols(B)
    XB = J(rows(X), i, .)
    for (;i;i--) {
        b = B[,i]
        if (k==0) XB[,i] = J(rows(X), 1, b)
        else      XB[,i] = X * b[|1 \ k|] :+ b[k+1]
    }
    return(XB)
}

// subroutine to compute statistics from distribution
real colvector _cdist_stats(real colvector y, real colvector w,
    transmorphic colvector s, real scalar d)
{
    string rowvector S
    real scalar      i
    real colvector   b
    real matrix      tmp
    
    if (isreal(s)) return(mm_quantile(y, w, s, d))
    i = rows(s)
    b = J(i,1,.)
    for (;i;i--) {
        if      (s[i]=="mean")     b[i] = mean(y, w)
        else if (s[i]=="variance") b[i] = mm_variance0(y, w)
        else if (s[i]=="sd")       b[i] = sqrt(mm_variance0(y, w))
        else if (s[i]=="median")   b[i] = mm_median(y, w, d)
        else if (s[i]=="iqr")      b[i] = mm_iqrange(y, w, d)
        else if (s[i]=="gini")     b[i] = mm_gini(y, w)
        else if (s[i]=="cv") {
            tmp = mean(y, w)
            if (tmp==0) {
                errprintf("statistics %s not allowed with outcome mean equal to zero\n", s)
                exit(3498)
            }
            b[i] = sqrt(mm_variance0(y, w)) / tmp
        }
        else if (s[i]=="mld") {
            _cdist_stats_check_ypos(y, "mld")
            b[i] = ln(mean(y, w)) - mean(ln(y), w)
        }
        else if (s[i]=="theil") {
            tmp = mean(y, w) 
            b[i] = mean(y:*ln(y), w)/tmp - ln(tmp)
        }
        else if (s[i]=="vlog") {
            _cdist_stats_check_ypos(y, "vlog")
            b[i] = mm_variance0(ln(y), w)
        }
        else if (s[i]=="sdlog") {
            _cdist_stats_check_ypos(y, "sdlog")
            b[i] = sqrt(mm_variance0(ln(y), w))
        }
        else {
            S = tokens(s[i], "(,)")
            if (S[1]=="iqr") {
                b[i] = mm_diff(mm_quantile(y, w, strtoreal(S[(3,5)])/100, d))
            }
            else if (S[1]=="qratio") {
                tmp = mm_quantile(y, w, strtoreal(S[(3,5)])/100, d)
                b[i] = tmp[2] / tmp[1]
            }
            else if (S[1]=="q") {
                b[i] = mm_quantile(y, w, strtoreal(S[3])/100, d)
            }
            else if (S[1]=="p") {
                b[i] = mm_quantile(y, w, strtoreal(S[3])/100, d)
            }
            else {
                errprintf("%s not supported\n", s[i])
                exit(3498)
            }
        }
    }
    return(b)
}

void _cdist_stats_check_ypos(real colvector y, string scalar s)
{
    if (min(y)>0) return
    errprintf("statistic %s not allowed with outcome values lower than or equal to 0\n", s)
    exit(3498)
}


end

exit

