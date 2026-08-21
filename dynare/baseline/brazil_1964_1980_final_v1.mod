/*
===============================================================================
 BRAZIL 1964-1980 DSGE - FINAL BASELINE, VERSION 1
 Linearized simulation model for Dynare 6.x

 Structure:
   Gali-Monacelli SOE core
 + money in utility / money demand
 + domestic public debt held by households
 + consolidated public-sector external debt
 + imported intermediate inputs
 + indexed Calvo Phillips curve
 + passive monetary policy / active fiscal policy
 + foreign-price and world-interest-rate shocks
 + public-investment (II-PND-type) policy shock

 IMPORTANT UNITS
 ---------------
 Most variables are log-deviations from the steady state.
 Exceptions:
   tb  = trade-balance deviation as a fraction of steady-state GDP
   d   = external public-debt/GDP deviation (level, not log)
   b   = domestic public-debt/GDP deviation (level, not log)
   sg  = seigniorage/GDP deviation
   tau = tax-revenue/GDP deviation
   i, istar, rp, pi, piH, pif = quarterly rate deviations in decimals

 A value of 0.01 therefore means one percentage point / roughly one percent,
 depending on the variable's stated unit.
===============================================================================
*/

var
    c           // aggregate household consumption
    n           // labor
    y           // domestic output
    z           // imported intermediate input volume
    mc          // real marginal cost
    piH         // domestic-goods inflation
    pi          // CPI inflation
    s           // terms of trade: log(P_F/P_H), higher = imported goods dearer
    e           // nominal exchange rate, domestic currency per foreign currency
    i           // domestic nominal policy/sovereign rate
    m           // real money balances
    ch          // household demand for domestic goods
    cf          // household demand for imported final goods
    x           // export volume
    tb          // trade balance / steady-state GDP deviation
    d           // consolidated public external debt / GDP deviation
    rp          // Brazil country-risk premium
    istar       // world nominal interest rate
    ig          // public/SOE investment expenditure
    g           // ordinary government consumption
    tau         // tax revenue / GDP deviation
    b           // domestic public debt / GDP deviation
    sg          // seigniorage / GDP deviation
    pif         // foreign imported-goods inflation
    a           // productivity
    ystar       // foreign output
;

varexo
    eps_pf      // foreign-price shock (1973/1979 type)
    eps_iw      // world-interest-rate shock (especially 1979/80 type)
    eps_rp      // country-risk shock
    eps_i       // domestic monetary-policy shock
    eps_ig      // public-investment / development-policy shock
    eps_g       // ordinary government-spending shock
    eps_tau     // tax/revenue shock
    eps_a       // productivity shock
    eps_ystar   // foreign-demand shock
;

parameters
    beta sigma varphi
    alpha eta mu lambda_s
    theta gammaI kappa
    eta_x
    cH_y x_y g_y ig_y cf_y z_y
    rhoF rhoiw
    phiD phiDD dbar rstar_ss
    rhoi phiPi phiE
    eta_mc eta_mi
    money_y Pi_ss invPi
    rhotau rhoG rhoIG rhoa rhoystar
    rdom_ss bbar
;

// -----------------------------------------------------------------------------
// 1. BASELINE CALIBRATION (quarterly)
//    These are starting values for model debugging, NOT final estimated values.
// -----------------------------------------------------------------------------

beta    = 0.99;
sigma   = 1.50;
varphi  = 1.00;

// Openness / consumption substitution
alpha   = 0.10;
eta     = 0.80;

// Imported intermediate input share
mu       = 0.12;
lambda_s = mu + (1-mu)*alpha;

// Calvo pricing with backward indexation
theta   = 0.75;
gammaI  = 0.70;
kappa   = ((1-theta)*(1-beta*theta))/theta;

// Export demand elasticity to the terms of trade
eta_x   = 0.70;

// Steady-state domestic-output absorption shares; these sum to 1
cH_y    = 0.68;
x_y     = 0.12;
g_y     = 0.14;
ig_y    = 0.06;

// Import expenditure shares used to linearize the trade balance.
// Baseline imports sum to exports, so the steady-state trade balance is zero.
cf_y    = 0.05;
z_y     = 0.07;

// Foreign-price persistence
rhoF    = 0.75;

// World-interest-rate persistence
rhoiw   = 0.85;

// External debt / country risk
phiD       = 0.030;
phiDD      = 0.050;
dbar       = 0.25;      // steady public external debt around 25% of GDP
rstar_ss   = 0.0125;    // approx. 5% annual real/borrowing cost benchmark

// Monetary policy: deliberately weak inflation response, no output-gap term
rhoi     = 0.75;
phiPi    = 0.35;        // passive: < 1
phiE     = 0.65;        // response to external financing pressure

// Log-linearized money demand from the MIU block
eta_mc   = 0.75;        // consumption elasticity of real balances
eta_mi   = 4.00;        // semi-elasticity to nominal interest rate

// Money/seigniorage calibration
money_y  = 0.08;        // steady real monetary base around 8% of quarterly GDP measure
Pi_ss    = (1.20)^(1/4); // 20% annual inflation baseline, gross quarterly rate
invPi    = 1/Pi_ss;

// Fiscal and real shock persistence
rhotau   = 0.85;
rhoG     = 0.80;
rhoIG    = 0.95;
rhoa     = 0.90;
rhoystar = 0.85;

// Domestic debt calibration
rdom_ss  = 0.010;       // approx. 4% annual steady real servicing component
bbar     = 0.20;        // steady domestic public debt around 20% of GDP

// -----------------------------------------------------------------------------
// 2. LINEARIZED EQUILIBRIUM SYSTEM
// -----------------------------------------------------------------------------

model(linear);

    // -------------------------------------------------------------------------
    // HOUSEHOLDS
    // -------------------------------------------------------------------------

    [name='1. Consumption Euler equation']
    c = c(+1) - (1/sigma)*( i - pi(+1) );

    // Domestic and imported consumption demands from the GM CES aggregator.
    // p_H-p = -alpha*s and p_F-p = (1-alpha)*s to first order.
    [name='2. Domestic consumption demand']
    ch = c + eta*alpha*s;

    [name='3. Imported final consumption demand']
    cf = c - eta*(1-alpha)*s;

    // MIU money demand. This is the operational log-linear form of the
    // household money FOC; it does NOT add a separate inflation-consumption rule.
    [name='4. Real money demand']
    m = eta_mc*c - eta_mi*i;

    // -------------------------------------------------------------------------
    // PRODUCTION WITH IMPORTED INTERMEDIATE INPUTS
    // -------------------------------------------------------------------------

    [name='5. Production technology']
    y = a + (1-mu)*n + mu*z;

    // Cost-minimizing imported-input demand: z = y + mc - s (up to constants).
    [name='6. Imported intermediate demand']
    z = y + mc - s;

    // Derived marginal-cost equation from labor supply + imported input costs.
    [name='7. Real marginal cost']
    mc = (1-mu)*(sigma*c + varphi*n) - a + lambda_s*s;

    // -------------------------------------------------------------------------
    // PRICES, TERMS OF TRADE, AND INDEXATION
    // -------------------------------------------------------------------------

    // CPI inflation = domestic inflation + alpha * change in terms of trade.
    [name='8. CPI inflation identity']
    pi = piH + alpha*(s-s(-1));

    // s = e + p_F* - p_H in first differences.
    [name='9. Terms-of-trade identity']
    s-s(-1) = e-e(-1) + pif - piH;

    // Generic foreign-price process. Large realizations stand in for 1973/1979.
    [name='10. Foreign-price process']
    pif = rhoF*pif(-1) + eps_pf;

    // Hybrid/indexed Calvo NKPC.
    [name='11. Indexed Calvo Phillips curve']
    (1+beta*gammaI)*piH = gammaI*piH(-1) + beta*piH(+1) + kappa*mc;

    // -------------------------------------------------------------------------
    // EXTERNAL DEMAND AND GOODS MARKET
    // -------------------------------------------------------------------------

    // A rise in s makes Brazilian goods cheaper relative to foreign goods.
    [name='12. Export demand']
    x = ystar + eta_x*s;

    // Domestic output is absorbed by domestic consumption, exports,
    // ordinary government consumption, and public/SOE investment.
    [name='13. Domestic goods market clearing']
    y = cH_y*ch + x_y*x + g_y*g + ig_y*ig;

    // Trade balance in fractions of steady GDP. Imported values react to both
    // volumes and their relative price s.
    [name='14. Trade balance']
    tb = x_y*x - cf_y*(cf+s) - z_y*(z+s);

    // -------------------------------------------------------------------------
    // PUBLIC EXTERNAL DEBT AND COUNTRY RISK
    // -------------------------------------------------------------------------

    // External public debt accumulation. A trade deficit (tb<0) raises debt.
    // Higher inherited foreign borrowing costs also increase the debt stock.
    [name='15. External public debt accumulation']
    d = (1+rstar_ss)*d(-1) - tb + dbar*(istar(-1)+rp(-1));

    // Country risk depends on both the debt level and its recent acceleration.
    [name='16. Country-risk premium']
    rp = phiD*d + phiDD*(d-d(-1)) + eps_rp;

    [name='17. World interest-rate process']
    istar = rhoiw*istar(-1) + eps_iw;

    // Sovereign UIP: international investors price domestic-currency public debt
    // relative to foreign-currency borrowing plus expected depreciation.
    [name='18. Sovereign UIP']
    i = istar + rp + e(+1)-e;

    // -------------------------------------------------------------------------
    // MONETARY POLICY
    // -------------------------------------------------------------------------

    // Weak inflation response; no output-gap term. The authority also responds
    // to external financing pressure (world rate + country risk).
    [name='19. Passive monetary policy rule']
    i = rhoi*i(-1)
        + (1-rhoi)*( phiPi*pi + phiE*(istar+rp) )
        + eps_i;

    // -------------------------------------------------------------------------
    // SEIGNIORAGE AND FISCAL POLICY
    // -------------------------------------------------------------------------

    // Linearization of SG_t = m_t - m_{t-1}/Pi_t around Pi_ss.
    // sg is measured as a deviation in fractions of steady-state GDP.
    [name='20. Seigniorage']
    sg = money_y*( m - invPi*m(-1) + invPi*pi );

    // Taxes are a persistent revenue-ratio process with NO debt feedback.
    [name='21. Tax revenue process - active fiscal regime']
    tau = rhotau*tau(-1) + eps_tau;

    [name='22. Ordinary government consumption']
    g = rhoG*g(-1) + eps_g;

    // Persistent public/SOE investment process. eps_ig represents the policy
    // response associated with the development strategy / II-PND, not oil itself.
    [name='23. Public investment / SOE expenditure']
    ig = rhoIG*ig(-1) + eps_ig;

    // Linearized consolidated public financing identity.
    // Domestic debt is the residual financing instrument after taxes,
    // seigniorage, and net external borrowing.
    // Current inflation lowers the real burden of inherited nominal domestic debt.
    [name='24. Domestic public debt / government financing']
    b = (1+rdom_ss)*b(-1)
        + g_y*g + ig_y*ig - tau - sg
        - (d-d(-1))
        + rstar_ss*d(-1) + dbar*(istar(-1)+rp(-1))
        + bbar*(i(-1)-pi);

    // -------------------------------------------------------------------------
    // OTHER EXOGENOUS REAL PROCESSES
    // -------------------------------------------------------------------------

    [name='25. Productivity process']
    a = rhoa*a(-1) + eps_a;

    [name='26. Foreign output process']
    ystar = rhoystar*ystar(-1) + eps_ystar;

end;

// -----------------------------------------------------------------------------
// 3. SHOCK CALIBRATION
//    These are intentionally provisional. stoch_simul reports one-s.d. IRFs.
// -----------------------------------------------------------------------------

shocks;
    var eps_pf;     stderr 0.020;   // foreign-price shock
    var eps_iw;     stderr 0.010;   // world-rate shock
    var eps_rp;     stderr 0.005;   // Brazil-risk shock
    var eps_i;      stderr 0.005;   // domestic policy shock
    var eps_ig;     stderr 0.020;   // public-investment policy shock
    var eps_g;      stderr 0.010;   // ordinary spending shock
    var eps_tau;    stderr 0.005;   // tax/revenue shock
    var eps_a;      stderr 0.010;   // productivity shock
    var eps_ystar;  stderr 0.010;   // foreign-demand shock
end;

// -----------------------------------------------------------------------------
// 4. DIAGNOSTICS AND IRFs
// -----------------------------------------------------------------------------

steady;
resid;
check;
model_diagnostics;

// Core variables to inspect first.
stoch_simul(order=1, irf=40)
    pi piH s mc tb d rp istar i sg b m y c ig;

