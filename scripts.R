remove(t)
t <- datain
t['UpperShadow'] <- (datain['Max'] - pmax(datain['Open'],datain['Close']))/datain['Open']*10
t['LowerShadow'] <- (pmin(datain['Open'],datain['Close']) - datain['Min'])/datain['Open']*10
t['Body'] <- (pmax(datain['Open'],datain['Close']) - pmin(datain['Open'],datain['Close']))/datain['Open']*10
t['Diff'] <- ifelse(
                    datain['Open'] - datain['Close']==0,
                    rep(0,nrow(t)),
                    ifelse(
                          datain['Close']>datain['Open'],
                          rep(5,nrow(t)),
                          rep(-5,nrow(t))
                          )
                    )
t['CapD'] <- 0
t['CapE'] <- 0

## set universe
sets_options("universe", seq(from = -15, to = 25, by = 0.1))

## set up fuzzy variables
variables <-
  set(uvar =
        fuzzy_variable(ushort = fuzzy_trapezoid(corners = c(0, 1.4, 4.2, 7.1)),
                       umedium = fuzzy_trapezoid(corners = c(4.2, 7.1, 10, 14.2))
        ),
      lvar =
        fuzzy_variable(lshort = fuzzy_trapezoid(corners = c(0, 1.4, 4.2, 7.1)),
                       lmedium = fuzzy_trapezoid(corners = c(4.2, 7.1, 10, 14.2))
                       ),
      color = 
        fuzzy_partition(varnames =
                          c(cross = 0, black = -5, white = 5),
                        FUN = fuzzy_cone, radius = 3)
                      ),
      res = fuzzy_partition(varnames =
                              c(buy = 5, wait = 8, sell = 11),
                            FUN = fuzzy_cone, radius = 5)
  )

## set up rules
rules <-
  set(
    fuzzy_rule((lvar %is% lmedium) && color %is% black,
               res %is% sell),
    fuzzy_rule((uvar %is% ushort && lvar %is% lshort ) && (color %is% black || color %is% white),
               res %is% wait),
    fuzzy_rule(color %is% cross,
               res %is% wait),
    fuzzy_rule((uvar %is% umedium) && color %is% white,
               res %is% buy)
  )

## combine to a system
system <- fuzzy_system(variables, rules)
print(system)
plot(system) ## plots variables

## do inference
#fi <- fuzzy_inference(system, list(uvar = t[1,'UpperShadow'], lvar = t[1,'LowerShadow'], color = t[1,'Diff']))

## plot resulting fuzzy set
#plot(fi)

## defuzzify
#gset_defuzzify(fi, "centroid")

t[1,'CapD'] <- 5000
t[1,'CapE'] <- 0

for (i in 2:nrow(t))
{
  #fi <- fuzzy_inference(system, list(uvar = t[i,'UpperShadow'], lvar = t[i,'LowerShadow'], color = t[i,'Diff']))
  fi <- fuzzy_inference(system, list(uvar = 10, lvar = 10, color = t[i,'Diff']))

  ## defuzzify
  t[i,'Res'] <- gset_defuzzify(fi, "centroid")
  if (t[i,'Res']==5)
  {
    if (t[i-1,'CapD']!=0)
    {
      t[i,'CapE'] = t[i-1,'CapD']*t[i,'Close']
      t[i,'CapD'] = 0
    }
    else
    {
      t[i,'CapE'] = t[i-1,'CapE']
      t[i,'CapD'] = t[i-1,'CapD']
    }
  }
  else
    if (t[i,'Res']==11)
    {
      if (t[i-1,'CapE']!=0)
      {
        t[i,'CapD'] = t[i-1,'CapE']/t[i,'Close']
        t[i,'CapE'] = 0
      }
      else
      {
        t[i,'CapE'] = t[i-1,'CapE']
        t[i,'CapD'] = t[i-1,'CapD']
      }
    }
    else
    {
      t[i,'CapE'] = t[i-1,'CapE']
      t[i,'CapD'] = t[i-1,'CapD']
    }
      
}

## reset universe
sets_options("universe", NULL)
