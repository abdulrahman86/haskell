type Name = String
type Val = String
type Rule = (Pattern,Skel)
type Rules = [Rule]
type Dictionary = [(String, Exp)]


data Pattern = PatOp Val [Pattern]
             | PatAnyVar Name
             | PatAnyConst Name
             | PatConst Name
             | Any Name
             | PatNil
             deriving Show

data Exp = ExpConst Name
         | ExpVar Name
         | ExpOp Name [Exp]
         | ExpNil
         deriving (Show, Eq)

data Skel = SkelOp Name [Skel]
              | SkelConst Name
              | SkelVar Name
              | SkelTemplate Name
              | SkelNil
              deriving Show


addDict :: Dictionary -> (String, Exp) -> Dictionary
addDict dict x = let val = findDict dict (fst x) in
                              if val == ExpNil
                                 then x : dict
                                 else if val == snd x
                                    then dict
                                    else addDict dict ("error", ExpConst "error")



findDict :: Dictionary -> String -> Exp
findDict [] _ = ExpNil
findDict dict x = let list = filter (\(k,v) -> k == x) dict in 
    if (length list) > 0 
       then snd (head list)
       else ExpNil


matcher :: [Pattern] -> Dictionary -> [Exp] -> Dictionary
matcher [] dict [] = dict
matcher [PatAnyVar x] dict [ExpVar ex] = addDict dict (x, ExpVar ex)
matcher [PatAnyConst x] dict [ExpConst ex] = addDict dict (x, ExpConst ex)
matcher [PatConst x] dict [ExpConst ex] = 
    if x == ex 
       then dict
       else addDict dict ("error", ExpConst "error")
matcher [PatOp x pys] dict [ExpOp ex eys] 
                            | x == ex = matcher pys dict eys
                            | otherwise = addDict dict ("error", ExpConst "error")
matcher [Any x] dict [p] = addDict dict (x, p)
matcher (p:ps1:ps2) dict (e:es1:es2) = foldl (\dict (p,e) -> matcher [p] dict [e]) (matcher [p] dict [e]) (zip (ps1:ps2) (es1:es2))
matcher _ dict _ = addDict dict ("error", ExpConst "error")



simplify :: Rules -> Rules -> Exp -> Exp
simplify grules [] (ExpOp x exs) = ExpOp x (map (simplify grules grules) exs)
simplify grules (rule:rules) (ExpOp x exs) = 
                               let dict = matcher [fst rule] []  [ExpOp x (map (simplify grules grules) exs)] in 
                                   let y = findDict dict "error" in 
                                       case y of ExpConst "error" -> simplify grules rules (ExpOp x exs)
                                                 ExpNil -> instantiator grules dict (snd rule) 
simplify _ _ x = x

instantiator :: Rules -> Dictionary -> Skel -> Exp
instantiator _ dict (SkelConst x) = ExpVar x
instantiator _ dict (SkelVar x) = ExpVar x
instantiator rules dict (SkelOp x skels) = simplify rules rules (ExpOp x (map (instantiator rules dict) skels))
instantiator rules dict (SkelTemplate x) = 
    case findDict dict x of ExpConst n -> ExpConst n
                            ExpVar n -> ExpVar n
                            ExpOp n exps -> simplify rules rules (ExpOp n exps)



--Tests applying the above interpreter to specify differentiation rules and simplify differentiation expressions

--rule sets

r1 = (PatOp "differentiate" [PatAnyConst "x", PatAnyVar "v"], SkelConst "0")
r2 = (PatOp "differentiate" [PatAnyVar "v", PatAnyVar "v"], SkelConst "1")
r3 = (PatOp "differentiate" [PatAnyVar "u", PatAnyVar "v"], SkelConst "0")
r4 = (PatOp "differentiate" [PatOp "+" [Any "u", Any "v"], PatAnyVar "y"], SkelOp "+" [SkelOp "differentiate" [SkelTemplate "u", SkelTemplate "y"], SkelOp "differentiate" [SkelTemplate "v", SkelTemplate "y"]])
r5 = (PatOp "differentiate" [PatOp "*" [Any "u", Any "v"], PatAnyVar "y"], SkelOp "+" [SkelOp "*" [SkelTemplate "u", SkelOp "differentiate" [SkelTemplate "v", SkelTemplate "y"]], SkelOp "*" [SkelTemplate "v", SkelOp "differentiate" [SkelTemplate "u", SkelTemplate "y"]]])


--expressions to differentiate
e1 = ExpOp "differentiate" [ExpConst "45", ExpVar "x"]
e2 = ExpOp "differentiate" [ExpVar "x", ExpVar "x"]
e3 = ExpOp "differentiate" [ExpVar "y", ExpVar "x"]
e4 = ExpOp "differentiate" [ExpOp "+" [ExpConst "5", ExpConst "6"], ExpVar "y"]
e5 = ExpOp "differentiate" [ExpOp "+" [ExpVar "y", ExpConst "6"], ExpVar "y"]
e6 = ExpOp "differentiate" [ExpOp "+" [ExpOp "+" [ExpVar "y", ExpConst "6"], ExpVar "y"], ExpVar "y"]
e7 = ExpOp "differentiate" [ExpOp "*" [ExpVar "5", ExpConst "11"], ExpVar "y"]
e8 = ExpOp "differentiate" [ExpOp "*" [ExpConst "5", ExpVar "y"], ExpVar "y"]
e9 = ExpOp "differentiate" [ExpOp "+" [ExpOp "*" [ExpVar "y", ExpConst "6"], ExpVar "y"], ExpVar "y"]


--results
res1 = matcher [fst r1] [] [e1]
res2 = simplify [r1, r2] [r1, r2] e2
res3 = simplify [r1, r2, r3] [r1, r2, r3] e3
res4 = simplify [r1, r2, r3, r4] [r1, r2, r3, r4] e4
res5 = simplify [r1, r2, r3, r4] [r1, r2, r3, r4] e5
res6 = simplify [r1, r2, r3, r4] [r1, r2, r3, r4] e6
res7 = simplify [r1, r2, r3, r4, r5] [r1, r2, r3, r4, r5] e7
res8 = simplify [r1, r2, r3, r4, r5] [r1, r2, r3, r4, r5] e8
res9 = simplify [r1, r2, r3, r4, r5] [r1, r2, r3, r4, r5] e9



