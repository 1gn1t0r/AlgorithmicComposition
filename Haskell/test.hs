module Main where
import Euterpea
import Data.List (elemIndex, groupBy, find, sort)
import System.Random
import System.Random.Distributions
import Data.Maybe
import Data.MarkovChain

t251 :: Music Pitch
t251 = let dminor = d 4 wn :=: f 4 wn :=: a 4 wn
		   ;gmajor = g 4 wn :=: b 4 wn :=: b 4 wn
		   ;cmajor = c 4 bn :=: e 4 bn :=: g 4 bn
	   in dminor :+: gmajor :+: cmajor

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p d = let fullpitches = [C, D, E, F, G, A, B] ++ fullpitches
					 ;(pclass, octave) = p
					 ;index = fromJust (elemIndex pclass fullpitches)
					 ;dminor = note d (fullpitches !! (index+0), octave) :=: note d (fullpitches !! (index+2), octave) :=: note d (fullpitches !! (index+4), octave)
					 ;gmajor = note d (fullpitches !! (index+3), octave) :=: note d (fullpitches !! (index+5), octave) :=: note d (fullpitches !! (index+5), octave+1)
					 ;cmajor = note (d*2) (fullpitches !! (index+6), octave) :=: note (d*2) (fullpitches !! (index+8), octave) :=: note (d*2) (fullpitches !! (index+10), octave)
	   			 in dminor :+: gmajor :+: cmajor

testones :: Int -> Music Pitch
testones 0 = twoFiveOne (D,0) wn
testones n = twoFiveOne (D,n) wn :+: testones (n-1)

main :: IO()
main = putStrLn "What is 2 + 2?"

{-t251 = let dminor = d 4 wn :=: f 4 wn :=: as 4 wn
		   gmajor = g 4 wn :=: b 4 wn :=: b 5 wn
		   cmajor = c 4 bn :=: e 4 bn :=: g 4 bn
	   in dminor :+: gmajor :+: cmajor-}


data Cluster = Cluster SNote [Cluster] deriving Show
type SNote = (Dur, AbsPitch)

addMult :: SNote -> SNote -> SNote
addMult (d0, p0) (d1, p1) = (d0 *d1, p0 + p1)

selfSim :: [SNote] -> Cluster
selfSim pat = Cluster (0,0) (map mkCluster pat)
			where mkCluster note = Cluster note (map (mkCluster . addMult note) pat)

fringe :: Int -> Cluster -> [SNote]
fringe 0 (Cluster note cls) = [note]
fringe n (Cluster note cls) = concatMap(fringe (n-1)) cls

mkNote :: (Dur, AbsPitch) -> Music Pitch
mkNote (d, ap) = note d (pitch ap)

simToMusic :: [SNote] -> Music Pitch
simToMusic = line . map mkNote

fringe' :: Int -> Cluster -> [[SNote]]
fringe' 0 (Cluster note cls) = [[note]]
fringe' n (Cluster note cls) = map (fringe (n-1)) cls

simToMusic' :: [[SNote]] -> Music Pitch
simToMusic' = chord . map (line . map mkNote)

ss' pat n tr te = transpose tr $ tempo te $ simToMusic' $ fringe' n $ selfSim pat

ss pat n tr te = transpose tr $ tempo te $ simToMusic $ fringe n $ selfSim pat

data Rules a = Uni [Rule a]
			 | Sto [(Rule a, Prob)]
	deriving (Eq, Ord, Show)

data Rule a = Rule {lhs :: a, rhs :: a}
	deriving (Eq, Ord, Show)

type Prob = Float

type ReplFun a = [[(Rule a, Prob)]] -> (a, [Rand]) -> (a, [Rand])
type Rand = Float

data LSys a = N a
			| LSys a :+ LSys a
			| LSys a :. LSys a
			| Id
	deriving (Eq, Ord, Show)


replFun :: Eq a => ReplFun (LSys a)
replFun rules (s, rands) = 
	case s of 
		a :+ b 	->	let (a', rands') = replFun rules (a, rands)
				    	;(b', rands'') = replFun rules (b, rands')
				    in (a' :+ b', rands'')
		a :. b 	->	let (a', rands') = replFun rules (a, rands)
						;(b', rands'') = replFun rules (b, rands')
					in (a' :. b', rands'')
		Id 		->	(Id, rands)
		N x		-> 	(getNewRHS rules (N x) (head rands), tail rands)

getNewRHS :: Eq a => [[(Rule a, Prob)]] -> a -> Rand -> a
getNewRHS rrs ls rand = 
	let loop ((r, p) : rs) 	= if rand <= p then rhs r else loop rs
		;loop []				= error "getNewRHS anomaly"
	in case (find (\((r,o):_) -> lhs r == ls ) rrs ) of 
		Just rs -> loop rs
		Nothing -> error "No rule match"

type IR a b = [(a, Music b -> Music b)] 

interpret :: (Eq a) => LSys a -> IR a b -> Music b -> Music b
interpret (a :. b) 	r m 	= interpret a r (interpret b r m)
interpret (a :+ b) 	r m 	= interpret a r m :+: interpret b r m
interpret Id 		r m 	= m
interpret (N x)		r m 	= case (lookup x r) of 
								Just f -> f m
								Nothing -> error "No interpretation rule"

data LFun = Inc | Dec | Same
	deriving (Eq, Ord, Show)

ir :: IR LFun Pitch
ir = [(Inc, transpose 1), (Dec, transpose (-1)), (Same, id)]
inc, dec, same :: LSys LFun
inc = N Inc
dec = N Dec
same = N Same

data Grammar a = Grammar a (Rules a)
	deriving Show


gen :: Ord a => ReplFun a -> Grammar a -> Int -> [a]
gen f (Grammar s rules) seed = 
    let  Sto newRules  = toStoRules rules
         rands         = randomRs (0.0,1.0) (mkStdGen seed)
    in  if checkProbs newRules
        then generate f newRules (s,rands)
        else (error "Stochastic rule-set is malformed.")
toStoRules :: (Ord a, Eq a) => Rules a -> Rules a  
toStoRules (Sto rs)  = Sto rs
toStoRules (Uni rs)  = 
  let rs' = groupBy (\r1 r2 -> lhs r1 == lhs r2) (sort rs)
  in Sto (concatMap insertProb rs')

insertProb :: [a] -> [(a, Prob)] 
insertProb rules =  let prb = 1.0 / fromIntegral (length rules)
	       	    in zip rules (repeat prb)
checkProbs :: (Ord a, Eq a) => [(Rule a, Prob)] -> Bool
checkProbs rs = and (map checkSum (groupBy sameLHS (sort rs)))

eps = 0.001 

checkSum :: [(Rule a, Prob)] -> Bool 
checkSum rules =  let mySum = sum (map snd rules)
                  in abs (1.0 - mySum) <= eps 

sameLHS :: Eq a => (Rule a, Prob) -> (Rule a, Prob) -> Bool 
sameLHS (r1,f1) (r2,f2) = lhs r1 == lhs r2
generate ::  Eq a =>  
             ReplFun a -> [(Rule a, Prob)] -> (a,[Rand]) -> [a] 
generate f rules xs = 
  let  newRules      =  map probDist (groupBy sameLHS rules)
       probDist rrs  =  let (rs,ps) = unzip rrs
                        in zip rs (tail (scanl (+) 0 ps))
  in map fst (iterate (f newRules) xs)



sc = inc :+ dec 
r1a = Rule inc (sc :. sc)
r1b = Rule inc sc
r2a = Rule dec (sc :. sc)
r2b = Rule dec sc
r3a = Rule same inc
r3b = Rule same dec
r3c = Rule same same

g1 = Grammar same (Uni [r1b, r1a, r2b, r2a, r3a, r3b])
t1 n = instrument Vibraphone $ interpret (gen replFun g1 42 !! n) ir (c 4 tn)

ssfMel2 :: Music Pitch
ssfMel2 = line (l 1 ++ l 2 ++ l 3 ++ l 4 )
		where	l 1 = [trilln 2 5 (bf 6 en),ef 7 en,ef 6 en,ef 7 en]
				;l 2 = [bf 6 sn,c 7 sn,bf 6 sn,g 6 sn,ef 6 en,bf 5 en]
				;l 3 = [ef 6 sn,f 6 sn,g 6 sn,af 6 sn,bf 6 en,ef 7 en]
				;l 4 = [trill 2 tn (bf 6 qn),bf 6 sn,denr ]

m0 = [(1,2), (1,0), (1,5), (1,7)]
m1 = [(1,0),(0.5,0),(0.5,0)]
m2 :: [SNote]
m2 = [(dqn,0),(qn,4)]
m3 :: [SNote]
m3 = [(hn, 3), (qn, 4), (qn, 0), (hn, 6)]




tm0 = instrument Vibraphone (ss m0 4 50 20)
tm1 = instrument Sitar (ss m0 4 50 20) :=: instrument Vibraphone (ss m0 4 50 20)
tm2 = instrument Vibraphone (ss m0 3 40 15)
tm3 = instrument Percussion (ss m1 4 43 2)

tm4 = ss m3 4 50 (1/4)
ttm4 =	let l1 = instrument Flute tm4
			;l2 = instrument AcousticBass $ transpose (-9) (revM tm4)
		in 	l1 :=: l2


sGen :: StdGen
sGen = mkStdGen 42


toAbsP1 :: Float -> AbsPitch
toAbsP1 x = round(40 * x +30)

toAbsP2 :: Float -> AbsPitch
toAbsP2 x = round (5 * x)

mkNote1 :: AbsPitch -> Music Pitch
mkNote1 = note tn . pitch

mkNote3 :: Pitch -> Music Pitch
mkNote3 = note tn

mkLine2 :: AbsPitch -> [AbsPitch] -> Music Pitch
mkLine2 start rands = line $ take 64 $ map mkNote1 $ scanl (+) start rands

mkLine1 :: [AbsPitch] -> Music Pitch
mkLine1 rands = line $ take 32 $ map mkNote1 rands

mkLine3 :: [Pitch] -> Music Pitch
mkLine3 ps = line $ take 64 $ map mkNote3 ps

m4 :: Music Pitch
m4 = mkLine1 $ randomRs (30,70) sGen

m5 :: Music Pitch
m5 = let rs1 = rands linear sGen in mkLine1 $ map toAbsP1 rs1

m6 :: Float -> Music Pitch
m6 lam = let rs1 = rands (exponential lam) sGen in mkLine1 $ map toAbsP1 rs1

m7 :: Float -> Float -> Music Pitch
m7 sig mu = let rs1 = rands (gaussian sig mu) sGen in mkLine1 $ map toAbsP1 rs1

m8 :: Float -> Music Pitch
m8 sig = let rs1 = rands (gaussian sig 0) sGen in mkLine2 50 $ map toAbsP2 rs1

m9 :: Float -> Music Pitch
m9 lam = let rs1 = rands (exponential lam) sGen in mkLine2 50 $ map (toAbsP2 . subtract (1/lam)) rs1







-- Markov

ps0,ps1,ps2 :: [Pitch]

ps0 = [(C,4),(D,4),(E,4)]
ps1 = [(C,4),(D,4),(E,4),(F,4),(G,4),(A,4),(B,4)]
ps2 = [(C,4),(E,4),(G,4),(E,4),(F,4),(A,4),(G,4),(E,4),(C,4),(E,4),(G,4),(E,4),(F,4),(D,4),(C,4)]

mc ps n = mkLine3 (Data.MarkovChain.run n ps 0 (mkStdGen 42))
mcm pss n = mkLine3 (concat (runMulti n pss 0 (mkStdGen 42)))