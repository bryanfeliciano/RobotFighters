-- constructor--

robot :: (a, b, c) -> ((a, b, c) -> t) -> t
robot (name , attack , hp) = \message -> message (name , attack , hp)

-- instance of robot -- 

killerRobot :: ((String, Integer, Integer) -> t) -> t
killerRobot = robot ("kill3r ", 25 , 200)

gentleGiant :: ((String, Integer, Integer) -> t) -> t
gentleGiant = robot ("Mr.Friendly ", 10 , 300)

fastRobot :: ((String, Integer, Integer) -> t) -> t
fastRobot = robot ("speedy " , 15 , 40)

slowRobot :: ((String, Integer, Integer) -> t) -> t
slowRobot = robot ("slowpoke " , 20 , 30)

-- map trough robots function --
-- ask ian--
mapRobots r = map (robot)

-- helper functions --

name :: (a, b, c) -> a
name (n , _ , _) = n

attack :: (a, b, c) -> b
attack (_ , a , _) = a

hp :: (a, b, c) -> c
hp (_, _ , hp) = hp

-- accesors -> to no longer worry about order of values --

getName :: (((a, b, c) -> a) -> t) -> t
getName aRobot = aRobot name

getAttack :: (((a, b, c) -> b) -> t) -> t
getAttack aRobot = aRobot attack

getHp :: (((a, b, c) -> c) -> t) -> t
getHp aRobot = aRobot hp

-- setters --

setName :: (((a1, b, c) -> ((a2, b, c) -> t1) -> t1) -> t2) -> a2 -> t2
setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))

setAttack :: (((a, b1, c) -> ((a, b2, c) -> t1) -> t1) -> t2) -> b2 -> t2
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))

setHp :: (((a, b, c1) -> ((a, b, c2) -> t1) -> t1) -> t2) -> c2 -> t2
setHp aRobot newHp = aRobot (\(n,a,h) -> robot (n,a,newHp))

-- new objects --

nicerRobot :: ((String, Integer, Integer) -> t) -> t
nicerRobot = setName killerRobot "kitty"

gentleRobot :: ((String, Integer, Integer) -> t) -> t
gentleRobot = setAttack killerRobot 5

softerRobot :: ((String, Integer, Integer) -> t) -> t
softerRobot = setHp killerRobot 50

-- print function --

printRobot aRobot = aRobot (\(n,a,h) -> n ++ "attack:" ++ (show a) ++ "hp:" ++ (show h))

-- damage function --

damage aRobot attackDamage = aRobot (\(n,a,h) -> robot (n,a,h - attackDamage))

-- fight function --

fight aRobot defender = damage defender attack 
  where attack = if getHp aRobot > 10 then getAttack aRobot else 0

-- Rounds of fighting --

gentleGiantRound1 :: ((String, Integer, Integer) -> t) -> t
gentleGiantRound1 = fight killerRobot gentleGiant

killerRobotRound1 :: ((String, Integer, Integer) -> t) -> t
killerRobotRound1 = fight gentleGiant killerRobot

gentleGiantRound2 :: ((String, Integer, Integer) -> t) -> t
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1

killerRobotRound2 :: ((String, Integer, Integer) -> t) -> t
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1

gentleGiantRound3 :: ((String, Integer, Integer) -> t) -> t
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2

killerRobotRound3 :: ((String, Integer, Integer) -> t) -> t
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

-- Three round fights with simultaneous attacks --

slowRobotRound1 :: ((String, Integer, Integer) -> t) -> t
slowRobotRound1 = fight fastRobot slowRobot

fastRobotRound1 :: ((String, Integer, Integer) -> t) -> t
fastRobotRound1 = fight slowRobotRound1 fastRobot

slowRobotRound2 :: ((String, Integer, Integer) -> t) -> t
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1

fastRobotRound2 :: ((String, Integer, Integer) -> t) -> t
fastRobotRound2 = fight slowRobotRound1 fastRobotRound1

slowRobotRound3 :: ((String, Integer, Integer) -> t) -> t
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2

fastRobotRound3 :: ((String, Integer, Integer) -> t) -> t
fastRobotRound3 = fight slowRobotRound3 fastRobotRound2

--multi round function--
-- it was suggested to use nested lambdas --
-- how do i use recursive calls withing nested lambdas --

threeRoundFight robotA robotB rounds = 
    (\robotA robotB rounds -> 
    fight robotA robotB 
    (\robotA robotB -> 
        fight robotB robotA) rounds + 1)

-- threeOnOne f challenger = (\challenger combatants -> map (fight) combatants challenger )
--   where combatants = robot [("Mr.Friendly ", 10 , 300),("kill3r ", 25 , 200),("slowpoke " , 20 , 30)]
--         challenger = robot ("destroyer", 40 , 100)

combatants = [killerRobot,gentleGiant,slowRobot]

threeOnOne f challenger comb = map (fight challenger) comb 
 