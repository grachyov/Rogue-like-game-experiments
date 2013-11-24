import System.Console.ANSI
data RelationType = Enemy | Friend | Unknown | Neutral | Player deriving(Show)
data EntityType = EntityType{entitySym::Char
                            ,relation::RelationType
                            ,hp::Int
                            ,mp::Int
                            ,dmg::Int} deriving(Show)

data WallType = WallType {wallSym::Char
                         ,breackable :: Bool
                         } deriving(Show)
data RoadType = RoadType {roadSym::Char
                         , walkable :: Bool
                         } deriving(Show)
data DoorType = DoorType {doorSym::Char
                         , opened :: Bool
                         } deriving(Show)
data PlantType = PlantType {plantSym::Char
                         , destroyable :: Bool
                         } deriving(Show)

data Slot = Entity EntityType | Wall WallType | Road RoadType | Door DoorType | Plant PlantType deriving(Show) 
cDefault,cBlack,cRed,cGreen,cYellow,cBlue,cMagenta,cCyan,cWhite,cPrefix,cSuffix :: String
cDefault = "0"
cBlack = "30"
cRed = "31"
cGreen = "32"
cYellow = "33"
cBlue = "34"
cMagenta = "35"
cCyan = "36"
cWhite = "37"

cPrefix = "\x1b["
cSuffix = "m"

colorProduct :: String -> String
colorProduct c = cPrefix ++ c ++ cSuffix

snake,goblin,rat,bat,player :: EntityType
snake =  EntityType 'S' Enemy 10 10 1
goblin = EntityType 'g' Enemy 5 0 2
rat = EntityType 'r' Enemy 3 0 1
bat = EntityType 'b' Enemy 7 0 2
player = EntityType '@' Player 10 10 2

type Table = [[Slot]]

getSym :: Slot -> Char
getSym slot = case slot of 
    Entity e -> entitySym e
    Wall w -> wallSym w
    Road r -> roadSym r
    Door d -> doorSym d
    Plant p -> plantSym p

produceTableFrom :: [String] -> Table
produceTableFrom = map produceFromStr
    where produceFromStr = map sym
            where sym s' = case s' of
                    '#' -> Wall (WallType s' False)
                    'W' -> Wall (WallType s' True)
                    '.' -> Road (RoadType s' True)
                    ',' -> Road (RoadType s' False)
                    'g' -> Entity goblin
                    'S' -> Entity snake
                    'r' -> Entity rat
                    'b' -> Entity bat
                    '@' -> Entity player
                    'Y' -> Plant $ PlantType s' False
                    _ -> Wall $ WallType s' False

colorizeSlot :: Slot -> String
colorizeSlot (Entity e) = case relation e of
    Friend ->  colorProduct cGreen
    Player -> colorProduct cMagenta
    Enemy -> colorProduct cRed
    Unknown -> colorProduct cYellow
    Neutral -> colorProduct cBlue
colorizeSlot (Wall w) = colorProduct cDefault
colorizeSlot (Road r) = colorProduct cDefault
colorizeSlot (Door d) = colorProduct cDefault
colorizeSlot (Plant p) = colorProduct cGreen

reduceTable2Strs :: Table -> [String]
reduceTable2Strs = map $ foldr ((++) . (\ y -> colorizeSlot y ++ [getSym y])) (colorProduct cDefault)

reduceStrs :: [String] -> String
reduceStrs = foldr (\ x y -> x ++ ('\n' : y)) "\n"

visibleRadius :: Int
visibleRadius = 5

getViewedChunk :: Table -> Int -> Int -> Table
getViewedChunk table x y = map (cut y) (cut x table)
    where cut w z = take (2 * visibleRadius + 1) (drop (w - visibleRadius - 1) z) 

main :: IO()
main = do 
    putStrLn $ reduceStrs $ reduceTable2Strs (getViewedChunk (produceTableFrom (map (\x ->map (\_->if x `mod` 2 == 0 then '.' else 'Y') (show x)) ([10000000000000000..10000000000001000]::[Integer]))) 13 11) 
    print player
