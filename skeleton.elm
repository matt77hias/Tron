-- Matthias Moulin

import Color
import Debug
import Text
import Keyboard
import Set

-- DATA & TYPES --
data Player = Player String
type Dir = (Keyboard.KeyCode, Keyboard.KeyCode, Keyboard.KeyCode, Keyboard.KeyCode)
data Configuration = Configuration Player Dir Color
data ConfigState = ConfigState [Configuration]

type Pos = (Float, Float)
type Vel = (Float, Float)
type Tail = [Pos]
data LightState = LightState Pos Vel Orientation Tail
data GameState = Playing Bool [LightState]
               | Ended Bool Player
               | Init
               
data KeybInput = KeybInput [Keyboard.KeyCode] Bool
data GameInput = GameInput [(Int, Int)] Bool

-- DEFAULTS --
initialConfigState : ConfigState
initialConfigState =
    let config1 = (Configuration (Player "Player 1") (87, 83, 65, 68) Color.lightBlue)
        config2 = (Configuration (Player "Player 2") (38, 40, 37, 39) Color.orange)
    in (ConfigState [config1, config2])
--for zsqd movement use (90, 83, 81, 68)
--for wsad movement use (87, 83, 65, 68)  
    
initialGameState : GameState
initialGameState =
    let light1 = (LightState (-initPosX, 0) (initSpeedX, initSpeedY) E [])
        light2 = (LightState ( initPosX, 0) (initSpeedX, initSpeedY) W [])
    in (Playing True [light1, light2])

initPosX : Float
initPosX = (toFloat width) / 2 - 50
initPosY : Float
initPosY = (toFloat height) / 2 - 50
initSpeedX : Float
initSpeedX = (toFloat 50) / 10
initSpeedY : Float
initSpeedY = (toFloat 50) / 10

maxTail = 256.0
    
-- INPUT LOGIC --
heartbeat = every (second/30)

keybInput : Signal KeybInput
keybInput =
    let realInput = lift2 KeybInput Keyboard.keysDown Keyboard.space
        sampledInput = sampleOn heartbeat realInput
    in lift (Debug.watch "keybInput") sampledInput

gameInput : ConfigState -> KeybInput -> GameInput
gameInput cs (KeybInput keys space) = (GameInput (dirs cs keys) space)
    
dirs : ConfigState -> [Keyboard.KeyCode] -> [(Int, Int)]
dirs (ConfigState configs) keys =
    let keyset = (Set.fromList keys)
    in map (\ (Configuration _ dir _) -> (dir_to_offsets dir keyset)) configs
 
dir_to_offsets : Dir -> Set.Set comparable -> (Int, Int)
dir_to_offsets (up, dn, lx, rx) keys =
    if 
    | Set.member up keys -> (0,1)
    | Set.member dn keys -> (0,-1)
    | Set.member lx keys -> (-1,0)
    | Set.member rx keys -> (1,0)
    | otherwise -> (0,0)

-- STATE LOGIC --
gameState_step : (ConfigState, GameInput) -> GameState -> GameState
gameState_step (configState, (GameInput dirs space)) gameState =
    case space of
        True -> case gameState of
                    (Playing sp lightStates) -> case sp of
                                                    True -> check_winner space configState (Playing space (zipWith lightState_step dirs lightStates))
                                                    False -> initialGameState
                    (Ended sp player) -> case sp of
                                            True -> (Ended space player)
                                            False -> initialGameState
                    _ -> initialGameState
        False -> case gameState of
                    (Playing sp lightStates) -> check_winner space configState (Playing space (zipWith lightState_step dirs lightStates))                     
                    (Ended sp player) -> (Ended space player)
                    _ -> gameState

--TODO: CLEAN UP FOR MORE PLAYERS
check_winner : Bool -> ConfigState -> GameState -> GameState
check_winner space cs gs = 
    let (ConfigState ((Configuration p1 _ _)::(Configuration p2 _ _)::[])) = cs
        (b1::b2::[]) = check_lost gs
    in case (b1 && b2) of
        True -> Ended space (Player "None")
        False -> case b1 of
                    True -> Ended space p2
                    False -> case b2 of
                                True -> Ended space p1
                                False -> gs
        
lightState_step : (Int, Int) -> LightState -> LightState
lightState_step (x, y) (LightState pos vel o tail) = 
    let orient = (to_orientation (x, y) o)
        (nx, ny) = (from_orientation orient)
        npos = (update_pos pos vel (nx, ny))
        ntail = (update_tail pos tail)
    in (LightState npos vel orient ntail)

-- Position logic
update_pos : Pos -> Vel -> (Int, Int) -> Pos
update_pos (xPos, yPos) (xVel, yVel) (x, y) = 
    (xPos + xVel * (toFloat x), yPos + yVel * (toFloat y))

-- Tail logic --
update_tail : Pos -> Tail -> Tail
update_tail pos tail =
    let ntail = tail_add pos tail
        cut = (tail_length ntail) - maxTail
    in case cut > 0 of
        True -> tail_cut cut ntail
        False -> ntail
            
tail_add : Pos -> Tail -> Tail
tail_add pos3 tail =
    let rtail = reverse tail
    in case rtail of
        [] -> [pos3]
        (pos::[]) -> (pos::pos3::[])
        (pos2::pos1::poss) -> (reverse poss) ++ (tail_component_add pos1 pos2 pos3)

tail_component_add (xPos1, yPos1) (xPos2, yPos2) (xPos3, yPos3) = 
    case ((xPos1 == xPos2) && (xPos2 == xPos3)) of
        True -> (xPos1, yPos1)::(xPos1, yPos3)::[]
        False ->  case ((yPos1 == yPos2) && (yPos2 == yPos3)) of
                     True -> (xPos1, yPos1)::(xPos3, yPos1)::[]
                     False -> (xPos1, yPos1)::(xPos2, yPos2)::(xPos3, yPos3)::[]

tail_cut : Float -> Tail -> Tail
tail_cut cut tail =
    case tail of
        [] -> []
        (pos::[]) -> (pos::[])
        (pos1::pos2::poss) ->
            case cut <= 0 of
                True -> (pos1::pos2::poss)
                False ->    let diff = cut - (tail_component_length pos1 pos2)
                            in case diff == 0.0 of
                                True -> (pos2::poss)
                                False -> case diff > 0 of
                                    True -> tail_cut diff (pos2::poss)
                                    False -> let npos1 = clip cut pos1 pos2
                                             in (npos1::pos2::poss)

clip : Float -> Pos -> Pos -> Pos
clip cut (xPos1, yPos1) (xPos2, yPos2) =
   case (xPos1 == xPos2) of
        True -> (xPos1, (clipf cut yPos1 yPos2))
        False ->  case (yPos1 == yPos2) of
                     True -> ((clipf cut xPos1 xPos2), yPos1)
                     False -> (0.0, 0.0)

clipf : Float -> Float -> Float -> Float
clipf cut p1 p2 =
    case p1 < p2 of
        True  -> (p1 + cut)
        False -> (p1 - cut)   

tail_length : Tail -> Float
tail_length tail = tail_length' tail 0.0

tail_length' : Tail -> Float -> Float
tail_length' tail acc =
    case tail of
        [] -> acc
        (_::[]) -> acc
        (pos1::pos2::poss) ->
            let nacc = acc + (tail_component_length pos1 pos2)
            in (tail_length' (pos2::poss) nacc)

tail_component_length : Pos -> Pos -> Float
tail_component_length (xPos1, yPos1) (xPos2, yPos2) = 
    case (xPos1 == xPos2) of
        True -> abs(yPos1 - yPos2)
        False ->  case (yPos1 == yPos2) of
                     True -> abs(xPos1 - xPos2)
                     False -> sqrt ((xPos1 - xPos2) * (xPos1 - xPos2) + (yPos1 - yPos2) * (yPos1 - yPos2))

from_orientation : Orientation -> (Int, Int)
from_orientation o =
    case o of
        N -> (0, 1)
        E -> (1, 0)
        S -> (0, -1)
        W -> (-1, 0)
  
to_orientation : (Int, Int) -> Orientation -> Orientation
to_orientation (x, y) o =
    case x of
        1 -> E
        (-1) -> W
        _ -> case y of
            1 -> N
            (-1) -> S
            _ -> o

-- collision logic --
check_lost : GameState -> [Bool]
check_lost (Playing _ lightStates) =
    let ts = map (\ (LightState _ _ _ t) -> t) lightStates
    in map (\ (LightState pos _ o _) -> (detect_collision pos o ts)) lightStates
    
detect_collision : Pos -> Orientation -> [Tail] -> Bool
detect_collision pos o ts =
    let r = rectangle pos o     
    in ((detect_board_collision r) || (detect_tails_collision r ts))
            
rectangle : Pos -> Orientation -> (Float, Float, Float, Float)
rectangle (xPos, yPos) o = 
    let w = toFloat playerW
        h = toFloat playerH
    in case o of
        N -> (xPos - h/2, yPos, h, w)
        E -> (xPos, yPos - h/2, w, h)
        S -> (xPos - h/2, yPos - w, h, w)
        W -> (xPos - w, yPos - h/2, w, h)          
    
detect_board_collision : (Float, Float, Float, Float) -> Bool
detect_board_collision (x, y, wt, ht) = 
    let w = (toFloat width) / 2
        h = (toFloat height) / 2
    in (x <= -w || w <= x || y <= -h || h <= y || (x+wt) <= -w || w <= (x+wt) || (y+ht) <= -h || h <= (y+ht))

detect_tails_collision : (Float, Float, Float, Float) -> [Tail] -> Bool
detect_tails_collision r ts =
    case ts of
        [] -> False
        (tail::tails) ->    let collision = (detect_tail_collision r tail)
                            in case collision of
                                True -> True
                                False -> detect_tails_collision r tails
    
detect_tail_collision : (Float, Float, Float, Float) -> Tail -> Bool
detect_tail_collision (x, y, wt, ht) tail =
    case tail of
        [] -> False
        (_::[]) -> False
        (pos1::pos2::poss) ->
            let collision = ((crossesH x y wt pos1 pos2) || (crossesV x y ht pos1 pos2) || (crossesV (x+wt) y ht pos1 pos2) || (crossesH x (y+ht) wt pos1 pos2))
            in case collision of
                True -> True
                False -> (detect_tail_collision (x, y, wt, ht) (pos2::poss))

crossesH : Float -> Float -> Float -> Pos -> Pos -> Bool
crossesH x y w (xPos1, yPos1) (xPos2, yPos2) = ((x <= xPos1) && (xPos1 <= x+w) && (x <= xPos2) && (xPos2 <= x+w) && (betweenf y yPos1 yPos2))

crossesV : Float -> Float -> Float -> Pos -> Pos -> Bool
crossesV x y h (xPos1, yPos1) (xPos2, yPos2) = ((y <= yPos1) && (yPos1 <= y+h) && (y <= yPos2) && (yPos2 <= y+h) && (betweenf x xPos1 xPos2))
                                 
betweenf : Float -> Float -> Float -> Bool
betweenf f f1 f2 =
    case (f1 <= f2) of
        True -> (f1 <= f && f <= f2)
        False -> (f2 <= f && f <= f1)
                                
-- UI LOGIC --
showGameState : ConfigState -> GameState -> Element
showGameState (ConfigState configs) gameState =
    case gameState of 
        Init -> (plainText "Press space to start a new game")
        (Ended _ (Player name)) -> plainText (name ++ " has won!")
        (Playing _ lightStates) -> let forms = [filled black (rect width height)] ++ (zipWith showLightState lightStates configs)
                                 in collage width height forms

showLightState : LightState -> Configuration -> Form
showLightState (LightState pos _ o tail) (Configuration _ _ color) =
    group [(showPlayer' color pos o), (showTail color tail)]

-- MAIN --
cState : Signal ConfigState
cState = constant initialConfigState

gInput : Signal GameInput
gInput = lift2 gameInput cState keybInput

gState : Signal GameState
gState = lift (Debug.watch "Game state") (foldp gameState_step Init (lift2 (,) cState gInput))

main = lift2 showGameState cState gState

-- default values
width = 1024
height = 768

playerW = 64
playerH = 16

data Orientation = N | E | S | W
-- example use: showPlayer' Color.lightBlue (10, 10) N
showPlayer' : Color -> Pos -> Orientation -> Form
showPlayer' color (x, y) o =
    let fw = toFloat playerW
        (xOffset, yOffset, degs) = case o of
                    N -> (0, fw / 2, 90)
                    E -> (fw / 2, 0, 0)
                    S -> (0, -fw / 2, -90)
                    W -> (-fw / 2, 0, 180)
    in rect (fw) (toFloat playerH) 
        |> outlined (solid color)
        |> move (x, y)
        |> move (xOffset, yOffset)
        |> rotate (degrees degs) 

tailWidth = 2

-- example use: showTail Color.lightBlue [(10, 10), (20, 10)]
showTail : Color -> [(Float, Float)] -> Form
showTail color positions = 
    traced { defaultLine |
        width <- tailWidth
        , color <- color
        } (path positions)
