import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Svg.Path exposing (..)
import Svg.Keyed

import Html.Attributes
import Html.Events
import Html exposing (..)
import List exposing (repeat, map)

stylesheet css = Html.node "style" [Html.Attributes.type_ "text/css"] [Html.text css]

main = Html.program {
    init = (initGame, Cmd.none),
    view = view,
    update = update,
    subscriptions = \_ -> Sub.none
    }

css = """
html {
    font-size: 16px;
    font-family: sans-serif;
}
main {
    padding: 1rem;
    text-align: center;
}
footer {
    margin-top: 4rem;
}
svg {
    max-width: 100vw;
    max-height: 70vh;
}

.light path {
    stroke-width: 0;
    stroke: #555;
    fill: black;
    transition: fill 1s;
}

.light.on path {
    fill: yellow;
}

.light.selected path {
    stroke: blue;
    stroke-width: 0.5;
    animation: select 0.1s;
}
@keyframes select {
    from {
        stroke-width: 0.2;
    }
}

"""

type Light = On | Off

type alias Circle = List Light

type Msg
 = Click Int
 | SetSize Int
 | SetHover Int
 | ClearHover

size : Circle -> Int
size = List.length

type Selection
 = NoSelection
 | Selected Int

type alias Game = 
    {
        circle: Circle,
        selection: Selection,
        hover: Maybe Int
    }

initCircle n = On :: (List.repeat (n-1) Off)

initGame : Game
initGame = 
    {
        circle = initCircle 12,
        selection = NoSelection,
        hover = Nothing
    }

noCmd = \x -> (x, Cmd.none)

update : Msg -> Game -> (Game, Cmd Msg)
update msg game = noCmd <| 
    case msg of
        Click i -> 
            case game.selection of
                NoSelection -> {game|selection=Selected i}
                Selected selected ->
                    if i==selected then 
                        {game|selection=NoSelection}
                    else if clickable game i then
                        let
                            game2 = toggle_circle game selected (circle_diff game.circle selected i)
                        in
                            {game2|selection = NoSelection}
                    else
                        game
        SetSize n -> {game|circle = initCircle n, selection = NoSelection}
        SetHover i -> {game|hover = Just i}
        ClearHover -> {game|hover = Nothing}
                        
toggle light = case light of
    On -> Off
    Off -> On
toggle_circle game start d = {game| circle = List.indexedMap (\i -> \l -> if (circle_diff game.circle start i) % d == 0 then toggle l else l) game.circle}
        

view : Game -> Html Msg
view game = 
    main_ [] 
        [
            stylesheet css,
            section [] [
                label [] 
                [
                    Html.text "Number of lights: ",
                    input [Html.Attributes.type_ "number", Html.Attributes.min "2", Html.Attributes.max "500", Html.Attributes.value (toString <| size game.circle), Html.Events.onInput (String.toInt >> Result.withDefault 6 >> SetSize)] []
                ]
            ],
            svg [viewBox "-12 -12 24 24"] [
                 g [class "circle"] (List.indexedMap (view_circle_segment game) game.circle)
            ],
            section [] [
                p [] [Html.text <| hover_description game]
            ],
            footer [] [
                p [] [Html.text "By ", Html.a [Html.Attributes.href "http://somethingorotherwhatever.com"] [Html.text "Christian Lawson-Perfect"]]
            ]
        ]

hover_description game =
    case (game.hover, game.selection) of
        (Just h, Selected i) ->
            let
                d = circle_diff game.circle i h
                
            in
                if clickable game h then
                    case d of
                        0 -> "Now click another light"
                        1 -> "Click to toggle every light"
                        _ -> "Click to toggle every "++(ordinal d)++" light"
                else
                    (toString d)++" doesn't divide "++(toString (size game.circle))
        (_,NoSelection) -> "Click a light to start toggling"
        (_,Selected i) -> "Now click another light"

ordinal n =
    let s = toString n in (++) s <|
        case (n%10) of
            1 -> "st"
            2 -> "nd"
            3 -> "rd"
            _ -> "th"

classList classes = class <| String.join " " (List.map Tuple.first <| List.filter Tuple.second classes)

circle_diff circle start i = 
    let
        n = size circle
        d = (n + i - start) % n
    in
        if d > n//2 then n - d else d

clickable game i =
    case game.selection of 
        NoSelection -> False
        Selected selected -> (selected /= i) && ((size game.circle) % (circle_diff game.circle selected i) == 0)


view_circle_segment : Game -> Int -> Light -> Html Msg
view_circle_segment game i light =
    let
        n = size game.circle
        fn = toFloat n
        fi = toFloat i
        gap = fn*30
        an1 = 2 * pi * (fi - 0.5 + fn/gap) / fn
        an2 = 2 * pi * (fi + 0.5 - fn/gap) / fn
        angle = 2*pi*(toFloat i)/(toFloat n)
        r1 = 10
        r2 = 5
        x1 = r1 * (cos an1)
        y1 = r1 * (sin an1)
        x2 = r1 * (cos an2)
        y2 = r1 * (sin an2)
        x3 = r2 * (cos an1)
        y3 = r2 * (sin an1)
        x4 = r2 * (cos an2)
        y4 = r2 * (sin an2)
        selected = game.selection == Selected i
        on = light == On
        outline = subpath (startAt (x3, y3)) closed [lineTo (x1,y1), arcTo (r1,r1) 0 (smallestArc, clockwise) (x2,y2), lineTo (x4,y4), arcTo (r2,r2) 0 (smallestArc, antiClockwise) (x3,y3)]
        push = 1
    in
        g 
            [
                classList [("light",True), ("selected",selected), ("on",on), ("clickable", clickable game i)], 
                onClick (Click i),
                onMouseOver (SetHover i),
                onMouseOut ClearHover
            ]
            [
                Svg.path [pathToAttribute [outline]] []
            ]
