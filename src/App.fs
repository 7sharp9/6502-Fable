module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Emish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

// MODEL

type Msg =
| Reset
| IncrementPC

type CPU6502 =
    { A: byte; X: byte; Y: byte; PC: byte; SP:byte; P: byte }
    member x.C = x.P &&& 0x1uy = 0uy
    member x.Z = x.P &&& 0x2uy = 0uy
    member x.I = x.P &&& 0x4uy = 0uy
    member x.D = x.P &&& 0x8uy = 0uy
    member x.B = x.P &&& 0x10uy = 0uy
    member x.V = x.P &&& 0x40uy = 0uy
    member x.N = x.P &&& 0x80uy = 0uy

let init() = { A = 0uy; X = 0uy; Y = 0uy; PC = 0uy; SP = 0uy; P = 0uy}

// UPDATE

let update (msg:Msg) (model:CPU6502) =
    match msg with
    | Reset -> init()
    | IncrementPC -> { model with PC = model.PC + 1uy}

// VIEW (rendered with React)

let header dispatch =
  nav [ Class "navbar navbar-inverse"
        Role "navigation"
        Style [ BackgroundColor "#428BCA"
                Border "0px"
                BorderRadius "0px" ] ]
    [ div [ Class "container" ]
        [ div [ Class "navbar-header" ]
            [ div [ Class "btn-group" ]
                [ button [ Type "button"
                           Class "btn btn-success navbar-btn" ]
                    [ span [ Class "glyphicon glyphicon-play" ]
                        [ str "Run" ] ]
                  button [ Type "button"
                           Class "btn btn-default navbar-btn" ]
                    [ span [ Class "glyphicon glyphicon-stop" ]
                        [ str "Stop" ] ]
                  button [ Type "button"
                           Class "btn btn-default navbar-btn"
                           OnClick (fun _ -> dispatch IncrementPC ) ]
                    [ span [ Class "glyphicon glyphicon-forward" ]
                        [ str "Step" ] ] ]
              button [ Type "button"
                       Class "btn btn-default navbar-btn"
                       OnClick (fun _ -> dispatch Reset ) ]
                [ str "Reset" ] ]
          div [ Class "navbar-header navbar-right" ]
            [ a [ Class "navbar-brand"
                  Style [ Color "#FFF" ] ]
                [ str "6502 Emulator" ] ] ] ]

let cpuElement data =
    td [ ]
        [ div [ Style [ Margin "auto" ] ]
            [ small [ ]
                [ str data ] ] ]

let cpuRegisterColumn columnTitle =
  th [ Style [ TextAlign "center" ] ] [ str columnTitle ]

let view (model:CPU6502) dispatch =
  body []
    [header dispatch
     div [ Class "container" ]
      [ div [ Class "row" ]
          [ div [ Class "clearfix visible-xs visible-sm" ]
              [ ]
            div [ Class "col-md-6 col-md-6" ]
              [ div [ Class "panel panel-default" ]
                  [ div [ Class "panel-heading" ]
                      [ h4 [ Class "panel-title" ]
                          [ str "CPU" ] ]
                    div [ Class "panel-body" ]
                      [ p [ Class "text-muted" ]
                          [ str "Registers / Flags" ]
                        table [ Class "table table-condensed" ]
                          [ thead [ ]
                              [ tr [ ]
                                  [ cpuRegisterColumn "A"
                                    cpuRegisterColumn "X"
                                    cpuRegisterColumn "Y"
                                    cpuRegisterColumn "PC"
                                    cpuRegisterColumn "SP"
                                    cpuRegisterColumn "C"
                                    cpuRegisterColumn "Z"
                                    cpuRegisterColumn "I"
                                    cpuRegisterColumn "D"
                                    cpuRegisterColumn "B"
                                    cpuRegisterColumn "V"
                                    cpuRegisterColumn "N" ] ]
                            tbody [ ]
                              [ tr [ Style [ TextAlign "center" ] ]
                                  [ cpuElement (sprintf "%02X" model.A)
                                    cpuElement (sprintf "%02X" model.X)
                                    cpuElement (sprintf "%02X" model.Y)
                                    cpuElement (sprintf "%02X" model.PC)
                                    cpuElement (sprintf "%A" model.SP)
                                    cpuElement (sprintf "%b" model.C)
                                    cpuElement (sprintf "%b" model.Z)
                                    cpuElement (sprintf "%b" model.I)
                                    cpuElement (sprintf "%b" model.D)
                                    cpuElement (sprintf "%b" model.B)
                                    cpuElement (sprintf "%b" model.V)
                                    cpuElement (sprintf "%b" model.N)
                                  ] ] ] ] ]
      ] ] ] ]

// App
Program.mkSimple init update view
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run
