module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.React.Browser

importAll "../sass/main.sass"

let thunk value _ = value
let thunk1 f arg _ = f arg

type Page = Closed | Home | Cart | Product
type Product = Office | Word
type Cash = {
        cashRegister: int
        safe: int
    } with static member fresh = {
            cashRegister = 600
            safe = 3600
        }

type Model = {
    page: Page
    isDeclaring: bool
    cart: Product list
    cash: Cash
    declarations: Cash list
    }
    with static member fresh = {
        page = Closed
        isDeclaring = true
        cart = []
        cash = Cash.fresh
        declarations = []
        }
type Msg = OpenStore
let declarationPage (m:Model) onDone dispatch =
    div[][
        form[OnSubmit <| thunk1 dispatch onDone] [
            button[Type "submit"]["OK"]
            ]
        ]

let view (m:Model) dispatch =
    div[][
        if m.page = Closed then
            if m.isDeclaring then
                declarationPage
            button[OnClick (thunk1 dispatch OpenStore)]["Open store"]
        ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update root
|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReact "elmish-app"
|> Program.run
