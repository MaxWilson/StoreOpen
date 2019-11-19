module App.View

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.React
open Fable.React.Props

importAll "../sass/main.sass"

let thunk value _ = value
let thunk1 f arg _ = f arg

type Page = Closed | Home | Cart | Product
type Product = Office | Word | Windows | Xbox
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
type PaymentMethod = Cash | Card
type Msg = CountCash | Goto of Page | AddProduct of Product | Pay of PaymentMethod
let declarationPage (m:Model) dispatch =
    div[][
        form[OnSubmit <| thunk1 dispatch CountCash] [
            button[Type "submit"][str "OK"]
            ]
        ]
let cost = function
    | Word -> 50
    | Office -> 100
    | Windows -> 30
    | Xbox -> 360
let view (m:Model) dispatch =
    div[][
        let button msg txt = button[OnClick (thunk1 dispatch msg)][str txt]
        if m.page = Closed then
            if m.isDeclaring then
                declarationPage m dispatch
            else
                button CountCash "Open store"
        else
            div[ClassName "header"] [
                button (Goto Home) "Home"
                button (Goto Product) "Products"
                button (Goto Cart) "Cart"
                ]
            div[][
                match m.page with
                | Home -> button CountCash "Close store"
                | Product ->
                    h2 [][str "Products"]
                    button (AddProduct Word) "Word"
                    button (AddProduct Office) "Word"
                | _ ->
                    div[][
                        h2 [][str "Your cart"]
                        ul[][
                            for p in m.cart ->
                                li[][str (sprintf "%A: $%d" p (cost p))]]
                        div[][str <| sprintf "Total: $%d" (m.cart |> List.sumBy cost)]
                        button (Pay Cash) "Pay cash"
                        button (Pay Card) "Pay with card"
                    ]

                ]
        ]

let init _ = Model.fresh, Cmd.Empty
let update msg (m:Model) =
    m, Cmd.Empty

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update view
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReact "elmish-app"
|> Program.run
