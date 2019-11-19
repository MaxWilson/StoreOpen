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
        cashRegister: int option
        safe: int option
    } with static member fresh = {
            cashRegister = None
            safe = None
        }

type Model = {
    page: Page
    cart: Product list
    cash: Cash
    declarations: Cash list
    bankDrop: int option
    }
    with
    static member fresh =
        {
        page = Closed
        cart = []
        cash = { cashRegister = Some 600; safe = Some 3000 }
        declarations = []
        bankDrop = None
        }
type PaymentMethod = Cash | Card
type Msg = CountCash | Goto of Page | AddProduct of Product | Pay of PaymentMethod | BankDrop of int | DeclareCash of int option | DeclareSafe of int option | Override
let declarationPage (m:Model) dispatch =
    let decl = m.declarations.Head
    let adapt = function true, v -> Some v | _ -> None
    div[][
        h2 [] [str (if m.declarations.Length = 1 then "Declare cash" else "Please re-count")]
        form[ClassName "border"; OnSubmit (fun e -> e.preventDefault(); dispatch CountCash)][
            div[][str "Money in registers"]
            input[Value (match decl.cashRegister with Some v -> v.ToString() | None -> "");
                    OnChange (fun e -> System.Int32.TryParse e.Value |> adapt |> DeclareCash |> dispatch)]
            div[][str "Money in safe"]
            input[Value (match decl.safe with Some v -> v.ToString() | None -> "");
                    OnChange (fun e -> System.Int32.TryParse e.Value |> adapt |> DeclareSafe |> dispatch)]
            br[]
            br[]
            Fable.React.Standard.button[Type "submit"][str "Declare"]
            ]
        if m.declarations.Length > 1 then
            h3[][str "Prior counts"]
            table[][
                tr[][th[][str "Register"];th[][str "Safe"]]
                for d in m.declarations.Tail do
                    tr[][th[][str <| sprintf "$%d" (defaultArg d.cashRegister 0)];th[][str <| sprintf "$%d" (defaultArg d.safe 0)]]

                ]
            br[]
            br[]
            button[OnClick(fun _ -> dispatch Override)][str "Declare variance!"]
        ]
let cost = function
    | Word -> 50
    | Office -> 100
    | Windows -> 30
    | Xbox -> 360
let view (m:Model) dispatch =
    div[][
        div[ClassName "border"][
            let button msg txt = button[OnClick (thunk1 dispatch msg)][str txt]
            if m.declarations.Length > 0 then
                declarationPage m dispatch
            elif m.bankDrop.IsSome then
                form[OnSubmit (fun e -> e.preventDefault(); dispatch CountCash)][
                    div[][str "How much money do you want to bank drop?"]
                    input[Value m.bankDrop.Value; OnChange (fun e -> e.Value |> System.Int32.Parse |> BankDrop |> dispatch)]
                    Fable.React.Standard.button[Type "submit"][str "OK"]
                    ]
            elif m.page = Closed then
                button CountCash "Open store"
            else
                div[ClassName "header"] [
                    button (Goto Home) "Home"
                    button (Goto Product) "Products"
                    button (Goto Cart) "Cart"
                    ]
                div[][
                    match m.page with
                    | Home ->
                        h2 [][str "Home screen"]
                        button CountCash "Close store"
                    | Product ->
                        h2 [][str "Products"]
                        for p in [Word; Office; Xbox; Windows] do
                            button (AddProduct p) (sprintf "%A" p)
                    | _ ->
                        div[][
                            h2 [][str "Your cart"]
                            ul[][
                                for p in m.cart ->
                                    li[][str (sprintf "%A: $%d" p (cost p))]]
                            div[][str <| sprintf "Total: $%d" (m.cart |> List.sumBy cost)]
                            button (Goto Product) "Add products"
                            button (Pay Cash) "Pay cash"
                            button (Pay Card) "Pay with card"
                        ]
                    ]
            ]
        div[ClassName "actual"][
            div[][str <| sprintf "Expected cash in register: $%d" (defaultArg m.cash.cashRegister 0)]
            div[][str <| sprintf "Expected cash in safe: $%d" (defaultArg m.cash.safe 0)]
            ]
        ]

let addCash (amt:int) (cash:Cash) =
    { cash with cashRegister = Some (defaultArg cash.cashRegister 0 + amt) }

let init _ = Model.fresh
let update msg (m:Model) =
    let valueOf (c:Cash) = (defaultArg c.cashRegister 0) + (defaultArg c.safe 0)
    match msg with
    | Goto page -> { m with page = page }
    | AddProduct p -> { m with cart = m.cart @ [p]; page = Cart }
    | Pay method ->
        { m with cart = []; cash = m.cash |> addCash (if method = Card then 0 else m.cart |> List.sumBy cost) }
    | Override ->
        let cashAfterVariance = m.declarations.Tail.Head
        { m with declarations = []; cash = cashAfterVariance; bankDrop = valueOf cashAfterVariance - 3600 |> Some }
    | CountCash ->
        match m.bankDrop with
        | None ->
            match m.declarations with
            | decl::_ ->
                if valueOf m.cash = valueOf decl then
                    if m.page = Closed then // no bank drop on store open
                        { m with declarations = []; page = Cart }
                    else
                        let bankDrop = valueOf m.cash - 3600
                        { m with declarations = []; bankDrop = Some bankDrop }
                else
                    { m with declarations = Cash.fresh::m.declarations }
            | [] -> { m with declarations = [Cash.fresh] }
        | Some drop ->
            { m with cash = m.cash |> addCash -drop; bankDrop = None;
                    page = Closed }
    | BankDrop n ->
        { m with bankDrop = Some n; }
    | DeclareCash n -> { m with declarations = match m.declarations with h::t -> { h with cashRegister = n } :: t | v -> v }
    | DeclareSafe n -> { m with declarations = match m.declarations with h::t -> { h with safe = n } :: t | v -> v }

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkSimple init update view
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReact "elmish-app"
|> Program.run
