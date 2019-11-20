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
type ShiftOpenCloseStep = CountingCash | ConfirmVariance | ExplainVariance | BankDrop of int
type Cash = {
        cashRegister: int option
        safe: int option
    } with static member fresh = {
            cashRegister = None
            safe = None
        }

type Model = {
    page: Page
    shiftStep: ShiftOpenCloseStep option
    cart: Product list
    cash: Cash
    declarations: Cash list
    }
    with
    static member fresh =
        {
        page = Closed
        cart = []
        cash = { cashRegister = Some 600; safe = Some 3000 }
        declarations = []
        shiftStep = None
        }
type PaymentMethod = Cash | Card
type Msg = BeginOpenOrClose | ContinueWizard | Goto of Page | AddProduct of Product | Pay of PaymentMethod | BankDrop of int | DeclareCash of int option | DeclareSafe of int option | Override

let cost = function
    | Word -> 50
    | Office -> 100
    | Windows -> 30
    | Xbox -> 360

// an input-like component which stores state locally until blur
let localInput =
    let component' =
        FunctionComponent.Of(
            fun (value, props, onChange) ->
                let v = Hooks.useState value
                Hooks.useEffect(
                    fun () ->
                        v.update(value) |> ignore // when value changes externally, make sure we detect that!
                    , [|value|] )
                let lst : IHTMLProp list = [
                    yield upcast Value v.current
                    yield upcast OnChange(fun e -> if e <> null then v.update(e.Value))
                    yield upcast OnKeyDown(fun e -> if e.keyCode = 13. then
                                                        e.preventDefault()
                                                        onChange v.current
                                                    )
                    yield upcast OnBlur(fun _ -> onChange v.current)
                    yield! props
                    ]
                input lst
            , memoizeWith = (fun (v1, p1, _) (v2, p2, _) -> v1 = v2))
    (fun value (props: seq<IHTMLProp>) onChange -> component'(value, props, onChange))

let declarationPage (m:Model) dispatch =
    let decl = m.declarations.Head
    let adapt = function true, v -> Some v | _ -> None
    div[][
        h2 [] [str (if m.declarations.Length = 1 then "Declare cash" else "Please re-count")]
        form[ClassName "border"; OnSubmit (fun e -> e.preventDefault(); dispatch ContinueWizard)][
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
        ]

let valueOf (c:Cash) = (defaultArg c.cashRegister 0) + (defaultArg c.safe 0)
let view (m:Model) dispatch =
    div[][
        div[ClassName "border"][
            let button msg txt = button[OnClick (thunk1 dispatch msg)][str txt]
            match m.shiftStep with
            | Some CountingCash ->
                declarationPage m dispatch
            | Some ConfirmVariance ->
                div[][
                    if m.declarations.Length > 1 then
                        h2[][str "Count Total is different than expected. Recount or perform declaration?"]
                    else
                        h2[][str "Count Total is different than expected. Please recount."]
                    br[]
                    div[][str <| sprintf "Registers $%d" m.declarations.Head.cashRegister.Value]
                    br[]
                    div[][str <| sprintf "Registers $%d" m.declarations.Head.safe.Value]
                    if m.declarations.Length > 1 then
                        h3[][str "Prior counts"]
                        table[][
                            tr[][th[][str "Register"];th[][str "Safe"]]
                            for d in m.declarations do
                                tr[][th[][str <| sprintf "$%d" (defaultArg d.cashRegister 0)];th[][str <| sprintf "$%d" (defaultArg d.safe 0)]]

                            ]
                    br[]
                    button ContinueWizard "Recount"
                    if m.declarations.Length > 1 then
                        h2[][str "Count Total is different than expected. Recount or perform declaration?"]
                        br[]
                        br[]
                        button Override "Declare with variance"
                ]
            | Some ExplainVariance ->
                div[][
                    h2[][str "Explain variance"]
                    br[]
                    h3[][str <| sprintf "Expected: $%d" (valueOf m.cash)]
                    h3[][str <| sprintf "Counted: $%d" (valueOf m.declarations.Head)]
                    let variance = valueOf m.declarations.Head - valueOf m.cash
                    h3[Style[Color (if variance < 0 then "red" else "auto")]][str <| sprintf "Variance: $%d" variance]
                    br[]
                    br[]
                    button ContinueWizard "Log variance"
                    ]
            | Some (ShiftOpenCloseStep.BankDrop bd) ->
                form[OnSubmit (fun e -> e.preventDefault(); dispatch ContinueWizard)][
                    div[][str "How much money do you want to bank drop?"]
                    input[Value bd; OnChange (fun e -> e.Value |> System.Int32.Parse |> BankDrop |> dispatch)]
                    Fable.React.Standard.button[Type "submit"][str "OK"]
                    ]
            | None ->
                if m.page = Closed then
                    button BeginOpenOrClose "Open store"
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
                            button BeginOpenOrClose "Close store"
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
    match m.shiftStep with
    | Some (ShiftOpenCloseStep.BankDrop v) ->
        match msg with
        | BankDrop newValue ->
            { m with shiftStep = Some (ShiftOpenCloseStep.BankDrop newValue) }
        | ContinueWizard ->
            { m with shiftStep = None; cash = m.cash |> addCash -v; page = Closed }
        | _ -> m
    | Some (ShiftOpenCloseStep.CountingCash) ->
        match msg with
        | DeclareCash n -> { m with declarations = match m.declarations with h::t -> { h with cashRegister = n } :: t | v -> v }
        | DeclareSafe n -> { m with declarations = match m.declarations with h::t -> { h with safe = n } :: t | v -> v }
        | ContinueWizard ->
            let decl = m.declarations.Head
            if valueOf m.cash = valueOf decl then
                if m.page = Closed then // no bank drop on store open
                    { m with declarations = []; page = Cart; shiftStep = None }
                else
                    let bankDrop = valueOf m.cash - 3600
                    { m with declarations = []; shiftStep = Some (ShiftOpenCloseStep.BankDrop bankDrop) }
            else
                { m with shiftStep = Some ShiftOpenCloseStep.ConfirmVariance }
        | _ -> m
    | Some ConfirmVariance ->
        match msg with
        | ContinueWizard -> { m with declarations = Cash.fresh::m.declarations; shiftStep = Some CountingCash }
        | Override -> { m with shiftStep = Some ExplainVariance }
        | _ -> m
    | Some ExplainVariance ->
        match msg with
        | ContinueWizard ->
            let cashAfterVariance = m.declarations.Head
            if m.page = Closed then
                { m with declarations = []; cash = cashAfterVariance; shiftStep = None; page = Cart } // no bank drop on open
            else
                { m with declarations = []; cash = cashAfterVariance; shiftStep = ShiftOpenCloseStep.BankDrop (valueOf cashAfterVariance - 3600) |> Some }
        | _ -> m
    | None ->
        match msg with
        | Goto page -> { m with page = page }
        | AddProduct p -> { m with cart = m.cart @ [p]; page = Cart }
        | Pay method ->
            { m with cart = []; cash = m.cash |> addCash (if method = Card then 0 else m.cart |> List.sumBy cost) }
        | BeginOpenOrClose ->
            { m with declarations = [Cash.fresh]; shiftStep = Some(CountingCash) }
        | _ -> m

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
