(* Define the types for Stock and Portfolio *)
type stock = {
  symbol : string;
  quantity : int;
  purchase_price : float;
  current_price : float;
}

type portfolio = stock list

(* 1. Add a Stock to the Portfolio *)
let add_stock (p : portfolio) (s : stock) : portfolio =
  s :: p

(* 2. Remove a Stock by its symbol *)
let remove_stock (p : portfolio) (symbol : string) : portfolio =
  List.filter (fun s -> s.symbol <> symbol) p

(* 3. Update the price of a Stock in the Portfolio *)
let update_price (p : portfolio) (symbol : string) (new_price : float) : portfolio =
  List.map (fun s ->
    if s.symbol = symbol then { s with current_price = new_price }
    else s
  ) p

(* 4. View the Portfolio *)
let display_stock s =
  Printf.printf "Symbol: %s | Qty: %d | Buy Price: %.2f | Current Price: %.2f | Gain/Loss: %.2f\n"
    s.symbol s.quantity s.purchase_price s.current_price
    ((s.current_price -. s.purchase_price) *. float_of_int s.quantity)

let view_portfolio (p : portfolio) =
  List.iter display_stock p

(* 5. Calculate the Total Value of the Portfolio *)
let rec total_value (p : portfolio) : float =
  match p with
  | [] -> 0.0
  | s :: rest -> (s.current_price *. float_of_int s.quantity) +. total_value rest

(* 6. Calculate the Overall Gain/Loss of the Portfolio *)
let rec total_gain_loss (p : portfolio) : float =
  match p with
  | [] -> 0.0
  | s :: rest ->
    let gain = (s.current_price -. s.purchase_price) *. float_of_int s.quantity in
    gain +. total_gain_loss rest

(* 7. Test Case Function to simulate and show results *)
let () =
  (* Initialize an empty portfolio *)
  let p0 = [] in

  (* Add some stocks to the portfolio *)
  let stock1 = { symbol = "AAPL"; quantity = 10; purchase_price = 150.0; current_price = 155.0 } in
  let stock2 = { symbol = "GOOGL"; quantity = 5; purchase_price = 2800.0; current_price = 2900.0 } in
  let p1 = add_stock p0 stock1 in
  let p2 = add_stock p1 stock2 in

  (* View the portfolio *)
  Printf.printf "Viewing Portfolio:\n";
  view_portfolio p2;

  (* Calculate and display total value of the portfolio *)
  Printf.printf "\nTotal Portfolio Value: %.2f\n" (total_value p2);

  (* Calculate and display overall gain/loss of the portfolio *)
  Printf.printf "Overall Gain/Loss: %.2f\n" (total_gain_loss p2);

  (* Update stock price and view the updated portfolio *)
  let p3 = update_price p2 "AAPL" 160.0 in
  Printf.printf "\nUpdated Portfolio after changing AAPL price:\n";
  view_portfolio p3;

  (* Remove a stock and view the updated portfolio *)
  let p4 = remove_stock p3 "GOOGL" in
  Printf.printf "\nUpdated Portfolio after removing GOOGL:\n";
  view_portfolio p4;
  
  (* Final calculations after update/removal *)
  Printf.printf "\nTotal Value after changes: %.2f\n" (total_value p4);
  Printf.printf "Overall Gain/Loss after changes: %.2f\n" (total_gain_loss p4);
