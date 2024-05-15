 (* use "peer4.sml"; *)
 use "hw2VANHOUZEN.sml";


val test1 = all_except_option ("butts", ["banana", "butter", "butts", "odin", "dexter"]) = SOME ["banana", "butter", "odin", "dexter"]

val test2 = get_substitutions1 ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test3 = get_substitutions2 ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test4 = get_substitutions2([["fred","fredrick","freddie","F","freddy"],["Will","William","Willy","Bill"]], "Bill") = ["Will","William","Willy"]

val test5 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test6 = card_color (Clubs, Queen) = Black

val test7 = card_value (Clubs, Queen) = 10

exception IllegalMove


val test8 = remove_card ([(Clubs, Num 9), (Spades, Num 4), (Diamonds, King), (Spades, Num 3), (Diamonds, King)], (Diamonds, King), IllegalMove ) = [(Clubs, Num 9), (Spades, Num 4), (Spades, Num 3)]


val test9 = ((remove_card ([], (Hearts, Ace), IllegalMove); false) handle IllegalMove => true)

val test10 = all_same_color ([(Clubs, Num 9), (Spades, Num 4), (Clubs, King), (Spades, Num 3), (Clubs, King)] ) = true

val test11 = all_same_color ([(Diamonds, Num 9), (Spades, Num 4), (Clubs, King), (Spades, Num 3), (Clubs, King)] ) = false

val test12 = sum_cards ([(Clubs, Num 9), (Spades, Num 4), (Diamonds, King)]) = 23

val test13 = score ([(Clubs, Num 9), (Spades, Num 4), (Diamonds, King)], 12) = 33

val test14 = score ([(Clubs, Num 9), (Spades, Num 5), (Spades, King)], 12) = 18

val test15 = score ([(Clubs, Num 9), (Spades, Num 5), (Spades, King)], 28) = 2

val test16 = score ([(Clubs, Num 9), (Spades, Num 5), (Diamonds, King)], 28) = 4

val test16a = score ([], 10) = 5

(* 16b. Non-Empty Hand (Different Colors) *)
val test16b = score ([(Spades, Num 2), (Hearts, Num 3), (Diamonds, Num 4)], 12) = 3

(* 16c. Non-Empty Hand (Same Color) *)

(* 16d. Non-Empty Hand (Exceeding Goal) *)
val test16d = score ([(Clubs, Num 8), (Spades, Num 9), (Diamonds, Num 10)], 20) = 21


(* Test case 1: Empty move list *)

val test17 = officiate ([], [], 20)
(* Expected result: 0 (since no moves were made) *)

(* Test case 2: Discard a card that is not in the held-cards *)
(* val test18 = ((officiate ([(Hearts, Num 5)], [(Discard (Spades, Num 7))], 15); false) handle IllegalMove => true) *)
(* Expected result: Raise IllegalMove exception *)

(* Test case 3: Draw a card when the card-list is empty *)
val test19 = officiate ([], [(Draw)], 10) = 5
(* Expected result: 0 (since no moves were made) *)

(* Test case 4: Draw a card that exceeds the goal *)
val test20 = officiate ([(Clubs, Num 8)], [(Draw)], 15) = 3
(* Expected result: 0 (since the sum of held-cards exceeds the goal) *)

(* Test case 5: Play a full game with valid moves *)
val test21 = officiate ([(Hearts, Num 7), (Clubs, Num 10)], [(Draw), (Discard (Hearts, Num 7)), (Draw)], 20) = 5
(* Expected result: The computed score based on the rules *)

val test22 = officiate ([(Hearts, Num 7), (Clubs, Num 10)], [(Draw), (Discard (Hearts, Num 7)), (Draw)], 5) = 3



(*

val () = print ("Test 17: " ^ Int.toString(test17) ^ "\n")
(* val () = print ("Test 18: " ^ Int.toString(test18) ^ "\n") *)
(* val () = print ("Test 3: " ^ Int.toString(test19) ^ "\n") *)
(* val () = print ("Test 4: " ^ Int.toString(test20) ^ "\n") *)
(* val () = print ("Test 5: " ^ Int.toString(test21) ^ "\n") *)


(* remove_card with print statements for debugging:


and string_of_suit (s : suit) : string =
  case s of
   Hearts => "Hearts"
  | Diamonds => "Diamonds"
  | Spades => "Spades"
  | Clubs => "Clubs"

fun card_to_string (c : card) : string =
    (* Assuming 'card' is a type representing your card structure *)
    case c of
        (Hearts, Num n) => "Hearts " ^ Int.toString n
      | (Diamonds, Num n) => "Diamonds " ^ Int.toString n
      | (Spades, Num n) => "Spades " ^ Int.toString n
      | (Clubs, Num n) => "Clubs " ^ Int.toString n
      | (suit, Ace) => "Ace of " ^ string_of_suit suit
      | (suit, King) => "King of " ^ string_of_suit suit
      | (suit, Queen) => "Queen of " ^ string_of_suit suit
      | (suit, Jack) => "Jack of " ^ string_of_suit suit

fun list_to_string (cs : card list) : string =
    (* Convert a list of cards to a comma-separated string *)
    String.concatWith ", " (map card_to_string cs)


fun remove_card (cs, c, e) = 
   case cs of
      [] => raise e
      | c1::cs' =>
         (* if c1 = c 
         then cs'
         else 
            c1 :: remove_card (cs', c, e) *)
         if c1 = c then
            cs'
        else
            let
                val cardString = card_to_string c1
                val listString = list_to_string cs'
            in
                (print ("Removing card: " ^ cardString ^ " from list: " ^ listString ^ "\n");
                 c1 :: remove_card (cs', c, e))
            end
     *)

      *)