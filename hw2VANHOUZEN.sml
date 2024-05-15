(* Brahm Van Houzen, Programming Languages Part A, Homework 2 *)


(* Returns a boolean if both parameters are equal *)

fun same_string(x, y) =
    x = y


(* This will take in a string and a string list, and return the original string list as an option sans the string parameter *)

fun all_except_option (s, lst) =
   case lst of 
      [] => NONE
      | s1::lst' => 
         if same_string(s, s1)
         then SOME lst'
         else 
            case all_except_option (s, lst') of
               NONE => NONE
               | SOME ys => SOME (s1::ys)


(* This will take in a string list list and a string and return a conjugation of the string lists that contain the 
input string, this uses recursion *)

fun  get_substitutions1 (lstlst, s) =  
   case lstlst of
      [] => []
      | lst::lstlst' =>
         case all_except_option (s, lst) of 
            SOME xs => xs @ get_substitutions1 (lstlst', s)
            | NONE => get_substitutions1 (lstlst', s) 


(* Same as last function, but it will use tail recursion and an accumulator to allow for a more linear stack *)

fun get_substitutions2 (lstlst, s) = 
   let 
      fun aux ([], acc) = acc
         | aux (x::xs, acc) = 
            case all_except_option (s, x) of
               NONE => aux (xs, acc)
               | SOME xs' => aux (xs, acc @ xs')
   in 
      aux (lstlst, [])
   end


(* This will take in a string list list and a record that represents someones full name 
and will use a conjugation of the string lists that contain the first name in the record
that is created using the last function and return a list of full name records, each with 
a first name from the conjugated string list  *)

fun similar_names (substitutions, {first=first, middle=middle, last=last}) =
   let 
      val names = first :: get_substitutions2 (substitutions, first)

      fun aux first_names =
         case first_names of
            [] => []
            | x::xs => {first=x, middle=middle, last=last} :: aux xs
   in 
      aux names
   end

         


datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


(* Returns the Color of the card parameter *)

fun card_color c =
   case c of
      (Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red


(* Returns the value of the card parameter *)

fun card_value c =
   case c of
      (_, Ace) => 11
      | (_, King) => 10
      | (_, Queen) => 10
      | (_, Jack) => 10
      | (_, Num x) => x


(* Takes in a list of cards, a card to be removed, and an exception
removes card from list, returns list without card, throws 
exception of card is not in list *)

fun remove_card (cs, c, e) = 
   let 
      fun aux (xs, acc) = 
         case xs of
            [] => raise e
            | x::xs' =>
               if x = c 
               then acc @ xs'
               else 
               aux (xs', acc @ [x])
   in 
      aux (cs, [])

   end 
         

(* Returns a boolean denoting if all cards in list are same color *)

fun all_same_color cs =
   case cs of
      [] => true 
      | _::[] => true
      | c1::(c2::cs') =>
         ((card_color c1 = card_color c2) andalso all_same_color (c2::cs'))

(* Returns the sum of the values of all cards in list *)

fun sum_cards cs = 
   let 
      fun aux (cs', acc) =
         case cs' of 
            [] => acc
            | x::xs => 
                  aux (xs, (card_value x) + acc)
   in    
      aux (cs, 0)
   end


(* Calculates the score of the solitaire game based on 
predefined rules *)

fun score (h, g) = 
   let 
      val h_sum = sum_cards h
      val p_score = 
         if 
            h_sum > g
         then 
            3 * (h_sum - g)
         else 
            g - h_sum
   in 
      if 
        all_same_color h
      then 
        p_score div 2
      else  
         p_score
   end

(* Officiates the solitare game, takes in a list of cards (the deck),
a list of moves, and a goal *)

fun officiate (cs, ms, g) =
   let 
      val e = IllegalMove
      fun aux (cs', ms', held) =
         if sum_cards held > g
         then score (held, g)
         else 
         case ms' of 
            [] => score (held, g)
            | x::xs => 
               case x of 
               Draw =>
                  (case cs' of
                     [] => score (held, g)
                     | y::[] => aux ([], xs, y::held)
                     | y::ys =>
                        aux (ys, xs, y::held ))
               | Discard c =>                   
                     aux (cs', xs, (remove_card (held, c, e)))
   in 
      aux (cs, ms, [])
   end
