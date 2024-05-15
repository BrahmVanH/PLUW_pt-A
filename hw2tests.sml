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

val test16b = score ([(Spades, Num 2), (Hearts, Num 3), (Diamonds, Num 4)], 12) = 3

val test16d = score ([(Clubs, Num 8), (Spades, Num 9), (Diamonds, Num 10)], 20) = 21

val test17 = officiate ([], [], 20)

val test19 = officiate ([], [(Draw)], 10) = 5

val test20 = officiate ([(Clubs, Num 8)], [(Draw)], 15) = 3

val test21 = officiate ([(Hearts, Num 7), (Clubs, Num 10)], [(Draw), (Discard (Hearts, Num 7)), (Draw)], 20) = 5

val test22 = officiate ([(Hearts, Num 7), (Clubs, Num 10)], [(Draw), (Discard (Hearts, Num 7)), (Draw)], 5) = 3

