(*  Module qui permet la décomposition et la recomposition de données **)
(*  Passage du type t1 vers une liste d'éléments de type t2 (décompose) **)
(*  et inversement (recopose).**)
module type DecomposeRecompose =
sig
  (*  Type de la donnée **)
  type mot
  (*  Type des symboles de l'alphabet de t1 **)
  type symbole

  val decompose : mot -> symbole list
  val recompose : symbole list -> mot
end

module DRString:DecomposeRecompose with type mot=string and type symbole=char=
struct
    type mot = string
    type symbole=char
    let decompose s =
        let rec sous_decompose i accu =
            if i < 0 then accu
            else sous_decompose (i-1) (s.[i]::accu)
        in sous_decompose (String.length s - 1) []

    let recompose lc =
    List.fold_right (fun t q -> String.make 1 t ^ q) lc ""
end
(*type chiffre=|0|1|2|3|4|5|6|7|8|9*)
module DRNat:DecomposeRecompose with type mot=int and type symbole=int =
struct
    type mot=int

    type symbole=int
    (*fonction auxiliaire*)
    let decompose_chaine s =
    let rec decompose i accu =
        if i < 0 then accu
        else decompose (i-1) (s.[i]::accu)
    in decompose (String.length s - 1) []
    (*fonction auxiliaire*)
    let recompose_chaine lc =
    List.fold_right (fun t q -> String.make 1 t ^ q) lc ""


    (*decomposer un entier en une listre de chiffres
      signature: decompose: int-> int list
      paramètres: 
        -e : un entier
      résultat: une liste d'entiers entre 0 et 9 issues de la décomposotion de e
    *)
    let decompose e =           
  (*match e with | 0 ->[]
                            | _ -> (e mod 10) :: decompose ((e - e mod 10 )/10) 
*)
        let str = string_of_int e in
        List.map int_of_char (decompose_chaine str)
    (*reconstruire un entier à partir d'une lilste d'entiers compris entre 0et 9
      signature: decompose: int list-> int 
      paramètres: 
        -l : liste de chiffres
      résultat: eniteir obtenue de la concaténation des chiffres*)
    let recompose l = int_of_string (recompose_chaine (List.map char_of_int l))
end

