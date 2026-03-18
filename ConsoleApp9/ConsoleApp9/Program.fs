open System

// Тип дерева
type Tree<'T> =
    | Node of 'T * Tree<'T> * Tree<'T>
    | Nil

// Вставка
let rec insertTree tree newValue =
    match tree with
    | Node(value, left, right) ->
        if newValue <= value then
            Node(value, insertTree left newValue, right)
        else
            Node(value, left, insertTree right newValue)
    | Nil -> Node(newValue, Nil, Nil)

// Проверка ввода
let rec checkQuantity() =
    let input = Console.ReadLine()
    let suc, n = Int32.TryParse(input)
    if suc && n > 0 then n
    else
        printfn "Введите положительное целое число:"
        checkQuantity()

// Генерация дерева случайных чисел
let generateTree n =
    let r = Random()
    let rec loop i tree =
        if i = 0 then tree
        else
            let value = r.NextDouble() * 20.0 - 10.0
            loop (i - 1) (insertTree tree value)
    loop n Nil

// TreeMap
let rec treeMap f tree =
    match tree with
    | Nil -> Nil
    | Node(value, left, right) ->
        Node(f value, treeMap f left, treeMap f right)

// Определяет обход
let infix root left right = (right(); root(); left())

let travDepth strategy action tree =
    let rec walk curTree depth =
        match curTree with
        | Node (value, left, right) -> 
            strategy (fun () -> action value depth)
                     (fun () -> walk left (depth + 4))
                     (fun () -> walk right (depth + 4))
        | Nil -> ()
    walk tree 0

let spaces n = String.replicate n "  "

// Главная функция вывода
let printTree T = 
    travDepth infix (fun x dep -> printfn "%s%A" (spaces dep) x) T

[<EntryPoint>]
let main args =
    printf "Количество элементов: "
    let n = checkQuantity()

    let Tree1 = generateTree n

    printfn "\nИсходное дерево:"
    printTree Tree1

    let Tree2 =
        treeMap (fun x -> if x < 0.0 then 0 else 1) Tree1

    printfn "\nНовое дерево:"
    printTree Tree2

    0

