open System

// Тип дерева
type Tree<'T> =
    | Node of 'T * Tree<'T> * Tree<'T>
    | Nil

// Вставка значения в дерево
let rec insertTree tree newValue =
    match tree with
    | Node(value, left, right) ->
        if newValue <= value then
            Node(value, insertTree left newValue, right)
        else
            Node(value, left, insertTree right newValue)
    | Nil -> Node(newValue, Nil, Nil)

// Проверка количества элементов
let rec checkQuantity() =
    let input = Console.ReadLine()
    let suc, n = Int32.TryParse(input)
    if suc && n > 0 then n
    else
        printfn "Введите положительное число:"
        checkQuantity()

// Проверка цифры (
let rec checkFigure() =
    let input = Console.ReadLine()
    let suc, n = Int32.TryParse(input)
    if suc && n >= 0 && n < 10 then n
    else
        printfn "Введите цифру (0-9):"
        checkFigure()

// Генерация дерева случайных чисел
let generateTree n =
    let r = Random()
    let rec loop i tree =
        if i = 0 then tree
        else
            let value = r.NextDouble() * 20.0 - 10.0
            loop (i - 1) (insertTree tree value)
    loop n Nil

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

// Treefold
let rec treeFold f acc tree =
    match tree with
    | Nil -> acc
    | Node(value, left, right) ->
        let accLeft = treeFold f acc left
        let accRoot = f accLeft value
        treeFold f accRoot right

[<EntryPoint>]
let main args =
    printf "Количество элементов: "
    let n = checkQuantity()

    let tree = generateTree n

    printfn "\nИсходное дерево:"
    printTree tree 

    printfn "\nЦифра для поиска: "
    let figure = checkFigure()
    let figureS = figure.ToString()

    // Функция проверки, содержит ли число нужную цифру
    let containsDigit (v: float) =
        let s = v.ToString("F6")
        s.IndexOf(figureS) >= 0

    // Подсчёт элементов через treeFold
    let count =
        treeFold (fun acc v -> if containsDigit v then acc + 1 else acc) 0 tree

    printfn "\nЭлементов с цифрой %s: %d" figureS count

    0
