// Задание 2: Вариант - 9
open System

// Тип дерева
type Leaf = { value: float; left: Leaf option; right: Leaf option }

// Рекурсивная вставка значения в бинарное дерево поиска
let rec insertTree (tree: Leaf option) (newValue: float) : Leaf =
    match tree with
    | Some t ->
        if newValue <= t.value then
            { t with left = Some (insertTree t.left newValue) } // Вставка в левое поддерево
        else 
            { t with right = Some (insertTree t.right newValue) } // Вставка в правое поддерево
    | None -> { value = newValue; left = None; right = None }

// Вывод дерева
let rec printTree t =
    match t with
    | Some node ->
        printTree node.left
        printf "%f " node.value
        printTree node.right
    | None -> ()

// Проверка на корректность ввода числа
let rec checkQuantity() =
    let input = Console.ReadLine()
    let suc, n = Int32.TryParse(input)
    if suc && n > 0 then n
    else
        printfn "Введите положительное целое число "
        checkQuantity()

// Проверка на корректность ввода цифры
let rec checkFigure() =
    let input = Console.ReadLine()
    let suc, n = Int32.TryParse(input)
    if suc && n >= 0 && n < 10 then 
       n
    else
        printfn "Введите положительную цифру: "
        checkFigure()

// Генерация случайного дерева со значениями [-10..10]
let generateRandomTree n =
    let rand = Random()
    let values =
        List.init n (fun _ -> 
            let number = rand.NextDouble()
            number * 20.0 - 10.0
        )
    List.fold (fun acc v -> Some (insertTree acc v)) None values

// Запись значений дерева в список
let rec reWriteList (tree: Leaf option) : float list =
    match tree with
    | None -> 
        []
    | Some t ->
        let leftValues = reWriteList t.left
        let rootValue = [t.value]
        let rightValues = reWriteList t.right
        let allValues = leftValues @ rootValue @ rightValues // Объединение списков 
        allValues

[<EntryPoint>]
let main args =
    printfn "Количество элементов дерева: "
    let n = checkQuantity()

    let tree = generateRandomTree n
    printf "Исходное дерево: "
    printTree tree

    printfn "\nЦифра для поиска в элементах: "
    let figure = checkFigure()
    let figureS = figure.ToString()

    let listValues = reWriteList tree
    let count =
        listValues
        |> List.fold (fun acc v ->
            let s = v.ToString("F6")
            if s.Contains(figureS) then 
               acc + 1
            else 
               acc
        ) 0  

    printfn "Элементов дерева c цифрой %s: %d" figureS count
    0
