Этот файл является файлом Literate Haskell. Его можно непосредственно загрузить
в интерпретаторе GHCi или скомпилировать.

Строки, начинающиеся на > являются фактическим кодом. Остальное -- комментарии.
Строки, начинающиеся на < являются примерами или пояснениями, но не относятся к
фактическому коду.

Краткое теоретическое введение доступно по адресу
https://wiki.livid.pp.ru/students/sp/labs/02.html

***

Включаем все предупреждения компилятора

> {-# OPTIONS_GHC -Wall -Wextra #-}

Объявляем имя модуля

> module RecursiveParser where

Импортируем модуль Lexer, поскольку будем использовать определения из него

> import Lexer

Объявляем абстрактную структуру дерева выражений. Выражение представляет из себя
либо бинарный оператор, применяемый к подвыражениям (каждое из которых является
выражением), либо идентификатор, либо числовой литерал, либо унарный минус,
применённый к выражению.

> data Expr =
>     BinOp Operator Expr Expr
>   | Identifier String
>   | ENumber Double
>   | UnaryMinus Expr
>   deriving Show

Определим возможные операторы

> data Operator =
>     OpPlus
>   | OpMinus
>   | OpMult
>   | OpDiv
>   | OpExp
>   deriving Show

Основная точка входа, функция parse. Принимает на вход поток токенов, возвращает
либо строку с описанием ошибки, либо определённый выше тип дерева выражения.

Основная точка входа -- разбор нетерминала E (стартового). Мы полагаем, что весь
поток токенов должен быть разборан, поэтому проверяем, что после разбора
нетерминала E на верхнем уровне, токенов больше не осталось. В противном
случае сообщаем об ошибке (см. определение reportError в конце файла)

> parse :: [Token] -> Either String Expr
> parse ts = case parseE ts of
>     Right (rest, eexpr) ->
>       if null rest
>       then Right eexpr
>       else reportError "end of input" rest
>     Left err -> Left err

Разбор нетерминала E соответствует последовательному разбору нетерминалов T и
E'. При этом, терминал E' соответствует "второй половине" выражения с бинарным
оператором "+" или "-" (если она есть), поэтому, чтобы построить узел бинарного
оператора, необходимо передать в него результат разбора нетерминала T.

> parseE :: [Token] -> Either String ([Token], Expr)
> parseE ts = parseT ts >>= uncurry parseE'

Функция uncurry принимает каррированную функцию двух аргументов x y и возвращает
функцию, принимающую пару (x, y). Оператор (>>=) в данном случае эквивалентен
следующей конструкции:

< arg >>= f = case arg of
<   Right val -> f val
<   Left err -> Left err

В общем случае, он позволяет "развернуть" многие "обёртки" M типов вида (M a), и
передать "внутреннее" значение типа (a) в функцию типа (a -> M b).

Аналогично, разбор нетерминала T подразумевает разбор нетерминалов F и T'

> parseT :: [Token] -> Either String ([Token], Expr)
> parseT ts = parseF ts >>= uncurry parseT'

Разбор терминала E' имеет несколько вариантов.

1. Первый символ -- токен оператора "+"
2. Первый символ -- токен оператора "-"
3. Терминал E' соответствует пустой строке

В случае варианта (3), следующий входной символ -- любой. При этом мы не
трогаем поток токенов, и в качестве результата возвращаем переданный результат
разбора предшествовавшего нетерминала T

> parseE' :: [Token] -> Expr -> Either String ([Token], Expr)
> parseE' ts t1 = case ts of
>   Token Operator "+" : ts' -> fmap (BinOp OpPlus t1) <$> parseT ts'
>   Token Operator "-" : ts' -> fmap (BinOp OpMinus t1) <$> parseT ts'
>   _ -> Right (ts, t1)

Функция fmap, как и оператор (<\$>) позволяют применить функцию к "внутреннему
содержимому" типа вида (F a), т.е. к значению типа (a). Собственно, можно их
рассматривать как синонимы.

В данном случае они используются вместе, чтобы "добраться" до значения типа
Expr, который "завёрнут" в (([Token],)) и в (Either String). Оператор (<\$>)
применяет здесь левый аргумент к (([Token], Expr)), а fmap применяте свой
аргумент к Expr.

Этот код эквивалентно можно записать здесь в виде:

< fmap f <$> parseT ts' = case parseT ts' of
<     Right (rest, val) -> Right (rest, f val)
<     Left err -> Left err

Разбор нетерминала T' аналогичен, только меняются операторы.

> parseT' :: [Token] -> Expr -> Either String ([Token], Expr)
> parseT' ts t1 = case ts of
>   Token Operator "*" : ts' -> fmap (BinOp OpMult t1) <$> parseT ts'
>   Token Operator "/" : ts' -> fmap (BinOp OpDiv t1) <$> parseT ts'
>   _ -> Right (ts, t1)

Разбор нетерминала F аналогичен разбору нетерминала E:

> parseF :: [Token] -> Either String ([Token], Expr)
> parseF ts = parseV ts >>= uncurry parseF'

Разбор нетерминала F' аналогичен разбору E' и T':

> parseF' :: [Token] -> Expr -> Either String ([Token], Expr)
> parseF' ts t1 = case ts of
>   Token Operator "^" : ts'
>     -> fmap (BinOp OpExp t1) <$> parseF ts'
>   _ -> Right (ts, t1)

Наконец, разбор нетерминала V. Рассматриваются 4 варианта.

1. Первый токен -- открывающаяся скобка (. В таком случае мы разбираем
нетерминал E, после которого ожидаем увидеть закрывающуюся скобку ). Здесь мы
опять используем оператор (>>=), чтобы "добраться" до ([Token], Expr), минуя
"обёртку" (Either String), но справа определяем анонимную (лямбда) функцию "на
ходу". Внутри лямбда-функции, мы проверяем, что первый токен после разбора E
действительно является закрывающей скобкой, убираем его из входного потока, и возвращаем результат разбора E. Иначе возвращаем ошибку.

2. V представляет собой какой-то идентификатор, первый токен -- это он и есть.

3. V представляет собой число, первый токен -- это оно и есть.

4. V представляет собой унарный минус, за которым снова следует V. Этот случай
в значительной мере аналогичен случаям для F', T' и E'.

> parseV :: [Token] -> Either String ([Token], Expr)
> parseV ts = case ts of
>   Token Lparen _ : ts' ->
>     parseE ts' >>= \(rest, eexpr) -> case rest of
>       Token Rparen _ : rest' -> Right (rest', eexpr)
>       x -> reportError ")" x
>   Token Id identifierStr : ts' -> Right (ts', Identifier identifierStr)
>   Token Number numberStr : ts' -> Right (ts', ENumber $ read numberStr)
>   Token Operator "-" : ts' -> fmap UnaryMinus <$> parseV ts'
>   x -> reportError "(, id, num or -" x

Функция reportError принимает два параметра, строку сообщения, какой символ
ожидалось найти, и поток токенов, первый из которых не совпал с ожидаемым.

Геренируется сообщение об ошибке вида "Expected <msg> but got
<firstInvalidToken>". Если список токенов пуст, то вместо <firstInvalidToken>
подставляется "end of input", иначе, исходная строка соответствующего токена.

> reportError :: String -> [Token] -> Either String a
> reportError msg ts = Left $ "Expected " <> msg <> " but got " <> case ts of
>   Token _ str : _ -> str
>   [] -> "end of input"
