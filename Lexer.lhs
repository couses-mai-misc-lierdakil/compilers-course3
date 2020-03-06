Этот файл является файлом Literate Haskell. Его можно непосредственно загрузить
в интерпретаторе GHCi или скомпилировать.

Строки, начинающиеся на > являются фактическим кодом. Остальное -- комментарии.
Строки, начинающиеся на < являются примерами или пояснениями, но не относятся к
фактическому коду.

Краткое теоретическое введение доступно по адресу
https://wiki.livid.pp.ru/students/sp/labs/01.html

***

Включаем все предупреждения компилятора

> {-# OPTIONS_GHC -Wall -Wextra #-}

и расширение RecordWildCards

> {-# LANGUAGE RecordWildCards #-}

Объявляем имя модуля

> module Lexer where

Импортируем стандартные модули. Data.List содержит функции
работы со списками (в данном случае нас интересует функция
uncons), Data.Char -- функции работы с символами, в частности
функции классификации символов. Наконец, из Control.Applicative мы импортируем
оператор <|>, который является оператором альтернативы для типов, для которых
он определён. В частности, для Maybe a он определён как

< Just x <|> y = Just x
< Nothing <|> y = y

то есть возвращает первый аргумент, если он Just, в противном случае второй.

> import Data.List (uncons)
> import Data.Char (isDigit, isAsciiLower)
> import Control.Applicative ((<|>))

Объявляем Enum-тип с именами (типами) токенов (лексем):

> data TokenName =
>     Number
>   | Operator
>   | Id
>   | Lparen
>   | Rparen
>   | Comma
>   deriving Show

deriving Show здесь позволяет использовать функцию show для отображения значений
типа TokenName в строку.

Объявляем тип токена: токен состоит из идентификатора имени токена, TokenName, и
атрибута. В данном случае атрибутом будет просто часть входной строки, которая
совпала с шаблоном токена при лексическом анализе.

> data Token = Token { tokenName :: TokenName, tokenStr :: String } deriving Show

Здесь используется синтаксис записей. Этот синтаксис в целом эквивалентен
определению типа и вспомогательных функций из значений типа в значения полей:

< data Token = Token TokenName String
< tokenName :: Token -> TokenName
< tokenName (Token x _) = x
< tokenStr :: Token -> String
< tokenStr  (Token _ x) = x

Кроме того, записи допускают свой синтаксис определения значений, например:

< token :: Token
< token = Token { tokenName = Number, tokenStr = "123" }

Объявляем синоним типа для номера состояния. Теоретически более корректным было
бы объявить новый ограниченный тип, но для краткости мы используем просто
машинное целое.

> type StateNum = Int

Объявляем функцию переходов. Эта функция полностью соответствует таблице,
определённой в https://wiki.livid.pp.ru/students/sp/labs/01.html.
Функции isDigit и isAsciiLower определены в Data.Char.

> transitionTable :: StateNum -> Char -> Maybe StateNum
> transitionTable st c =
>   case st of
>     0 | isLetter' c              -> Just 7
>       | c == '('                 -> Just 8
>       | c == ')'                 -> Just 9
>       | c == ','                 -> Just 10
>       | c `elem` "+*/^-"         -> Just 6
>       | isDigit c                -> Just 1
>       | c == ' '                 -> Just 0
>     1 | isDigit c                -> Just 1
>       | c == '.'                 -> Just 2
>       | c `elem` "eE"            -> Just 3
>     2 | isDigit c                -> Just 2
>       | c `elem` "eE"            -> Just 3
>     3 | c `elem` "+-"            -> Just 4
>       | isDigit c                -> Just 5
>     4 | isDigit c                -> Just 5
>     5 | isDigit c                -> Just 5
>     7 | isLetter' c || isDigit c -> Just 7
>     _                            -> Nothing
>   where
>   isLetter' ch = isAsciiLower ch || ch == '_'

Определим принимающие состояния, и к каким токенам они относятся:

> stateToTokenName :: StateNum -> Maybe TokenName
> stateToTokenName st =
>   case st of
>     1  -> Just Number
>     2  -> Just Number
>     5  -> Just Number
>     6  -> Just Operator
>     7  -> Just Id
>     8  -> Just Lparen
>     9  -> Just Rparen
>     10 -> Just Comma
>     _  -> Nothing

Определим структуру, целиком описывающую текущее состояние системы: входную
строку, текущий буфер выходной строки, и номер состояния самого автомата

> data FullState = FullState { fsInput :: String
>                            , fsOutput :: String
>                            , fsCurrentState :: StateNum
>                            }

> data LastAcceptingState = LAState { lasInput :: String
>                                   , lasToken :: Token
>                                   }

Определим так же "багаж" информации, которую нам надо отслеживать. Поскольку нам
необходимо возвращаться к прошлому полному состоянию системы, когда автомат
последний раз находился в принимающем состоянии, нам надо кроме текущего
состояния так же хранить последнее виденное принимающее. В начале работы
автомата, никакого принимающего состояния нет.

> data Baggage = Baggage { bagCurrentState :: FullState
>                        , bagLastAccState :: Maybe LastAcceptingState
>                        }

Наконец, в зависимости от поведения автомата, управляющая программа может:

1) Продолжить работу автомата -- передать автомату следующий входной символ
2) Перезапустить автомат -- откатить состояние к последнему принимающему и
запустить автомат с начального состояния. При этом, текущее состояние можно
отбросить.

Если перезапуск происходит при пустой входной строке, то алгоритм
останавливается.

Определяем тип, кодирующий действие:

> data Action = Continue Baggage | Restart (Maybe LastAcceptingState)

Определяем функцию, реализующую один шаг ДКА. На вход функция получает текущий
"багаж". На выходе -- действие (остановка, перезапуск или продолжение) и новый
"багаж".

Основная рабочая функция здесь makeOneStep. Она принимает текущее состояние
системы, берёт первый символ c входа fsInput (если есть), и если из текущего
состояния fsCurrentState есть переход по текущему символу c, то переносит этот
символ в fsOutput, и устанавливает новое состояние fsCurrentState в соответствии
с таблицей переходов. Если эта операция успешна, то действие -- Continue,
поскольку автомат не зашёл в тупик.

Если же автомат заходит в тупик, то действие -- Restart.

> dfaStep :: Baggage -> Action
> dfaStep b@Baggage{..} =
>   case makeOneStep bagCurrentState of
>     Nothing -> Restart newAccSt
>     Just st -> Continue $ b{ bagCurrentState = st
>                            , bagLastAccState = newAccSt }
>  where
>  -- если stateToToken bagCurrentState = Nothing, то newAccSt = bagLastAccState
>  -- иначе, newAccSt = stateToToken bagCurrentState
>  newAccSt = stateToToken bagCurrentState <|> bagLastAccState
>  stateToToken FullState{..}
>    = case stateToTokenName fsCurrentState of
>        Just name -> Just $ LAState {
>             lasInput=fsInput
>           , lasToken =Token{tokenName=name, tokenStr=fsOutput}
>           }
>        Nothing -> Nothing
>  makeOneStep FullState{..}
>    = case uncons fsInput of
>        Just (c, cs) -> case transitionTable fsCurrentState c of
>          Just newSt -> Just FullState{
>              fsInput = cs
>            , fsOutput = if c == ' ' then fsOutput else fsOutput <> [c]
>            , fsCurrentState = newSt
>            }
>          Nothing -> Nothing
>        Nothing -> Nothing

Это функция, реализующая цикл ДКА. В зависимости результата dfaStep, функция
либо делает рекурсивный вызов самой себя, либо останавливается. Каждый раз при
команде Restart, возвращаемый токен сохраняется в списке. Если при получении
команды Restart оказывается, что принимающего состояния не было, работа
останавливается. По-хорошему, там должна быть обработка ошибок.

> dfaRun :: Baggage -> [Token]
> dfaRun b =
>   case dfaStep b of
>     Restart (Just LAState{..})
>       -> lasToken : if null lasInput
>                     then []
>                     else dfaRun (initState lasInput)
>     Continue bag
>       -> dfaRun bag
>     _ -> []

Это функция, создающее начальное состояние из входной строки: ДКА в состоянии 0,
входная строка заполнена, выходная строка пуста, последнего принимающего
состояния пока не было.

> initState :: String -> Baggage
> initState input = Baggage { bagCurrentState =FullState input "" 0
>                           , bagLastAccState = Nothing}

Это основная функция входа: принимает строку, возвращает поток токенов.

> lexer :: String -> [Token]
> lexer input = dfaRun (initState input)

Упражнения:

1. Реализуйте аналогичный лексический анализатор на любимом языке
программирования.

2. Добавьте обработку лексических ошибок в Ваш анализатор. Обратите внимание,
что все лексические ошибки, которые могут происходить, здесь происходят во время
 работы функции dfaStep, точнее makeOneStep. Чтобы добавить обработку ошибок
 здесь, необходимо вместо Maybe возвращать из makeOneStep Either с
 диагностической информацией, и передавать её дальше как часть Restart.
