{-# LANGUAGE GADTs, LambdaCase #-}
module Parser (lexer, toBracketTree, toFuncTree, FuncTree(..), Lexem(..)) where


data FuncTree where
    AndNode     :: FuncTree -> FuncTree -> FuncTree
    OrNode      :: FuncTree -> FuncTree -> FuncTree
    NotNode     :: FuncTree -> FuncTree -> FuncTree
    ListNode    :: String -> FuncTree
  deriving Show

data BracketTree where
    Brakets :: [BracketTree] -> BracketTree
    Lexem   :: Lexem -> BracketTree
  deriving Show

data Lexem where
    Tag     :: String -> Lexem
    NotTag  :: String -> Lexem
  deriving Show


-- разбираем операции над множествами
toFuncTree :: BracketTree -> FuncTree
toFuncTree = \case
    Lexem (Tag a)   -> ListNode a
    Lexem (NotTag a) -> error $ "Parser.toFuncTree:" ++ a ++ "is'nt a tag."
    Brakets aLexems  -> aOrNot [] (reverse aLexems)
  where
    aOrNot :: [BracketTree] -> [BracketTree] -> FuncTree
    aOrNot aLexems = \case
        Lexem (NotTag "+") : xs -> OrNode  (aAnd $ reverse aLexems) (aOrNot [] xs)
        Lexem (NotTag "-") : xs -> NotNode (aOrNot [] xs) (aAnd $ reverse aLexems)
        x:xs                    -> aOrNot (x:aLexems) xs
        _                       -> aAnd $ reverse aLexems


    aAnd :: [BracketTree] -> FuncTree
    aAnd = \case
        x : Lexem (NotTag "*") : xs -> AndNode (aBracketToFunc x) (aAnd xs)
        [x] -> aBracketToFunc x
        _   -> error "Parser.toFuncTree.aAnd: empty input."


    aBracketToFunc :: BracketTree -> FuncTree
    aBracketToFunc = \case
        Brakets x           -> aOrNot [] x
        Lexem (Tag x)       -> ListNode x
        Lexem (NotTag x)    -> error $
            "Parser.toFuncTree.aBracketToFunc: " ++ x ++ "is'nt a tag."



-- разбираем скобочную структуру
toBracketTree :: [Lexem] -> BracketTree
toBracketTree = Brakets . fst . aToBracketTree []
  where
    aToBracketTree :: [BracketTree] -> [Lexem] -> ([BracketTree], [Lexem])
    aToBracketTree aLexems = \case
        NotTag ")":xs -> (reverse aLexems, xs)
        NotTag "(":xs -> let (aLexs, aTail) = aToBracketTree [] xs in
            aToBracketTree (Brakets aLexs : aLexems) aTail
        x:xs          -> aToBracketTree (Lexem x:aLexems) xs
        _             -> (reverse aLexems, [])


-- разбиваем запрос на лексемы
lexer :: String -> [Lexem]
lexer = filter notNullLexem . map cleanLexem . aLexer ""
  where
    -- проходим по строке
    aLexer :: String -> String -> [Lexem]
    aLexer aLex = \case
        x:xs
            | x == '"'         -> NotTag (reverse aLex) : aTagLexer "" xs
            | x`elem`"()+-*"    -> NotTag (reverse aLex) : NotTag [x] : aLexer "" xs
            | otherwise         -> aLexer (x:aLex) xs
        _                       -> [NotTag $ reverse aLex]

    -- Разбирае теги
    aTagLexer aLex = \case
        x:xs
            | x /= '"' -> aTagLexer (x:aLex) xs
            | otherwise -> Tag (reverse aLex) : aLexer "" xs
        _               -> error "Parser.lexer.aTagLexer"

-- чистим лексемы
cleanLexem :: Lexem -> Lexem
cleanLexem = \case
    NotTag a    -> NotTag $ filter (`notElem`" \n") a
    Tag a       -> Tag a

-- проверяем, что лексема не пуста
notNullLexem :: Lexem -> Bool
notNullLexem = \case
    NotTag ""   -> False
    _           -> True
