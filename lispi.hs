import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Map
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token(natural)

import Debug.Trace

data Expr = List [Expr] | Symbol String | Number Integer | Character Char | Nil | String String deriving (Eq,Show)
{--instance Show Expr where
	show (List x) = show x
	show (Symbol x) = show x
	show (Number x) = show x
	show (String x) = show x
	show Nil = "nil"
--}

sh :: Ord k => k -> [(Map k a)] -> Maybe a
sh _ [] = Nothing
sh k m = listToMaybe $ Data.Maybe.mapMaybe (Data.Map.lookup k) m

--head and last
hl [] = []
hl (x:[]) = [x]
hl (x:xs) = [x, (last xs)]

expr :: Parser Expr
expr = do
	try list
	<|>
	try atom

list :: Parser Expr
list = do
	char '('
	spaces
	x <- sepBy expr (many1 space)
	spaces
	char ')'
	return $ List x

atom :: Parser Expr
atom = do
	s <- many $ oneOf ['+','-']
	v <- many $ oneOf (['0'..'9']++['a'..'z']++['A'..'Z']++['+','-','*','/','=','<','>','?','!','_'])
	return $ if (not $ Prelude.null v)&&(all isDigit v) then (Number $ read $ if (s == "-") then '-':v else v) else Symbol $ s++v


eval :: Expr -> StateT [(Map String Expr)] IO Expr
eval (List ((Symbol "+"):xs)) = do
	return $ foldl1 (\(Number x) (Number y) -> Number $ x + y) xs

eval (List ((Symbol "-"):xs)) = do
	return $ foldl1 (\(Number x) (Number y) -> Number $ x - y) xs

eval (List ((Symbol "quote"):xs)) = do
	return $ head xs

eval (List ((Symbol "setq"):xs)) = do
	(m:ms) <- get
	let ((Symbol s):e:_) = xs
	put $ (Data.Map.insert s e m):ms
	return $ Symbol "setq"

eval (List ((Symbol "print"):s@(Symbol _):[])) = do
	v <- eval s
	lift $ print v
	return v

eval (List ((Symbol "print"):(Number n):[])) = do
	lift $ print n
	return $ Number n

eval (List ((List ((Symbol "lambda"):(List param):f:[])):args)) = do
	let t = (length param) == (length args)
	unless t (lift $ error "(length param) != (length args)")
	let a = zip (Prelude.map (\(Symbol x) -> x) param) args
	modify (fromList a:)
	r <- eval f
	modify (tail)
	return r

eval (List ((Symbol "lambda"):param:f:[])) = do
	eval f

eval (Symbol s) = do
	m <- get
	case sh s (hl m) of
		Just v -> return v
		Nothing -> lift $ error "error eval symbol"

--eval (List ((Symbol "read"):xs)) = do

eval s = do
	return $ Symbol "_"

main = do
	parseTest expr "(+ 3 3)"
	{--
	parseTest expr "(print x ab12 124 12ab)"
	parseTest expr "(123)"
	parseTest expr "(-123)"
	parseTest expr "(+123)"
	parseTest expr "(print 3)"
	print =<< runStateT (eval $ Symbol "b") [(Data.Map.singleton "a" (Number 3)), (Data.Map.singleton "b" (Number 4))]
	print =<< runStateT (eval $ List [Symbol "setq", Symbol "b", Number 2]) [(Data.Map.singleton "a" (Number 3))]
	print =<< runStateT (eval $ List [Symbol "+", Number 3, Number 5]) []
	print =<< runStateT (eval $ List [Symbol "-", Number 3, Number 5, Number 3]) []
	print =<< runStateT (eval $ List [Symbol "quote", List [Number 0, Number 1, Number 2]]) []
	print =<< runStateT (eval $ List [List [Symbol "lambda", List [Symbol "x"], List [Symbol "print", Symbol "x"]], Number 1]) [empty]
	print =<< runStateT (eval $ List [Symbol "print",Number 3]) [(Data.Map.singleton "a" (Number 3))]
	--}
	--print "a"
	--print =<< runStateT (eval $ Symbol "a") [(Data.Map.singleton "a" (Number 3))]
	--print =<< runStateT (eval $ List [Symbol "setq", Symbol "b", Number 2]) [(Data.Map.singleton "a" (Number 3))]
	case (parse expr "" "(+ 1 2 3 4 5)") of
		Right x -> print =<< runStateT (eval x) [(Data.Map.singleton "a" (Number 6))]
		Left x -> print x

	case (parse expr "" "(+ 1)") of
		Right x -> print =<< runStateT (eval x) [(Data.Map.singleton "a" (Number 6))]
		Left x -> print x
	--print =<< runStateT (eval $ (parseTest expr "(print 3)")) [empty]
