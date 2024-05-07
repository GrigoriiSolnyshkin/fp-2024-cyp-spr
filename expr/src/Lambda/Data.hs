module Lambda.Data(LambdaTerm(..), (&*), (&->), (&*>), (&*<), (&*=)) where

data LambdaTerm = Primitive String | Application LambdaTerm LambdaTerm | Abstraction String LambdaTerm deriving (Eq)

data Origin = Pure | AppLeft | AppRight

infixl 4 &*
(&*) :: LambdaTerm -> LambdaTerm -> LambdaTerm
(&*) = Application

infixr 3 &->
(&->) :: String -> LambdaTerm -> LambdaTerm
(&->) = Abstraction

infixl 4 &*>
(&*>) :: LambdaTerm -> String -> LambdaTerm
(&*>) term primitive = Application term (Primitive primitive)

infixl 4 &*<
(&*<) :: String -> LambdaTerm -> LambdaTerm
(&*<) primitive = Application $ Primitive primitive

infixl 4 &*=
(&*=) :: String -> String -> LambdaTerm 
(&*=) primitive = (&*>) $ Primitive primitive

showInOrigin :: Origin -> LambdaTerm -> String
showInOrigin origin term = case (origin, term) of
    (_, Primitive var) -> var
    (AppRight, Application term1 term2) -> inParentheses $ showApplication term1 term2
    (_, Application term1 term2) -> showApplication term1 term2
    (Pure, abstraction) -> case showAbstractionRow abstraction of
        (s, term) -> showAbstraction s term
    (_, abstraction) -> case showAbstractionRow abstraction of
        (s, term) -> inParentheses $ showAbstraction s term
  where
    inParentheses :: String -> String
    inParentheses s = '(':s ++ ")"

    showApplication :: LambdaTerm -> LambdaTerm -> String
    showApplication term1 term2 = showInOrigin AppLeft term1 ++ " " ++ showInOrigin AppRight term2

    showAbstraction :: String -> LambdaTerm -> String
    showAbstraction var term = "\\" ++ var ++ "." ++ showInOrigin Pure term

    showAbstractionRow :: LambdaTerm -> (String, LambdaTerm)
    showAbstractionRow (Abstraction var body@(Abstraction _ _)) = case showAbstractionRow body of
        (result, term) -> (var ++ " " ++ result, term)
    showAbstractionRow (Abstraction var term) = (var, term)


instance Show LambdaTerm where
    show = showInOrigin Pure

