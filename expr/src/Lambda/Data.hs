module Lambda.Data(LambdaTerm(..), (&*), (&->), (&*>), (&*<), (&*=)) where

data LambdaTerm = Primitive String | Application LambdaTerm LambdaTerm | Abstraction String LambdaTerm

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
    (origin, Abstraction var1 (Abstraction var2 term)) -> showInOrigin origin (Abstraction (var1 ++ " " ++ var2) term)
    (Pure, Abstraction var term) -> showAbstraction var term
    (_, Abstraction var term) -> inParentheses $ showAbstraction var term
  where
    inParentheses :: String -> String
    inParentheses s = '(':s ++ ")"

    showApplication :: LambdaTerm -> LambdaTerm -> String
    showApplication term1 term2 = showInOrigin AppLeft term1 ++ " " ++ showInOrigin AppRight term2

    showAbstraction :: String -> LambdaTerm -> String
    showAbstraction var term = "\\" ++ var ++ "." ++ showInOrigin Pure term



instance Show LambdaTerm where
    show = showInOrigin Pure

