infixl 4 :@:

data Term = Idx Int
          | Term :@: Term
          | Lmb Term
          deriving (Eq, Read, Show)

shift :: Int -> Term -> Term
shift = helper 0
    where
        helper m val (Idx x) | x < m = Idx x
                             | x >= m = Idx (x + val)
        helper m val (Lmb l) = Lmb $ helper (m + 1) val l
        helper m val (l :@: r) = (helper m val l) :@: (helper m val r)


substDB :: Int -> Term -> Term -> Term
substDB j s (Idx x) | j == x = s
                    | otherwise = Idx x
substDB j s (Lmb t) = Lmb $ substDB (j + 1) (shift 1 s) t
substDB j s (l :@: r) = (substDB j s l) :@: (substDB j s r)


betaRuleDB :: Term -> Term
betaRuleDB (Lmb t :@: s) = shift (-1) (substDB 0 (shift 1 s) t)


oneStepDBN :: Term -> Maybe Term
oneStepDBN term@(Lmb t :@: s) = Just $ betaRuleDB term
oneStepDBN (Lmb l) = Lmb <$> oneStepDBN l
oneStepDBN (l :@: r) = case oneStepDBN r of
    Just right -> Just $ l :@: right
    Nothing -> case oneStepDBN l of 
        Just left -> Just $ left :@: r
        Nothing -> Nothing
oneStepDBN _ = Nothing

oneStepDBA :: Term -> Maybe Term
oneStepDBA term@(Lmb t :@: s) = case oneStepDBA s of 
    Just s' -> Just $ (Lmb t :@: s') 
    Nothing -> Just $ betaRuleDB term
oneStepDBA (Lmb l) = Lmb <$> oneStepDBA l
oneStepDBA (l :@: r) = case oneStepDBA r of
    Just right -> Just $ l :@: right
    Nothing -> case oneStepDBA l of 
        Just left -> Just $ left :@: r
        Nothing -> Nothing
oneStepDBA _ = Nothing


nfDB :: (Term -> Maybe Term) -> Term -> Term 
nfDB f t = case f t of
    Just s -> nfDB f s
    Nothing -> t







