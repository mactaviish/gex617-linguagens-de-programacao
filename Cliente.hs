
--module Main(Main) where
--main :: IO ()
--main = do putStrLn "Qual seu nome?"
--    let Pessoa

data Cliente = OrgGov String
             | Empresa String Integer String String
             | Individuo Pessoa
               deriving Show

data Pessoa = Pessoa String String Genero
              deriving Show

data Genero = Masculino | Feminino | Outro
              deriving Show

-- primeira versão nomeCliente
nomeCliente :: Cliente -> String
nomeCliente cliente =
    case cliente of
        OrgGov nome                      -> nome
        Empresa nome _ _ _               -> nome
        Individuo (Pessoa pNome sNome _) -> pNome ++ " " ++ sNome

-- segunda versão nomeCliente
nomeCliente2 :: Cliente -> String
nomeCliente2 (OrgGov nome)                      = nome
nomeCliente2 (Empresa nome _ _ _)               = nome
nomeCliente2 (Individuo (Pessoa nome sobrenome _)) = nome ++ " " ++ sobrenome

-- função parcial
nomeEmpresa :: Cliente -> Maybe String
nomeEmpresa cliente =
    case cliente of
        Empresa nome _ _ _ -> Just nome
        _                  -> Nothing