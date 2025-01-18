module AbstractSyntaxTree where
{-

    So here's the idea:
    Não temos como fazer tudo isso em um único Datatype, já que
    nem todos os tipos de dados são recursivos, por exemplo:

    [title]Titulo Qualquer[/title]
    [main]
        [title]Titulo dentro do corpo???[/title]
    [/main]

    Então, nesse caso específico, creio que teremos que
    adicionar Constraints. Eu farei isso declarando
    vários tipos de dados e dando seus contraints.

    Isso será definido por comentários na teoria.

    Inclusive, devido ao fato de estarmos trabalhando estritamente
    com texto, eu não farei a árvore geral ser polimórfica.
   
    Simplesmente não faz sentido. Mas qualquer coisa eu aplico
    isso depois se for necessário.

-}


{- 

Texto Padrão.
1) Default é automático e é o tipo usado dentro das tags.

2) O texto também segue a regra CUBIK
-- [c]crossed[/c]
-- [u]underlined[/u]
-- [b]bold[/b]
-- [i]italic[/i]
-- [k]inline code [/k] 

-}

data TextTag = Default String 
            | Crossed String    -- [c]
            | Underlined String -- [u]
            | Bold String       -- [b]
            | Italic String     -- [i]
            | CodeInline String -- [k]
            deriving (Show)

data AbntSection = Author String
                 | Institution String
                 | Subtitle String
                 | Location String
                 | Year String
                 deriving (Show)

{- 

    A seção principal, definida pela tag [main].
    A [main] é uma tag que possui vários elementos
    recursivos, tal como [>>], [chap].
    
    Neste caso, uma das formas que eu consegui fazer
    com que a Main Section pudesse conter variados
    parágrafos (sem tags definidas) foi usando
    da lógica de uma Árvore Binária.

    A Paragraph em especial é composta por um Text
    (seja ele Default, Bold, Italic ou o que seja) e
    uma chamada recursiva da própria Main.

    Caso removemos isso, somos incapazes de fazer o 
    básico como:
        
        [main]
            Isso é um texto com lista.
            [ >> | Mas essa lista nunca será chamada ]
                O que é uma pena.
            [/>>]
        [/main]
    
    Para arrumar isso, defini a existencia de Paragraph
    desta mesma forma, além de adicionar um Conclusion
    e um Empty como método de prevenção.

    Podemos usar um ou outro, não importa. Vou deixar
    ao critério do usuário.

        [main]
            Agora isso é possivel.
            [ >> | Maluquice. ]
                Com certeza doideira..
            [/>>]
        [/main]
    
        [main]
            Isso aqui também é válido.
            [chap | Doideira. ]
                Com certeza doideira...
            [/chap]
        [/main]

    Uma versão visual da definição de cada elemento
    se encontra em Markers/tree.png
-}

data MainSection =  
                    Empty
                    | Paragraph TextTag
                    | Abnt [AbntSection]
                    | List String [MainSection] -- [>> | titulo]
                    | Chap String [MainSection] -- [chap | titulo]
                    | Ref String String String String String [MainSection] -- [ref | URL | Nome do Autor | Ano | Data de Acesso] QualquerCoisa [/ref]
                    | Link String [MainSection] -- [link | URL] texto [/link]
                    | Image String [MainSection] -- [img | URI] texto [/img]
                    | Code [MainSection] -- [code]
                    | LineBreak 
                    -- | DictPage String [TextTag] -- MELHOR NÃO PENSAR NISSO AGORA.
                    deriving (Show)
{- 

    Definição do Arquivo.
    Isso é a definição padrão de um Arquivo Markers.

    Título
    [main]
        Conteudo da Main...
    [/main]

    Como título é uma das poucas "tags" que não podem ser
    chamada recursivamente, tive que fazer esse voodoo.
    Acho que está certo.

-}

data Markers = MarkersMain String [MainSection]
               deriving (Show)
{-
testing :: Markers
testing = MarkersMain
    "Primeiro Arquivo do Markers"
    ( Paragraph (Default "Isso é um teste do ") (
               Paragraph (Bold "Markers.") (
                
               Chap "Será que Funcionou?" 
               (Paragraph (Default "Se um dia isso parsear...")
                    (List "Então o que acontece?" 
                    ((Italic "Ganhamo")))
                )
                )
    )) -}


-- PS isso aqui tudo foi feito na raça sem uso de LLM
-- Espero que esteja orgulhoso de mim, mestre garcia.

-- Agora é descobrir como parsear sadisgraça.

{- 
::::::::::::::::-===+############################################################*=-::::::::::::::::
:::::::::::::::-+##################################################################*=-::::::::::::::
:::::::::::::-=**####################################################################+:.::::::::::::
::::::::::.:=+=:+####################################################################+-:::::::::::::
:::::::::::::-+########################################################################+-:::::::::::
::::::::::.:=*#####################################*+=-=+################################+:.::::::::
::::::::::.:+####################***#############*=---+*########*===+*###################+-.::::::::
::::::::.:=+*##############*+++*#########*+*######*++**#######*=-:::=*############**#####+-.::::::::
::::::::.:+##############******##########+--=+########*+======-:::-=*##**########*++**###*+=::::::::
::::::::-=*############**++*###****######+--=+######*+-:::::::::-=+###*==*#######*+++*#####*-.::::::
::::::.:+#############*++++*#**=--=+####*=:=*#*****+-:::::::--==+#####*+=*########****#####*=-::::::
:::::::-*#############*++*#**=---==+*##+---=++-:::::::::::--+##******############*=+#########*-..:::
::::.-+###############*+++==----=+*=-+*+-=*+-:::::::..:-+####*=.....-+###########*=+#*++#####*-..:::
::::.-*###############*=---------==--=*+-:-:::::::...::::---::......-+*=-+#+---+##*+===+#####*-..:::
::.-+*##############*++=----------------::::::::...................::-=+*#*=::::--==---+*####*-.::::
:..-*###############*+==--------------::::::::::...................::.:-==--:::::::----+*#####*+-.::
:..-*################*+=--------------:::::::::::............................::::::::--+*######*-..:
:..-*#################*=-------------::::::::::::::..........................::::::::--+*######*-..:
:..-*#################*=-----------:::::::::::::::::::.....................:::::::::---+*######*-..:
:::=*#################*=--------:::::-----------------:::::::::::::::::----------------+*######*-..:
.-*###################*=---------:::-+###############*+-:::::::::::::-=*#############*++#######*-..:
.-*###################*=-----:::::-+###################*=:::::::::::-*#########################*-..:
.-*###################*=----=+*****######################*=:::::::::=*#########################*-..:
.-*####################******########**++=-=*####*+=*######**********########+-:-=+++++*#######*-..:
::-+#########################*+=+*#**++++-:=*####*-:-=+***##################*=..:-==---=+*#####*-..:
:..-*###*++*###########*+++++=---+*+==+++=-==++++=-:--=+++*#**+++==++##**+++=-::----:::--=*####*-..:
:..-*#*+=--=*#########*=--------=+*=:-=====--::::--==+===+*#*=-:...:-**+=---------::::---=*###*=-:::
:..-**+=-:-=*#########*=---------==-:::------==---=======+**+=-:...:-+*---=====-::::::::-=*##*-..:::
:..-*##*=::-+#########+=---------===----:-======-:::------===--:...:-==-:::::::-------::-=*##+-.::::
:..-**+=--:-+#######*==----------==-::::::::::::::::::::--==-:::...:-==-:..:::::::::::--=+*+-:::::::
:..:-=--:::-+#######*=------------===-::::::::::::::::--===--:::...:-===-::...::::::::.-*+-:::::::::
::::.::::::-+#####*++=-----------------:::::::::::::--====-:::::...::--==--:::::::::::..::::::::::::
::::..:::-=**#****+++==-----------::::::::....::---===---:::::::.....::--====--:::........::::::::::
::::..:::-=++==-=+++++==---------:::::::::...::::---==-::::::::......::::--==--:::...:::..::::::::::
::::...:::::::::-=++++==-------::::::::::::::::::::----:::::::::::::::::::::-:::::::::::..::::::::::
::::::.:-=-:--::-=++++===-------::::::::::::::::::--::---::::::---------::::::::::::::::..::::::::::
::::::.:+*+=--:-=+++++++==--------:::::::::::-----::::::-----------------::::---::::::::..::::::::::
::::::::.:+**+++++++++++++==--------::::::::----::::::::::-------------::::::::-----::::..::::::::::
:::::::::::-+###*+++++++++==--------------------:::::::::::::::::::::::::::::::--------:..::::::::::
::::::::::.:+###*+++++++++++==----------------::::::::::---------------::::::::::-------..::::::::::
::::::::.:=+*####**+++++++++==----------------::::::-==========-::-=====--:::::::-------..::::::::::
::::::::::-===*###*++++++++++===-----------------:::-----------::::------:::::----------..::::::::::
:::::::::::..:=###*++++++++++===-------------------------:::-------:::------------------..::::::::::
::::::::::::.:-+*#*+++++++++===---------------------------::-=====-::-------------------..::::::::::
::::::::::::::::=+++++++++++++===------------------------::::::::::::::-----------------..::::::::::
::::::::::::::::::-=+++++++++++++==------------------------::::::::::::----------------:..::::::::::
::::::::::......-=++++++++++++++++==-------------------::::::::::::::::---------------..::::::::::::
::.......:=***+==+++++++++++++++++++====----------------:::::::::::::--------------==-:.::::::::::::
.::------=*###*+++++++++++++++++++++++++=========----------------------------======-::::::::::::::::
:--------=*###*+++++++++++++++++++++++++++++++++=========-----------==========+++=-:::::::::::::::::
---------=+*###*++++++++++++++++++++++++++++++++++++++++=============+++++++==---:::::::::::::::::::
-----------=+*##**++++++++++++++++++++++++++++++++++++++++++++++++++++++++**+-...:::::::::::::::::::
-------------=+*##**++++++++++++++++++++++++++++++++++++++++++++++++++++*###*=::::::::::::::::::::::
-}