import Data.List
import Data.Function

type Castka = Integer

data Operace
  = Prihlaseni
  | Odhlaseni
  | Vyber Castka
  | Pripis Castka
  deriving (Show, Read, Eq)

type Cas = Integer

type Uzivatel = String

data Zaznam =
  Zaznam Cas
         Uzivatel
         Operace
  deriving (Show, Read, Eq)

type Zaznamy = [Zaznam]

main = do
  log <- (map read . lines <$> readFile "banka.log") :: IO [Zaznam] --nacteni a rozparsovani logu
  let result cmt f --pomocna funkce na vypisovani vysledku
       = do
        putStrLn (cmt ++ ":")
        print (f log)
        putChar '\n'
  {- pocitani a vypisovani vysledku zacina zde -}
  result
    "DEMO -- jmeno prvniho uzivatele v souboru se smichanymi zaznamy"
    demoPrvniZaznam

  result "Seznam uzivatelu serazenych podle abecedy" serazeniUzivatele
  result "Casy top 10 nejvetsich vyberu" top10vyber
  result "Jmena uzivatelu 10 nejmensich pripisu" top10pripis
  result "Nejaktivnejsi uzivatel" topUzivatel
  result "Uzivatel ktery vydelal nejvic penez" topPrirustek
  result "Prumerna vybrana castka uzivatelu zacinajicich od J" prumerVyberuJ
  result
    "Uzivatel s nejdelsi posloupnosti akci nerusenou v logu jinymi uzivateli"
    nejdelsiSingleRun
  result
    "Pocet vyberu pri kterych uzivatele s 5-pismennymi jmeny nebyli prokazatelne prihlaseni"
    neprihlaseneVybery

{- Priklad -}
demoPrvniZaznam :: Zaznamy -> Uzivatel
demoPrvniZaznam (Zaznam _ jm _:_) = jm

{- Ukol zacina tady. Misto `undefined` dodejte definice funkci, ktere z logu
 - vytahnou pozadovany vysledek. -}
serazeniUzivatele :: Zaznamy -> [Uzivatel]
serazeniUzivatele list = (sort . nub) [uzivatel | Zaznam _ uzivatel _ <- list]

top10vyber :: Zaznamy -> [(Cas, Castka)]
top10vyber(list) =  (take 10 . reverse . sortBy (compare `on` snd)) [(cas, castka)| Zaznam cas _ (Vyber castka) <- list]

top10pripis :: Zaznamy -> [(Uzivatel, Castka)]
top10pripis (list) = (take 10 . sortBy (compare `on` snd)) [ (jmeno, castka) | Zaznam _ jmeno (Pripis castka) <- list]

topUzivatel :: Zaznamy -> [(Int, Uzivatel)]  
topUzivatel (list) = (take 1 . reverse . sortBy (compare `on` fst) . frequency) [jmeno | Zaznam _ jmeno _ <- list]

topPrirustek :: Zaznamy -> [(Integer, Uzivatel)]
topPrirustek (list) = (take 1 .reverse . sortBy (compare `on` fst)) (zip (map (`vydelek` list) (serazeniUzivatele list)) (serazeniUzivatele list))

-- prumer vyberu spocitejte zaokrouhleny dolu
prumerVyberuJ :: Zaznamy -> [(Integer, Uzivatel)]
prumerVyberuJ list = undefined

nejdelsiSingleRun :: Zaznamy -> Uzivatel
nejdelsiSingleRun = undefined

neprihlaseneVybery :: Zaznamy -> Integer
neprihlaseneVybery = undefined


-- Pomocne funkce
frequency :: Ord a => [a] -> [(Int,a)] 
frequency list = map (\l -> (length l, head l)) (group (sort list))

vydelek :: Uzivatel -> Zaznamy -> Integer
vydelek hledanyUzivatel list = 
  let pripisy = [castka | Zaznam _ uzivatel (Pripis castka) <- list, uzivatel == hledanyUzivatel]
      vybery = [castka | Zaznam _ uzivatel (Vyber castka) <- list, uzivatel == hledanyUzivatel]
  in sum(pripisy) - sum(vybery)


