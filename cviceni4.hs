import Data.List
import Data.Function
import Data.Ord

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
serazeniUzivatele log = (sort . nub) [uzivatel | Zaznam _ uzivatel _ <- log]

top10vyber :: Zaznamy -> [(Cas, Castka)]
top10vyber log = (take 10 . reverse . sortBy (compare `on` snd)) [(cas, castka)| Zaznam cas _ (Vyber castka) <- log]

top10pripis :: Zaznamy -> [(Uzivatel, Castka)]
top10pripis log = (take 10 . sortBy (compare `on` snd)) [ (jmeno, castka) | Zaznam _ jmeno (Pripis castka) <- log]

topUzivatel :: Zaznamy -> [(Uzivatel, Int)]  
topUzivatel log = (take 1 . reverse . sortBy (compare `on` snd) . map ( \uzivatel -> (head uzivatel, length uzivatel)) . group . sort) [jmeno | Zaznam _ jmeno _ <- log]

topPrirustek :: Zaznamy -> [(Integer, Uzivatel)]
topPrirustek log = (take 1 .reverse . sortBy (compare `on` fst)) (zip (map (`vydelek` log) (serazeniUzivatele log)) (serazeniUzivatele log))

-- prumer vyberu spocitejte zaokrouhleny dolu
prumerVyberuJ :: Zaznamy -> Integer
prumerVyberuJ log = 
  let vybery = [castka| Zaznam _ uzivatel (Vyber castka) <- log, head uzivatel == 'J']
  in sum(vybery) `div` genericLength(vybery)

nejdelsiSingleRun :: Zaznamy -> [(Uzivatel, Int)]
nejdelsiSingleRun log = 
  let serazeniUzivatele = [uzivatel | (Zaznam _ uzivatel _ ) <- sort log]
  in (take 1 . reverse . sortBy (compare `on` snd) . map ( \uzivatel -> (head uzivatel, length uzivatel)). group) serazeniUzivatele

neprihlaseneVybery :: Zaznamy -> Integer
neprihlaseneVybery = undefined


-- Pomocne funkce
vydelek :: Uzivatel -> Zaznamy -> Integer
vydelek hledanyUzivatel log = 
  let pripisy = [castka | Zaznam _ uzivatel (Pripis castka) <- log, uzivatel == hledanyUzivatel]
      vybery = [castka | Zaznam _ uzivatel (Vyber castka) <- log, uzivatel == hledanyUzivatel]
  in sum(pripisy) - sum(vybery)

instance Ord Zaznam where
  compare (Zaznam cas1 _ _) (Zaznam cas2 _ _) = compare cas1 cas2 
