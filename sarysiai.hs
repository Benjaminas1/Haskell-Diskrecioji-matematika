import Data.List --funkcijai transpose
import System.IO
type Vektorius = [Int]
type Matrica = [[Int]]

eilSk :: Matrica -> Int
eilSk = length

stulpSk :: Matrica -> Int
stulpSk = length . head

vektoriuSuma :: Vektorius -> Vektorius -> Vektorius
vektoriuSuma = zipWith (+)

matricuSuma :: Matrica -> Matrica -> Matrica
matricuSuma = zipWith vektoriuSuma

vektoriuAtimtis :: Vektorius -> Vektorius -> Vektorius
vektoriuAtimtis = zipWith (-)

matricuAtimtis :: Matrica -> Matrica -> Matrica
matricuAtimtis = zipWith vektoriuAtimtis

---------------------------------------------------------
-- Atvirkštinio sąryšio matricos suradimas

-- Veikimas:
-- Funkcija 'sukurtiAtvirkstineMatrica' paduoda 'pridetiEilute' funkcijai matricos stulpelio indeksą
-- 'pridetiEilute' funkcija iš kiekvienos eilutės paima skaičius pagal gautą stulpelio indeksą ir sudeda į vieną vektorių, kurį grąžina 'sukurtiAtvirkstineMatrica' funkcijai
-- 'sukurtiAtvirkstineMatrica' gautus vektorius sudeda į vieną listą ir grąžina atvirkštinę sąryšio matricą
pridetiEilute :: Matrica -> Int -> Vektorius
pridetiEilute m i2 = [m !! i1 !! i2 | i1<-[0..(eilSk(m)-1)]]

sukurtiAtvirkstineMatrica :: Matrica -> Matrica
sukurtiAtvirkstineMatrica m = [pridetiEilute m i2 | i2<-[0..(stulpSk(m)-1)]]

---------------------------------------------------------
-- Tapatumo sąryšio sukūrimas

-- Veikimas:
-- Vyksta dvigubas ciklas, jo metu į matricą pridedamas skaičius 1, kai jos eilutės ir stulpelio indeksai sutampa, kitu atveju pridedamas 0
pridetiEilute2 :: Matrica -> Int -> Vektorius
pridetiEilute2 m i2 = [if i1==i2 then 1 else 0 | i1<-[0..(eilSk(m)-1)]]

sukurtiTapatumoSarysi :: Matrica -> Matrica
sukurtiTapatumoSarysi m = [pridetiEilute2 m i2 | i2<-[0..(stulpSk(m)-1)]]

---------------------------------------------------------

-- Suranda ir susumuoja matricos įstrižainėje esancius simbolius
istrizaine :: Matrica -> Int
istrizaine m = sum(zipWith (!!) m [0..])

refleksyvumas :: Matrica -> IO ()
refleksyvumas m = 
    if (istrizaine m) == 0 then putStrLn("Antirefleksyvusis")
    else if (istrizaine m) == (eilSk m) then putStrLn("Refleksyvusis")
    else putStrLn("Nei refleksyvusis, nei antirefleksyvusis")

---------------------------------------------------------

-- Veikimas:
-- Pasinaudojus 'matricuSuma' ir 'matricuAtimtis' funkcijomis yra sudedamos pradinio sąryšio matrica ir atvirkštinio sąryšio matrica, o iš jų sudėties atimama tapatumo sąryšio matrica
-- Komanda "2 `elem` sujungtiElementai" patikrina ar matricoje yra bent vienas skaičius 2, 
-- jeigu yra - reiškia pradinė matrica turi elementų (esančių ne tapatumo sąryšyje), kurie sutampa su atvirkštinio sąryšio matricos elementais.
-- Tokiu atveju matrica yra antisimetrinė.
yraAntisimetrine :: Matrica -> Matrica -> Matrica -> Bool
yraAntisimetrine m aM tS = do
        let modifikuotaMatrica = matricuAtimtis (matricuSuma m aM) tS
        let sujungtiElementai = concat modifikuotaMatrica
        if (2 `elem` sujungtiElementai) then False else True

-- m - pradinė sąryšio matrica
-- aM - atvirkštinė sąryšio matrica
-- tS - tapatumo sąryšio matrica
simetriskumas :: Matrica -> Matrica -> Matrica -> IO ()
simetriskumas m aM tS =
    if m == aM then putStrLn("Simetrinis")
    else if (yraAntisimetrine m aM tS) then putStrLn("Antisimetrinis")
    else putStrLn("Asimetrinis")

---------------------------------------------------------

-- Veikimas:
-- Pasinaudojus 'matricuSuma' funkcija yra sudedamos matricos - pradinio sąryšio matrica, atvirkštinė ir tapatumo sąryšio matrica
-- Komanda "0 `elem` sujungtiElementai" patikrina ar sudėjus matricas lieka elementas 0, jeigu lieka - reiškia, kad sąryšis nėra pilnasis.
yraPilnoji :: Matrica -> Matrica -> Matrica -> Bool
yraPilnoji m aM tS = do
        let sudetosMatricos = matricuSuma (matricuSuma m aM) tS
        let sujungtiElementai = concat sudetosMatricos
        if (0 `elem` sujungtiElementai) then False else True


pilnumas :: Matrica -> Matrica -> Matrica -> IO ()
pilnumas m aM tS =
    if (yraPilnoji m aM tS) then putStrLn("Pilnasis")
    else putStrLn("Ne pilnasis")

---------------------------------------------------------

-- Veikimas:
-- Tikrinamas kiekvienas matricos elementas, jeigu jo reikšmė yra lygi 1, matricos eilutės/stulpelio indeksas įdedamas į listą
sarysioEil :: Matrica -> Vektorius
sarysioEil m = [x | x <- [0..(eilSk(m)-1)], y <- [0..(stulpSk(m)-1)], (m !! x !! y)==1]

sarysioStulp :: Matrica -> Vektorius
sarysioStulp m = [y | x <- [0..(eilSk(m)-1)], y <- [0..(stulpSk(m)-1)], (m !! x !! y)==1]

-- Veikimas:
-- Vyksta ciklas per visus sąryšio s (kurio struktūra [(a,b)]) x elementus ir ieško y elementų, kurių skaičius a yra lygus x elemento skaičiui b
kompozicija :: [(Int,Int)] -> [(Int,Int)]
kompozicija s = [(,) (fst (s !! x)) (snd (s !! y)) | x <- [0..((length s)-1)], y <- [0..((length s)-1)], (snd (s !! x)) == (fst (s !! y)), x /= y]

-- Veikimas:
-- Pasinaudojus komanda 'filter' pašalinami visi kompozicijos elementai, kurie neegzistuoja sąryšyje.
-- Jeigu atsiranda pašalintų reikšmių - reiškia, kad sąryšis nėra tranzityvus.
yraTranzityvus :: [(Int,Int)] -> Bool
yraTranzityvus s = do
        let komp = kompozicija s
        if length (filter (`elem` s) komp) /= length komp then False
        else True

tranzityvumas :: Matrica -> IO ()
tranzityvumas m = do
        let sarysis = zip (sarysioEil m) (sarysioStulp m)
        let tranzityvus = yraTranzityvus sarysis
        if tranzityvus then putStrLn("Tranzityvus")
        else putStrLn("Ne tranzityvus")

---------------------------------------------------------

isnagrinetiMatrica m = do
    let atvirkstineM = sukurtiAtvirkstineMatrica m
    let tapatumoSarysis = sukurtiTapatumoSarysi m

    refleksyvumas m
    simetriskumas m atvirkstineM tapatumoSarysis
    pilnumas m atvirkstineM tapatumoSarysis
    tranzityvumas m

run2 = do
    let m = [[1,0,0,0,0,1],[0,1,0,0,1,0],[0,0,0,1,0,0],[0,0,1,1,0,0],[0,1,0,1,1,0],[1,0,0,0,0,0]]
    putStrLn("----------------------------------------")
    
    if (eilSk m) /= (stulpSk m)
        then putStrLn("Ivesta matrica ne kvadratine")
        else isnagrinetiMatrica m

    putStrLn("----------------------------------------")
            
run m = do
    putStrLn("----------------------------------------")

    if (eilSk m) /= (stulpSk m)
        then putStrLn("Ivesta matrica ne kvadratine")
        else isnagrinetiMatrica m

    putStrLn("----------------------------------------")