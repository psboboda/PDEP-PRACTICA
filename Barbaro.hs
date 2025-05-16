{-
--------------------------------------------------------PUNTO 1--------------------------------------------------------------

Se sabe que los bárbaros tienen nombre, fuerza, habilidades y objetos, que los ayudarán más adelante en su lucha contra el mal. Por ejemplo: 

dave = Barbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]

Se pide definir los siguientes objetos y definir algunos bárbaros de ejemplo
    a. Las espadas aumentan la fuerza de los bárbaros en 2 unidades por cada kilogramo de peso.
    b. Los amuletosMisticos puerco-marranos otorgan una habilidad dada a un bárbaro.
    c. Las varitasDefectuosas, añaden la habilidad de hacer magia, pero desaparecen todos los demás objetos del bárbaro.
    d. Una ardilla, que no hace nada.
    e. Una cuerda, que combina dos objetos distintos,obteniendo uno que realiza las transformaciones de los otros dos. 
-}
import Data.Char (toUpper)

instance Show Barbaro where
  show (UnBarbaro nombre fuerza habilidades _) =
    "Barbaro: " ++ nombre ++ ", Fuerza: " ++ show fuerza ++ ", Habilidades: " ++ show habilidades

data Barbaro = UnBarbaro {
    nombre :: String,
    fuerza :: Double,
    habilidades :: [String],
    objetos :: [Objeto]
} 


type Objeto = Barbaro -> Barbaro     

dave = UnBarbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]

espadasAumentadorasDeFuerza :: Double -> Objeto
espadasAumentadorasDeFuerza unPeso unBarbaro = unBarbaro { fuerza = fuerza unBarbaro + unPeso * 2}

{- 
amuletosMisticos :: Barbaro -> Barbaro
amuletosMisticos unBarbaro = unBarbaro { habilidades = "puerco-marrano" : habilidades unBarbaro }

varitasDefectuosas :: Barbaro -> Barbaro
varitasDefectuosas unBarbaro = unBarbaro { habilidades = "hacer magia" : habilidades unBarbaro, objetos = [] } 
Como se repite: habilidades = "habilidades = "puerco-marrano" : habilidades unBarbaro " creo funcion agregrarHabilidad 
-}

agregarHabilidad :: Barbaro -> String -> Barbaro 
agregarHabilidad unBarbaro nuevaHabilidad = unBarbaro { habilidades = nuevaHabilidad : habilidades unBarbaro }

amuletosMisticos :: Objeto
amuletosMisticos unBarbaro = agregarHabilidad unBarbaro  "puerco-marrano"

varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro =  (agregarHabilidad unBarbaro "hacer magia") { objetos = [] }

ardilla :: Objeto
ardilla = id 

unaCuerda :: Objeto -> Objeto -> Objeto
unaCuerda unObjeto1 unObjeto2 = unObjeto1 . unObjeto2 

------------------------------------------- PUNTO 2 ------------------------------------------------------------
{-
El megafono es un objeto que potencia al bárbaro, concatenando sus habilidades y poniéndolas en mayúsculas. 

*Main> megafono dave
Barbaro "Dave" 100 ["TEJERESCRIBIRPOESIA"] [<function>,<function>]

Sabiendo esto, definir al megafono, y al objeto megafonoBarbarico, que está formado por una cuerda, una ardilla y un megáfono. 
 Recuerden que esta la función toUpper que toma una Char y devuelve un Char pero en mayúscula
-}

megafono :: Objeto
megafono unBarbaro = unBarbaro { habilidades = [map toUpper $ concat (habilidades unBarbaro)]}

megafonoBarbarico :: Objeto
megafonoBarbarico = unaCuerda ardilla megafono 

------------------------------------------- PUNTO 3 ------------------------------------------------------------
{-
Los bárbaros suelen ir de aventuras por el reino luchando contra las fuerzas del mal, pero ahora que tienen nuestra ayuda, quieren que se les diga si un grupo de bárbaros puede sobrevivir a cierta aventura.  Una aventura se compone de uno o más eventos, por ejemplo:

    invasionDeSuciosDuendes: Un bárbaro sobrevive si sabe “Escribir Poesía Atroz”
    cremalleraDelTiempo: Un bárbaro sobrevive si no tiene pulgares. Los bárbaros llamados Faffy y Astro no tienen pulgares, los demás sí. 
ritualDeFechorias: Un bárbaro puede sobrevivir si pasa una o más pruebas como las siguientes: 
    *saqueo: El bárbaro debe tener la habilidad de robar y tener más de 80 de fuerza.
    *gritoDeGuerra: El bárbaro debe tener un poder de grito de guerra igual a la cantidad de letras de sus habilidades. El poder necesario para aprobar es 4 veces la cantidad de objetos del bárbaro.
    *caligrafia: El bárbaro tiene caligrafía perfecta (para el estándar barbárico de la época) si sus habilidades contienen más de 3 vocales y comienzan con mayúscula.

Sabiendo esto, se pide:
Definir los eventos, modelar las aventuras y dar un ejemplo. 
Definir la función sobrevivientes que tome una lista de bárbaros y una aventura, y diga cuáles bárbaros la sobreviven (es decir, pasan todas las pruebas)
-}
type Evento = Barbaro -> Bool
type Aventura = [Evento]

tieneHabilidad :: String -> Barbaro -> Bool
tieneHabilidad unaHabilidad unBarbaro = any ((==) unaHabilidad) (habilidades unBarbaro)

invasionDeSuciosDuendes :: Evento 
invasionDeSuciosDuendes  = tieneHabilidad "Escribir Poesia Atroz" 

cremalleraDelTiempo :: Evento
cremalleraDelTiempo unBarbaro = nombre unBarbaro == faffy  || nombre unBarbaro == astro   

saqueo :: Evento
saqueo = tieneHabilidad "Robar" && fuerza > 80

gritoDeGuerra :: 

ritualDeFechorias :: Evento
ritualDeFechorias unEvento 
  | saqueo
  | 