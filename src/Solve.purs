module RubiksCube.Solve
   ( calcSolution
   , rotation
   , Direction
   ) where

import Prelude

import RubiksCube ( Cube
                  , f, f'
                  , r, r'
                  , b, b'
                  , u, u'
                  , l, l'
                  , d, d'
                  , s, s'
                  , m, m'
                  , e, e'
                  , x, x'
                  , y, y'
                  , z, z'
                  )

data Direction = F | F'
               | R | R'
               | B | B'
               | U | U'
               | L | L'
               | D | D'
               | S | S'
               | M | M'
               | E | E'
               | X | X'
               | Y | Y'
               | Z | Z'

instance showDirection :: Show Direction where
   show F  = "f"
   show F' = "f'"
   show R  = "r"
   show R' = "r'"
   show B  = "b"
   show B' = "b'"
   show U  = "u"
   show U' = "u'"
   show L  = "l"
   show L' = "l'"
   show D  = "d"
   show D' = "d'"
   show S  = "s"
   show S' = "s'"
   show M  = "m"
   show M' = "m'"
   show E  = "e"
   show E' = "e'"
   show X  = "x"
   show X' = "x'"
   show Y  = "y"
   show Y' = "y'"
   show Z  = "z"
   show Z' = "z'"

rotation :: Direction -> (Cube -> Cube)
rotation F  = f
rotation F' = f'
rotation R  = r
rotation R' = r'
rotation B  = b
rotation B' = b'
rotation U  = u
rotation U' = u'
rotation L  = l
rotation L' = l'
rotation D  = d
rotation D' = d'
rotation S  = s
rotation S' = s'
rotation M  = m
rotation M' = m'
rotation E  = e
rotation E' = e'
rotation X  = x
rotation X' = x'
rotation Y  = y
rotation Y' = y'
rotation Z  = z
rotation Z' = z'

directions :: Array Direction
directions = [F, F', R, R', B, B', U, U', L, L', D, D', S, S', M, M', E, E', X, X', Y, Y', Z, Z']

calcSolution :: Cube -> Array Direction
calcSolution cube = []
