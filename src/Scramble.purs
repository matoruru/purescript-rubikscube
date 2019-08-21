module RubiksCube.Scrabmle
   ( scramble
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

import Data.Array (length, (!!), (:))
import Data.Foldable (foldr)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Random (randomInt)

scramble :: Cube -> Int -> Effect Cube
scramble cube n = foldr ($) cube <$> genScramble n

genScramble :: Int -> Effect (Array (Cube -> Cube))
genScramble 0 = pure []
genScramble n' = (:) <$> scrmbl <*> (genScramble $ n' - 1)
   where
      scrmbl  = fromMaybe identity <$> scrmbl'
      scrmbl' = (!!) operations <$> randomInt 0 maxIdx
      maxIdx  = length operations - 1

operations :: Array (Cube -> Cube)
operations = [ f, f', r, r', b, b', u, u', l, l', d, d', s, s', m, m', e, e', x, x', y, y', z, z' ]
