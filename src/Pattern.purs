module RubiksCube.Pattern where

import Prelude

import RubiksCube

checkerCube :: Cube -> Cube
checkerCube = m >>> m >>> e >>> e >>> s >>> s

cross :: Cube -> Cube
cross = r >>> r >>> l' >>> d >>> f >>> f >>> r' >>> d' >>> r' >>> l >>> u' >>> d >>> r >>> d >>> b >>> b >>> r' >>> u >>> d >>> d

cross4 :: Cube -> Cube
cross4 = u >>> u >>> r >>> r >>> l >>> l >>> f >>> f >>> b >>> b >>> d >>> d >>> l >>> l >>> r >>> r >>> f >>> f >>> b >>> b

cu6 :: Cube -> Cube
cu6 = u' >>> b >>> b >>> u >>> l >>> l >>> d >>> l >>> l >>> r >>> r >>> d' >>> b' >>> r >>> d' >>> l >>> r' >>> b >>> b >>> u >>> u >>> f' >>> l' >>> u'

cubeInCube :: Cube -> Cube
cubeInCube = f >>> l >>> f >>> u' >>> r >>> u >>> f >>> f >>> l >>> l >>> u' >>> l' >>> b >>> d' >>> b' >>> l >>> l >>> u

cubeInCubeInCube :: Cube -> Cube
cubeInCubeInCube = u' >>> l' >>> u' >>> f' >>> r >>> r >>> b' >>> r >>> f >>> u >>> b >>> b >>> u >>> b' >>> l >>> u' >>> f >>> u >>> r >>> f'

deckerboard :: Cube -> Cube
deckerboard = u >>> d >>> r >>> l' >>> f' >>> b >>> u >>> d' >>> r >>> r >>> u >>> r >>> r >>> l >>> l >>> d >>> d >>> f >>> f >>> b >>> b >>> d

giftBox :: Cube -> Cube
giftBox = u >>> b >>> b >>> r >>> r >>> b >>> b >>> l >>> l >>> f >>> f >>> r >>> r >>> d' >>> f >>> f >>> l >>> l >>> b >>> f' >>> l >>> f >>> f >>> d >>> u' >>> r >>> r >>> f' >>> l' >>> r'

h6 :: Cube -> Cube
h6 = u >>> u >>> d >>> d >>> l >>> l >>> u >>> u >>> d >>> d >>> r >>> r >>> f >>> f >>> b >>> b >>> l >>> l >>> f >>> f >>> b >>> b >>> r >>> r >>> u >>> u >>> d >>> d >>> f >>> f >>> u >>> u >>> d >>> d >>> b >>> b

plusMinus :: Cube -> Cube
plusMinus = u >>> u >>> r >>> r >>> l >>> l >>> u >>> u >>> r >>> r >>> l >>> l

plusMinusCheck :: Cube -> Cube
plusMinusCheck = u >>> d >>> r >>> r >>> l >>> l >>> u >>> d >>> r >>> r >>> l >>> l

spot4 :: Cube -> Cube
spot4 = f >>> f >>> b >>> b >>> u >>> d' >>> r >>> r >>> l >>> l >>> u >>> d'

spot6 :: Cube -> Cube
spot6 = e >>> m >>> e' >>> m'

superflip :: Cube -> Cube
superflip = u >>> r >>> r >>> f >>> b >>> r >>> b >>> b >>> r >>> u >>> u >>> l >>> b >>> b >>> r >>> u' >>> d' >>> r >>> r >>> f >>> r' >>> l >>> b >>> b >>> u >>> u >>> f >>> f

t4 :: Cube -> Cube
t4 = f >>> f >>> d >>> d >>> f' >>> l >>> l >>> d >>> d >>> u >>> u >>> r >>> r >>> b' >>> u >>> u >>> f >>> f

t6 :: Cube -> Cube
t6 = l >>> l >>> u >>> u >>> f >>> f >>> d >>> d >>> r >>> r >>> u >>> u >>> r >>> r >>> f >>> f >>> r >>> r >>> u >>> u

tablecloth :: Cube -> Cube
tablecloth = r >>> l >>> u >>> u >>> f' >>> u >>> u >>> d >>> d >>> r >>> r >>> l >>> l >>> f' >>> d >>> d >>> f >>> f >>> d >>> r >>> r >>> l >>> l >>> f >>> f >>> b >>> b >>> d >>> b >>> b >>> l >>> l

vertialStripes :: Cube -> Cube
vertialStripes = f >>> u >>> f >>> r >>> l >>> l >>> b >>> d' >>> r >>> d >>> d >>> l >>> d' >>> b >>> r >>> r >>> l >>> f >>> u >>> f
