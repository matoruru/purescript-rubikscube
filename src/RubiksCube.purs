module RubiksCube
   ( Cube
   , createCube
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
   ) where

import Prelude

data C = W | O | Y | G | R | B

instance showC :: Show C where
   show W = "W"
   show O = "O"
   show Y = "Y"
   show G = "G"
   show R = "R"
   show B = "B"

derive instance eqC :: Eq C

data Cube = Cube C C C  C C C  C C C  C C C  C C C  C C C
                 C C C  C C C  C C C  C C C  C C C  C C C
                 C C C  C C C  C C C  C C C  C C C  C C C

instance showCube :: Show Cube where
   show (Cube f1 f2 f3  r1 r2 r3  b1 b2 b3  u1 u2 u3  l1 l2 l3  d1 d2 d3
              f4 f5 f6  r4 r5 r6  b4 b5 b6  u4 u5 u6  l4 l5 l6  d4 d5 d6
              f7 f8 f9  r7 r8 r9  b7 b8 b9  u7 u8 u9  l7 l8 l9  d7 d8 d9)
        = line1 <> line2 <> line3 <> line4 <> line5 <> line6 <> line7 <> line8 <> line9
             where
                line1 = "    " <> show l7 <> show l8 <> show l9 <> "\n"
                line2 = "    " <> show l4 <> show l5 <> show l6 <> "\n"
                line3 = "    " <> show l1 <> show l2 <> show l3 <> "\n"
                line4 = show f7 <> show f8 <> show f9 <> " " <>
                        show d1 <> show d4 <> show d7 <> " " <>
                        show b9 <> show b6 <> show b3 <> " " <>
                        show u3 <> show u2 <> show u1 <> "\n"
                line5 = show f4 <> show f5 <> show f6 <> " " <>
                        show d2 <> show d5 <> show d8 <> " " <>
                        show b8 <> show b5 <> show b2 <> " " <>
                        show u6 <> show u5 <> show u4 <> "\n"
                line6 = show f1 <> show f2 <> show f3 <> " " <>
                        show d3 <> show d6 <> show d9 <> " " <>
                        show b7 <> show b4 <> show b1 <> " " <>
                        show u9 <> show u8 <> show u7 <> "\n"
                line7 = "    " <> show r9 <> show r6 <> show r3 <> "\n"
                line8 = "    " <> show r8 <> show r5 <> show r2 <> "\n"
                line9 = "    " <> show r7 <> show r4 <> show r1

derive instance eqCube :: Eq Cube

createCube :: Cube
createCube = Cube W W W  O O O  Y Y Y  G G G  R R R  B B B
                  W W W  O O O  Y Y Y  G G G  R R R  B B B
                  W W W  O O O  Y Y Y  G G G  R R R  B B B

newtype Rotation (Cube -> Cube)

f_ :: Cube -> Cube
f_   (Cube f1 f2 f3  r1 r2 r3  b1 b2 b3  u1 u2 u3  l1 l2 l3  d1 d2 d3
           f4 f5 f6  r4 r5 r6  b4 b5 b6  u4 u5 u6  l4 l5 l6  d4 d5 d6
           f7 f8 f9  r7 r8 r9  b7 b8 b9  u7 u8 u9  l7 l8 l9  d7 d8 d9)
   =  Cube f7 f4 f1  r1 r2 r3  b1 b2 b3  l1 u2 u3  d3 l2 l3  r9 r8 r7
           f8 f5 f2  r4 r5 r6  b4 b5 b6  l4 u5 u6  d2 l5 l6  d4 d5 d6
           f9 f6 f3  u1 u4 u7  b7 b8 b9  l7 u8 u9  d1 l8 l9  d7 d8 d9

s_ :: Cube -> Cube
s_   (Cube f1 f2 f3  r1 r2 r3  b1 b2 b3  u1 u2 u3  l1 l2 l3  d1 d2 d3
           f4 f5 f6  r4 r5 r6  b4 b5 b6  u4 u5 u6  l4 l5 l6  d4 d5 d6
           f7 f8 f9  r7 r8 r9  b7 b8 b9  u7 u8 u9  l7 l8 l9  d7 d8 d9)
   =  Cube f1 f2 f3  r1 r2 r3  b1 b2 b3  u1 l2 u3  l1 d6 l3  d1 d2 d3
           f4 f5 f6  u2 u5 u8  b4 b5 b6  u4 l5 u6  l4 d5 l6  r6 r5 r4
           f7 f8 f9  r7 r8 r9  b7 b8 b9  u7 l8 u9  l7 d4 l9  d7 d8 d9

x_ :: Cube -> Cube
x_   (Cube f1 f2 f3  r1 r2 r3  b1 b2 b3  u1 u2 u3  l1 l2 l3  d1 d2 d3
           f4 f5 f6  r4 r5 r6  b4 b5 b6  u4 u5 u6  l4 l5 l6  d4 d5 d6
           f7 f8 f9  r7 r8 r9  b7 b8 b9  u7 u8 u9  l7 l8 l9  d7 d8 d9)
   =  Cube d3 d6 d9  r7 r4 r1  u7 u4 u1  f9 f8 f7  l3 l6 l9  b9 b8 b7
           d2 d5 d8  r8 r5 r2  u8 u5 u2  f6 f5 f4  l2 l5 l8  b6 b5 b4
           d1 d4 d7  r9 r6 r3  u9 u6 u3  f3 f2 f1  l1 l4 l7  b3 b2 b1

reverse :: (Cube -> Cube) -> (Cube -> Cube)
reverse f = f >>> f >>> f

f  :: Cube -> Cube
f  = f_

f' :: Cube -> Cube
f' = reverse f

r  :: Cube -> Cube
r  = y >>> f >>> y'

r' :: Cube -> Cube
r' = reverse r

b  :: Cube -> Cube
b  = y >>> y >>> f >>> y >>> y

b' :: Cube -> Cube
b' = reverse b

u  :: Cube -> Cube
u  = x' >>> f >>> x

u' :: Cube -> Cube
u' = reverse u

l  :: Cube -> Cube
l  = y' >>> f >>> y

l' :: Cube -> Cube
l' = reverse l

d  :: Cube -> Cube
d  = x >>> f >>> x'

d' :: Cube -> Cube
d' = reverse d

s  :: Cube -> Cube
s  = s_

s' :: Cube -> Cube
s' = reverse s

m  :: Cube -> Cube
m  = y' >>> s >>> y

m' :: Cube -> Cube
m' = reverse m

e  :: Cube -> Cube
e  = x >>> s >>> x'

e' :: Cube -> Cube
e' = reverse e

x  :: Cube -> Cube
x  = x_

x' :: Cube -> Cube
x' = reverse x

y  :: Cube -> Cube
y  = u >>> e' >>> d'

y' :: Cube -> Cube
y' = reverse y

z  :: Cube -> Cube
z  = f >>> s >>> b'

z' :: Cube -> Cube
z' = reverse z
