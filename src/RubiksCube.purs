module RubiksCube where

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

f :: Cube -> Cube
f   (Cube f1 f2 f3  r1 r2 r3  b1 b2 b3  u1 u2 u3  l1 l2 l3  d1 d2 d3
          f4 f5 f6  r4 r5 r6  b4 b5 b6  u4 u5 u6  l4 l5 l6  d4 d5 d6
          f7 f8 f9  r7 r8 r9  b7 b8 b9  u7 u8 u9  l7 l8 l9  d7 d8 d9)
  =  Cube f7 f4 f1  r1 r2 r3  b1 b2 b3  l1 u2 u3  d3 l2 l3  r9 r8 r7
          f8 f5 f2  r4 r5 r6  b4 b5 b6  l4 u5 u6  d2 l5 l6  d4 d5 d6
          f9 f6 f3  u1 u4 u7  b7 b8 b9  l7 u8 u9  d1 l8 l9  d7 d8 d9

r :: Cube -> Cube
r   (Cube f1 f2 f3  r1 r2 r3  b1 b2 b3  u1 u2 u3  l1 l2 l3  d1 d2 d3
          f4 f5 f6  r4 r5 r6  b4 b5 b6  u4 u5 u6  l4 l5 l6  d4 d5 d6
          f7 f8 f9  r7 r8 r9  b7 b8 b9  u7 u8 u9  l7 l8 l9  d7 d8 d9)
  =  Cube d3 d6 d9  r7 r4 r1  u7 b2 b3  u1 u2 u3  l1 l2 l3  d1 d2 b7
          f4 f5 f6  r8 r5 r2  u8 b5 b6  u4 u5 u6  l4 l5 l6  d4 d5 b4
          f7 f8 f9  r9 r6 r3  u9 b8 b9  f3 f2 f1  l7 l8 l9  d7 d8 b1

b :: Cube -> Cube
b   (Cube f1 f2 f3  r1 r2 r3  b1 b2 b3  u1 u2 u3  l1 l2 l3  d1 d2 d3
          f4 f5 f6  r4 r5 r6  b4 b5 b6  u4 u5 u6  l4 l5 l6  d4 d5 d6
          f7 f8 f9  r7 r8 r9  b7 b8 b9  u7 u8 u9  l7 l8 l9  d7 d8 d9)
  =  Cube f1 f2 f3  d9 d8 d7  b7 b4 b1  u1 u2 r1  l1 l2 u3  d1 d2 d3
          f4 f5 f6  r4 r5 r6  b8 b5 b2  u4 u5 r2  l4 l5 u6  d4 d5 d6
          f7 f8 f9  r7 r8 r9  b9 b6 b3  u7 u8 r3  l7 l8 u9  l9 l6 l3

u :: Cube -> Cube
u   (Cube f1 f2 f3  r1 r2 r3  b1 b2 b3  u1 u2 u3  l1 l2 l3  d1 d2 d3
          f4 f5 f6  r4 r5 r6  b4 b5 b6  u4 u5 u6  l4 l5 l6  d4 d5 d6
          f7 f8 f9  r7 r8 r9  b7 b8 b9  u7 u8 u9  l7 l8 l9  d7 d8 d9)
  =  Cube r1 f2 f3  b3 r2 r3  l9 l8 l7  u7 u4 u1  l1 l2 l3  d1 d2 d3
          r4 f5 f6  b2 r5 r6  b4 b5 b6  u8 u5 u2  l4 l5 l6  d4 d5 d6
          r7 f8 f9  b1 r8 r9  b7 b8 b9  u9 u6 u3  f1 f4 f7  d7 d8 d9

l :: Cube -> Cube
l   (Cube f1 f2 f3  r1 r2 r3  b1 b2 b3  u1 u2 u3  l1 l2 l3  d1 d2 d3
          f4 f5 f6  r4 r5 r6  b4 b5 b6  u4 u5 u6  l4 l5 l6  d4 d5 d6
          f7 f8 f9  r7 r8 r9  b7 b8 b9  u7 u8 u9  l7 l8 l9  d7 d8 d9)
  =  Cube f1 f2 f3  r1 r2 r3  b1 b2 d7  b3 b6 b9  l7 l4 l1  f7 d2 d3
          f4 f5 f6  r4 r5 r6  b4 b5 d4  u4 u5 u6  l8 l5 l2  f8 d5 d6
          u3 u2 u1  r7 r8 r9  b7 b8 d1  u7 u8 u9  l9 l6 l3  f9 d8 d9

d :: Cube -> Cube
d   (Cube f1 f2 f3  r1 r2 r3  b1 b2 b3  u1 u2 u3  l1 l2 l3  d1 d2 d3
          f4 f5 f6  r4 r5 r6  b4 b5 b6  u4 u5 u6  l4 l5 l6  d4 d5 d6
          f7 f8 f9  r7 r8 r9  b7 b8 b9  u7 u8 u9  l7 l8 l9  d7 d8 d9)
  =  Cube f1 f2 l1  r1 r2 f3  b1 b2 b3  u1 u2 u3  b9 b8 b7  d7 d4 d1
          f4 f5 l2  r4 r5 f6  b4 b5 b6  u4 u5 u6  l4 l5 l6  d8 d5 d2
          f7 f8 l3  r7 r8 f9  r9 r6 r3  u7 u8 u9  l7 l8 l9  d9 d6 d3

s :: Cube -> Cube
s   (Cube f1 f2 f3  r1 r2 r3  b1 b2 b3  u1 u2 u3  l1 l2 l3  d1 d2 d3
          f4 f5 f6  r4 r5 r6  b4 b5 b6  u4 u5 u6  l4 l5 l6  d4 d5 d6
          f7 f8 f9  r7 r8 r9  b7 b8 b9  u7 u8 u9  l7 l8 l9  d7 d8 d9)
  =  Cube f1 f2 f3  r1 r2 r3  b1 b2 b3  u1 l2 u3  l1 d6 l3  d1 d2 d3
          f4 f5 f6  u2 u5 u8  b4 b5 b6  u4 l5 u6  l4 d5 l6  r6 r5 r4
          f7 f8 f9  r7 r8 r9  b7 b8 b9  u7 l8 u9  l7 d4 l9  d7 d8 d9

m :: Cube -> Cube
m   (Cube f1 f2 f3  r1 r2 r3  b1 b2 b3  u1 u2 u3  l1 l2 l3  d1 d2 d3
          f4 f5 f6  r4 r5 r6  b4 b5 b6  u4 u5 u6  l4 l5 l6  d4 d5 d6
          f7 f8 f9  r7 r8 r9  b7 b8 b9  u7 u8 u9  l7 l8 l9  d7 d8 d9)
  =  Cube f1 f2 f3  r1 r2 r3  b1 d8 b3  u1 u2 u3  l1 l2 l3  d1 f4 d3
          u6 u5 u4  r4 r5 r6  b4 d5 b6  b2 b5 b8  l4 l5 l6  d4 f5 d6
          f7 f8 f9  r7 r8 r9  b7 d2 b9  u7 u8 u9  l7 l8 l9  d7 f6 d9

e :: Cube -> Cube
e   (Cube f1 f2 f3  r1 r2 r3  b1 b2 b3  u1 u2 u3  l1 l2 l3  d1 d2 d3
          f4 f5 f6  r4 r5 r6  b4 b5 b6  u4 u5 u6  l4 l5 l6  d4 d5 d6
          f7 f8 f9  r7 r8 r9  b7 b8 b9  u7 u8 u9  l7 l8 l9  d7 d8 d9)
  =  Cube f1 l4 f3  r1 f2 r3  b1 b2 b3  u1 u2 u3  l1 l2 l3  d1 d2 d3
          f4 l5 f6  r4 f5 r6  r8 r5 r2  u4 u5 u6  b6 b5 b4  d4 d5 d6
          f7 l6 f9  r7 f8 r9  b7 b8 b9  u7 u8 u9  l7 l8 l9  d7 d8 d9

x :: Cube -> Cube
x = r <<< m' <<< l'

y :: Cube -> Cube
y = u <<< e' <<< d'

z :: Cube -> Cube
z = f <<< s <<< b'

f' :: Cube -> Cube
f' = f <<< f <<< f

r' :: Cube -> Cube
r' = r <<< r <<< r

b' :: Cube -> Cube
b' = b <<< b <<< b

u' :: Cube -> Cube
u' = u <<< u <<< u

l' :: Cube -> Cube
l' = l <<< l <<< l

d' :: Cube -> Cube
d' = d <<< d <<< d

s' :: Cube -> Cube
s' = s <<< s <<< s

m' :: Cube -> Cube
m' = m <<< m <<< m

e' :: Cube -> Cube
e' = e <<< e <<< e

x' :: Cube -> Cube
x' = x <<< x <<< x

y' :: Cube -> Cube
y' = y <<< y <<< y

z' :: Cube -> Cube
z' = z <<< z <<< z
