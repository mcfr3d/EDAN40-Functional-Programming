{- Test for Program -}
module TestProgram where

import Program

p, p1, p2, p3, p4, p1' :: Program.T
p = fromString  ("\
\read k;\
\read n;\
\m := 1;\
\while n-m do\
\  begin\
\    if m - m/k*k then\
\      skip;\
\    else\
\      write m;\
\    m := m + 1;\
\  end")

p1 = fromString  ("\
\read n;\
\read b;\
\m := 1;\
\s := 0;\
\p := 1;\
\while n do\
\  begin\
\    q := n/b;\
\    r := n - q*b;\
\    write r;\
\    s := p*r+s;\                    
\    p := p*10;\
\    n :=q;\
\  end\
\write s;")

s1 = "\
\read n;\
\read b;\
\m := 1;\
\s := 0;\
\p := 1;\
\while n do\
\  begin\
\    q := n/b;\
\    r := n - q*b;\
\    write r;\
\    s := p*r+s;\                    
\    p := p*10;\
\    n :=q;\
\  end\
\write s;"

p1' = fromString s1


sp = putStr (toString p)

sp1 = putStr (toString p1)

sp2 = putStr (toString p1')

sp3 = putStr (toString p4)

p2 = fromString (toString p)

p3 = fromString (toString p1)

rp = Program.exec p [3,16]

rp1 = Program.exec p1 [1024, 2]

s4 = "\
\read a;\
\read b;\
\-- a comment\n\
\s := 3;\
\while a do\
\  begin\
\    c := a^s;\
\    d := 2^a;\
\    write c;\
\    write d;\                    
\    a := a-1;\
\  end\
\write a;"

p4 = fromString s4

rp4 = Program.exec p4 [4,4]

{-- *TestProgram> Program.exec (fromString "m := 1+2;write m;") [0]
[3]
*TestProgram> Program.exec (fromString "m := 2^2;write m;") [0]
[4]
*TestProgram> Program.exec (fromString "m := 2^5;write m;") [0]
[32]
*TestProgram> Program.exec (fromString "m := 2^(3+5);write m;") [0]
[256]
*TestProgram> Program.exec (fromString "m := (1+1)^(3+5);write m;") [0]
[256]
*TestProgram> Program.exec (fromString "a := 3; m := (1+1)^(a+5);write m;") [0]
[256]
*TestProgram> Program.exec (fromString "b:=1; a := 3; m := (b+1)^(a+5);write m;") [0]
[256]
*TestProgram> Program.exec (fromString "b:=1; a := 3; m := (b+(1*2-1))^(a+5);write m;") [0]
[256]
*TestProgram> Program.exec (fromString "b:=1; a := 3; m := (b+(1*2-1))^(a+5*2-5);write m;") [0]
[256]
*TestProgram> Program.exec (fromString "b:=1; a := 3; m := (b+(1*2-1))^(a+5*2-5)+2;write m;") [0]
[258]
*TestProgram> Program.exec (fromString "b:=1; a := 3; m := (b+(1*2-1))^(a+5*2-5)*2;write m;") [0]
[512]
*TestProgram> Program.exec (fromString "b:=1; a := 3; m := (b+(1*2-1))^(a+5*2-5)*2+2;write m;") [0]
[514]}