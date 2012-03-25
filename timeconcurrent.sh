ghc --make -fforce-recomp -O2 -threaded gcon.hs
time ./gcon c +RTS -N2 -RTS
time ./gcon s +RTS -N2 -RTS
