
data SingleBond a b c d e = STerminal a
                          | SBent b (SingleBond a b c d e) (SingleBond a b c d e)
                          | STrigonalPlanar c (SingleBond a b c d e) (SingleBond a b c d e) (SingleBond a b c d e)
                          | STrigonalPyramidal d (SingleBond a b c d e) (SingleBond a b c d e) (SingleBond a b c d e)
                          | STetrhedral e (SingleBond a b c d e) (SingleBond a b c d e) (SingleBond a b c d e) (SingleBond a b c d e)
                          deriving (Show, Eq, Ord, Read, Functor)

data DoubleBond a b c d e f g i h = DTerminal a
                                  | DBent b (SingleBond e f g i h)
                                  | DLinear c (DoubleBond a b c d e f g i h)
                                  | DTrigonalPlanar d (  SingleBond e f g i h) (SingleBond e f g i h)
                                  deriving (Show, Eq, Ord, Read)

data TripleBond a b c d e f g = TTerminal a
                              | TLinear b (SingleBond c d e f g)
                              deriving (Show, Eq, Ord, Read)

--data Attachment a b c d e f g h = SAttachment (SingleBond a b c d)
--                                | DAttachment (DoubleBond e f g a b c d)
--                                | TAttachment (TripleBond h a b c d)
--                          deriving (Show, Eq, Ord, Read)
