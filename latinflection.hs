import GHC.ForeignPtr (ForeignPtrContents(PlainForeignPtr))
data Tense = Pr | F | I
            deriving (Show, Eq)

data Mood = In | Su
            deriving (Show, Eq)

data Voice = A | P
            deriving (Show, Eq)

data Aspect = Im | Pe
            deriving (Show, Eq)

data Person = P1 | P2 | P3
            deriving (Show, Eq)

data Number = S | Pl
            deriving (Show, Eq)

newtype Phi = Phi (Person, Number)
          deriving (Show, Eq)


newtype Verb = Verb (Tense,
                  Mood,
                  Aspect,
                  Voice,
                  Phi
                  )
          deriving (Show, Eq)

inflect :: String -> Verb -> String
inflect "port" (Verb (Pr, In, Im, A, Phi (P1, S))) = "portoo"
inflect "port" (Verb (Pr, In, Im, A, Phi (P1, Pl))) = "portaamus"
inflect "port" (Verb (Pr, In, Im, A, Phi (P2, S))) = "portaas"
inflect "port" (Verb (Pr, In, Im, A, Phi (P2, Pl))) = "portaatis"
inflect "port" (Verb (Pr, In, Im, A, Phi (P3, S))) = "portat"
inflect "port" (Verb (Pr, In, Im, A, Phi (P3, Pl))) = "portant"
inflect "port" (Verb (F, In, Im, A, Phi (P1, S))) = "portabo"

-- tense: Ø, future, imperfect
-- mood: Ø, imperative
-- aspect: Ø, perfect
-- voices: Ø, passive
-- person: Ø, 1, 2
-- number: Ø, plural

-- subjunctive = imperative future perfect
-- past perfect = imperfect perfect

----

-- 
-- future
-- imperfect

-- imperative
-- imperative future
-- subjunctive imperfect

-- perfect
-- future perfect
-- past perfect

-- subjunctive perfect
-- subjunctive
-- subjunctive past perfect

-- passive
-- passive future
-- passive imperfect

-- imerative passive
-- imperative passive future
-- subjunctive passive imperfect

-- passive perfect
-- passive future perfect
-- passive past perfect

-- subjunctive passive perfect
-- subjunctive passive
-- subjunctive passive past perfect

----

-- infinative
-- participle
-- infinative future
-- participle future
-- infinative passive
-- participle passive ?
-- infinative future passive
-- participle future passive
-- infinative perfect
-- participle perfect ?
-- infinative future perfect ?
-- participle future perfect ?
-- infinative perfect potential
-- participle perfect potential ?
-- infinative perfect passive
-- participle perfect passive
-- infinative future perfect passive
-- participle future perfect passive ?
-- infinative perfect potential passive ?
-- participle perfect potential passive ?