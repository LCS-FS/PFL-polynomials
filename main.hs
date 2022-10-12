type Nomial = (Int, [(Char, Int)])
type Polynomial = [Nomial]

addNomial :: Nomial -> Nomial
addNomial n1 n2 = if n1.snd == n2.snd then Nomial(n1.fst + n2.fst, n1.snd)