---
jupytext:
  text_representation:
    extension: .md
    format_name: myst
    format_version: 0.12
    jupytext_version: 1.8.0
kernelspec:
  display_name: OCaml 4.11
  language: OCaml
  name: ocaml-jupyter
---

# λ-račun

```{code-cell}
:tags: [remove-cell, remove-stdout]

(* Ko se v Jupytru prvič požene OCaml, program Findlib izpiše neko sporočilo.
   Da se to sporočilo ne bi videlo v zapiskih, je tu ta celica, ki sproži izpis,
   vendar ima nastavljeno, da je v zapiskih v celoti skrita. *)
```

Najpomembnejše mesto v teoriji programskih jezikov zaseda λ-račun, ki tvori jedro vsakega funkcijskega programskega jezika.

## Sintaksa

Za razliko od IMPa sintaksa λ-računa vsebuje le eno vrsto izrazov:

$$
  \text{izraz } M, N ::= x \mid \lambda x. M \mid M \, N
$$

Vsak izraz je lahko:

- spremenljivka,
- _abstrakcijo_ (oziroma _lambdo_), ki predstavlja funkcijo s _parametrom_ $x$ in _telesom_ $M$, ter
- _aplikacijo_, ki funkcijo, predstavljeno z $M$, uporabi na _argumentu_, predstavljenem z izrazom $N$.

Na primer:

- $\lambda x. x$ predstavlja identično funkcijo,
- $\lambda c. \lambda x. c$ predstavlja funkcijo, ki sprejme argument $c$ in vrne konstantno funkcijo, ki vedno vrne $c$,
- $\lambda f. \lambda g. \lambda x. f (g x)$ predstavlja kompozitum, ki sprejme funkciji $f$ in $g$ ter vrne funkcijo, ki na $x$ najprej uporabi $g$, nato pa še $f$.

Če bi želeli, bi lahko izraze razširili še z drugimi konstrukti, na primer aritmetičnimi operacijami ali pogojnimi izrazi, vendar bomo videli, da jih načeloma ne potrebujemo, zato zaenkrat počakajmo z njimi.

Parameter abstrakcije je vezana spremenljivka, zato na primer izraza $\lambda x. x y$ in $\lambda w. w y$ predstavljata isto funkcijo. Spremenljivka $y$ v tem primeru ni vezana, temveč _prosta_. Natančneje lahko definiramo množico vseh prostih spremenljivk $fv(M)$ v izrazu $M$ kot

$$
\begin{align*}
  fv(x) &:= \{ x \} \\
  fv(\lambda x. M) &:= fv(M) \setminus \{ x \} \\
  fv(M \, N) &:= fv(M) \cup fv(N)
\end{align*}
$$

Funkcije bomo uporabili tako, da bomo vse pojavitve parametra v telesu zamenjali z argumentom. V ta namen rekurzivno definiramo _substitucijo_ $M[N / x]$ kot:

$$
\begin{align*}
  x[N / x] &:= N \\
  y[N / x] &:= y \qquad (x \ne y) \\
  (\lambda y. M)[N / x] &:= \lambda y. M[N / x] \qquad (x \ne y, y \notin fv(N)) \\
  (M_1 \, M_2)[N / x] &:= (M_1[N / x]) \, (M_2[N / x])
\end{align*}
$$

Kot vidimo, moramo paziti, da pri substituciji abstrakcij ne bi zamenjali vezane spremenljivke. Na primer (saj $(\lambda y. y)[x / y] \ne \lambda y. x$) ali po nesreči vezali prej prosto spremenljivko (saj $(\lambda y. x)[y / x] \ne \lambda y. y$). Da se izognemo težavam, bomo parametre abstrakcij po potrebi preimenovali.

## Operacijska semantika

Ukazi v IMPu so zaključili izvajanje, ko so prispeli do ukaza $\skip$, pri λ-računu pa v ta namen definiramo podmnožico _vrednosti_ kot

$$
  \text{vrednost } V ::= \lambda x. M
$$

Ker lahko izrazi v lambda računu divergirajo, bomo tako kot v IMPu tudi tu podali semantiko malih korakov $M \leadsto M'$, podano s sledečimi pravili:

$$
  \infer{M \leadsto M'}{M \, N \leadsto M' \, N} \qquad
  \infer{N \leadsto N'}{V \, N \leadsto V \, N'} \qquad
  \infer{}{(\lambda x. M) \, V \leadsto M[V /x]}
$$

Prvo pravilo govori o tem, da najprej do vrednosti izračunamo prvi člen. Ko je ta izračunan, lahko uporabimo drugo pravilo in izračunamo še drugi člen. Če sta oba vrednosti (in je prvi funkcija), lahko funkcijo s pomočjo substitucije uporabimo na argumentu. Zadnji korak ima posebno ime in sicer _β-redukcija_.

Izbrana pravila niso edini način, na katerega lahko podamo semantiko λ-računa. V lenih funkcijskih jezikih, kot je Haskell, argumenta pred β-redukcijo ne gremo računati, saj ga morda ne bomo nikoli potrebovali. Takemu načinu računanja pravimo _leno izvajanje_, oziroma _call-by-name_ ali _CBN_. Načinu izvajanja, ki smo ga spoznali prej, pravimo _neučakano izvajanje_, oziroma _call-by-value_. Leno izvajanje bi podali samo s praviloma:

$$
  \infer{M \leadsto M'}{M \, N \leadsto M' \, N} \qquad
  \infer{}{(\lambda x. M) \, N \leadsto M[N /x]}
$$

## Churchevo kodiranje

Izkaže se, da lahko s pomočjo funkcij predstavimo tudi ostale vrednosti, ki jih običajno srečamo v programskih jezikih.

### Naravna števila

Na primer naravno število $n$ lahko predstavimo s funkcijo $\intsym{n}$, ki svoj prvi argument $n$-krat uporabi na svojem drugem argumentu:

$$
  \intsym{n} := \lambda f. \lambda x. \underbrace{f(\cdots( f}_n \, x) \cdots)
$$

Na primer $\intsym{3} = \lambda f. \lambda x. f (f (f x))$. Iz takih funkcij hitro dobimo tudi druge. Na primer, če $f$ uporabimo $n$-krat na $f \, x$, smo v resnici $(n + 1)$-krat uporabili $f$ na $x$. Torej lahko naslednika definiramo kot

$$
  succ := \lambda n. \lambda f. \lambda x. n \, f \, (f \, x)
$$

lahko pa tudi kot

$$
  succ' := \lambda n. \lambda f. \lambda x. f (n \, f \, x)
$$

da najprej $f$ uporabimo $n$-krat na $x$, nato pa na vsem skupaj še enkrat uporabimo $f$.

Podobno lahko definiramo tudi seštevanje in množenje:

$$
\begin{align*}
  plus &:= \lambda m. \lambda n. \lambda f. \lambda x. m \, f \, (n \, f \, x) \\
  times &:= \lambda m. \lambda n. \lambda f. \lambda x. m \, (n \, f) \, x
\end{align*}
$$

Pri seštevanju $f$ na $x$ najprej uporabimo $n$-krat, nato pa še $m$-krat. Pri množenju pa $m$-krat uporabimo $n$-krat $f$ na $x$. Predstavitev predhodnika je [bolj zahtevna](https://en.wikipedia.org/wiki/Church_encoding#Derivation_of_predecessor_function).

### Logične vrednosti

Logični vrednosti predstavimo s funkcijama, ki vzameta dva argumenta, vrneta pa prvega oziroma drugega:

$$
\begin{align*}
  true &:= \lambda x. \lambda y. x \\
  false &:= \lambda x. \lambda y. y
\end{align*}
$$

Predikat, ki ugotovi, ali je naravno število nič, lahko definiramo kot:

$$
  isZero := \lambda n. n \, (\lambda x. false) \, true
$$

Torej, $n$-krat na $true$ uporabimo funkcijo, ki konstantno vrača $false$. Če je $n = 0$, dobimo $true$, sicer pa $false$. Če imamo logično vrednost ter dve možnosti, se med njima enostavno odločimo tako, da logično vrednost kar uporabimo.

$$
  ifThenElse := \lambda b. \lambda x. \lambda y. (b x) y
$$

### Rekurzija

Recimo, da bi radi definirali:

$$
  fact := \lambda n. ifThenElse (isZero \, n) \, \underline{1} \, (times \, n \, (fact \, (pred \, n)))
$$

Za razliko do prejšnjih definicij se ta ne sklicuje samo na poprej definirane izraze, temveč tudi samo nase. Ciklične definicije se lahko znebimo tako, da problem prevedemo na iskanje fiksne točke. Definirajmo:

$$
  \Psi := \lambda f. \lambda n. ifThenElse (isZero \, n) \, \underline{1} \, (times \, n \, (f \, (pred \, n)))
$$

Vse je tako kot v definiciji $fact$, le da sprejmemo dodatni argument $f$, ki ga uporabimo na mestu rekurzivnega klica uporabimo argument $f$. Zdaj pa si želimo najti tako funkcijo $fact$, ki je enaka $\Psi \, fact$, torej fiksno točko funkcije (oz. funkcionala), ki jo predstavlja izraz $\Psi$.

Pri izračunu fiksne točke si pomagamo z _Y-kombinatorjem_:

$$
  Y ::= \lambda f. (\lambda x. \lambda y. f \, (x \, x) \, y) \, (\lambda x. \lambda y. f \, (x \, x) \, y)
$$

Definicija Y-kombinatorja je malo nenavadna, zato si najprej oglejmo, kako se izvaja izraz

$$
  \Omega ::= (\lambda x. x \, x) \, (\lambda x. x \, x)
$$

Da ne bo zmešnjave, pred tem v drugi abstrakciji preimenujmo $x$ v $y$:

$$
  \begin{align*}
    \Omega
      &= (\lambda x. x \, x) \, (\lambda x. x \, x) \\
      &= (\lambda x. x \, x) \, (\lambda y. y \, y) \\
      &\leadsto (x \, x)[(\lambda y. y \, y) / x] \\
      &= (\lambda y. y \, y) \, (\lambda y. y \, y) \\
      &= \Omega
  \end{align*}
$$

Velja torej $\Omega \leadsto \Omega$, zato je $\Omega$ divergenten izraz, ki v nedogled dela korak nazaj vase. Če pa v vsak del izraza $\Omega$ vrinemo $\Psi$, pa velja:

$$
  \begin{align*}
    \Omega'
      &= (\lambda x. \Psi (x \, x)) \, (\lambda x. \Psi (x \, x)) \\
      &= (\lambda x. \Psi (x \, x)) \, (\lambda y. \Psi (y \, y)) \\
      &\leadsto (\Psi (x \, x))[(\lambda y. \Psi (y \, y)) / x] \\
      &= \Psi ((\lambda y. \Psi (y \, y)) \, (\lambda y. \Psi y \, y)) \\
      &= \Psi \, \Omega'
  \end{align*}
$$

Dobili smo izraz, ki intuitivno predstavlja fiksno točko $\Psi$, vendar ga ne moremo uporabiti v izračunih, saj njegov izračun divergira. Ker bo naša fiksna točka funkcija, vsak izraz $M$, ki predstavlja funkcijo, pa lahko zamenjamo z ekvivalentnim izrazom $\lambda x. M \, x$, ki se ne bo izvajal, dokler ga ne bomo uporabili na argumentu, nas to pripelje do končne oblike, na kateri nato le še $\Psi$ pretvorimo v parameter:

$$
  Y ::= \lambda \psi. (\lambda x. \lambda y. \psi \, (x \, x) \, y) \, (\lambda x. \lambda y. \psi \, (x \, x) \, y)
$$

Poglejmo, zakaj $Y \Psi$ zares računa fakulteto.

$$
  \begin{align*}
    Y \, \Psi
      &\leadsto (\lambda x. \lambda y. \Psi \, (x \, x) \, y) \, (\lambda x. \lambda y. \Psi \, (x \, x) \, y) \\
      &\leadsto (\lambda y. \Psi \, (x \, x) \, y)[(\lambda x. \lambda y. \Psi \, (x \, x) \, y) / x] \\
      &= (\lambda y. \Psi \, ((\lambda x. \lambda y. \Psi \, (x \, x) \, y) \, (\lambda x. \lambda y. \Psi \, (x \, x) \, y)) \, y) \\
      &= \lambda y. \Psi \, (Y \, \Psi) \, y
  \end{align*}
$$

Na tej točki se izvajanje ustavi, saj smo dobili funkcijo. Če jo uporabimo na številu $m$, dobimo

$$
  \begin{align*}
    (Y \, \Psi) \, m
      &\leadsto^* 
      (\lambda y. \Psi \, (Y \, \Psi) \, y) \, m \\
      &\leadsto \Psi \, (Y \, \Psi) \, m \\
      &= (\lambda f. \lambda n. ifThenElse (isZero \, n) \, \underline{1} \, (times \, n \, (f \, (pred \, n)))) \, (Y \, \Psi) \, m \\
      &\leadsto (\lambda n. ifThenElse (isZero \, n) \, \underline{1} \, (times \, n \, ((Y \, \Psi) \, (pred \, n)))) \, m \\
      &\leadsto ifThenElse (isZero \, m) \, \underline{1} \, (times \, m \, ((Y \, \Psi) \, (pred \, m)))
  \end{align*}
$$

Sedaj je izvajanje odvisno od vrednosti $m$, ampak vidimo lahko, da na mestu rekurzivnega klica že čaka $(Y \, \Psi) \, (pred \, m)$, kar je ravno to, kar potrebujemo.

## Vaje

### Naloga 1

Dopolnite lambda račun s celimi števili <u>`n`</u> in seštevanjem `e1 + e2`.  

- Dopolnite pravila za leno in neučakano izvajanje velikih in malih korakov. Komentirajte, kje se pojavijo ključne razlike.

### Naloga 2

Izračunajte spodnje izraze v lambda računu. Jasno označite korake izvajanja pri vsakem.

- `(\y -> (\x -> x + y)) 2 3`
- `(\y -> (\x -> x + x)) 2 3`
- `(\x -> x x) (\x -> x x)`

### Naloga 3

Razložite kaj naredijo naslednje funkcije za Churchova števila

- `\n f z -> f (n f z)`
- `\n f z -> n f (f z)`
- `\m n f z -> m f (n f x)`
- `\m n f z -> m (n f) z`
- `\m n -> n m`

### Naloga 4

Uporabite Churchovo kodiranje za predstavitev seznamov kot aplikacije `fold` na tem seznamu.
