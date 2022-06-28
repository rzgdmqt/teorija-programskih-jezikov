# Tipi

Videli smo, da lahko v λ-računu zakodiramo vse običajne programske konstrukte, vendar to ni najbolj praktično. Običajno vzamemo λ-račun, v katerem te dodatne konstrukte vzamemo kot osnovne in ne izpeljane. Prej so vsi izrazi predstavljali funkcije, z novimi razširitvami pa temu ni več tako, zato si za razločevanje pomagamo s tipi, ki jih bomo priredili vsem veljavnim izrazom. Dobimo _λ-račun s preprostimi tipi_ (_simply-typed λ-calculus_ ali kar _STLC_). Za primer razširitve vzemimo cela števila in logične vrednosti.

## Definicija jezika

### Sintaksa

$$
    \begin{align*}
    \text{izraz } M &::= x
        \mid \true
        \mid \false
        \mid \ifthenelse{M}{M_1}{M_2} \\
        &\mid \intsym{n}
        \mid M_1 + M_2
        \mid M_1 * M_2
        \mid M_1 < M_2 \\
        &\mid \lambda x. M
        \mid M_1 \, M_2
\end{align*}
$$

Tudi za tipe podamo sintakso in sicer:

$$
\begin{align*}
A, B &::= \boolty
        \mid \intty
        \mid A \to B
\end{align*}
$$

Vidimo, da funkcijski tip ne pove le tega, da imamo opravka s funkcijo, temveč tudi to, od kod in kam ta funkcija slika.

### Določanje tipov

Tako kot smo pri preverjanju veljavnih izrazov v IMPu morali vedeti, katera množica lokacij $L$ je definirana, moramo tu vedeti, kakšne spremenljivke imamo in kakšni so njihovi tipi. S tem namenom uvedemo _kontekste_ $\Gamma = x_1 : A_1, \ldots, x_n : A_n$, ki povedo, da za spremenljivko $x_i$ predpostavimo tip $A_i$. Tedaj definiramo relacijo $\Gamma \vdash M : A$, ki pove, da ima izraz $M$ tip $A$, če predpostavimo, da imajo spremenljivke tipe, določene s kontekstom $\Gamma$. Če je kontekst prazen, pišemo kar $\vdash M : A$. Relacijo podamo s pravili:

$$
\infer{
    (x : A) ∈ \Gamma
}{
    \Gamma \vdash x : A
} \qquad

\infer{}{
    \Gamma \vdash \true : \boolty
} \qquad

\infer{}{
    \Gamma \vdash \false : \boolty
} \\[2em]

\infer{
    \Gamma \vdash M : \boolty \qquad
    \Gamma \vdash M_1 : A \qquad
    \Gamma \vdash M_2 : A
}{
    \Gamma \vdash \ifthenelse{M}{M_1}{M_2} : A
} \\[2em]

\infer{}{
    \Gamma \vdash \intsym{n} : \intty
} \qquad

\infer{
    \Gamma \vdash M_1 : \intty \qquad
    \Gamma \vdash M_2 : \intty
}{
    \Gamma \vdash M_1 + M_2 : \intty
} \\[2em]

\infer{
    \Gamma \vdash M_1 : \intty \qquad
    \Gamma \vdash M_2 : \intty
}{
    \Gamma \vdash M_1 * M_2 : \intty
} \qquad

\infer{
    \Gamma \vdash M_1 : \intty \qquad
    \Gamma \vdash M_2 : \intty
}{
    \Gamma \vdash M_1 < M_2 : \boolty
} \\[2em]

\infer{
    \Gamma, x : A \vdash M : B
}{
    \Gamma \vdash \lambda x. M : A \to B
} \qquad

\infer{
    \Gamma \vdash M_1 : A \to B \qquad
    \Gamma \vdash M_2 : A
}{
    \Gamma \vdash M_1 \, M_2 : B
}
$$

Primer izpeljave tipa je:

$$
\infer{
    \infer{
        \infer{
            \infer{}{x : \intty \vdash x : \intty} \qquad
            \infer{}{x : \intty \vdash \intsym{0} : \intty}
        }{
            x : \intty \vdash x < \intsym{0} : \boolty
        }
    }{
        \vdash \lambda x. x < \intsym{0} : \intty \to \boolty
    }
    \qquad
    \infer{}{\vdash \intsym{3} : \intty}
}{
    \vdash (\lambda x. x < \intsym{0}) \, \intsym{3} : \boolty
}
$$

Vsak izraz seveda nima tipa, na primer $\true + \intsym{2}$ ga nima.

### Operacijska semantika

Podobno se spremeni operacijska semantika, kjer vrednosti razširimo s konstantami:

$$
    \begin{align*}
    \text{vrednost } V ::= \true
        \mid \false
        \mid \intsym{n}
        \mid \lambda x. M
    \end{align*}
$$

Operacijsko semantiko malih korakov pa podamo s pravili:

$$
\infer{
    M  \leadsto  M'
}{
    \ifthenelse{M}{M_1}{M_2}  \leadsto  \ifthenelse{M'}{M_1}{M_2}
} \\[2em]

\infer{}{
    \ifthenelse{\true}{M_1}{M_2}  \leadsto  M_1
} \qquad

\infer{}{
    \ifthenelse{\false}{M_1}{M_2}  \leadsto  M_2
} \\[2em]

\infer{
    M_1  \leadsto  M_1'
}{
    M_1 + M_2  \leadsto  M_1' + M_2
} \qquad

\infer{
    M_2  \leadsto  M_2'
}{
    V_1 + M_2  \leadsto  V_1 + M_2'
} \qquad

\infer{}{
    \intsym{n_1} + \intsym{n_2}  \leadsto  \intsym{n_1 + n_2}
} \\[2em]

\infer{
    M_1  \leadsto  M_1'
}{
    M_1 * M_2  \leadsto  M_1' * M_2
} \qquad

\infer{
    M_2  \leadsto  M_2'
}{
    V_1 * M_2  \leadsto  V_1 * M_2'
} \qquad

\infer{}{
    \intsym{n_1} * \intsym{n_2}  \leadsto  \intsym{n_1 \cdot n_2}
} \\[2em]

\infer{
    M_1  \leadsto  M_1'
}{
    M_1 < M_2  \leadsto  M_1' < M_2
} \qquad

\infer{
    M_2  \leadsto  M_2'
}{
    V_1 < M_2  \leadsto  V_1 < M_2'
} \\[2em]

\infer{
    n_1 < n_2
}{
    \intsym{n_1} < \intsym{n_2}  \leadsto  \true
} \qquad

\infer{
    n_1 ≮ n_2
}{
    \intsym{n_1} < \intsym{n_2}  \leadsto  \false
} \\[2em]

\infer{
    M_1  \leadsto  M_1'
}{
    M_1 \, M_2  \leadsto  M_1' \, M_2
} \qquad

\infer{
    M_2  \leadsto  M_2'
}{
    V_1 \, M_2  \leadsto  V_1 \, M_2'
} \qquad

\infer{}{
    (\lambda x. M) \, V  \leadsto  M[V / x]
}
$$

## Izrek o varnosti

Za tiste izraze, ki imajo tip v praznem kontekstu, velja podoben izrek o varnosti, kot smo ga spoznali za IMP. Tako kot prej razdelimo na dva dela:

1. napredek, ki pove, da je vsak izraz, ki ima tip, bodisi vrednost bodisi lahko naredi korak, ter
2. ohranitev, ki pove, da izraz v vsakem koraku ohrani prvotni tip.

Ker v operacijski semantiki nastopa substitucija, moramo najprej dokazati, da ima pričakovani tip.

### Lema (o substituciji)

Če velja $\Gamma, x : A, \Gamma' \vdash M : B$ in $\Gamma, \Gamma' \vdash N : A$, tedaj velja $\Gamma, \Gamma' \vdash M[N / x] : B$.

#### Dokaz

Z indukcijo na izpeljavo $\Gamma, x : A, \Gamma' \vdash M : B$.
Če je zaključek zadnjega uporabljenega pravila:

* $\Gamma, x : A, \Gamma' \vdash x : A$, ker je $M = x$, velja $M[N / x] = N$,
    torej velja $\Gamma, \Gamma' \vdash M[N / x] : B$ po drugi predpostavki.

* $\Gamma, x : A, \Gamma' \vdash y : B$ za $y ≠ x$, iz prve predpostavke sledi $(y : B) ∈ \Gamma, \Gamma'$.
    Iz tega sledi $\Gamma, \Gamma' \vdash M[N / x] : B$, saj iz $M = y$ sledi $M[N / x] = y$.

* $\Gamma, x : A, \Gamma' \vdash \true : \boolty$, velja tudi $\Gamma, \Gamma' \vdash \true : \boolty$

* $\Gamma, x : A, \Gamma' \vdash \false : \boolty$, velja tudi $\Gamma, \Gamma' \vdash \false : \boolty$

* $\Gamma, x : A, \Gamma' \vdash \ifthenelse{M}{M_1}{M_2} : A$, mora veljati
    $\Gamma, x : A, \Gamma' \vdash M : \boolty$, $\Gamma, x : A, \Gamma' \vdash M_1 : A$ in $\Gamma, x : A, \Gamma' \vdash M_2 : A$.
    Po indukcijski predpostavki zato velja
    $\Gamma, \Gamma' \vdash M[N / x] : \boolty$, $\Gamma, \Gamma' \vdash M_1[N / x] : A$ in $\Gamma, \Gamma' \vdash M_2[N / x] : A$,
    iz česar sledi $\Gamma, \Gamma' \vdash (\ifthenelse{M}{M_1}{M_2})[N / x] : A$, saj je
    $(\ifthenelse{M}{M_1}{M_2})[N / x] = \ifthenelse{M[N / x]}{M_1[N / x]}{M_2}[N / x]$.

* $\Gamma, x : A, \Gamma' \vdash \intsym{n} : \intty$, velja tudi $\Gamma, \Gamma' \vdash \intsym{n} : \intty$

* $\Gamma, x : A, \Gamma' \vdash M_1 + M_2 : \intty$, mora veljati
    $\Gamma, x : A, \Gamma' \vdash M_1 : \intty$ in $\Gamma, x : A, \Gamma' \vdash M_2 : \intty$.
    Po indukcijski predpostavki zato velja
    $\Gamma, \Gamma' \vdash M_1[N / x] : \intty$ in $\Gamma, \Gamma' \vdash M_2[N / x] : \intty$
    iz česar sledi $\Gamma, \Gamma' \vdash (M_1 + M_2)[N / x] : \intty$, saj je
    $(M_1 + M_2)[N / x] = M_1[N / x] + M_2[N / x]$.

* $\Gamma, x : A, \Gamma' \vdash M_1 * M_2 : \intty$, mora veljati
    $\Gamma, x : A, \Gamma' \vdash M_1 : \intty$ in $\Gamma, x : A, \Gamma' \vdash M_2 : \intty$.
    Po indukcijski predpostavki zato velja
    $\Gamma, \Gamma' \vdash M_1[N / x] : \intty$ in $\Gamma, \Gamma' \vdash M_2[N / x] : \intty$
    iz česar sledi $\Gamma, \Gamma' \vdash (M_1 * M_2)[N / x] : \intty$, saj je
    $(M_1 * M_2)[N / x] = M_1[N / x] * M_2[N / x]$.

* $\Gamma, x : A, \Gamma' \vdash M_1 < M_2 : \boolty$, mora veljati
    $\Gamma, x : A, \Gamma' \vdash M_1 : \intty$ in $\Gamma, x : A, \Gamma' \vdash M_2 : \intty$.
    Po indukcijski predpostavki zato velja
    $\Gamma, \Gamma' \vdash M_1[N / x] : \intty$ in $\Gamma, \Gamma' \vdash M_2[N / x] : \intty$
    iz česar sledi $\Gamma, \Gamma' \vdash (M_1 < M_2)[N / x] : \boolty$, saj je
    $(M_1 < M_2)[N / x] = M_1[N / x] < M_2[N / x]$.

* $\Gamma, x : A, \Gamma' \vdash \lambda y. M' : A' \to B'$, mora veljati
    $\Gamma, x : A, \Gamma', y : A' \vdash M' : B'$.
    Po indukcijski predpostavki zato velja
    $\Gamma, \Gamma', y : A' \vdash M'[N / x] : B'$
    iz česar sledi $\Gamma, \Gamma' \vdash (\lambda y. M')[N / x] : A' \to B'$, saj je
    $(\lambda y. M')[N / x] = \lambda y. M'[N / x]$.

* $\Gamma, x : A, \Gamma' \vdash M_1 \, M_2 : B$, mora veljati
    $\Gamma, x : A, \Gamma' \vdash M_1 : A' \to B$ in $\Gamma, x : A, \Gamma' \vdash M_2 : A'$.
    Po indukcijski predpostavki zato velja
    $\Gamma, \Gamma' \vdash M_1[N / x] : A' \to B$ in $\Gamma, \Gamma' \vdash M_2[N / x] : A'$
    iz česar sledi $\Gamma, \Gamma' \vdash (M_1 \, M_2)[N / x] : B$, saj je
    $(M_1 \, M_2)[N / x] = M_1[N / x] \, M_2[N / x]$.

### Trditev (napredek)

Če velja $\vdash M : A$, tedaj:

1. je $M$ vrednost ali
2. obstaja $M'$, da velja $M \leadsto M'$.

**Dokaz.**
Z indukcijo na predpostavko o določenem tipu.
Če je zaključek zadnjega uporabljenega pravila:

* $\vdash x : A$, imamo protislovje, saj je kontekst prazen.

* $\vdash \true : \boolty$, imamo vrednost (1).

* $\vdash \false : \boolty$, imamo vrednost (1).

* $\vdash \ifthenelse{M}{M_1}{M_2} : A$, mora veljati $\vdash M : \boolty$.
    Po indukciji dobimo dva primera:
    1. $M$ je vrednost, torej $\true$, $\false$, $\intsym{n}$ ali $\lambda x. M$.
    Ker velja $\vdash M : \boolty$, zadnji dve možnosti odpadeta.
    Če je $M = \true$, velja $\ifthenelse{M}{M_1}{M_2} \leadsto M_1$,
    če je $M = \false$, velja $\ifthenelse{M}{M_1}{M_2} \leadsto M_2$.
    2. Obstaja $M'$, da velja $M \leadsto M'$, zato velja tudi $\ifthenelse{M}{M_1}{M_2} \leadsto \ifthenelse{M'}{M_1}{M_2}$.

    V vseh primerih izraz torej lahko naredi korak (2).

* $\vdash \intsym{n} : \intty$, imamo vrednost (1).

* $\vdash M_1 + M_2 : \intty$, mora veljati $\vdash M_1 : \intty$ in $\vdash M_2 : \intty$.
    Po indukciji za $M_1$ dobimo dva primera:
    1. $M_1$ je vrednost tipa $\intty$, torej število $\intsym{n_1}$. V tem primeru po indukciji za $M_2$ dobimo dva primera:
        1. Tudi $M_2$ je vrednost tipa $ \intty$, torej število $\intsym{n_2}$. Tedaj velja $M_1 + M_2 = \intsym{n_1} + \intsym{n_2} \leadsto \intsym{n_1 + n_2}$.
        2. Obstaja $M_2'$, da velja $M_2 \leadsto M_2'$, zato velja tudi $M_1 + M_2 = \intsym{n_1} + M_2 \leadsto \intsym{n_1} + M_2'$.
    2. Obstaja $M_1'$, da velja $M_1 \leadsto M_1'$, zato velja tudi $M_1 + M_2 \leadsto M_1' + M_2$.

    V vseh primerih izraz torej lahko naredi korak (2).

* $\vdash M_1 * M_2 : \intty$, je dokaz podoben kot za vsoto.

* $\vdash M_1 < M_2 : \boolty$, je dokaz podoben kot za vsoto.

* $\vdash \lambda x. M : A \to B$, imamo vrednost (1).

* $\vdash M_1 \, M_2 : B$, mora veljati $\vdash M_1 : A \to B$ in $\vdash M_2 : A$ za nek $A$.
    Po indukciji za $M_1$ dobimo dva primera:
    1. $M_1$ je vrednost $V_1$. V tem primeru po indukciji za $M_2$ dobimo dva primera:
        1. Tudi $M_2$ je vrednost $V_2$. Ker velja $\vdash M_1 : A \to B$, mora veljati $M_1 = \lambda x. M$ za neka $x$ in $M$. Tedaj velja $M_1 \, M_2 = (\lambda x. M) V_2 \leadsto M[V_2 / x]$.
        2. Obstaja $M_2'$, da velja $M_2 \leadsto M_2'$, zato velja tudi $M_1 \, M_2 = V_1 \, M_2 \leadsto V_1 M_2'$.
    2. Obstaja $M_1'$, da velja $M_1 \leadsto M_1'$, zato velja tudi $M_1 \, M_2 \leadsto M_1' M_2$.

    V vseh primerih izraz torej lahko naredi korak (2).

### Trditev (ohranitev)

Če velja $\Gamma \vdash M : A$ in $M \leadsto M'$, tedaj velja tudi $\Gamma \vdash M' : A$.

**Dokaz.**
Z indukcijo na predpostavko o koraku.
Če je zaključek zadnjega uporabljenega pravila:

* $\ifthenelse{M}{M_1}{M_2} \leadsto \ifthenelse{M'}{M_1}{M_2}$, mora veljati $M \leadsto M'$,
  iz $\Gamma \vdash \ifthenelse{M}{M_1}{M_2} : A$ pa sledi
  $\Gamma \vdash M : \boolty$, $\Gamma \vdash M_1 : A$ in $\Gamma \vdash M_2 : A$.
  Po indukcijski predpostavki velja $\Gamma \vdash M' : \boolty$, iz česar sledi tudi
  $\Gamma \vdash \ifthenelse{M'}{M_1}{M_2} : A$.

* $\ifthenelse{\true}{M_1}{M_2} \leadsto M_1$,
  iz $\Gamma \vdash \ifthenelse{M}{M_1}{M_2} : A$ sledi $\Gamma \vdash M_1 : A$, kar želimo.

* $\ifthenelse{\false}{M_1}{M_2} \leadsto M_2$
  iz $\Gamma \vdash \ifthenelse{M}{M_1}{M_2} : A$ sledi $\Gamma \vdash M_2 : A$, kar želimo.

* $M_1 + M_2 \leadsto M_1' + M_2$, mora veljati $M_1 \leadsto M_1'$,
  iz $\Gamma \vdash M_1 + M_2 : \intty$ pa sledi
  $\Gamma \vdash M_1 : \intty$ in $\Gamma \vdash M_2 : \intty$.
  Po indukcijski predpostavki velja $\Gamma \vdash M_1' : \intty$, iz česar sledi tudi
  $\Gamma \vdash M_1' + M_2 : \intty$.

* $V_1 + M_2 \leadsto V_1 + M_2'$, mora veljati $M_2 \leadsto M_2'$,
  iz $\Gamma \vdash V_1 + M_2 : \intty$ pa sledi
  $\Gamma \vdash V_1 : \intty$ in $\Gamma \vdash M_2 : \intty$.
  Po indukcijski predpostavki velja $\Gamma \vdash M_2' : \intty$, iz česar sledi tudi
  $\Gamma \vdash V_1 + M_2' : \intty$.

* $\intsym{n_1} + \intsym{n_2} \leadsto \intsym{n_1 + n_2}$, kjer sta obe strani tipa $\intty$.

* Dokazi ohranitve za produkt in primerjavo števil so enaki kot pri vsoti.

* $M_1 \, M_2 \leadsto M_1' \, M_2$, mora veljati $M_1 \leadsto M_1'$,
  iz $\Gamma \vdash M_1 \, M_2 : A$ pa sledi
  $\Gamma \vdash M_1 : B \to A$ in $\Gamma \vdash M_2 : B$ za nek $B$.
  Po indukcijski predpostavki velja $\Gamma \vdash M_1' : B \to A$, iz česar sledi tudi
  $\Gamma \vdash M_1' \, M_2 : A$.

* $V_1 \, M_2 \leadsto V_1 \, M_2'$, mora veljati $M_2 \leadsto M_2'$,
  iz $\Gamma \vdash V_1 \, M_2 : A$ pa sledi
  $\Gamma \vdash V_1 : B \to A$ in $\Gamma \vdash M_2 : B$ za nek $B$.
  Po indukcijski predpostavki velja $\Gamma \vdash M_2' : B$, iz česar sledi tudi
  $\Gamma \vdash V_1 \, M_2' : A$.

* $(\lambda x. M) \, V \leadsto M[V / x]$,
  iz $\Gamma \vdash (\lambda x. M) \, V : A$ sledi
  $\Gamma \vdash (\lambda x. M) : B \to A$ in $\Gamma \vdash V : B$ za nek $B$.
  Iz prvega sledi $\Gamma, x : B \vdash M : A$,
  z drugim pa prek leme o substituciji izpeljemo $\Gamma \vdash M[V / x] : A$.

## Rekurzija

Če želimo uporabljati rekurzijo, lahko uvedemo izraz $\kwdpre{rec} f \, x . M$, ki predstavlja rekurzivno funkcijo s telesom $M$, v katerem se lahko poleg argumenta $x$ pojavljajo tudi rekurzivni klici, dostopni prek spremenljivke $x$. Pravilo za določanje tipa je

$$
\infer{
    \Gamma, f : A \to B, x : A \vdash M : A \to B
}{
    \Gamma \vdash \kwdpre{rec} f \, x . M : A \to B
}
$$

pri operacijski semantiki pa razširimo vrednosti

$$
    V ::= \dots \mid \kwdpre{rec} f \, x . M
$$

ter dodamo pravilo za izvajanje klicev rekurzivnih funkcij:

$$
    \infer{}{
        (\kwdpre{rec} f \, x . M) \, V \leadsto M[V / x, (\kwdpre{rec} f \, x . M) / f]
    }
$$

Spremenljivko $x$ kot prej zamenjamo z argumentom $V$, spremenljivko $f$ pa z rekurzivno funkcijo samo.

## Vaje

### Naloga 1

Izračunajte naslednji izraz:

    (rec f x. if x = 0 then 1 else x * f (x - 1)) 3

### Naloga 2

Preverite tipe izrazov. Izrazi morda nimajo primernega tipa. V tem primeru poiščite mesto, kjer se postopek zatakne.

  1. `b:bool, x:int |- 1 + (if b then 2 else x) : int`
  2. `|- fun x -> (fun y -> x > y) : int -> int -> bool`
  3. `|- (rec f x -> if x then f x else 0) true : int`
  4. `f : int -> int |- (f 3) > (f (f 0)) 2 : bool`

### Naloga 3

Napišite nekaj izrazov, katerim ni možno dodeliti tipa, vendar se izračunajo v vrednost.

### Naloga 4

Razširite jezik in sistem tipov s pari, seznami in vsotami. Za pare dodajte projekciji na posamezno komponento, za sezname in vsote pa dodajte razgrajevalnik `match`.

### Naloga 5

V jeziku iz naloge 3 poiščite primeren tip za spodnji izraz in ustreznost preverite z izpeljavo, v primeru ko sta s `fst` in `snd` označeni projekciji na prvo in drugo komponento para.

``` (fun p -> (match fst p with [] -> true | x :: xs -> snd p)) (1::2::[], false) ```

### Naloga 6

Pokažite, kako lahko v jezik dodamo medsebojno rekurzivne funkcije.
