# Denotacijska semantika

Denotacijska semantika pomen programom daje prek obstoječih matematičnih pojmov, s čimer dobimo nov pogled in tudi dostop do široke palete orodij. Na primer, v matematiki smo navajeni, da eno vrednost lahko kjerkoli zamenjamo z drugo, ki ji je enaka. Na primer, $6 \cdot 7$ lahko vedno zamenjamo z $42$ in dobimo enak rezultat.

V programskih jezikih zgodba ni tako enostavna, saj izraza $M = \intsym{6} + \intsym{7}$ in $\intsym{42}$ nista enaka, pa bi vendar radi prvega zamenjali z drugim, da na primer s tem dobimo krajši in hitrejši program. Kdaj to lahko storimo?

Res je, da velja $M \leadsto \intsym{42}$, vendar pa že izraza $\lambda x. M * x$ in $\lambda x. \intsym{42} * x$ nista enaka, hkrati pa sta vrednosti, zato niti ne obstaja zaporedje korakov, ki bi ju pripeljalo skupaj. A vseeno sta obe funkciji na nek način ekvivalentni, saj bosta uporabljeni na istih številih na koncu dali isti rezultat. Seveda se izraz $M$ lahko pojavi tudi v funkcijah višjega reda in z zamenjavo z $\intsym{42}$ bomo dobili drugačno funkcijo, ki pa bo na _ekvivalentnih_ argumentih dala _ekvivalentne_ rezultate. Ampak tudi s funkcijami višjega reda nismo izčrpali vseh možnih izrazov, v katerih se lahko pojavi $M$.

## Kontekstna ekvivalenca

Formalizacija zgornjih misli nas pripelje do pojma kontekstne ekvivalence. _Konteksti_ $\ctxt$ (ki imajo žal isto ime, ampak drugačen pomen od že znanih kontekstov $\Gamma$) predstavljajo izraze, v katerih se lahko na poljubnih mestih pojavijo _luknje_ $[]$. Na primer, za lambda račun s preprostimi tipi jih podamo s sintakso:

$$
    \begin{align*}
    \text{kontekst } \ctxt &::=
        [\,]
        \mid x
        \mid \true
        \mid \false
        \mid \ifthenelse{\ctxt}{\ctxt_1}{\ctxt_2} \\
        &\mid \intsym{n}
        \mid \ctxt_1 + \ctxt_2
        \mid \ctxt_1 * \ctxt_2
        \mid \ctxt_1 < \ctxt_2 \\
        &\mid \lambda x. \ctxt
        \mid \ctxt_1 \, \ctxt_2
\end{align*}
$$

Če imamo kontekst $\ctxt$, lahko vse luknje zamenjamo z izrazom $M$ in dobimo izraz, ki ga označimo z $\ctxt[M]$. Na primer, če je $\ctxt = \lambda x. \ifthenelse{x < [\,]}{x}{[\,]}$ in $M = \intsym{6} * \intsym{7}$, je
$$
  \ctxt[M] = \lambda x. \ifthenelse{x < (\intsym{6} * \intsym{7})}{x}{(\intsym{6} * \intsym{7})}
$$

Pravimo, da sta izraza $M$ in $N$ _kontekstno ekvivalentna_, kar pišemo kot  $M \simeq N$, kadar za poljuben kontekst $\ctxt$ velja $\ctxt[M] \leadsto^* \true$ natanko takrat, kadar velja $\ctxt[N] \leadsto^* \true$.

Izkaže se, da zaradi poljubne izbire kontekstov $\ctxt$ iz zgornje definicije sledi tudi enakost drugih rezultatov.

**Trditev.** Če velja $M \simeq N$, potem za poljuben kontekst $\ctxt$ velja $\ctxt[M] \leadsto^* \false$ natanko takrat, kadar velja $\ctxt[N] \leadsto^* \false$.

**Dokaz.** Naj velja $\ctxt[M] \leadsto^* \false$. Tedaj definirajmo $\ctxt' = \ifthenelse{\ctxt}{\false}{\true}$, zato velja $\ctxt'[M] \leadsto^* \ifthenelse{\false}{\false}{\true} \leadsto \true$, zato iz $M \simeq N$ sledi tudi $\ctxt'[N] \leadsto^* \true$, kar pa je možno le, če je $\ctxt'[N] \leadsto^* \false$. V drugo smer je dokaz enak. ■

**Trditev.** Če velja $M \simeq N$, potem za poljuben kontekst $\ctxt$ velja $\ctxt[M] \leadsto^* \intsym{n}$ natanko takrat, kadar velja $\ctxt[N] \leadsto^* \intsym{n}$.

**Dokaz.** Naj velja $\ctxt[M] \leadsto^* \intsym{n}$. Tedaj definirajmo $\ctxt'$ kot $(\ctxt = \intsym{n})$. Tedaj velja $\ctxt'[M] \leadsto^* (\intsym{n} = \intsym{n}) \leadsto \true$, zato iz $M \simeq N$ sledi tudi $\ctxt'[N] \leadsto^* \true$, kar pa je možno le, če je $\ctxt'[N] \leadsto^* \intsym{n}$. V drugo smer je dokaz enak. ■

Če imamo v jeziku na voljo rekurzijo, lahko kontekstno ekvivalenco definiramo celo tako, da izraz $\ctxt[M]$ konvergira natanko tedaj, kadar konvergira izraz $\ctxt[N]$, saj lahko kontekst ovijemo s pogojnim izrazom, ki v eni veji konvergira, v drugi pa divergira.

Ob vsem moramo biti pozorni na dejstvo, da za veliko večino kontekstov $\ctxt$ izraz $\ctxt[M]$ sploh nima tipa in se pri izvajanju zatakne, vendar se tudi pri teh kontekstih popolnoma enako obnaša izraz $\ctxt[N]$. Tako se lahko omejimo samo na kontekste $\ctxt$, za katere ima  $\ctxt[M]$ veljaven tip.

Kontekstna ekvivalenca je tisto, kar želimo, vendar je delo z njo precej neobvladljivo, saj moramo kvantificirati čez vse kontekste, ki nimajo preveč lepih lastnosti. Namesto tega se vrnemo nazaj na začetek. Pojme iz našega jezika bomo prevedli na dobro znane matematične pojme, kot so množice in funkcije, saj lahko na njih uporabimo običajno enakost.

## Interpretacije tipov in izrazov

Ko delamo s tipi, si v ozadju vedno predstavljamo množico vrednosti, ki jih zasedajo. To bomo formalizirali in za vsak tip $A$ definirali njegovo _interpretacijo_ $\itp{A}$, ki je množica, rekurzivno definirana kot:

$$
    \begin{align*}
    \itp{\boolty} &= \mathbb{B} = \{ \ttt, \fff \} \\
    \itp{\intty} &= \mathbb{Z} \\
    \itp{A \to B} &= \itp{B}^{\itp{A}}
    \end{align*}
$$

Pri interpretaciji izrazov se bomo omejili na tiste, ki imajo dobro definiran tip. Predstavljamo si lahko, da bomo vsak izraz $M$ tipa $A$ interpretirali z elementom $\itp{M} \in \itp{A}$. Stvar se malo zaplete, ker se v $M$ lahko pojavijo proste spremenljivke iz nekega konteksta $\Gamma$, kar pomeni, da moramo tudi za vsako izmed njih določiti ustrezno vrednost. Zato definiramo interpretacijo kontekstov s kartezičnim produktom

$$
  \itp{x_1 : A_1, \dots, x_n : A_n} = \itp{A_1} \times \dots \times \itp{A_n}
$$

Tedaj lahko interpretacijo izraza $\Gamma \vdash M : A$ podamo s preslikavo $\itp{M} : \itp{\Gamma} \to \itp{A}$. Pri tem moramo biti pozorni, da preslikava ni odvisna samo od izraza, temveč od celotne določitve tipa, zato bi v resnici morali pisati

$$
  \itp{\Gamma \vdash M : A} : \itp{\Gamma} \to \itp{A}
$$

čeprav bomo povečini uporabljali krajši zapis. Interpretacijo podamo rekurzivno kot:

$$
\begin{align*}
\itp{\Gamma \vdash x_i : A_i}(a_1, \dots, a_n) &= a_i \\
\itp{\Gamma \vdash \true : \boolty}(\gamma) &= \ttt \\
\itp{\Gamma \vdash \false : \boolty}(\gamma) &= \fff \\
\itp{\Gamma \vdash \ifthenelse{M}{M_1}{M_2} : A}(\gamma) &=
  \begin{cases}
    \itp{M_1}(\gamma) & \itp{M}(\gamma) = \ttt \\
    \itp{M_2}(\gamma) & \itp{M}(\gamma) = \fff
  \end{cases} \\
\itp{\Gamma \vdash \intsym{n} : \intty}(\gamma) &= n \\
\itp{\Gamma \vdash M_1 + M_2 : \intty}(\gamma) &= \itp{M_1}(\gamma) + \itp{M_2}(\gamma) \\
\itp{\Gamma \vdash M_1 * M_2 : \intty}(\gamma) &= \itp{M_1}(\gamma) \cdot \itp{M_2}(\gamma) \\
\itp{\Gamma \vdash M_1 < M_2 : \boolty}(\gamma) &=
  \begin{cases}
    \ttt & \itp{M_1}(\gamma) < \itp{M_2}(\gamma) \\
    \fff & \text{sicer}
  \end{cases} \\
\itp{\Gamma \vdash \lambda x. M : A \to B}(\gamma) &= a \mapsto \itp{\Gamma, x : A \vdash M : B}(\gamma, a) \\
\itp{\Gamma \vdash M_1 \, M_2 : B}(\gamma) &= \big(\itp{M_1}(\gamma)\big)\big(\itp{M_2}(\gamma)\big)
\end{align*}
$$

Na primer, interpretacija izraza $x : \intty \vdash \lambda y. \intsym{2} * x > y + 5$ je funkcija $\mathbb{Z} \to \mathbb{B}^{\mathbb{Z}}$, podana z

$$
\begin{align*}
\itp{x : \intty \vdash \lambda y. \intsym{2} * x > y + 5}(m)
  &= n \mapsto \itp{x : \intty, y : \intty \vdash \intsym{2} * x > y + 5}(m, n) \\
  &= n \mapsto \begin{cases}
    \ttt & \itp{\intsym{2} * x}(m, n) > \itp{y + 5}(m, n) \\
    \fff & \text{sicer}
  \end{cases} \\
  &= n \mapsto \begin{cases}
    \ttt & 2 \cdot m > n + 5 \\
    \fff & \text{sicer}
  \end{cases}
\end{align*}
$$

## Povezava med denotacijsko in operacijsko semantiko

Ker imamo za isti jezik dve različni semantiki, se lahko vprašamo, ali se ujemata. In res, če en izraz naredi korak v drugega, se njuni interpretaciji ujemata. Po izreku o varnosti vemo, da bo tudi drugi izraz imel interpretacijo, če jo ima prvi.

**Trditev (skladnost).** Če za izraz $\vdash M : A$ velja $M \leadsto M'$, potem je $\itp{M} = \itp{M'}$.

V dokazu bomo uporabili lemo o substituciji, ki jo dokažemo z rutinsko indukcijo.

**Lema.** Za izraza $\Gamma, x : A \vdash M : B$ in $\Gamma \vdash N : A$ velja

$$
  \itp{\Gamma \vdash M[N / x] : B}(\gamma) = \itp{M}(\gamma, \itp{N}(\gamma))
$$

**Dokaz.** Dokaz poteka z indukcijo na $M \leadsto M'$. Za primer si oglejmo pravilo

$$\infer{
    M_1  \leadsto  M_1'
}{
    M_1 \, M_2  \leadsto  M_1' \, M_2
}$$

Če velja $\vdash M_1 \, M_2 : A$, potem je $\itp{M_1 \, M_2} = \itp{M_1}(\itp{M_2})$, pri čemer smo zaradi praznega konteksta izpustili pisanje trivialnih argumentov. Po indukcijski predpostavki je $\itp{M_1} = \itp{M_1'}$, zato je tudi $\itp{M_1 \, M_2} = \itp{M_1' \, M_2}$.

Pri pravilu

$$\infer{}{
    (\lambda x. M) \, V  \leadsto  M[V / x]
}$$

pa na levi strani dobimo

$$
  \itp{(\lambda x. M) \, V}(\gamma) = \itp{M}(\gamma, \itp{V}(\gamma))
$$

kar je po lemi o substituciji enako $\itp{M[V / x]}$. ■

V obratno smer trditev ne velja. Na primer, $\itp{\lambda x. x + x} = \itp{\lambda x. \intsym{2} * x}$, sta različna izraza, ki sta vrednosti, zato noben ne more narediti enega ali več korakov v drugega. A kot smo omenili že na začetku, ne zanima nas enakost, temveč samo ekvivalentnost rezultatov. Dovolj je, če obrat pokažemo že v enem konkretnem primeru.

**Trditev (zadostnost).** Če za $\vdash M : \boolty$ velja $\itp{M} = \ttt$, potem velja $M \leadsto^* \true$.

Dokaz je zahteven, zato ga bomo izpustili. Lažji pa je dokaz spološnejše posledice.

**Izrek.** Če za $\Gamma \vdash M : A$ in $\Gamma \vdash N : A$ velja $\itp{M} = \itp{N}$, potem velja tudi $M \simeq N$.

**Dokaz.** Vzemimo poljuben kontekst $\ctxt$, za katerega velja $\ctxt[M] \leadsto^* \true$. Brez škode za splošnost (čeprav tega nismo dokazali) se lahko omejimo na kontekste, za katere je $\vdash \ctxt[M] : \boolty$, torej je $\itp{\ctxt[M]}$ definirana in po skladnosti z operacijsko semantiko enaka $\itp{\true} = \ttt$. Ker je interpretacija podana strukturno, je vrednost $\itp{\ctxt[M]}$ na tistih mestih, kjer se v $\ctxt$ pojavljajo luknje $[\,]$, odvisna le od $\itp{M}$. Ker je $\itp{M} = \itp{N}$, je torej tudi $\itp{\ctxt[N]} = \itp{\ctxt[M]} = \ttt$. Po zadostnosti torej obstaja zaporedje korakov $\ctxt[N] \leadsto^* \true$, kar smo želeli pokazati. Obrat pokažemo simetrično. ■
