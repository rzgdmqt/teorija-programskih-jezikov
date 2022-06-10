# Kontekstna ekvivalenca

Recimo, da želimo izraz $M = \intsym{6} + \intsym{7}$ poenostaviti do izraza $\intsym{42}$. Kdaj to lahko storimo? Res je, da velja $M \leadsto \intsym{42}$, vendar pa izraza $\lambda x. M * x$ in $\lambda x. \intsym{42} * x$ nista enaka, hkrati pa sta vrednosti, zato niti ne obstaja zaporedje korakov, ki ju pripeljal skupaj. A vseeno sta obe funkciji na nek način ekvivalentni, saj bosta uporabljeni na istih številih na koncu dali isti rezultat. Seveda se izraz $M$ lahko pojavi tudi v funkcijah višjega reda in z zamenjavo z $\intsym{42}$ bomo dobili drugačno funkcijo, ki pa bo na _ekvivalentnih_ argumentih dala _ekvivalentne_ rezultate. Ampak tudi s funkcijami višjega reda nismo izčrpali vseh možnih izrazov, v katerih se lahko pojavi $M$. Formalizacija zgornjih misli nas pripelje do pojma kontekstne ekvivalence.

## Konteksti

_Konteksti_ $\ctxt$ (ki imajo žal isto ime, ampak drugačen pomen od že znanih kontekstov $\Gamma$) predstavljajo izraze, v katerih se lahko na poljubnih mestih pojavijo _luknje_ $[]$. Na primer, za lambda račun s preprostimi tipi jih podamo s sintakso:

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

## Lastnosti kontekstne ekvivalence

Pravimo, da sta izraza $M$ in $N$ kontekstno ekvivalentna, kar pišemo kot  $M \simeq N$, kadar za poljuben kontekst $\ctxt$ velja $\ctxt[M] \leadsto^* \true$ natanko takrat, kadar velja $\ctxt[N] \leadsto^* \true$.

Izkaže se, da zaradi poljubne izbire kontekstov $\ctxt$ iz zgornje definicije sledi tudi enakost drugih rezultatov.

**Trditev.** Če velja $M \simeq N$, potem za poljuben kontekst $\ctxt$ velja $\ctxt[M] \leadsto^* \false$ natanko takrat, kadar velja $\ctxt[N] \leadsto^* \false$.

**Dokaz.** Naj velja $\ctxt[M] \leadsto^* \false$. Tedaj definirajmo $\ctxt' = \ifthenelse{\ctxt}{\false}{\true}$. Tedaj velja $\ctxt'[M] \leadsto^* \ifthenelse{\false}{\false}{\true} \leadsto \true$, zato iz $M \simeq N$ sledi tudi $\ctxt'[N] \leadsto^* \true$, kar pa je možno le, če je $\ctxt'[N] \leadsto^* \false$. V drugo smer je dokaz enak. ■


**Trditev.** Če velja $M \simeq N$, potem za poljuben kontekst $\ctxt$ velja $\ctxt[M] \leadsto^* \intsym{n}$ natanko takrat, kadar velja $\ctxt[N] \leadsto^* \intsym{n}$.

**Dokaz.** Naj velja $\ctxt[M] \leadsto^* \intsym{n}$. Tedaj definirajmo $\ctxt'$ kot $(\ctxt = \intsym{n})$. Tedaj velja $\ctxt'[M] \leadsto^* (\intsym{n} = \intsym{n}) \leadsto \true$, zato iz $M \simeq N$ sledi tudi $\ctxt'[N] \leadsto^* \true$, kar pa je možno le, če je $\ctxt'[N] \leadsto^* \intsym{n}$. V drugo smer je dokaz enak. ■

Ob vsem moramo biti pozorno na dejstvo, da za veliko večino kontekstov $\ctxt$ izraz $\ctxt[M]$ sploh nima tipa in se pri izvajanju zatakne, vendar se tudi pri teh kontekstih popolnoma enako obnaša izraz $\ctxt[N]$.