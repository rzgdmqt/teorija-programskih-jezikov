# Monade

Videli smo, da je operacijska semantika računskih učinkov precej raznolika. Pri denotacijski semantiki ni nič drugače. Recimo, da tip $A$ predstavimo z množico (ali domeno, čeprav zaradi enostavnosti delajmo z množicami) vrednosti $\itp{A}$. Programe, ki računajo vrednosti tipa $A$ smo do sedaj predstavljali s funkcijami, ki so slikale v $\itp{A}$. A če dodamo učinke, taka predstavitev ni več dovolj dobra.

- Če želimo v programu podpirati izjeme, bo njegova interpretacija morala razločevati programe, ki vračajo vrednosti iz $A$ od tistih, ki sprožajo izjeme. Tako bi bila ustreznejša kodomena funkcij $\itp{A} + \mathbb{E}$.
- Če program izpisuje znake iz množice $O$, moramo poleg rezultata vrniti še seznam izpisanih znakov, torej bi bila kodomena $\itp{A} \times O^*$.
- Če naš program lahko bere in piše po pomnilniku z množico možnih stanj $S$, bo interpretacija programa odvisna od začetnega stanja, hkrati pa bo vrnila novo, spremenjeno stanje. Boljša interpretacija bi bila $(\itp{A} \times S)^S$.
- Če je izvajanje programa nedeterministično in gre lahko po različnih poteh, bi bilo bolje, če bi rezultat predstavili z množico možnih vrnjenih vrednosti, torej kodomeno $\mathcal{P}(\itp{A})$.

Opazimo, da so vsi zgornji primeri kodomen oblike $T \itp{A}$, kjer je $T$ neka konstrukcija, ki množico $X$ slika v množico $T X$. Poleg tega za interpretacijo programov, ki povzročajo učinke, potrebujemo še dve operaciji:

- Če imamo vrednost tipa $A$, moramo znati predstaviti tudi program, ki vrača samo to vrednost in ne sproža nobenih učinkov. Torej potrebujemo operacijo, ki $\itp{A}$ vloži v $T \itp{A}$.
- Če imamo program, ki vrača vrednosti tipa $A$, in še en program, ki vrača vrednosti tipa $B$, ju želimo izvesti enega za drugim. Pri tem je drugi program lahko odvisen od rezultata prvega. Torej potrebujemo operacijo, ki zna iz interpretacije v $T \itp{A}$ in funkcije $\itp{A} \to T \itp{B}$ sestaviti interpretacijo združenega programa v $T \itp{B}$.

## Definicija monad

Tako strukturo opisujejo _monade_. Monada je podana s trojico $(T, \eta, \bind)$, kjer je:

- $T$ preslikava (funktor), ki vsaki množici $X$ priredi množico $T X$,
- _enoto_ $\eta$, ki je družina preslikav (naravna transformacija) $\eta_X : X \to T X$ za poljubno množico $X$,
- _veriženjem_ $\bind$, ki je družina preslikav (naravna transformacija) $\bind_{X, Y} : T X \to (X \to T Y) \to T Y$ za poljubni množici $X$ in $Y$,

ki zadoščajo zakonom:

- $\eta(x) \bind_{X, Y} k = k(x)$ za poljuben $x \in X$ ter $k : X \to T Y$,
- $m \bind_{X, X} \eta_X = m$ za poljuben $m \in T X$,
- $(m \bind_{X, Y} k) \bind_{Y, Z} k' = m \bind_{X, Z} (x \mapsto k(x) \bind_{Y, Z} k')$ za poljuben $m \in T X$, $k : X \to T Y$ in $k' : Y \to T Z$.

## Primeri monad

### Izjeme

Monado za izjeme iz množice $\mathbb{E}$ podamo kot:

$$\begin{align*}
    T X &= X + \mathbb{E} \\
    \eta_X(x) &= \iota_1(x) \\
    m \bind_{X, Y} k &= \begin{cases}
        k(x) & m = \iota_1(x) \\
        \iota_2(e) & m = \iota_2(e)
    \end{cases}
\end{align*}$$

Preverimo še veljavnost zakonov:

$$\begin{align*}
    \eta(x) \bind_{X, Y} k  &= \iota_1(x) \bind_{X, Y} k = k(x) \\
    m \bind_{X, Y} \eta_X(x) &= \left(\begin{cases}
        \iota_1(x) & m = \iota_1(x) \\
        \iota_2(e) & m = \iota_2(e)
    \end{cases}\right) = m \\
    (m \bind k) \bind k' &= \left(\begin{cases}
        k(x) & m = \iota_1(x) \\
        \iota_2(e) & m = \iota_2(e)
    \end{cases}\right) \bind k' \\
    &= \begin{cases}
        k(x) \bind k' & m = \iota_1(x) \\
        \iota_2(e) \bind k' & m = \iota_2(e)
    \end{cases} \\
    &= \begin{cases}
        k(x) \bind k' & m = \iota_1(x) \\
        \iota_2(e) & m = \iota_2(e)
    \end{cases} \\
    &= m \bind (x \mapsto k(x) \bind k')
\end{align*}$$

### Pomnilnik

Monado za delo s pomnilnikom nad množico stanj $S$ podamo kot:

$$\begin{align*}
    T X &= (X \times S)^S \\
    \eta_X(x) &= (s \mapsto (x, s)) \\
    m \bind_{X, Y} k &= (s \mapsto k(\underbrace{\pi_1(m(s))}_x)(\underbrace{\pi_2(m(s))}_{s'}))
\end{align*}$$

Veriženje deluje tako, da $m : S \to (X \times S)$ uporabi na začetnem stanju $s \in S$ in dobi vrednost $x = \pi_1(m(s)) \in X$ ter novo stanje $s' = \pi_2(m(s)) \in S$. Nato $k : X \mapsto (Y \times S)^S$ uporabi na $x$, da dobi $k(x) : S \to (Y \times S)$, ki ga uporabi še na $s'$, da dobi končni rezultat $k(x)(s') : Y \times S$.

Preverimo še veljavnost zakonov::

$$\begin{align*}
    (\eta(x) \bind_{X, Y} k)
        &= (s \mapsto k(\pi_1(\eta(x)(s)))(\pi_2(\eta(x)(s)))) \\
        &= (s \mapsto k(\pi_1((x, s)))(\pi_2((x, s)))) \\
        &= (s \mapsto k(x)(s)) \\
        &= k(x) \\
    m \bind_{X, Y} \eta(x)
        &= (s \mapsto \eta(\pi_1(m(s)))(\pi_2(m(s)))) \\
        &= (s \mapsto (\pi_1(m(s)), \pi_2(m(s)))) \\
        &= (s \mapsto m(s)) \\
        &= m \\
    (m \bind k) \bind k'
        &= (s \mapsto k(\pi_1(m(s)))(\pi_2(m(s)))) \bind k' \\
        &= (s \mapsto k(\pi_1(m(s)))(\pi_2(m(s)))) \bind k' \\
        &= (s \mapsto k'(\pi_1(k(\pi_1(m(s)))(\pi_2(m(s)))))(\pi_2(k(\pi_1(m(s)))(\pi_2(m(s)))))) \\
        &= m \bind (x \mapsto (s \mapsto k'(\pi_1(k(x)(s)))(\pi_2(k(x)(s))))) \\
        &= m \bind (x \mapsto k(x) \bind k')
\end{align*}$$
