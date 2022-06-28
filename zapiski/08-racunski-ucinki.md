# Računski učinki

V praksi računalniki poleg računanja vrednosti sprožajo tudi _stranske_ oziroma _računske učinke_, kot so izjeme, interakcija z zunanjim svetom in drugimi računalniki, naključje in podobno. Kot bomo videli, dodatek učinkov precej zaplete teorijo.

## Drobnozrnati neučakani λ-račun

Preden uvedemo učinke, si oglejmo _drobnozrnati neučakani_ (_fine-grain call-by-value_) λ-račun_, ki je različica λ-računa, v katerem je vrstni red izvajanja eksplicitno določen s sintakso. Spomnimo se, da smo aplikacijo $M \, N$ računali tako, da smo najprej izračunali $M$, nato $N$, nazadnje pa naredili β-redukcijo. Alternativa bi bila, da bi najprej izračunali $N$, nato pa $M$. Če naši programi ne sprožajo učinkov, to ne bi naredilo velike razlike, ob prisotnosti učinkov pa jo. Težava je, da vrstni red iz sintakse ni natanko določen. Na primer, programi v OCamlu uporabljajo en vrstni red pri prevodu v strojno kodo, pri tolmačenju pa drugega. Tej dvoumnosti se bomo izognili tako, da bomo v aplikaciji in podobnih izrazih s sintakso zahtevali, da morajo biti nekateri podizrazi že vrednosti.

### Sintaksa

Do sedaj so vrednosti $V$ tvorile podmnožico vseh izrazov $M$, pri drobnozrnatem neučakanem λ-računu pa ti dve družini ločimo:

$$
    \begin{align*}
    \text{vrednost } V &::= x
        \mid \true
        \mid \false
        \mid \intsym{n}
        \mid \lambda x. M \\
    \text{izračun } M, N &::=
        \return V
        \mid \letin{x = M} N
        \mid \ifthenelse{V}{M_1}{M_2} \\
        &\mid V_1 + V_2
        \mid V_1 * V_2
        \mid V_1 < V_2
        \mid V_1 \, V_2
\end{align*}
$$

Vrednosti so podobne kot prej, pri izrazih pa vidimo, da je pogoj v logičnem izrazu vrednost, prav tako morajo biti vrednosti vsi argumenti operacij in aplikacije. Telo funkcije in veji logičnega izraza pa so _izračuni_, torej izrazi, ki se še morajo izvesti. Poleg tega dodamo še dva izračuna $\return V$ predstavlja izračun, ki vrne vrednost $V$, _veriženje_ $\letin{x = M} N$ pa najprej izračuna $M$, dobljeno vrednost veže v $x$ ter nadaljuje z izvajanjem izračuna $N$.

V dobljeni sintaksi $M \, N$ sploh ni veljaven izraz. Pišemo lahko kvečjemu $\letin{f = M} (\letin{x = N} f \, x)$, ali pa $\letin{x = N} (\letin{f = M} f \, x)$, pri čemer je očitno, kaj bomo izračunali najprej. Sintaksa takega jezika je sicer malo bolj nerodna, je pa zato metateorija toliko lepša.

### Operacijska semantika

Ker vrednosti predstavljajo že izračunane izraze, moramo operacijsko semantiko podati le na izračunih. To storimo z relacijo $M \leadsto M'$, podano s pravili:

$$
\infer{M \leadsto M'}{
    \letin{x = M} N \leadsto  \letin{x = M'} N
} \qquad
\infer{}{
    \letin{x = \return V} N \leadsto N[V / x]
} \\[2em]
\infer{}{
    \ifthenelse{\true}{M_1}{M_2}  \leadsto  M_1
} \qquad

\infer{}{
    \ifthenelse{\false}{M_1}{M_2}  \leadsto  M_2
} \\[2em]

\infer{}{
    \intsym{n_1} + \intsym{n_2}  \leadsto  \return \intsym{n_1 + n_2}
} \qquad

\infer{}{
    \intsym{n_1} * \intsym{n_2}  \leadsto  \return \intsym{n_1 \cdot n_2}
} \\[2em]

\infer{
    n_1 < n_2
}{
    \intsym{n_1} < \intsym{n_2}  \leadsto  \return \true
} \qquad

\infer{
    n_1 ≮ n_2
}{
    \intsym{n_1} < \intsym{n_2}  \leadsto  \return \false
} \\[2em]

\infer{}{
    (\lambda x. M) \, V  \leadsto  M[V / x]
}
$$

Če primerjamo pravila s tistimi iz običajnega λ-računa, vidimo, da jih je veliko manj, saj so vsi potrebni podizrazi že vrednosti, zato ne rabimo pravil za njihovo računanje. Edino pravilo, v katerem računamo vrednost podizraza, je pravilo za veriženje. Opazimo še, da aritmetične operacije ne vračajo števil $\intsym{n}$, ampak izračune $\return \intsym{n}$.

### Določanje tipov

Ker imamo dve različni vrsti izrazov, potrebujemo tudi dve relaciji za določanje tipov: $\Gamma \vdash_v V : A$ pravi, da ima vrednost $V$ tip $A$, relacija $\Gamma \vdash_c M : A$ pa pravi, da izračun $M$ vrača vrednosti tipa $A$.

$$
\infer{
    (x : A) ∈ \Gamma
}{
    \Gamma \vdash_v x : A
} \qquad

\infer{}{
    \Gamma \vdash_v \true : \boolty
} \qquad

\infer{}{
    \Gamma \vdash_v \false : \boolty
} \\[2em]

\infer{
    \Gamma \vdash_v V : \boolty \qquad
    \Gamma \vdash_c M_1 : A \qquad
    \Gamma \vdash_c M_2 : A
}{
    \Gamma \vdash_c \ifthenelse{V}{M_1}{M_2} : A
} \\[2em]

\infer{}{
    \Gamma \vdash_v \intsym{n} : \intty
} \qquad

\infer{
    \Gamma \vdash_v V_1 : \intty \qquad
    \Gamma \vdash_v V_2 : \intty
}{
    \Gamma \vdash_c V_1 + V_2 : \intty
} \\[2em]

\infer{
    \Gamma \vdash_v V_1 : \intty \qquad
    \Gamma \vdash_v V_2 : \intty
}{
    \Gamma \vdash_c V_1 * V_2 : \intty
} \qquad

\infer{
    \Gamma \vdash_v V_1 : \intty \qquad
    \Gamma \vdash_v V_2 : \intty
}{
    \Gamma \vdash_c V_1 < V_2 : \boolty
} \\[2em]

\infer{
    \Gamma, x : A \vdash_c M : B
}{
    \Gamma \vdash_v \lambda x. M : A \to B
} \qquad

\infer{
    \Gamma \vdash_v V_1 : A \to B \qquad
    \Gamma \vdash_v V_2 : A
}{
    \Gamma \vdash_c V_1 \, V_2 : B
} \\[2em]

\infer{
    \Gamma \vdash_v V : A
}{
    \Gamma \vdash_c \return V : A
} \qquad

\infer{
    \Gamma \vdash_c M : A \qquad
    \Gamma, x : A \vdash_c N : B
}{
    \Gamma \vdash_c \letin{x = M} N : B
}
$$

### Izrek o varnosti

Kot pričakovano velja izrek o varnosti. Spet je sestavljen iz napredka in ohranitve, skupaj pa ju lahko povzamemo takole.

Če velja $\vdash_c M : A$, potem:

1. obstaja $\vdash_c M' : A$, da velja $M \leadsto M'$, ali
2. obstaja vrednost $\vdash_v V : A$, da je $M = \return V$.

## Primeri učinkov

Oglejmo si nekaj primerov, kako dodajanje učinkov spremeni naš jezik.

### Izjeme

Predpostavimo, da imamo vnaprej definirano množico $\mathbb{E}$ izjem $E$. V jeziku običajno izjeme želimo sprožati in loviti. Za vsakega izmed teh dveh dejanj imamo ustrezen izračun:

$$
    \text{izračun } M, N ::=
        \cdots
        \mid \kwdpre{raise} E
        \mid \kwdpre{try} M \kwdmid{with} \{ E_1 \to N_1, \dots, E_k \to N_k \}
$$

Prvi sproži izjemo $E$, ki se iz podizračunov širi navzven, drugi pa poskusi izvesti izračun $M$. Če se vmes sproži izjema $E_i$, požene nadomestni izračun $N_i$. Če nadomestnega izračuna ni, sproži izjemo. V pravilih to zapišemo kot:

$$
    \infer{}{
    \letin{x = \kwdpre{raise} E} N \leadsto \kwdpre{raise} E
} \\[2em]
\infer{M \leadsto M'}{
    \kwdpre{try} M \kwdmid{with} \{ E_1 \to N_1, \dots, E_k \to N_k \} \leadsto \kwdpre{try} M' \kwdmid{with} \{ E_1 \to N_1, \dots, E_k \to N_k \}
} \\[2em]
\infer{}{
    \kwdpre{try} (\return V) \kwdmid{with} \{ E_1 \to N_1, \dots, E_k \to N_k \} \leadsto \return V
} \\[2em]
\infer{}{
    \kwdpre{try} (\kwdpre{raise} E_i) \kwdmid{with} \{ E_1 \to N_1, \dots, E_k \to N_k \} \leadsto N_i
}
$$

Če bi namesto neučakanega drobnozrnatega λ-računa uporabili običajnega, bi morali poskrbeti še za širjenje morebitnih izjem iz vseh podizrazov, na primer za aplikacijo bi morali dodati še pravili

$$
    \infer{}{
    (\kwdpre{raise} E) \, N \leadsto \kwdpre{raise} E
} \qquad
    \infer{}{
    V \, (\kwdpre{raise} E) \leadsto \kwdpre{raise} E
}$$

za vsako aritmetično operacijo pa prav tako.

Ker lahko izjemo sprožimo kjerkoli, ima poljuben tip, nadomestni izračuni pri lovljenju izjem pa morajo imeti enak tip kot prvotni izračun.

$$
    \infer{}{
    \Gamma \vdash_c \kwdpre{raise} E : A
} \qquad
    \infer{
        \Gamma \vdash_c M : A \qquad
        (\Gamma \vdash_c N_i : A)_{i = 1}^k
    }{
    \Gamma \vdash_c \kwdpre{try} M \kwdmid{with} \{ E_1 \to N_1, \dots, E_k \to N_k\} : A
}$$

Izrek o varnosti je zaradi izjem malo bolj nevaren, še vedno pa izključuje primere, ko se izvajanje zatakne:

Če velja $\vdash_c M : A$, potem:

1. obstaja $\vdash_c M' : A$, da velja $M \leadsto M'$, ali
2. obstaja vrednost $\vdash_v V : A$, da je $M = \return V$.
3. obstaja izjema $E \in \mathbb{E}$, da je $M = \kwdpre{raise} E$.

Če želimo, lahko pravila za določanje tipov izračunov razširimo do relacije $\Gamma \vdash_c M : A ! \mathcal{E}$, kjer množica $\mathcal{E}$ našteje vse izjeme, ki se lahko zgodijo med izvajanjem. V tem primeru tudi izrek o varnosti omeji izjeme, ki se lahko zgodijo. Konkretno, če bi veljalo $\vdash_c M : A ! \emptyset$, bi izrek o varnosti zagotavljal, da se ne bo sprožila nobena izjema.

### Pomnilnik

Zaradi enostavnosti predpostavimo, da imamo taka stanja pomnilnika kot v IMPu, torej množico lokacij $\ell$ s pripadajočimi celoštevilskimi vrednostmi. Za branje in pisanje potrebujemo dva izračuna:

$$
    \text{izračun } M, N ::=
        \cdots
        \mid {! \ell}
        \mid \ell := V
$$

Prvi vrne število, shranjeno v lokaciji $\ell$, drugi pa v lokacijo $\ell$ zapiše število, predstavljeno z vrednostjo $V$. Ker pisanje v pomnilnik običajno vrača enotsko vrednost $() : \kwd{unit}$, razširimo še vrednosti in tipe:

$$\begin{align*}
    \text{vrednost } V &::=
        \cdots
        \mid () \\
    \text{tip } A &::=
        \cdots
        \mid \kwd{unit}
\end{align*}$$

Alternativa bi bila, da bi $\ell := V$ vrednost $V$ tudi vrnil, tako kot na primer v C-ju ali pri novi operaciji `:=`v Pythonu. Pravili za določanje tipov sta

$$
    \infer{}{
        \Gamma \vdash_c {! \ell} : \intty
    }
    \qquad
    \infer{\Gamma \vdash_v V : \intty}{
        \Gamma \vdash_c {\ell := V} : \kwd{unit}
    }
$$

Ker izračuni spreminjajo pomnilnik, se mora to odražati tudi v operacijski semantiki, ki je, podobno kot v IMPu, oblike $s, M \leadsto s', M'$. Pri tem moramo spremeniti vsa pravila, pri čemer je pravilo za veriženje enako:

$$
\infer{s, M \leadsto s', M'}{
    s, \letin{x = M} N \leadsto s', \letin{x = M'} N
}
$$

pri vseh ostalih pravilih, ki nimajo predpostavk, pa samo označimo, da stanje $s$ ostane nespremenjeno. Za nova dva izračuna dodamo pravili:

$$
\infer{(\ell \mapsto n) \in s}{
    s, {! \ell} \leadsto s, \return n
} \qquad
\infer{}{
    s, \ell := \intsym{n} \leadsto s[\ell \mapsto n], \return ()
}
$$

Spet nam drobnozrnati λ-račun prihrani veliko dodatnih pravil, ki bi jih dobili, če bi pomnilnik lahko spreminjali tudi na primer podizrazi aritmetičnih operacij.

Izrek o varnosti za jezik, kot smo ga definirali, ne velja, saj nikjer ne preverjamo, ali so vse lokacije, ki jih beremo, tudi definirane. Obstajata dve možnosti. Prva je, da s pravilom

$$
\infer{\ell \notin s}{
    s, {! \ell} \leadsto s, \return \intsym{0}
}
$$
določimo, da so vse lokacije privzeto nastavljene na $0$, in dobimo običajen izrek o varnosti. Druga možnost pa je, da v pravilih za določanje tipov sledimo tudi lokacijam in podobno kot v IMPu definiramo relacijo oblike $\Gamma, L \vdash_c M : A$.
