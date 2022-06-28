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

# Metateorija programskih jezikov

Vsak programski jezik ima svojo _teorijo_, torej pravila, ki formalno opisujejo, kako se programi izvajajo ali določajo, katere programe smatramo za veljavne. Od teorije so odvisne lastnosti programskega jezika, na primer to, da se veljavni programi lahko vedno uspešno izvedejo, ali pa to, da je rezultat enolično določen. Lastnostim programskega jezika, ki sledijo iz dane teorije, pravimo _metateorija_. Za primer si oglejmo že znani programski jezik IMP.

## Teorija

### Sintaksa

Del teorije IMPa smo že srečali, in sicer njegovo abstraktno sintakso, ki jo tu le ponovimo:

$$
  \begin{aligned}
    \text{aritmetični izraz } e &::=
      \ell \mid
      \intsym{n} \mid
      e_1 + e_2 \mid
      e_1 - e_2 \mid
      e_1 * e_2 \\
    \text{logični izraz } b &::=
      \true \mid
      \false \mid
      e_1 = e_2 \mid
      e_1 < e_2 \mid
      e_1 > e_2 \\
    \text{ukaz } c &::=
      \ifthenelse{b}{c_1}{c_2} \mid
      \whiledo{b}{c} \mid
      c_1 ; c_2 \mid
      \ell := e \mid
      \skip
  \end{aligned}
$$

### Operacijska semantika

V sintaksi uporabljamo besede in simbole, ki nakazujejo na pomen posameznih izrazov in ukazov, ampak to pomena formalno ne podaja. Najprej podajmo _dinamično_ oziroma _operacijsko semantiko_, ki določa, kako se programi izvajajo. To smo enkrat že storili prek programa v OCamlu, zdaj pa to storimo še v jeziku matematike.

Začnimo z aritmetičnimi izrazi, ki naj bi predstavljali cela števila. Ker v aritmetičnih izrazih lahko dostopamo do lokacij $\ell$, moramo pomen podati relativno na neko stanje, ki ga predstavimo z množico parov lokacij in prirejenih vrednosti $\{ \ell_1 \mapsto n_1, \dots, \ell_k \mapsto n_k \}$. Tromestno relacijo $s, e \Downarrow n$, ki pove, da izraz $e$ v stanju $s$ predstavlja število $n$, podamo kot najmanjšo relacijo, zaprto za sledeča pravila:

$$
  \infer{(\ell \mapsto n) \in s}{s, \ell \Downarrow n} \qquad
  \infer{}{s, \intsym{n} \Downarrow n} \\[2em]
  \infer{s, e_1 \Downarrow n_1 \qquad s, e_2 \Downarrow n_2}{s, (e_1 + e_2) \Downarrow (n_1 + n_2)} \qquad
  \infer{s, e_1 \Downarrow n_1 \qquad s, e_2 \Downarrow n_2}{s, (e_1 - e_2) \Downarrow (n_1 - n_2)} \qquad
  \infer{s, e_1 \Downarrow n_1 \qquad s, e_2 \Downarrow n_2}{s, (e_1 * e_2) \Downarrow (n_1 \cdot n_2)}
$$

Na primer, za stanje $s = \{ \ell \mapsto 6, m \mapsto -5 \}$ velja $s, \ell * (\underline{3} + \underline{4}) \Downarrow 42$, saj imamo sledeče drevo izpeljave:

$$
  \infer{
    \infer{(\ell \mapsto 6) \in s}{s, \ell \Downarrow 6} \qquad
    \infer{
      \infer{}{s, \intsym{3} \Downarrow 3} \qquad
      \infer{}{s, \intsym{4} \Downarrow 4}
    }{
      s, (\intsym{3} + \intsym{4}) \Downarrow 7
    }
  }{
    s, \ell * (\intsym{3} + \intsym{4}) \Downarrow 42
  }
$$

Če aritmetični izraz $e$ vsebuje dostop do lokacije $\ell$, ki v stanju $s$ ni nastavljena, bi to v implementaciji sprožilo napako ob izvajanju, v matematični formulaciji pa preprosto pomeni, da za nobeno število $n$ ne velja $s, e \Downarrow n$.

Podobno podamo relacijo $s, b \Downarrow r$, ki pove, kdaj logični izraz $b$ v danem stanju $s$ predstavlja resničnostno vrednost $r \in \mathbb{B} = \{ t\!t, f\!\!f \}$:

$$
  \infer{}{s, \true \Downarrow \ttt} \qquad
  \infer{}{s, \false \Downarrow \fff} \\[2em]
  \infer{s, e_1 \Downarrow n_1 \qquad s, e_2 \Downarrow n_2 \qquad n_1 = n_2}{s, (e_1 = e_2) \Downarrow \ttt} \qquad
  \infer{s, e_1 \Downarrow n_1 \qquad s, e_2 \Downarrow n_2 \qquad n_1 \ne n_2}{s, (e_1 = e_2) \Downarrow \fff} \\[2em]
  \infer{s, e_1 \Downarrow n_1 \qquad s, e_2 \Downarrow n_2 \qquad n_1 < n_2}{s, (e_1 < e_2) \Downarrow \ttt} \qquad
  \infer{s, e_1 \Downarrow n_1 \qquad s, e_2 \Downarrow n_2 \qquad n_1 \ge n_2}{s, (e_1 < e_2) \Downarrow \fff} \\[2em]
  \infer{s, e_1 \Downarrow n_1 \qquad s, e_2 \Downarrow n_2 \qquad n_1 > n_2}{s, (e_1 > e_2) \Downarrow \ttt} \qquad
  \infer{s, e_1 \Downarrow n_1 \qquad s, e_2 \Downarrow n_2 \qquad n_1 \le n_2}{s, (e_1 > e_2) \Downarrow \fff}
$$

Pri ukazih so pravila malo drugačna, saj za razliko od izrazov ne predstavljajo nobenih vrednosti, temveč je njihov končni rezultat spremenjeno stanje. Poleg tega se zaradi morebitnih neskončnih zank izvajanje ukazov ne konča vedno. Zaradi slednjega bomo pomen ukazov podali prek semantike _malih korakov_ $s, c \leadsto s', c'$, ki ukaza $c$ v stanju $s$ ne bo povezala s končnim rezultatom, temveč z novim ukazom $c'$ in novim stanjem $s'$, ki predstavljata spremembe, ki so se zgodile v enem računskem koraku.

$$
  \infer{s, b \Downarrow \ttt}{s, \ifthenelse{b}{c_1}{c_2} \leadsto s, c_1} \qquad
  \infer{s, b \Downarrow \fff}{s, \ifthenelse{b}{c_1}{c_2} \leadsto s, c_2} \\[2em]
  \infer{s, b \Downarrow \ttt}{s, \whiledo{b}{c} \leadsto s, (c; \whiledo{b}{c})} \qquad
  \infer{s, b \Downarrow \fff}{s, \whiledo{b}{c} \leadsto s, \skip}  \\[2em]
  \infer{s, c_1 \leadsto s', c_1'}{s, (c_1 ; c_2) \leadsto s', (c_1' ; c_2)} \qquad
  \infer{}{s, (\skip ; c_2) \leadsto s, c_2}  \\[2em]
  \infer{s, e \Downarrow n}{s, \ell := e \leadsto s[\ell \mapsto n], \skip}
$$

Vidimo, da takrat, kadar pogoj v logičnem ukazu predstavlja resnico, v naslednjem računskem koraku začnemo izvajati prvo, sicer pa drugo vejo, obe v nespremenjenem stanju. Podobno ločimo dva primera pri zankah. Če je pogoj resničen, izvedemo telo, ki mu sledi še ena ponovitev zanke, sicer pa zaključimo izvajanje. Pri zaporednem izvajanju korak naredimo v prvem ukazu in ustrezno spremenimo stanje. Če je prvi ukaz končal z izvajanjem, začnemo izvajati drugi ukaz. Pri prirejanju spremenimo stanje in zaključimo izvajanje. Ukaz $\mathbf{skip}$ predstavlja zaključeno izvajanje, zato pravila zanj nimamo.

S tako podanimi pravili lahko pokažemo, da je semantika ukazov deterministična. Če velja tako $s, c \leadsto s', c'$ kot $s, c \leadsto s'', c''$, tedaj je $s' = s''$ in $c' = c''$. Dokaz poteka z indukcijo, pri tem pa si pomagamo s pomožnima trditvama, da sta tudi semantiki aritmetičnih in logičnih izrazov deterministični.

### Določanje veljavnih programov

Želeli bi pokazati, da vsak program lahko vedno naredi korak, vendar to ni res, saj lahko nekateri programi dostopajo do lokacij, ki v stanju niso nastavljene. Da bi se tem primerom izognili, smo v implementaciji napisali funkcijo `check`, ki je program preverila, preden ga je začela izvajati. Matematično pa bomo to zopet podali z induktivnimi relacijami. Relacijo $L \vdash e$, ki podaja, kdaj aritmetični izraz $e$ dostopa le do lokacij iz množice $L$, podamo s sledečimi pravili:

$$
  \infer{\ell \in L}{L \vdash \ell} \qquad
  \infer{}{L \vdash \intsym{n}} \\[2em]
  \infer{L \vdash e_1 \qquad L \vdash e_2}{L \vdash (e_1 + e_2)} \qquad
  \infer{L \vdash e_1 \qquad L \vdash e_2}{L \vdash (e_1 - e_2)} \qquad
  \infer{L \vdash e_1 \qquad L \vdash e_2}{L \vdash (e_1 * e_2)}
$$

Podobno to storimo za logične izraze:

$$
  \infer{}{L \vdash \true} \qquad
  \infer{}{L \vdash \false} \\[2em]
  \infer{L \vdash e_1 \qquad L \vdash e_2}{L \vdash (e_1 = e_2)} \qquad
  \infer{L \vdash e_1 \qquad L \vdash e_2}{L \vdash (e_1 < e_2)} \qquad
  \infer{L \vdash e_1 \qquad L \vdash e_2}{L \vdash (e_1 > e_2)}
$$

Pri ukazih je situacija malenkostno drugačna, saj spreminjajo stanje in s tem tudi množico veljavnih lokacij. Tako podamo relacijo $L \vdash c, L'$, ki pove, da ukaz $c$ dostopa le do lokacij iz množice $L$, po izvajanju pa bodo definirane vse lokacije iz množice $L'$.

$$
  \infer{L \vdash b \qquad L \vdash c_1, L_1 \qquad L \vdash c_2, L_2}{L \vdash \ifthenelse{b}{c_1}{c_2}, L_1 \cap L_2} \qquad
  \infer{L \vdash b \qquad L \vdash c, L'}{L \vdash \whiledo{b}{c}, L'} \\[2em]
  \infer{L \vdash c_1, L' \qquad L' \vdash c_2, L''}{L \vdash (c_1 ; c_2), L''} \qquad
  \infer{L \vdash e}{L \vdash \ell := e, L \cup \{ \ell \}} \qquad
  \infer{}{L \vdash \skip, L}
$$

## Izrek o varnosti

Eden glavnih izrekov, ki jih običajno dokažemo, ko obravnavamo metateorijo programskega jezika, je izrek o varnosti. Ta govori o tem, da se veljavni programi lahko uspešno izvedejo. Razdeljen je na dva dela:

1. napredek, ki pove, da veljaven program bodisi lahko naredi še en računski korak bodisi je uspešno zaključil izvajanje,
2. ohranitev, ki pove, da veljaven program po enem računskem koraku ostane veljaven.

Oba skupaj povesta, da vsak veljaven program izvaja računske korake do točke, ko zaključi izvajanje, ali pa teče v neskončnost. Oglejmo si vsakega posebej.

### Napredek

Naj bo $s$ stanje, definirano na vseh lokacijah iz množice $L$. Tedaj:

- Za vsak aritmetični izraz $e$, za katerega velja $L \vdash e$, obstaja $n \in \mathbb{Z}$, da velja $s, e \Downarrow n$.
- Za vsak logični izraz $b$, za katerega velja $L \vdash b$, obstaja $r \in \mathbb{B}$, da velja $s, b \Downarrow r$.
- Za vsak ukaz $c$ in množico lokacij $L'$, za kateri velja $L \vdash c, L'$:
    1. bodisi obstajata ukaz $c'$ in stanje $s'$, da velja $s, c \leadsto s', c'$,
    2. bodisi velja $c = \skip$.

#### Dokaz

Vsak del dokažemo z indukcijo.

- Obravnavajmo vsa pravila, po katerih smo lahko prišli do zaključka $L \vdash e$.

  - Če smo uporabili pravilo $\infer{\ell \in L}{L \vdash \ell}$, potem je $\ell \in L$, torej po predpostavki obstaja $n$, da velja $(\ell \mapsto n) \in s$, zato velja tudi $s, \ell \Downarrow n$.
  - Če smo uporabili pravilo $\infer{}{L \vdash \intsym{n}}$, potem velja $s, \intsym{n} \Downarrow n$.
  - Če smo uporabili pravilo $\infer{L \vdash e_1 \qquad L \vdash e_2}{L \vdash (e_1 + e_2)}$, potem po indukcijski predpostavki obstajata $n_1$ in $n_2$, da velja $s, e_1 \Downarrow n_1$ in $s, e_2 \Downarrow n_2$. Tedaj velja tudi $s, (e_1 + e_2) \Downarrow (n_1 + n_2)$. Podoben dokaz uporabimo še v primerih $L \vdash (e_1 - e_2)$ in $L \vdash (e_1 * e_2)$.

- Obravnavajmo vsa pravila, po katerih smo lahko prišli do zaključka $L \vdash b$.

  - Če smo uporabili pravilo $\infer{}{L \vdash \true}$, potem velja $s, \true \Downarrow \ttt$. Podoben dokaz uporabimo še v primeru $L \vdash \false$.
  - Če smo uporabili pravilo $\infer{L \vdash e_1 \qquad L \vdash e_2}{L \vdash (e_1 = e_2)}$, potem po poprej dokazani trditvi obstajata $n_1$ in $n_2$, da velja $s, e_1 \Downarrow n_1$ in $s, e_2 \Downarrow n_2$. Ker velja bodisi $n_1 = n_2$ bodisi $n_1 \ne n_2$, vedno obstaja $r \in \mathbb{B}$, da velja $s, (e_1 = e_2) \Downarrow r$. Podoben dokaz uporabimo še v primerih $L \vdash (e_1 < e_2)$ in $L \vdash (e_1 > e_2)$.

- Obravnavajmo vsa pravila, po katerih smo lahko prišli do zaključka $L \vdash c, L'$.

  - Če smo uporabili pravilo $\infer{L \vdash b \qquad L \vdash c_1, L_1 \qquad L \vdash c_2, L_2}{L \vdash \ifthenelse{b}{c_1}{c_2}, L_1 \cap L_2}$, potem po prejšnji trditvi obstaja $r \in \mathbb{B}$, da velja $s, b \Downarrow r$. Če je $r = \ttt$, potem velja $c, s \leadsto c_1, s$, če pa je $r = \fff$, pa velja $c, s \leadsto c_2, s$. V obeh primerih torej lahko naredimo korak.

  - Podoben dokaz lahko naredimo v primeru $L \vdash \whiledo{b}{c}, L'$.

  - Če smo uporabili pravilo $\infer{L \vdash c_1, L' \qquad L' \vdash c_2, L''}{L \vdash (c_1 ; c_2), L''}$, po indukcijski predpostavki za $L \vdash c_1$ velja:

    1. bodisi obstajata ukaz $c_1'$ in stanje $s'$, da velja $s, c_1 \leadsto s', c_1'$, zaradi česar velja tudi $s, (c_1; c_2) \leadsto s, (c_1'; c_2)$,
    2. bodisi je $c_1 = \skip$, zaradi česar velja $s, (\skip; c_2) \leadsto s, c_2$.

    V obeh primerih torej lahko naredimo korak.

  - Če smo uporabili pravilo $\infer{L \vdash e}{L \vdash \ell := e, L \cup \{ \ell \}}$, potem po prejšnji trditvi obstaja $n$, da velja $s, e \Downarrow n$, zato velja tudi $s, (\ell := e) \leadsto s[\ell \mapsto n], \skip$.

  - Če smo uporabili pravilo $\infer{}{L \vdash \skip, L}$, potem velja $c = \skip$.

Opazimo lahko, da v dokazu napredka za ukaze ne uporabimo druge množice lokacij $L'$. Ta pride v poštev, če želimo narediti več kot en korak.

### Varnost

Vzemimo ukaza $c, c'$, množici lokacij $L, L'$ ter stanji $s, s'$, tako da:

1. je stanje $s$ definirano na vseh lokacijah iz množice $L$,
2. velja $L \vdash c, L'$, ter
3. velja $s, c \leadsto s', c'$.

Tedaj obstaja množica lokacij $L''$, da velja $L'' \vdash c', L'$ in je $s'$ definirano na vseh lokacijah iz množice $L''$.

Pozorni bodimo na dejstvo, da množica lokacij $L'$ ne ustreza stanju $s'$, saj gre za množico lokacij po koncu izvajanja ukaza $c$, med tem ko se je v $s'$ lahko v enem samem koraku spremenila le ena. Na primer, velja $\emptyset \vdash (\ell := \intsym{3}; m := \intsym{4}), \{ \ell, m \}$ in $\emptyset, (\ell := \intsym{3}; m := \intsym{4}) \leadsto \{ \ell \mapsto 3 \}, (\skip; m := \intsym{4})$. V tem primeru je $L = \emptyset$, $L' = \{ \ell, m \}$ ter $L'' = \{ \ell \}$.

## Vaje

### Naloga 1

Preverite primere, izpuščene v zgornjih dokazih.

### Naloga 2

Vpeljite relacijo $\leadsto^*$, ki je tranzitivna in refleksivna ovojnica relacije malih korakov $\leadsto$. Razložite, kakšen je njen intuitivni pomen relacije ter pokažite, da je res tranzitivna.

### Naloga 3

Definirajte _vrednosti_ `v` za posamezne družine izrazov.

Pokažite pomožno lemo oblike:

    s , c1 ~~> s' , c1' 
    => 
    s' , c1' --> s'' 
    => 
    s , c1 --> s''

Pokažite ujemanje `~~*>` in `-->` (semantika velikih korakov):

    s , e --> s', v 
    <=> 
    s , e ~~*> s' , v

Ujemanje je potrebno pokazati za vsako družino izrazov.
