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

# Praksa programskih jezikov

```{code-cell}
:tags: [remove-cell, remove-stdout]

(* Ko se v Jupytru prvič požene OCaml, program Findlib izpiše neko sporočilo.
   Da se to sporočilo ne bi videlo v zapiskih, je tu ta celica, ki sproži izpis,
   vendar ima nastavljeno, da je v zapiskih v celoti skrita. *)
```

Da lahko začnemo raziskovati lastnosti programskih jezikov, potrebujemo primer takega jezika. To bo enostaven ukazni (oz. imperativni) programski jezik IMP, ki podpira aritmetične izraze (cela števila, spremenljivke, aritmetične operacije), logične izraze (logični konstanti ter primerjave aritmetičnih izrazov) ter ukaze (pogojne stavke, zanke, …).

## Sintaksa jezika

Prva stvar, ki jo moramo podati v definiciji programskega jezika, je njegova sintaksa - torej zapis vseh izrazov, ki jih jezik omogoča. Ločujemo med _konkretno sintakso_, ki definira zaporedja znakov, ki predstavljajo veljavne programe, ter _abstraktno sintakso_, ki možne izraze jezika predstavi z drevesno strukturo. Na primer, `1 + (2 * 3)` in `1+2*3` sta različna niza konkretne sintakse, ki oba predstavljata izraz abstraktne sintakse, predstavljen z drevesom

      +
     / \
    1   *
       / \
      2   3

ki ga na krajše pišemo kar kot $1 + (2 * 3)$, pri čemer matematična pisava nakazuje, da nas podrobnosti, kot so presledki, ne zanimajo.

### Konkretna sintaksa

Konkretno sintakso običajno podajamo v [Backus-Naurovi obliki (BNF)](https://en.m.wikipedia.org/wiki/Backus–Naur_form). Ker mora konkretno sintakso razumeti tudi računalnik, v njej upoštevamo tudi presledke, zamike, oklepaje, komentarje, …  Ta je sestavljena iz pravil, ki povedo, kakšne vrste simbolov lahko nastopajo v jeziku. Pravilo, ki podaja sintakso določenega simbola je oblike

    <simbol> ::= moznost1 | moznost2 | ...

kjer je vsaka izmed možnosti sestavljena iz enega ali več nizov ali drugih simbolov. Na primer, simbol za števko `<digit>` je lahko kateri koli izmed nizov `"0"`, `"1"`, …, `"9"`, številka pa je sestavljena iz ene ali več števk ter morebitnega predznaka:

    <digit> ::= "0" | "1" | ... | "9"
    <digits> ::= "" | <digit> <digits>
    <integer> ::= <digits> | "-" <digits>

Za programski jezik IMP bo konkretna sintaksa

    <space> ::= " " | "\n" | "\t" | "\r"
    <spaces> ::= "" | <spaces1>
    <spaces1> ::= <space> <spaces>

    <alpha> ::= "a" | "b" | ... | "z"
    <alphanum> ::= <alpha> | <digit>
    <alphanums> ::= "" | <alphanum> <alphanums>
    <ident> ::= <alpha> <alhpanums>
    <location> ::= "#" <ident>

    <exp> ::= <atomic_exp> <spaces> "+" <spaces> <atomic_exp>
           |  <atomic_exp> <spaces> "-" <spaces> <atomic_exp>
           |  <atomic_exp> <spaces> "*" <spaces> <atomic_exp>
           |  <atomic_exp>
    <atomic_exp> ::= <location>
                  |  <integer>
                  |  "(" <spaces> <exp> <spaces> ")"
    <bexp> ::= "true"
            |  "false"
            |  <exp> <spaces> "=" <spaces> <exp>
            |  <exp> <spaces> "<" <spaces> <exp>
            |  <exp> <spaces> ">" <spaces> <exp>
    <cmd> ::= "IF" <spaces1> <bexp> <spaces1> "THEN" <spaces1> <cmd> <spaces1> "ELSE" <spaces1> <cmd>
           |  "WHILE" <spaces1> <bexp> <spaces1> "DO" <spaces1> <cmd>
           |  <atomic_cmd> <spaces> ";" <spaces> <cmd>
    <atomic_cmd> ::= <location> <spaces> ":=" <spaces> <exp>
                  |  "SKIP"
                  |  "(" <spaces> <cmd> <spaces> ")"

Kaj predstavljajo zgoraj omenjeni simboli, si bomo ogledali pri abstraktni sintaksi jezika IMP, za idejo pa lahko vseeno podamo primer veljavnega programa v konkretni sintaksi:

    #fact := 1;
    WHILE #m > 0 DO (
        #fact := #fact * #m;
        #m := #m - 1
    )

Videti je, da program v pomnilniško lokacijo `#fact` shrani fakulteto števila, shranjenega v `#m`, vendar tega ne vemo, dokler ne podamo semantike jezika.

### Abstraktna sintaksa

Kot smo že videli, v abstraktni sintaksi možne izraze predstavimo z drevesi, katerih otroci predstavljajo njihove podizraze. Zaradi krajšega zapisa pa tudi abstraktno sintakso podamo v zapisu, podobnemu BNF, pri čemer nas podrobnosti, kot so oklepaji ali točen zapis spremenljivk in števil ne zanima.  Tako bomo predpostavili, da $n$ predstavlja poljubno celo število, $\ell$ pa poljubno pomnilniško lokacijo.

Kot smo že omenili, je jezik IMP sestavljen iz aritmetičnih izrazov, ki jih bomo označevali s spremenljivko $e$, logičnih izrazov, ki jih bomo označevali z $b$, ter ukazov, ki jih bomo označevali s $c$.

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

Oglejmo si vse veljavne dele jezika, pri čemer bomo za vsakega neformalno povedali, kaj predstavlja. Aritmetični izrazi so sestavljeni iz branja vrednosti pomnilniških lokacij, celoštevilskih konstant (ki jih podčrtamo, da jih ločimo od celih števil) ter aritmetičnih operacij, logični izrazi pa so sestavljeni iz logičnih konstant ter primerjav. Ukazi so bolj zanimivi:

- pogojni ukaz, ki izvede $c_1$, kadar $b$ predstavlja resnično logično vrednost, oziroma $c_2$, kadar $b$ predstavlja neresnično logično vrednost;
- zanka while` izvaja ukaz $c$, dokler $b$ predstavlja resnično logično vrednost;
- zaporedno izvajanje najprej izvede $c_1$ - ko (če) se ta konča, izvede še $c_2$;
- prirejanje izračuna aritmetični izraz $e$ ter njegovo vrednost zapiše v pomnilniško lokacijo $\ell$;
- zadnji ukaz ne naredi ničesar, uporabimo pa ga na primer takrat, kadar v pogojnem stavku želimo nekaj storiti le v eni izmed vej.

Zgornji program bi v abstraktni sintaksi lahko predstavili z ukazom

$$
  \begin{aligned}
    &\mathsf{fact} := \intsym{1}; \\
    &\whiledo{\mathsf{m} > \intsym{0}}{} \\
    &\quad \mathsf{fact} := \mathsf{fact} * \mathsf{m}; \\
    &\quad \mathsf{m} := \mathsf{m} - \intsym{1}
  \end{aligned}
$$

## Implementacija jezika

Običajno bi po sintaksi jezika formalno podali še njegovo semantiko, torej pomen posameznih delov. Ker za to še nimamo ustreznih matematičnih orodij, bomo ravnali podobno kot pri večini programskih jezikov: napisali bomo implementacijo, torej program, ki prebere ukaze, zapisane v konkretni sintaksi, ter jih na nek (po vsej sreči smiselen) način izvede. Nato pa bomo proglasili, da je pomen programa v IMPu tisto, kar implementacija z njim naredi. Implementacijo bomo napisali v programskem jeziku [OCaml](https://ocaml.org), ki je eden najprikladnejših jezikov za implementacije programskih jezikov. Končnica `ML` namreč pomeni _meta-language_ oziroma metajezik, torej jezik za opis jezikov.

### Implementacija abstraktne sintakse

Sintakso, ki je sestavljena iz treh vrst izrazov, bomo predstavili s tremi induktivnimi tipi. Pomnilniške lokacije bomo predstavili kar z nizi, vendar jih bomo zaradi lažjega razločevanja ovili s konstruktorjem.

```{code-cell}
:tags: [remove-output]

type location = Location of string

type exp =
  | Lookup of location
  | Int of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp

type bexp =
  | Bool of bool
  | Equal of exp * exp
  | Less of exp * exp
  | Greater of exp * exp

type cmd =
  | IfThenElse of bexp * cmd * cmd
  | WhileDo of bexp * cmd
  | Seq of cmd * cmd
  | Assign of location * exp
  | Skip
```

Na primer, aritmetična izraza $e_1 = (\intsym{6} * \intsym{7})$ in $e_2 = (\mathsf{m} - \intsym{1})$ bi predstavili z

```{code-cell}
let exp1 = Times (Int 6, Int 7)
let exp2 = Minus (Lookup (Location "m"), Int 1)
```

logični izraz $b = (\mathsf{m} > \intsym{0})$ z

```{code-cell}
let bexp = Greater (Lookup (Location "m"), Int 0)
```

ukaza $c_1 = (\mathsf{m} := e_2)$ in $c_2 = (\whiledo{b}{c_1})$ pa z

```{code-cell}
let cmd1 = Assign (Location "m", exp2)
let cmd2 = WhileDo (bexp, cmd1)
```

### Implementacija izvajanja

Pomen bomo najprej določili aritmetičnim izrazom, ki naj bi predstavljali cela števila. Ker izrazi lahko vsebujejo pomnilniške lokacije, mora funkcija za evalvacijo poleg izraza sprejeti tudi trenutno stanje pomnilnika, ki ga bomo predstavili kar s seznamom parov, ki danim lokacijam pripiše cela števila, na primer

```{code-cell}
let st1 = [(Location "m", 10); (Location "n", 0)]
let st2 = [(Location "m", 5)]
```

```{code-cell}
let rec eval_exp st = function
  | Lookup l -> List.assoc l st
  | Int n -> n
  | Plus (e1, e2) -> eval_exp st e1 + eval_exp st e2
  | Minus (e1, e2) -> eval_exp st e1 - eval_exp st e2
  | Times (e1, e2) -> eval_exp st e1 * eval_exp st e2
```

Tako na primer velja

```{code-cell}
eval_exp [] exp1
```

```{code-cell}
eval_exp st1 exp2
```

```{code-cell}
eval_exp st2 exp2
```

Podobno lahko definiramo funkcijo za evalvacijo logičnih izrazov, ki sprejme stanje in logični izraz ter vrne logično vrednost:

```{code-cell}
let eval_bexp st = function
  | Bool b -> b
  | Equal (e1, e2) -> eval_exp st e1 = eval_exp st e2
  | Less (e1, e2) -> eval_exp st e1 < eval_exp st e2
  | Greater (e1, e2) -> eval_exp st e1 > eval_exp st e2
```

```{code-cell}
eval_bexp st1 bexp
```

```{code-cell}
eval_bexp st2 bexp
```

Nazadnje definirajmo še funkcijo za evalvacijo ukazov. Funkcija sprejme stanje in ukaz, vrne pa končno vrednost stanja po izvršenem ukazu.

```{code-cell}
let rec eval_cmd st = function
  | IfThenElse (b, c1, c2) ->
      if eval_bexp st b then eval_cmd st c1 else eval_cmd st c2
  | WhileDo (b, c) ->
      (* eval_cmd st (IfThenElse (b, Seq (c, WhileDo (b, c)), Skip)) *)
      if eval_bexp st b then
        let st' = eval_cmd st c in
        eval_cmd st' (WhileDo (b, c))
      else st
  | Seq (c1, c2) ->
      let st' = eval_cmd st c1 in
      eval_cmd st' c2
  | Assign (l, e) -> (l, eval_exp st e) :: List.remove_assoc l st
  | Skip -> st
```

```{code-cell}
eval_cmd st1 cmd1
```

```{code-cell}
eval_cmd st2 cmd1
```

```{code-cell}
eval_cmd st1 cmd2
```

```{code-cell}
eval_cmd st2 cmd2
```

### Implementacija razčlenjevalnika

### Glavni program

## Vaje

### Naloga 1

Napišite sintaktična drevesa, ki ustrezajo programom:

    #a := 2 + #b

    if #x = 2 then 
      #x := 3
    else
      skip

    while #z > 0 do 
      #z := #z - 1;
      #w := #z + #w

    (while #z > 0 do #z := #z - 1);
    #w := #z + #w

### Naloga 2

Programe najprej napišite v OCamlu, nato pa jih prevedite v programski jezik IMP.

1. Napišite program, ki sešteje vsa naravna števila manjša od `n`.

2. Napišite program, ki preveri, ali je podano število praštevilo.

### Naloga 3

Razmislite, kako bi dopolnili sintakso in implementacijo jezika IMP z:

1. logičnima veznikoma `&&` in `||`,

2. ukazom `SWITCH`, ki zamenja vrednosti dveh lokacij,

3. ukazom `FAIL`, ki prekine izvajanje programa.

### Naloga 4

Izboljšajte razčlenjevalnik, da bo dopolnil nepopolne pogojne ukaze. Ukaz `IF b THEN c` naj se prevede v enako sintaktično drevo kot `IF b THEN c ELSE SKIP`.

### Naloga 5

Dopolnite vse dele jezika IMP s podporo za zanke `FOR` oblike:

    FOR l := e1 TO e2 DO
      c

ki najprej izračuna vrednosti aritmetičnih izrazov `e1` in `e2`, nato pa zaporedoma vsako celo število med njima shrani v `l` ter izvede ukaz `c`.
