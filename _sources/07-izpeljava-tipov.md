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

# Izpeljava tipov

```{code-cell}
:tags: [remove-cell, remove-stdout]

(* Ko se v Jupytru prvič požene OCaml, program Findlib izpiše neko sporočilo.
   Da se to sporočilo ne bi videlo v zapiskih, je tu ta celica, ki sproži izpis,
   vendar ima nastavljeno, da je v zapiskih v celoti skrita. *)
```

Videli smo, da za izraze, ki jim lahko priredimo tip, velja izrek o varnosti, kmalu pa bomo videli, da jih lahko interpretiramo z obstoječimi matematičnimi koncepti. Vendar je ročno prirejanje tipov zamudno, zato bi radi, da to namesto nas naredi računalnik. V ta namen bomo spoznali Hindley-Milnerjev algoritem, ki izpelje najbolj splošen tip, ki ga lahko priredimo izrazu.

## Hindley-Milnerjev algoritem

Hindley-Milnerjev algoritem poteka v dveh fazah. Najprej se rekurzivno sprehodi čez izraz in določi tipe tam, kjer je to očitno (na primer $\intsym{42}$ bo tipa $\intty$), na preostalih mestih pa pusti neznanke, ki morajo zadoščati določenim enačbam. Na primer, vemo, da bo aplikacija $M \, N$ nekega tipa $B$, če bo $M$ tipa $A \to B$ in bo $N$ tipa $A$. V ta namen sintakso tipov razširimo z neznankami $\alpha$, ki jih zaradi razločevanja od že znanih spremenljivk $x$ imenujemo _parametri_. V drugi fazi dobljene enačbe reši.

$$
\begin{align*}
A, B &::= \alpha
        \mid \boolty
        \mid \intty
        \mid A \to B
\end{align*}
$$

Da dobimo intuicijo, poskusimo izpeljati tip izraza $M = \lambda f. \lambda x. f \, (f \, x)$. Ker je $M$ funkcija, bo njen tip funkcijski. Problem lahko prevedemo na manjšega, če gremo izračunati tip telesa $\lambda x. f \, (f \, x)$, vendar moramo pred tem vedeti, kakšen tip bomo priredili spremenljivki $f$. Ker tega na tej točki še ne vemo, si izberemo nek svež parameter $\alpha$. Ko bomo izračunali, da je tip telesa recimo $B$, bomo vedeli tudi, da je tip celotnega izraza $M$ enak $\alpha \to B$. Ker je tudi telo funkcija, podoben korak ponovimo še enkrat ter se ob dodatni predpostavki $x : \beta$ lotimo računanja tipa $f \, (f \, x)$. Vse skupaj lahko predstavimo z drevesom

$$
  \infer{
    \infer{
      \infer{
        \infer{ }{\Gamma \vdash f : \alpha} \qquad
        \infer{
          \infer{}{\Gamma \vdash f : \alpha} \qquad
          \infer{}{\Gamma \vdash x : \beta} \qquad
          (\alpha = \beta \to \gamma)
        }{\Gamma \vdash f \, x : \gamma} \qquad
        (\alpha = \gamma \to \delta)
      }{
        \underbrace{f : \alpha, x : \beta}_\Gamma \vdash f \, (f \, x) : \delta
      }
    }{
      f : \alpha \vdash \lambda x. f \, (f \, x) : \beta \to \delta
    }
  }{
    \vdash \lambda f. \lambda x. f \, (f \, x) : \alpha \to (\beta \to \delta)
  }
$$

Na primer, ko želimo izračunati tip aplikacije $f \, x$, vemo, da zaradi predpostavk v kontekstu velja $f : \alpha$ in $x : \beta$. Po drugi strani pa mora biti $f$ funkcija, ki sprejema argumente tipa $\beta$, torej velja $\alpha = \beta \to \gamma$ za nek neznani tip $\gamma$, ki je tudi tip aplikacije. Podobno lahko ob predpostavki $\alpha = \gamma \to \delta$ izračunamo, da je tip telesa funkcije $f \, (f \, x)$ enak $\delta$.

Če si ogledamo dobljeni enačbi $\alpha = \beta \to \gamma =  \gamma \to \delta$ hitro vidimo še, da velja $\beta = \gamma = \delta$, zato je tip izraza $M$ enak $(\beta \to \beta) \to (\beta \to \beta)$, in to je tudi najbolj splošen tip.

### Nastavljanje enačb

Določanje sistema enačb opišemo z relacijo oblike $\Gamma \vdash M : A \mid \eqs$, ki pravi, da ima v kontekstu $\Gamma$ izraz $M$ tip $A$, če veljajo enačbe $\eqs$, predstavljene s seznamom oblike $A_1 = B_1, \dots, A_n = B_n$. Relacijo podamo s pravili, ki so podobna pravilom za izpeljavo tipov, le da je tip podizrazov v predpostavkah poljuben, enakosti med njimi pa zapišemo v $\eqs$. Na primer, pravilo za določanje tipa aplikacije je:

$$
\infer{
    \Gamma \vdash M_1 : A \to B \qquad
    \Gamma \vdash M_2 : A
}{
    \Gamma \vdash M_1 \, M_2 : B
}
$$

kjer vidimo, da je moral biti tip $M_1$ funkcijski z domeno enako tipu od $M_2$. Pravilo za izpeljavo tipa pa je

$$
\infer{
    \Gamma \vdash M_1 : A_1 \mid \eqs_1 \qquad
    \Gamma \vdash M_2 : A_2 \mid \eqs_2
}{
    \Gamma \vdash M_1 \, M_2 : \alpha \mid A_1 = \alpha \to A_2, \eqs_1, \eqs_2
}
$$

kjer za podizraza predpostavimo poljubna tipa $A_1$ in $A_2$ ne zahtevamo ničesar, ujemanje med njima pa zapišemo v enačbo $A_1 = \alpha \to A_2$ v zaključku pravila. Poleg te enačbe bo morala vsaka rešitev zadostiti vsem enačbam $\eqs_1$ in $\eqs_2$, ki jih ravno tako dodamo v zaključek.

Relacijo $\Gamma \vdash M : A \mid \eqs$ tako podamo s pravili:

$$
\infer{
    (x : A) ∈ \Gamma
}{
    \Gamma \vdash x : A \mid \emptyset
} \qquad

\infer{}{
    \Gamma \vdash \true : \boolty \mid \emptyset
} \qquad

\infer{}{
    \Gamma \vdash \false : \boolty \mid \emptyset
} \\[2em]

\infer{
    \Gamma \vdash M : A \mid \eqs \qquad
    \Gamma \vdash M_1 : A_1 \mid \eqs_1 \qquad
    \Gamma \vdash M_2 : A_2 \mid \eqs_2
}{
    \Gamma \vdash \ifthenelse{M}{M_1}{M_2} : A_1 \mid A = \boolty, A_1 = A_2, \eqs, \eqs_1, \eqs_2
} \\[2em]

\infer{}{
    \Gamma \vdash \intsym{n} : \intty\mid \emptyset
} \qquad

\infer{
    \Gamma \vdash M_1 : A_1 \mid \eqs_1 \qquad
    \Gamma \vdash M_2 : A_2 \mid \eqs_2
}{
    \Gamma \vdash M_1 + M_2 : \intty \mid A_1 = \intty, A_2 = \intty, \eqs_1, \eqs_2
} \\[2em]

\infer{
    \Gamma \vdash M_1 : A_1 \mid \eqs_1 \qquad
    \Gamma \vdash M_2 : A_2 \mid \eqs_2
}{
    \Gamma \vdash M_1 * M_2 : \intty \mid A_1 = \intty, A_2 = \intty, \eqs_1, \eqs_2
} \\[2em]

\infer{
    \Gamma \vdash M_1 : A_1 \mid \eqs_1 \qquad
    \Gamma \vdash M_2 : A_2 \mid \eqs_2
}{
    \Gamma \vdash M_1 < M_2 : \boolty \mid A_1 = \intty, A_2 = \intty, \eqs_1, \eqs_2

} \\[2em]

\infer{
    \Gamma, x : \alpha \vdash M : A \mid \eqs
}{
    \Gamma \vdash \lambda x. M : \alpha \to A \mid \eqs
} \qquad

\infer{
    \Gamma \vdash M_1 : A_1 \mid \eqs_1 \qquad
    \Gamma \vdash M_2 : A_2 \mid \eqs_2
}{
    \Gamma \vdash M_1 \, M_2 : \alpha \mid A_1 = \alpha \to A_2, \eqs_1, \eqs_2
}
$$

Pri tem upoštevamo, da morajo biti vsi parametri $\alpha$ sveži, torej da jih poprej še nismo uporabili. Drevo izpeljave za izraz $\lambda f. \lambda x. f \, (f \, x)$ od prej je natančneje torej:

$$
  \infer{
    \infer{
      \infer{
        \infer{ }{\Gamma \vdash f : \alpha \mid \emptyset} \qquad
        \infer{
          \infer{}{\Gamma \vdash f : \alpha \mid \emptyset} \qquad
          \infer{}{\Gamma \vdash x : \beta \mid \emptyset}
        }{\Gamma \vdash f \, x : \gamma \mid \alpha = \beta \to \gamma}
      }{
        \underbrace{f : \alpha, x : \beta}_\Gamma \vdash f \, (f \, x) : \delta \mid \alpha = \gamma \to \delta, \alpha = \beta \to \gamma
      }
    }{
      f : \alpha \vdash \lambda x. f \, (f \, x) : \beta \to \delta \mid \alpha = \gamma \to \delta, \alpha = \beta \to \gamma
    }
  }{
    \vdash \lambda f. \lambda x. f \, (f \, x) : \alpha \to (\beta \to \delta) \mid \alpha = \gamma \to \delta, \alpha = \beta \to \gamma
  }
$$

Ker pa je v pravilih množico enačb $\eqs$ le povečujemo, jo v praksi samo napišemo na stran in tako izpeljavo bolj ekonomično napišemo kot:

$$
  \infer{
    \infer{
      \infer{
        \infer{ }{\Gamma \vdash f : \alpha} \qquad
        \infer{
          \infer{}{\Gamma \vdash f : \alpha} \qquad
          \infer{}{\Gamma \vdash x : \beta}
        }{\Gamma \vdash f \, x : \gamma}
      }{
        \underbrace{f : \alpha, x : \beta}_\Gamma \vdash f \, (f \, x) : \delta
      }
    }{
      f : \alpha \vdash \lambda x. f \, (f \, x) : \beta \to \delta
    }
  }{
    \vdash \lambda f. \lambda x. f \, (f \, x) : \alpha \to (\beta \to \delta)
  }
  \qquad
  \begin{aligned}
   \alpha &= \gamma \to \delta \\
   \alpha &= \beta \to \gamma
  \end{aligned}
$$

### Reševanje sistema enačb

Rešitev sistema enačb bomo predstavili s _substitucijo_ $\sigma = \alpha_1 \mapsto A_1, \ldots, \alpha_n \mapsto A_n$, ki pove, da je vrednost parametra $\alpha_i$ enaka $A_i$. Dano substitucijo $\sigma$ lahko uporabimo na poljubnem tipu $A$, da dobimo tip $\sigma(A)$, ki je rekurzivno definiran kot:

$$
  \begin{align*}
    \sigma(\alpha) &= \begin{cases}
      A & (\alpha \mapsto A) \in \sigma \\
      \alpha & \text{sicer}
    \end{cases} \\
    \sigma(\boolty) &= \boolty \\
    \sigma(\intty) &= \intty \\
    \sigma(A \to B) &= \sigma(A) \to \sigma(B)  
  \end{align*}
$$

Hitro lahko preverimo, da velja $(\alpha \mapsto A)(B) = B[A / \alpha]$.

Pravimo, da substitucija $\sigma$ reši množico enačb $\eqs$, kar pišemo kot $\sigma \models \eqs$, kadar sta za vsako enačbo $(A = B) \in \eqs$ tipa $\sigma(A)$ in $\sigma(B)$ enaka.

Najbolj splošno rešitev množice enačb $\eqs$ dobimo tako, da postopoma razrešujemo enačbe v njej. Če sta tipa v prvi enačbi že enaka, se je lahko znebimo. Če sta oba funkcijska, enačbo razbijemo na dve enačbi med domenama in kodomenama. Zadnji primer, kjer rešitev obstaja, pa je ta, da na eni strani nastopa parameter in je oblike $\alpha = A$ (ali obratno). Načeloma to pomeni, da bomo $\alpha$ substituirali z $A$, težavo imamo le, kadar $A$ vsebuje $\alpha$. V tem primeru enačba ne more imeti rešitve. Formalno reševanje podamo z relacijo $\eqs \searrow \sigma$, podano s pravili:

$$
\infer{}{\emptyset \searrow \emptyset} \qquad
\infer{
  \eqs \searrow \sigma
}{
  A = A, \eqs \searrow \sigma
} \\[2em]
\infer{
  A_1 = B_1, A_2 = B_2, \eqs \searrow \sigma
}{
  A_1 \to A_2 = B_1 \to B_2, \eqs \searrow \sigma
} \\[2em]
\infer{
  (\alpha \mapsto A)(\eqs) \searrow \sigma \qquad
  \alpha \notin fv(A)
}{
  \alpha = A, \eqs \searrow (\alpha \mapsto A) \circ \sigma
} \\[2em]
\infer{
  (\alpha \mapsto A)(\eqs) \searrow \sigma \qquad
  \alpha \notin fv(A)
}{
  A = \alpha, \eqs \searrow (\alpha \mapsto A) \circ \sigma
} \\
$$
kjer je množica prostih parametrov $fv(A)$ definirana kot
$$
fv(\alpha) = \{ \alpha \} \qquad fv(\boolty) = fv(\intty) = \emptyset \qquad fv(A \to B) = fv(A) \cup fv(B)
$$
kompozicija $(\alpha \mapsto A) \circ \sigma$ pa parameter $\alpha$ slika v $A$, ostale parametre $\beta$ pa v $(\alpha \mapsto A)(\sigma(\beta))$.
