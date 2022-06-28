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

Hindley-Milnerjev algoritem poteka v dveh fazah. Najprej se rekurzivno sprehodi čez izraz in določi tipe tam, kjer je to očitno (na primer $\intsym{42}$ bo tipa $\intty$), na preostalih mestih pa pusti neznanke, ki morajo zadoščati določenim enačbam. Na primer, vemo, da bo aplikacija $M \, N$ nekega tipa $B$, če bo $M$ tipa $A \to B$ in bo $N$ tipa $A$. V ta namen sintakso tipov razširimo z neznankami $\alpha$, ki jih zaradi razločevanja od že znanih spremenljivk $x$ imenujemo _parametri_. V drugi fazi dobljene enačbe reši.

$$
\begin{align*}
A, B &::= \alpha
        \mid \boolty
        \mid \intty
        \mid A \to B
\end{align*}
$$

## Primer

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

## Nastavljanje enačb

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
    \Gamma \vdash M_1 \, M_2 : \alpha \mid A_1 = A_2 \to \alpha, \eqs_1, \eqs_2
}
$$

kjer za podizraza predpostavimo poljubna tipa $A_1$ in $A_2$ ne zahtevamo ničesar, ujemanje med njima pa zapišemo v enačbo $A_1 = A_2 \to \alpha$ v zaključku pravila. Poleg te enačbe bo morala vsaka rešitev zadostiti vsem enačbam $\eqs_1$ in $\eqs_2$, ki jih ravno tako dodamo v zaključek.

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
    \Gamma \vdash M_1 \, M_2 : \alpha \mid A_1 = A_2 \to \alpha, \eqs_1, \eqs_2
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

Ker pa v pravilih množico enačb $\eqs$ le povečujemo, jo v praksi samo napišemo na stran in tako izpeljavo bolj ekonomično napišemo kot:

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

## Reševanje enačb

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

Definiramo lahko tudi kompozitum $\sigma \circ \sigma'$, ki vse parametre $\alpha$, za katere obstaja $(\alpha \mapsto A) \in \sigma'$, slika v $\sigma(A)$, vse preostale parametre pa tako kot $\sigma$. Hitro lahko preverimo, da velja $(\alpha \mapsto A)(B) = B[A / \alpha]$ ter $(\sigma \circ \sigma')(A) = \sigma(\sigma'(A))$.

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
  \alpha = A, \eqs \searrow (\alpha \mapsto \sigma(A)) \circ \sigma
} \\[2em]
\infer{
  (\alpha \mapsto A)(\eqs) \searrow \sigma \qquad
  \alpha \notin fv(A)
}{
  A = \alpha, \eqs \searrow (\alpha \mapsto \sigma(A)) \circ \sigma
} \\
$$
kjer je množica prostih parametrov $fv(A)$ definirana kot
$$
fv(\alpha) = \{ \alpha \} \qquad fv(\boolty) = fv(\intty) = \emptyset \qquad fv(A \to B) = fv(A) \cup fv(B)
$$

## Lastnosti Hindley-Milnerjevega algoritma

Da Hindley-Milnerjev algoritem res vrne najbolj splošen tip, pokažemo s sledečimi trditvami.

### Pravilnost nastavljanja enačb

Najprej pokažemo, da vsaka rešitev dobljenih enačb res predstavlja veljavno prirejanje tipa.

**Trditev.** Če velja $\Gamma \vdash M : A \mid \eqs$ in $\sigma \models \eqs$, potem $\sigma(\Gamma) \vdash M : \sigma(A)$.

**Dokaz.** Z indukcijo na $\Gamma \vdash M : A \mid \eqs$. Za primer si oglejmo pravilo za aplikacijo

$$\infer{
    \Gamma \vdash M_1 : A_1 \mid \eqs_1 \qquad
    \Gamma \vdash M_2 : A_2 \mid \eqs_2
}{
    \Gamma \vdash M_1 \, M_2 : \alpha \mid A_1 = A_2 \to \alpha, \eqs_1, \eqs_2
}$$

Če velja $\sigma \models A_1 = A_2 \to \alpha, \eqs_1, \eqs_2$, potem velja tudi $\sigma \models \eqs_1$ in $\sigma \models \eqs_2$, saj gre za manjšo množico enačb. Po indukcijski predpostavki potem velja $\sigma(\Gamma) \vdash M_1 : \sigma(A_1)$ in $\sigma(\Gamma) \vdash M_2 : \sigma(A_2)$. Hkrati velja $\sigma(A_1) = \sigma(A_2) \to \sigma(\alpha)$, zato velja $\sigma(\Gamma) \vdash M_1 : \sigma(\alpha) \to \sigma(A_2)$, zato lahko uporabimo pravilo za določanje tipa aplikacije, da dobimo

$$\infer{
    \sigma(\Gamma) \vdash M_1 : \sigma(A_2) \to \sigma(\alpha) \qquad
    \sigma(\Gamma) \vdash M_2 : \sigma(A_2)
}{
    \sigma(\Gamma) \vdash M_1 \, M_2 : \sigma(\alpha)
}$$

kar je natanko to, kar smo želeli. ■

### Pravilnost reševanja enačb

Prav tako moramo pokazati pravilnost druge faze.

**Trditev.** Če velja $\eqs \searrow \sigma$, potem velja $\sigma \models \eqs$.

**Dokaz.** Tudi tu uporabimo indukcijo na $\eqs \searrow \sigma$. Za primer vzemimo pravilo

$$
\infer{
  (\alpha \mapsto A)(\eqs) \searrow \sigma \qquad
  \alpha \notin fv(A)
}{
  \alpha = A, \eqs \searrow (\alpha \mapsto \sigma(A)) \circ \sigma
}
$$

Najprej se spomnimo, da velja $(\alpha \mapsto A)(\eqs) = \eqs[A / \alpha]$. Ker v množici enačb $\eqs[A / \alpha]$ parameter $\alpha$ ne nastopa, lahko pokažemo, da tudi v $\sigma$ ni omenjen, zato velja $\sigma(\alpha) = \alpha$.

Tako v prvi enačbi na levi strani dobimo
$$
  ((\alpha \mapsto \sigma(A)) \circ \sigma)(\alpha) = (\alpha \mapsto \sigma(A))(\sigma(\alpha)) = (\alpha \mapsto \sigma(A))(\alpha) = \sigma(A)
$$
na desni strani pa
$$
  ((\alpha \mapsto \sigma(A)) \circ \sigma)(A) = (\alpha \mapsto \sigma(A))(\sigma(A)) = \sigma(A)
$$
saj $\alpha \notin fv(\sigma(A))$, ker $\alpha \notin fv(A)$, v substituciji $\alpha$ pa se prav tako ne pojavi.

Dokazati moramo še $(\alpha \mapsto \sigma(A)) \circ \sigma \models \eqs$. Vzemimo torej poljubno enačbo $A_1 = A_2 \in \eqs$ in pokažimo, da velja $(\alpha \mapsto \sigma(A))(\sigma(A_1)) = (\alpha \mapsto \sigma(A))(\sigma(A_2))$ oziroma $\sigma(A_1)[\sigma(A) / \alpha] = \sigma(A_2)[\sigma(A) / \alpha]$. Po indukcijski predpostavki velja $\sigma \models (\alpha \mapsto A)(\eqs)$, iz česar sledi $\sigma(A_1[A / \alpha]) = \sigma(A_2[A / \alpha])$, kar je natanko to, kar moramo pokazati, saj velja $\sigma(A_1)[\sigma(A) / \alpha] = \sigma(A_1[A / \alpha])$ in podobno za $A_2$. ■

### Polnost nastavljanja enačb

Pokazati moramo še, da so nastavljene enačbe najbolj splošne, torej da lahko vsak tip, ki bi ga lahko priredili danemu izrazu, dobimo kot konkretno substitucijo izpeljanega tipa. Najprej pokažemo, da za vsak izraz s tipom lahko nastavimo sistem enačb:

**Trditev.** Naj bo $\Gamma \vdash M : A$ Tedaj obstaja $A'$ in $\eqs$, da velja $\Gamma \vdash M : A' \mid \eqs$.

**Dokaz.** Dokaz je rutinska indukcija, saj so pravila za $\Gamma \vdash M : A' \mid \eqs$ tako splošna, da jih lahko uporabimo na poljubnem izrazu. Paziti moramo le na to, da v katerem se vse proste spremenljivke pojavijo v $\Gamma$, kar pa nam zagotavlja predpostavka $\Gamma \vdash M : A$. ■

Ko vemo, da enačbe lahko vedno nastavimo, pa lahko povemo tudi, da so najbolj splošne:

**Trditev.** Naj bo $\Gamma \vdash M : A \mid \eqs$. Tedaj za poljuben tip $A'$, za katerega velja in $\Gamma \vdash M : A$, obstaja $\sigma \models \eqs$, da velja $\sigma(A) = A'$.

Dokaza ne bomo navajali, ker je bolj tehničen, najdete pa ga lahko v razdelku 22.5 učbenika [Types and programming languages](https://www.cis.upenn.edu/~bcpierce/tapl/).

### Polnost reševanja enačb

Tudi v drugi fazi moramo pokazati, da je najdena rešitev res najbolj splošna. Tudi tu najprej pokažemo, da rešitev vedno obstaja.

**Trditev.** Če velja $\sigma \models \eqs$, tedaj obstaja $\sigma'$, da velja $\eqs \models \sigma'$.

**Dokaz.** Dokaz poteka z malo bolj zapleteno indukcijo, saj ni očitno, katera stvar se v predpostavkah manjša. Na primer, včasih se enačb znebimo, včasih zamenjamo eno enačbo z več novimi, vendar z manjšimi tipi, včasih pa tipe povečamo, vendar se znebimo parametrov. Vse to zajamemo z leksikografsko urejenostjo med sistemi enačb.

Definirajmo velikost $|A|$ tipa $A$ kot
$$
  |\alpha| = |\boolty| = |\intty| = 1 \qquad
  |A \to B| = 1 + |A| + |B|
$$
Velikost in proste parametre enačb $\eqs$ pa lahko definiramo kot
$$
\begin{align*}
  |\eqs| &= \sum_{(A_1 = A_2) \in \eqs} (|A_1| + |A_2|) \\
  fv(\eqs) &= \bigcup_{(A_1 = A_2) \in \eqs} fv(A_1) \cup fv(A_2)
\end{align*}
$$

Vzemimo zdaj enačbe $\eqs$ in nadaljujmo z leksikogafsko indukcijo na $(|fv(\eqs)|, |\eqs|)$. Torej, na vsakem koraku se bo število prostih parametrov zmanjšalo, ali pa bo ostalo enako, zmanjšala pa se bo skupna velikost vseh tipov v enačbah.

Če je $\eqs = \emptyset$, velja $\eqs \searrow \emptyset$. V nasprotnem primeru imamo $\eqs = (A_1 = A_2, \eqs')$, kjer velja $\sigma \models \eqs'$ in $\sigma(A_1) = \sigma(A_2)$. Poglejmo, v kakšnih primerih se slednje lahko zgodi.

Če je $A_1 = A_2$, lahko uporabimo pravilo 
$$
\infer{
  \eqs' \searrow \sigma'
}{
  A_1 = A_1, \eqs' \searrow \sigma'
}$$
saj po indukciji obstaja $\eqs' \searrow \sigma'$.

Če pa je $A_1 \ne A_2$, hkrati pa $\sigma(A_1) = \sigma(A_2)$, pa morata biti $A_1$ in $A_2$ vsaj kompatibilne oblike. Torej sta oba funkcijska tipa, ali pa je eden od njiju parameter.

V prvem primeru je $A_1 = A_1' \to A_1''$ in $A_2 = A_2' \to A_2''$, zato definiramo $\eqs'' = (A_1' = A_2', A_1'' = A_2'', \eqs')$. Ker velja $fv(\eqs'') = fv(\eqs)$ in $|\eqs''| < |\eqs|$, hkrati pa velja $\sigma \models \eqs''$, po indukciji obstaja rešitev $\eqs'' \searrow \sigma'$, zato lahko uporabimo pravilo

$$\infer{
  \underbrace{A_1 = B_1, A_2 = B_2, \eqs'}_{\eqs''} \searrow \sigma'
}{
  \underbrace{A_1 \to A_2 = B_1 \to B_2, \eqs'}_{\eqs} \searrow \sigma'
}
$$

V drugem primeru pa najprej poglejmo možnost, da je parameter na levi strani, torej $A_1 = \alpha$. Naj bo $\eqs'' = \eqs[A_2 / \alpha]$. Tedaj je $|fv(\eqs'')| < |fv(\eqs)|$, saj smo se $\alpha$ znebili, novih parametrov pa nismo dodajali, saj je $fv(A_2) \subseteq fv(\eqs)$. Če velja $\sigma \models \eqs$, potem velja tudi $\sigma \models \eqs[A_2 / \alpha]$, saj smo s substitucijo kvečjemu izenačili več tipov. Torej po indukciji obstaja rešitev $\eqs[A_2 / \alpha] \searrow \sigma'$. Hkrati mora veljati $\alpha \notin fv(A_2)$, saj sicer ne more veljati $\sigma(\alpha) = \sigma(A_2)$, ker bo desna stran striktno večja od leve. Tako so izpolnjene vse predpostavke, da uporabimo drugo pravilo:


$$
\infer{
  (\alpha \mapsto A_2)(\eqs) \searrow \sigma' \qquad
  \alpha \notin fv(A_2)
}{
  \alpha = A_2, \eqs \searrow (\alpha \mapsto \sigma(A_2)) \circ \sigma
}
$$

Če je parameter na desni strani, ravnamo simetrično. ■

Ko vemo, da rešitev obstaja, pokažemo še, da je najbolj splošna.

**Trditev.** Če velja $\eqs \searrow \sigma$, tedaj za poljubno $\sigma' \models \eqs$ obstaja $\sigma''$, da velja $\sigma' = \sigma'' \circ \sigma$.

**Dokaz.** Dokaz poteka z indukcijo na $\eqs \searrow \sigma$. Za primer vzemimo pravilo 
$$
\infer{
  A_1 = B_1, A_2 = B_2, \eqs \searrow \sigma
}{
  A_1 \to A_2 = B_1 \to B_2, \eqs \searrow \sigma
}.$$
Naj velja $\sigma' \models A_1 \to A_2 = B_1 \to B_2, \eqs$. Tedaj velja tudi $\sigma' \models A_1 = B_1, A_2 = B_2, \eqs$ zato po indukcijski predpostavki obstaja $\sigma''$, da velja $\sigma' = \sigma'' \circ \sigma$, kar je natanko to, kar smo želeli pokazati.
