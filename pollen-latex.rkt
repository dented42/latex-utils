#lang racket

(provide (contract-out
          [process/math
           (->* (#:math-mode (or/c 'latex-inline 'latex-display
                                   'tex-inline 'tex-display))
                (#:unicode-pairings (listof
                                     (cons/c string? string?)))
                #:rest any/c
                string?)])
         tmath dmath
         group
         rm it bf sf cal scr bb frak
         mrm mit mbf msf mcal mscr mbb mfrak
         operator
         delims parens bracks braces chevs verts vverts
         (contract-out
          [latex-matrix (-> (or/c '() 'p 'b 'B 'v 'V 'small)
                            any/c
                            any)]))


(define (tmath . math)
  (process/math math
                #:math-mode 'latex-inline))

(define (dmath . math)
  (process/math math
                #:math-mode 'latex-display))


(define (process/math
         #:math-mode math-mode
         #:unicode-pairings [unicode-pairings racket-unicode-latex-pairings]
         . math)
  (let ([left-math-delim (case math-mode
                           [(latex-inline) "\\("]
                           [(latex-display) "\\["]
                           [(tex-inline) "$"]
                           [(tex-display) "$$"])]
        [right-math-delim (case math-mode
                            [(latex-inline) "\\)"]
                            [(latex-display) "\\]"]
                            [(tex-inline) "$"]
                            [(tex-display) "$$"])])
    (unicode->latex unicode-pairings
                    (string-append left-math-delim
                                   (tree->tex-block math)
                                   right-math-delim))))


; internal helper function definitions

(define (tree->tex-block content)
  (define (stringify things)
    (cond [(list? things) (map stringify things)]
          [(string? things) things]
          [(number? things) (number->string things)]
          [(symbol? things) (symbol->string things)]
          [(char? things) (string things)]))
  (apply string-append (flatten (stringify content))))


(define (unicode->latex pairings math-string)
  (define (asciify string pairings-list)
    (if (null? pairings-list)
        string
        (string-replace (asciify string (cdr pairings-list)) (caar pairings-list) (cdar pairings-list))))
  (asciify math-string pairings))


(define (group . content)
  `("{" ,content "}"))


; math operator definitions

(define (operator . oper)
  `(\\operatorname (group oper)))

#;(define (binary-operator . oper)
    `(\\mathbin (group oper)))

#;(define (binary-relation . oper)
    `(\\mathrel (group oper)))

#;(define (open-delim . delim)
    `(\\mathopen (group delim)))

#;(define (close-delim . delim)
    `(\\mathclose (group delim)))


; maths fonts definitions

(define (rm . expr)
  (group '\\mathrm (group expr)))

(define (it . expr)
  (group '\\mathit (group expr)))

(define (bf . expr)
  (group '\\mathbf (group expr)))

(define (sf . expr)
  (group '\\mathsf (group expr)))

(define (cal . expr)
  (group '\\mathcal (group expr)))

(define (scr . expr)
  (group '\\mathscr (group expr)))

(define (bb . expr)
  (group '\\mathbb (group expr)))

(define (frak . expr)
  (group '\\mathfrak (group expr)))

; inline maths fonts shortcuts

(define (mrm . expr)
  (tmath (rm expr)))

(define (mit . expr)
  (tmath (it expr)))

(define (mbf . expr)
  (tmath (bf expr)))

(define (msf . expr)
  (tmath (it expr)))

(define (mcal . expr)
  (tmath (cal expr)))

(define (mscr . expr)
  (tmath (scr expr)))

(define (mbb . expr)
  (tmath (bb expr)))

(define (mfrak . expr)
  (tmath (frak expr)))


; maths delimeter definitions

(define (delims left right #:scale [scale 'auto] . innards)
  (let ([left-scale (case scale
                      [(big) " \\bigl"]
                      [(Big) " \\Bigl"]
                      [(bigg) " \\biggl"]
                      [(Bigg) " \\Biggl"]
                      [(auto) " \\left"]
                      [else " "])]
        [right-scale (case scale
                       [(big) " \\bigr"]
                       [(Big) " \\Bigr"]
                       [(bigg) " \\biggr"]
                       [(Bigg) " \\Biggr"]
                       [(auto) " \\right"]
                       [else " "])])
    (group left-scale left " " innards " " right-scale right)))


(define (parens #:scale [scale 'auto] . innards)
  (delims "(" ")" #:scale scale innards))

(define (bracks #:scale [scale 'auto] . innards)
  (delims "\\lbrack" "\\rbrack" #:scale scale innards))

(define (braces #:scale [scale 'auto] . innards)
  (delims "\\{" "\\}" #:scale scale innards))

(define (chevs #:scale [scale 'auto] . innards)
  (delims "\\langle" "\\rangle" #:scale scale innards))

(define (verts #:scale [scale 'auto] . innards)
  (delims "\\lvert" "\\rvert" #:scale scale innards))

(define (vverts #:scale [scale 'auto] . innards)
  (delims "\\lVert" "\\rVert" #:scale scale innards))



; environment definitions

(define (latex-env type . innards)
  `(\\begin ,(group type) ,innards \\end ,(group type)))

(define (latex-matrix type . innards)
  (latex-env `(,type matrix) innards))


(define racket-unicode-latex-pairings
  '(("⇓" . "\\Downarrow ")
    ("↖" . "\\nwarrow ")
    ("↓" . "\\downarrow ")
    ("⇒" . "\\Rightarrow" )
    ("→" . "\\rightarrow ")
    ("↦" . "\\mapsto ")
    ("↘" . "\\searrow ")
    ("↙" . "\\swarrow ")
    ("←" . "\\leftarrow ")
    ("↑" . "\\uparrow ")
    ("⇐" . "\\Leftarrow" )
    ("−" . "\\longrightarrow ")
    ("⇑" . "\\Uparrow")
    ("⇔" . "\\Leftrightarrow ")
    ("↕" . "\\updownarrow ")
    ("↔" . "\\leftrightarrow ")
    ("↗" . "\\nearrow ")
    ("⇕" . "\\Updownarrow ")
    ("א" . "\\aleph ")
    ("′" . "\\prime ")
    ("∅" . "\\emptyset ")
    ("∇" . "\\nabla ")
    ("♦" . "\\diamondsuit ")
    ("♠" . "\\spadesuit ")
    ("♣" . "\\clubsuit ")
    ("♥" . "\\heartsuit ")
    ("♯" . "\\sharp ")
    ("♭" . "\\flat ")
    ("♮" . "\\natural ")
    ("√" . "\\sqrt ")
    ("¬" . "\\neg ")
    ("△" . "\\triangle ")
    ("∀" . "\\forall ")
    ("∃" . "\\exists ")
    ("∞" . "\\infty ")
    ("∘" . "\\circ ")
    ("α" . "\\alpha ")
    ("θ" . "\\theta ")
    ("τ" . "\\tau ")
    ("β" . "\\beta ")
    ("ϑ" . "\\vartheta ")
    ("π" . "\\pi ")
    ("υ" . "\\upsilon ")
    ("γ" . "\\gamma ")
    ("ϖ" . "\\varpi ")
    ("ϕ" . "\\phi ")
    ("δ" . "\\delta ")
    ("κ" . "\\kappa ")
    ("ρ" . "\\rho ")
    ("φ" . "\\varphi ")
    ("ϵ" . "\\epsilon ")
    ("λ" . "\\lambda ")
    ("ϱ" . "\\varrho ")
    ("χ" . "\\chi ")
    ("ε" . "\\varepsilon ")
    ("μ" . "\\mu ")
    ("σ" . "\\sigma ")
    ("ψ" . "\\psi ")
    ("ζ" . "\\zeta ")
    ("ν" . "\\nu ")
    ("ς" . "\\varsigma ")
    ("ω" . "\\omega ")
    ("η" . "\\eta ")
    ("ξ" . "\\xi ")
    ("ι" . "\\iota ")
    ("Γ" . "\\Gamma ")
    ("Λ" . "\\Lambda ")
    ("Σ" . "\\Sigma ")
    ("Ψ" . "\\Psi ")
    ("Δ" . "\\Delta ")
    ("Ξ" . "\\Xi ")
    ("Υ" . "\\Upsilon ")
    ("Ω" . "\\Omega ")
    ("Θ" . "\\Theta ")
    ("Π" . "\\Pi ")
    ("Φ" . "\\Phi ")
    ("±" . "\\pm ")
    ("∩" . "\\cap ")
    ("◇" . "\\diamond ")
    ("⊕" . "\\oplus ")
    ("∓" . "\\mp ")
    ("∪" . "\\cup ")
    ("△" . "\\bigtriangleup ")
    ("⊖" . "\\ominus ")
    ("×" . "\\times ")
    ("⊎". "\\uplus ")
    ("▽" . "\\bigtriangledown ")
    ("⊗" . "\\otimes ")
    ("÷" . "\\div ")
    ("⊓" . "\\sqcap ")
    ("▹" . "\\triangleright ")
    ("⊘" . "\\oslash ")
    ("∗" . "\\ast ")
    ("⊔" . "\\sqcup ")
    ("∨" . "\\vee ")
    ("∧" . "\\wedge ")
    ("◃" . "\\triangleleft ")
    ("⊙" . "\\odot ")
    ("★" . "\\star ")
    ("†" . "\\dagger ")
    ("•" . "\\bullet ")
    ("‡" . "\\ddagger ")
    ("≀" . "\\wr ")
    ("⨿" . "\\amalg ")
    ("≤" . "\\leq ")
    ("≥" . "\\geq ")
    ("≡" . "\\equiv ")
    ("⊨" . "\\models ")
    ("≺" . "\\prec ")
    ("≻" . "\\succ ")
    ("⋖" . "\\precdot ")
    ("⋗" . "\\succdot ")
    ("∼" . "\\sim ")
    ("⊥" . "\\perp ")
    ("⊥" . "\\bot ")
    ("⊤" . "\\top ")
    ("≼" . "\\preceq ")
    ("≽" . "\\succeq ")
    ("≃" . "\\simeq ")
    ("≪" . "\\ll ")
    ("≫" . "\\gg ")
    ("≍" . "\\asymp ")
    ("∥" . "\\parallel ")
    ("⊂" . "\\subset ")
    ("⊃" . "\\supset ")
    ("≈" . "\\approx ")
    ("⋈" . "\\bowtie ")
    ("⊆" . "\\subseteq ")
    ("⊇" . "\\supseteq ")
    ("≌" . "\\cong ")
    ("⊏" . "\\sqsubsetb ")
    ("⊐" . "\\sqsupsetb ")
    ("≠" . "\\neq ")
    ("⌣" . "\\smile ")
    ("⊑" . "\\sqsubseteq ")
    ("⊒" . "\\sqsupseteq ")
    ("≐" . "\\doteq ")
    ("⌢" . "\\frown ")
    ("∈" . "\\in ")
    ("∋" . "\\ni ")
    ("∉" . "\notin ")
    ("∝" . "\\propto ")
    ("⊢" . "\\vdash ")
    ("⊣" . "\\dashv ")
    ("·" . "\\cdot ")
    ("∑" . "\\sum ")
    ("∏" . "\\prod ")
    ("∐" . "\\coprod ")
    ("∫" . "\\int ")
    ("∮" . "\\oint ")
    ("☠" . "\\skull ")
    ("☺" . "\\smiley ")
    ("☻" . "\\blacksmiley ")
    ("☹" . "\\frownie ")
    ("§" . "\\S ")
    ("ł" . "\\l ")
    ("⋮" . "\\vdots ")
    ("⋱" . "\\ddots ")
    ("⋯" . "\\cdots ")
    ("⋯" . "\\ldots ")
    ("⟨" . "\\langle ")
    ("⟩" . "\\rangle ")))



(define math-italic-roman-unicode-pairings
  '(; uppercase italic roman
    ("" . "{\\mathit{A}}")
    ("" . "{\\mathit{B}}")
    ("" . "{\\mathit{C}}")
    ("" . "{\\mathit{D}}")
    ("" . "{\\mathit{E}}")
    ("" . "{\\mathit{F}}")
    ("" . "{\\mathit{G}}")
    ("" . "{\\mathit{H}}")
    ("" . "{\\mathit{I}}")
    ("" . "{\\mathit{J}}")
    ("" . "{\\mathit{K}}")
    ("" . "{\\mathit{L}}")
    ("" . "{\\mathit{M}}")
    ("" . "{\\mathit{N}}")
    ("" . "{\\mathit{O}}")
    ("" . "{\\mathit{P}}")
    ("" . "{\\mathit{Q}}")
    ("" . "{\\mathit{R}}")
    ("" . "{\\mathit{S}}")
    ("" . "{\\mathit{T}}")
    ("" . "{\\mathit{U}}")
    ("" . "{\\mathit{V}}")
    ("" . "{\\mathit{W}}")
    ("" . "{\\mathit{X}}")
    ("" . "{\\mathit{Y}}")
    ("" . "{\\mathit{Z}}")
    ; lowercase italic roman
    ("" . "{\\mathit{a}}")
    ("" . "{\\mathit{b}}")
    ("" . "{\\mathit{c}}")
    ("" . "{\\mathit{d}}")
    ("" . "{\\mathit{e}}")
    ("" . "{\\mathit{f}}")
    ("" . "{\\mathit{g}}")
    ("" . "{\\mathit{h}}")
    ("" . "{\\mathit{i}}")
    ("" . "{\\mathit{j}}")
    ("" . "{\\mathit{k}}")
    ("" . "{\\mathit{l}}")
    ("" . "{\\mathit{m}}")
    ("" . "{\\mathit{n}}")
    ("" . "{\\mathit{o}}")
    ("" . "{\\mathit{p}}")
    ("" . "{\\mathit{q}}")
    ("" . "{\\mathit{r}}")
    ("" . "{\\mathit{s}}")
    ("" . "{\\mathit{t}}")
    ("" . "{\\mathit{u}}")
    ("" . "{\\mathit{v}}")
    ("" . "{\\mathit{w}}")
    ("" . "{\\mathit{x}}")
    ("" . "{\\mathit{y}}")
    ("" . "{\\mathit{z}}")
    ; italic numerals
    ("" . "{\\mathit{0}}")
    ("" . "{\\mathit{1}}")
    ("" . "{\\mathit{2}}")
    ("" . "{\\mathit{3}}")
    ("" . "{\\mathit{4}}")
    ("" . "{\\mathit{5}}")
    ("" . "{\\mathit{6}}")
    ("" . "{\\mathit{7}}")
    ("" . "{\\mathit{8}}")
    ("" . "{\\mathit{9}}")))


(define math-calligraphic-unicode-pairings
  '(; uppercase calligraphic
    ("𝒜" . "{\\mathcal{A}}")
    ("ℬ" . "{\\mathcal{B}}")
    ("𝒞" . "{\\mathcal{C}}")
    ("𝒟" . "{\\mathcal{D}}")
    ("ℰ" . "{\\mathcal{E}}")
    ("ℱ" . "{\\mathcal{F}}")
    ("𝒢" . "{\\mathcal{G}}")
    ("ℋ" . "{\\mathcal{H}}")
    ("ℐ" . "{\\mathcal{I}}")
    ("𝒥" . "{\\mathcal{J}}")
    ("𝒦" . "{\\mathcal{K}}")
    ("ℒ" . "{\\mathcal{L}}")
    ("ℳ" . "{\\mathcal{M}}")
    ("𝒩" . "{\\mathcal{N}}")
    ("𝒪" . "{\\mathcal{O}}")
    ("𝒫" . "{\\mathcal{P}}")
    ("𝒬" . "{\\mathcal{Q}}")
    ("ℛ" . "{\\mathcal{R}}")
    ("𝒮" . "{\\mathcal{S}}")
    ("𝒯" . "{\\mathcal{T}}")
    ("𝒰" . "{\\mathcal{U}}")
    ("𝒱" . "{\\mathcal{V}}")
    ("𝒲" . "{\\mathcal{W}}")
    ("𝒳" . "{\\mathcal{X}}")
    ("𝒴" . "{\\mathcal{Y}}")
    ("𝒵" . "{\\mathcal{Z}}")
    ; lowercase calligraphic
    ("𝒶" . "{\\mathcal{a}}")
    ("𝒷" . "{\\mathcal{b}}")
    ("𝒸" . "{\\mathcal{c}}")
    ("𝒹" . "{\\mathcal{d}}")
    ("ℯ" . "{\\mathcal{e}}")
    ("𝒻" . "{\\mathcal{f}}")
    ("ℊ" . "{\\mathcal{g}}")
    ("𝒽" . "{\\mathcal{h}}")
    ("𝒾" . "{\\mathcal{i}}")
    ("𝒿" . "{\\mathcal{j}}")
    ("𝓀" . "{\\mathcal{k}}")
    ("𝓁" . "{\\mathcal{l}}")
    ("𝓂" . "{\\mathcal{m}}")
    ("𝓃" . "{\\mathcal{n}}")
    ("ℴ" . "{\\mathcal{o}}")
    ("𝓅" . "{\\mathcal{p}}")
    ("𝓆" . "{\\mathcal{q}}")
    ("𝓇" . "{\\mathcal{r}}")
    ("𝓈" . "{\\mathcal{s}}")
    ("𝓉" . "{\\mathcal{t}}")
    ("𝓊" . "{\\mathcal{u}}")
    ("𝓋" . "{\\mathcal{v}}")
    ("𝓌" . "{\\mathcal{w}}")
    ("𝓍" . "{\\mathcal{x}}")
    ("𝓎" . "{\\mathcal{y}}")
    ("𝓏" . "{\\mathcal{z}}")))


(define math-fraktur-unicode-pairings
  '(; uppercase fraktur
    ("𝔄" . "{\\mathfrak{A}}")
    ("𝔅" . "{\\mathfrak{B}}")
    ("ℭ" . "{\\mathfrak{C}}")
    ("𝔇" . "{\\mathfrak{D}}")
    ("𝔈" . "{\\mathfrak{E}}")
    ("𝔉" . "{\\mathfrak{F}}")
    ("𝔊" . "{\\mathfrak{G}}")
    ("ℌ" . "{\\mathfrak{H}}")
    ("ℑ" . "{\\mathfrak{I}}") ; \Im vs \mathfrak{I} ?
    ("𝔍" . "{\\mathfrak{J}}")
    ("𝔎" . "{\\mathfrak{K}}")
    ("𝔏" . "{\\mathfrak{L}}")
    ("𝔐" . "{\\mathfrak{M}}")
    ("𝔑" . "{\\mathfrak{N}}")
    ("𝔒" . "{\\mathfrak{O}}")
    ("𝔓" . "{\\mathfrak{P}}")
    ("𝔔" . "{\\mathfrak{Q}}")
    ("ℜ" . "{\\mathfrak{R}}") ; \Re vs \mathfrack{R} ?
    ("𝔖" . "{\\mathfrak{S}}")
    ("𝔗" . "{\\mathfrak{T}}")
    ("𝔘" . "{\\mathfrak{U}}")
    ("𝔙" . "{\\mathfrak{V}}")
    ("𝔚" . "{\\mathfrak{W}}")
    ("𝔛" . "{\\mathfrak{X}}")
    ("𝔜" . "{\\mathfrak{Y}}")
    ("ℨ" . "{\\mathfrak{Z}}")
    ; lowercase fraktur
    ("𝔞" . "{\\mathfrak{a}}")
    ("𝔟" . "{\\mathfrak{b}}")
    ("𝔠" . "{\\mathfrak{c}}")
    ("𝔡" . "{\\mathfrak{d}}")
    ("𝔢" . "{\\mathfrak{e}}")
    ("𝔣" . "{\\mathfrak{f}}")
    ("𝔤" . "{\\mathfrak{g}}")
    ("𝔥" . "{\\mathfrak{h}}")
    ("𝔦" . "{\\mathfrak{i}}")
    ("𝔧" . "{\\mathfrak{j}}")
    ("𝔨" . "{\\mathfrak{k}}")
    ("𝔩" . "{\\mathfrak{l}}")
    ("𝔪" . "{\\mathfrak{m}}")
    ("𝔫" . "{\\mathfrak{n}}")
    ("𝔬" . "{\\mathfrak{o}}")
    ("𝔭" . "{\\mathfrak{p}}")
    ("𝔮" . "{\\mathfrak{q}}")
    ("𝔯" . "{\\mathfrak{r}}")
    ("𝔰" . "{\\mathfrak{s}}")
    ("𝔱" . "{\\mathfrak{t}}")
    ("𝔲" . "{\\mathfrak{u}}")
    ("𝔳" . "{\\mathfrak{v}}")
    ("𝔴" . "{\\mathfrak{w}}")
    ("𝔵" . "{\\mathfrak{x}}")
    ("𝔶" . "{\\mathfrak{y}}")
    ("𝔷" . "{\\mathfrak{z}}")))


(define math-blackboard-bold-unicode-pairings
  '(; uppercase blackboard bold
    ("𝔸" . "{\\mathbb{A}}")
    ("𝔹" . "{\\mathbb{B}}")
    ("ℂ" . "{\\mathbb{C}}")
    ("𝔻" . "{\\mathbb{D}}")
    ("𝔼" . "{\\mathbb{E}}")
    ("𝔽" . "{\\mathbb{F}}")
    ("𝔾" . "{\\mathbb{G}}")
    ("ℍ" . "{\\mathbb{H}}")
    ("𝕀" . "{\\mathbb{I}}")
    ("𝕁" . "{\\mathbb{J}}")
    ("𝕂" . "{\\mathbb{K}}")
    ("𝕃" . "{\\mathbb{L}}")
    ("𝕄" . "{\\mathbb{M}}")
    ("ℕ" . "{\\mathbb{N}}")
    ("𝕆" . "{\\mathbb{O}}")
    ("ℙ" . "{\\mathbb{P}}")
    ("ℚ" . "{\\mathbb{Q}}")
    ("ℝ" . "{\\mathbb{R}}")
    ("𝕊" . "{\\mathbb{S}}")
    ("𝕋" . "{\\mathbb{T}}")
    ("𝕌" . "{\\mathbb{U}}")
    ("𝕍" . "{\\mathbb{V}}")
    ("𝕎" . "{\\mathbb{W}}")
    ("𝕏" . "{\\mathbb{X}}")
    ("𝕐" . "{\\mathbb{Y}}")
    ("ℤ" . "{\\mathbb{Z}}")
    ; lowercase blackboard bold
    ("𝕒" . "{\\mathbb{a}}")
    ("𝕓" . "{\\mathbb{b}}")
    ("𝕔" . "{\\mathbb{c}}")
    ("𝕕" . "{\\mathbb{d}}")
    ("𝕖" . "{\\mathbb{e}}")
    ("𝕗" . "{\\mathbb{f}}")
    ("𝕘" . "{\\mathbb{g}}")
    ("𝕙" . "{\\mathbb{h}}")
    ("𝕚" . "{\\mathbb{i}}")
    ("𝕛" . "{\\mathbb{j}}")
    ("𝕜" . "{\\mathbb{k}}")
    ("𝕝" . "{\\mathbb{l}}")
    ("𝕞" . "{\\mathbb{m}}")
    ("𝕟" . "{\\mathbb{n}}")
    ("𝕠" . "{\\mathbb{o}}")
    ("𝕡" . "{\\mathbb{p}}")
    ("𝕢" . "{\\mathbb{q}}")
    ("𝕣" . "{\\mathbb{r}}")
    ("𝕤" . "{\\mathbb{s}}")
    ("𝕥" . "{\\mathbb{t}}")
    ("𝕦" . "{\\mathbb{u}}")
    ("𝕧" . "{\\mathbb{v}}")
    ("𝕨" . "{\\mathbb{w}}")
    ("𝕩" . "{\\mathbb{x}}")
    ("𝕪" . "{\\mathbb{y}}")
    ("𝕫" . "{\\mathbb{z}}")
    ; blackboard numerals
    ("𝟘" . "{\\mathbb{0}}")
    ("𝟙" . "{\\mathbb{1}}")
    ("𝟚" . "{\\mathbb{2}}")
    ("𝟛" . "{\\mathbb{3}}")
    ("𝟜" . "{\\mathbb{4}}")
    ("𝟝" . "{\\mathbb{5}}")
    ("𝟞" . "{\\mathbb{6}}")
    ("𝟟" . "{\\mathbb{7}}")
    ("𝟠" . "{\\mathbb{8}}")
    ("𝟡" . "{\\mathbb{9}}")))


(define math-sans-unicode-pairings
  '(; uppercase sans
    ("𝖠" . "{\\mathsf{A}}")
    ("𝖡" . "{\\mathsf{B}}")
    ("𝖢" . "{\\mathsf{C}}")
    ("𝖣" . "{\\mathsf{D}}")
    ("𝖤" . "{\\mathsf{E}}")
    ("𝖥" . "{\\mathsf{F}}")
    ("𝖦" . "{\\mathsf{G}}")
    ("𝖧" . "{\\mathsf{H}}")
    ("𝖨" . "{\\mathsf{I}}")
    ("𝖩" . "{\\mathsf{J}}")
    ("𝖪" . "{\\mathsf{K}}")
    ("𝖫" . "{\\mathsf{L}}")
    ("𝖬" . "{\\mathsf{M}}")
    ("𝖭" . "{\\mathsf{N}}")
    ("𝖮" . "{\\mathsf{O}}")
    ("𝖯" . "{\\mathsf{P}}")
    ("𝖰" . "{\\mathsf{Q}}")
    ("𝖱" . "{\\mathsf{R}}")
    ("𝖲" . "{\\mathsf{S}}")
    ("𝖳" . "{\\mathsf{T}}")
    ("𝖴" . "{\\mathsf{U}}")
    ("𝖵" . "{\\mathsf{V}}")
    ("𝖶" . "{\\mathsf{W}}")
    ("𝖷" . "{\\mathsf{X}}")
    ("𝖸" . "{\\mathsf{Y}}")
    ("𝖹" . "{\\mathsf{Z}}")
    ; lowercase sans
    ("𝖺" . "{\\mathsf{a}}")
    ("𝖻" . "{\\mathsf{b}}")
    ("𝖼" . "{\\mathsf{c}}")
    ("𝖽" . "{\\mathsf{d}}")
    ("𝖾" . "{\\mathsf{e}}")
    ("𝖿" . "{\\mathsf{f}}")
    ("𝗀" . "{\\mathsf{g}}")
    ("𝗁" . "{\\mathsf{h}}")
    ("𝗂" . "{\\mathsf{i}}")
    ("𝗃" . "{\\mathsf{j}}")
    ("𝗄" . "{\\mathsf{k}}")
    ("𝗅" . "{\\mathsf{l}}")
    ("𝗆" . "{\\mathsf{m}}")
    ("𝗇" . "{\\mathsf{n}}")
    ("𝗈" . "{\\mathsf{o}}")
    ("𝗉" . "{\\mathsf{p}}")
    ("𝗊" . "{\\mathsf{q}}")
    ("𝗋" . "{\\mathsf{r}}")
    ("𝗌" . "{\\mathsf{s}}")
    ("𝗍" . "{\\mathsf{t}}")
    ("𝗎" . "{\\mathsf{u}}")
    ("𝗏" . "{\\mathsf{v}}")
    ("𝗐" . "{\\mathsf{w}}")
    ("𝗑" . "{\\mathsf{x}}")
    ("𝗒" . "{\\mathsf{y}}")
    ("𝗓" . "{\\mathsf{z}}")
    ; sans numerals
    ("𝟢" . "{\\mathsf{0}}")
    ("𝟣" . "{\\mathsf{1}}")
    ("𝟤" . "{\\mathsf{2}}")
    ("𝟥" . "{\\mathsf{3}}")
    ("𝟦" . "{\\mathsf{4}}")
    ("𝟧" . "{\\mathsf{5}}")
    ("𝟨" . "{\\mathsf{6}}")
    ("𝟩" . "{\\mathsf{7}}")
    ("𝟪" . "{\\mathsf{8}}")
    ("𝟫" . "{\\mathsf{9}}")))


(define math-typewriter-unicode-pairings
  '(; uppercase typewriter
    ("𝙰" . "{\\mathtt{A}}")
    ("𝙱" . "{\\mathtt{B}}")
    ("𝙲" . "{\\mathtt{C}}")
    ("𝙳" . "{\\mathtt{D}}")
    ("𝙴" . "{\\mathtt{E}}")
    ("𝙵" . "{\\mathtt{F}}")
    ("𝙶" . "{\\mathtt{G}}")
    ("𝙷" . "{\\mathtt{H}}")
    ("𝙸" . "{\\mathtt{I}}")
    ("𝙹" . "{\\mathtt{J}}")
    ("𝙺" . "{\\mathtt{K}}")
    ("𝙻" . "{\\mathtt{L}}")
    ("𝙼" . "{\\mathtt{M}}")
    ("𝙽" . "{\\mathtt{N}}")
    ("𝙾" . "{\\mathtt{O}}")
    ("𝙿" . "{\\mathtt{P}}")
    ("𝚀" . "{\\mathtt{Q}}")
    ("𝚁" . "{\\mathtt{R}}")
    ("𝚂" . "{\\mathtt{S}}")
    ("𝚃" . "{\\mathtt{T}}")
    ("𝚄" . "{\\mathtt{U}}")
    ("𝚅" . "{\\mathtt{V}}")
    ("𝚆" . "{\\mathtt{W}}")
    ("𝚇" . "{\\mathtt{X}}")
    ("𝚈" . "{\\mathtt{Y}}")
    ("𝚉" . "{\\mathtt{Z}}")
    ; lowercase typewriter
    ("𝚊" . "{\\mathtt{a}}")
    ("𝚋" . "{\\mathtt{b}}")
    ("𝚌" . "{\\mathtt{c}}")
    ("𝚍" . "{\\mathtt{d}}")
    ("𝚎" . "{\\mathtt{e}}")
    ("𝚏" . "{\\mathtt{f}}")
    ("𝚐" . "{\\mathtt{g}}")
    ("𝚑" . "{\\mathtt{h}}")
    ("𝚒" . "{\\mathtt{i}}")
    ("𝚓" . "{\\mathtt{j}}")
    ("𝚔" . "{\\mathtt{k}}")
    ("𝚕" . "{\\mathtt{l}}")
    ("𝚖" . "{\\mathtt{m}}")
    ("𝚗" . "{\\mathtt{n}}")
    ("𝚘" . "{\\mathtt{o}}")
    ("𝚙" . "{\\mathtt{p}}")
    ("𝚚" . "{\\mathtt{q}}")
    ("𝚛" . "{\\mathtt{r}}")
    ("𝚜" . "{\\mathtt{s}}")
    ("𝚝" . "{\\mathtt{t}}")
    ("𝚞" . "{\\mathtt{u}}")
    ("𝚟" . "{\\mathtt{v}}")
    ("𝚠" . "{\\mathtt{w}}")
    ("𝚡" . "{\\mathtt{x}}")
    ("𝚢" . "{\\mathtt{y}}")
    ("𝚣" . "{\\mathtt{z}}")
    ; typewriter numerals
    ("𝟶" . "{\\mathtt{0}}")
    ("𝟷" . "{\\mathtt{1}}")
    ("𝟸" . "{\\mathtt{2}}")
    ("𝟹" . "{\\mathtt{3}}")
    ("𝟺" . "{\\mathtt{4}}")
    ("𝟻" . "{\\mathtt{5}}")
    ("𝟼" . "{\\mathtt{6}}")
    ("𝟽" . "{\\mathtt{7}}")
    ("𝟾" . "{\\mathtt{8}}")
    ("𝟿" . "{\\mathtt{9}}")))




; DEPRECATED

#;(define (magic-delimiters delim-marker math-string)
    (define (delimiters-parse string pairings-list)
      (if (null? pairings-list)
          string
          (string-replace (delimiters-parse string (cdr pairings-list)) (caar pairings-list) (cdar pairings-list))))
    (let* ([pairings `((,(string-append "(" delim-marker) . "{\\left(")
                       (,(string-append "[" delim-marker) . "{\\left[")
                       (,(string-append "\\{" delim-marker) . "{\\left\\{")
                       (,(string-append "\\langle" delim-marker) . "{\\left\\langle ")
                       (,(string-append "\\lceil" delim-marker) . "{\\left\\lceil ")
                       (,(string-append "\\lfloor" delim-marker) . "{\\left\\lfloor ")
                       (,(string-append "\\lmoustache" delim-marker) . "{\\left\\lmoustache ")
                       (,(string-append "\\lgroup" delim-marker) . "{\\left\\lgroup ")
                       (,(string-append "||" delim-marker) . "{\\left\\lVert ")
                       (,(string-append "|" delim-marker) . "{\\left\\lvert ")
                       ; (,(string-append delim-marker "|" delim-marker) . "\\middle\\vert ")
                       ; (,(string-append delim-marker "||" delim-marker) . "\\middle\\Vert ")
                       (,(string-append delim-marker ")") . "\\right)}")
                       (,(string-append delim-marker "]") . "\\right]}")
                       (,(string-append delim-marker "\\}") . "\\right\\}}")
                       (,(string-append delim-marker "\\rangle") . "\\right\\rangle}")
                       (,(string-append delim-marker "\\rceil") . "\\right\\rceil}")
                       (,(string-append delim-marker "\\rfloor") . "\\right\\rfloor}")
                       (,(string-append delim-marker "\\rmoustache") . "\\right\\rmoustache}")
                       (,(string-append delim-marker "\\rgroup") . "\\right\\rgroup}")
                       (,(string-append delim-marker "||") . "\\right\\rVert}")
                       (,(string-append delim-marker "|") . "\\right\\rvert}"))])
      (delimiters-parse math-string pairings)))