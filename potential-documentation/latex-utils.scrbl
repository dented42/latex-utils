#lang scribble/manual

@(require pollen pollen/decode pollen/misc/tutorial
          txexpr sugar hyphenate
          scribble/example
          latex-utils/scribble)

@title[#:date "October, 2016"]{LaTeX Utils for Scribble and Pollen}

@author{M. Eyzaguirre and K. L. Smith}

@section{General Math Tools}

@subsection{Math Modes}

@defproc[(m [body (listof? value?)]) content?]{
 Produces LaTeX inline maths. More specifically, parses @racket[body] into valid LaTeX markup and wraps it in @tt{\(...\)}.
}

@defproc[(mp [body (listof? value)]) content?]{
 Like @racket[(m body)], but produces display math instead. After @racket[body] is parsed into valid markup, it is wrapped in @tt{\[...\]}.
}


@subsection{Fonts}

@defform[(cal body)]{
 Applies the LaTeX bold math font @tt{\matcal} to @racket[body].
}

@defform[(mcal body)]{
 Equivalent to @racket[(m (cal body))].
}


@defform[(bb body)]{
 Applies the LaTeX bold math font @tt{\mathbb} to @racket[body].
}

@defform[(mbb body)]{
 Equivalent to @racket[(m (bb body))].
}


@defform[(bf body)]{
 Applies the LaTeX bold math font @tt{\mathbf} to @racket[body].
}

@defform[(mbf body)]{
 Equivalent to @racket[(m (bf body))].
}


@defform[(sf body)]{
 Applies the LaTeX bold math font @tt{\mathsf} to @racket[body].
}

@defform[(msf body)]{
 Equivalent to @racket[(m (sf body))].
}


@defform[(rm body)]{
 Applies the LaTeX bold math font @tt{\mathrm} to @racket[body].
}

@defform[(mrm body)]{
 Equivalent to @racket[(m (rm body))].
}


@subsection{Delimiters}

@defform[(delim delims-expr body)
         #:grammar ([delims-expr (code:line)
                     delims-string
                     delims-list]
                    [delims-string (code:line)
                     "()"
                     "[]"
                     "{}"
                     "<>"
                     "||"]
                    [delims-list (code:line)
                     (list "\\langle" "\\rangle")
                     (list "\\lvert" "\\rvert")
                     (list "\\lVert" "\\rVert")])]{
 Wraps @racket[body] in the math delimiters specified by @racket[delims-expr].
}

@defform[(parens body)]{
 Equivalent to @racket[(delim "()" body)].
}


@subsection{Math Structures and Objects}

@defform[(od var body)]{
 Produces a Leibniz @emph{ordinary} derivative expression of @racket[body] with respect to @racket[var].
}

@defform[(pd var body)]{
 Produces a Leibniz @emph{partial} derivative expression of @racket[body] with respect to @racket[var].
}


@defform[(one body)]{
 Produces a fraction that is equal to one (@emph{i.e.,} @racket[body]/@racket[body]).
}



@section{Scribble Theorem Tools}