(TeX-add-style-hook
 "readme"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "spanish" "12pt" "letterpaper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("babel" "spanish") ("inputenc" "utf8")))
   (add-to-list 'LaTeX-verbatim-environments-local "code")
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "babel"
    "inputenc"
    "authblk"
    "listings"
    "dsfont")
   (LaTeX-add-bibitems
    "lamport94"))
 :latex)

