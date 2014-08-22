(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defparameter *rudimentary-spanish-grammar*
  '((sentence -> (article-noun verb-phrase))
    (verb-phrase -> (Verb prepositional-phrase))
    (article-noun -> "el gato" "el perro" "el raton" "el gallito" "la vaca" "la nena" "Bobby" "la UPR" "la pizza" "el pirata" "el oso" "el hipster" "el ojo de sauron" "Humberto" "Andrew" "Jonathan" "Luicito" "Alejandro" "la programadora" "la doctora" "la cientifica" "la jefa" "la ingeniera" "la capitana")
    (Verb -> mordio vio rompio lamio abrio guio programo borro olio mostro lavo observo miro toco sobo seducio mato estrangulo encadeno quemo congelo)
    (prepositional-phrase -> "al arroz" "a los libros" "a las yales" "a los carros" "a las computadoras" "a las bebidas" "a la comida" "a las zanahorias" "a los cacos" "a los conejos" "a R2D2" "a Mayaguez" "a la Politecnica" "a Sagrado" "a la Inter" "a Ganondorf"))
  "A really basic Spanish grammar.")

(defvar *grammar* *rudimentary-spanish-grammar*
  "The grammar used by generate.")

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun rule-lhs (rule)
  "The left hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun mappend (fn list)
  (apply #'append (mapcar fn list)))

