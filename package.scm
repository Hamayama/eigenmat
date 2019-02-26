;;
;; Package eigenmat
;;

(define-gauche-package "eigenmat"
  ;; 
  :version "1.15"

  ;; Description of the package.  The first line is used as a short
  ;; summary.
  :description "Matrix operation using Eigen library."

  ;; List of dependencies.
  ;; Example:
  ;;     :require (("Gauche" (>= "0.9.5"))  ; requires Gauche 0.9.5 or later
  ;;               ("Gauche-gl" "0.6"))     ; and Gauche-gl 0.6
  :require ()

  ;; List name and contact info of authors.
  ;; e.g. ("Eva Lu Ator <eval@example.com>"
  ;;       "Alyssa P. Hacker <lisper@example.com>")
  :authors ("Hamayama <fkyo0985@gmail.com>")

  ;; List name and contact info of package maintainers, if they differ
  ;; from authors.
  ;; e.g. ("Cy D. Fect <c@example.com>")
  :maintainers ()

  ;; List licenses
  ;; e.g. ("BSD")
  :licenses ()

  ;; Homepage URL, if any.
  :homepage "https://github.com/Hamayama/eigenmat"

  ;; Repository URL, e.g. github
  :repository "https://github.com/Hamayama/eigenmat.git"
  )
