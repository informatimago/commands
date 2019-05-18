;;;; -*- mode:lisp; coding:utf-8 -*-

(defparameter insults
  '(
    ("accapareur" (nm))
    ("aérolithe" (nm))
    ("amiral de bateau­lavoir" (gnm))
    ("amphitryon" (nm))
    ("anacoluthe" (nf))
    ("analphabète" (n a))
    ("anthracite" (nm))
    ("anthropophage" (nm a))
    ("anthropopithèque" (nm))
    ("apache" (nm))
    ("apprenti dictateur à la noix de coco" (gnm))
    ("arlequin" (nm))
    ("astronaute d'eau douce" (gn))
    ("athlète complet" (n))
    ("autocrate" (nm))
    ("autodidacte" (n a))
    ("azteque" (nm))
    ("babouin" (nm))
    ("bachi­bouzouk" (nm))
    ("bande de" (|...|))
    ("bandit" (nm))
    ("bayadère" (nf))
    ("bibendum" (nm))
    ("boit­sans­soif" (ni))
    ("brontosaure" (nm))
    ("bougre de" (|...|))
    ("brute" (nf))
    ("bulldozer à réaction" (gnm))
    ("vieux" (a))
    ("cachalot" (nm))
    ("canaille" (nf))
    ("canaque" (nm a))
    ("cannibale" (nm))
    ("carnaval" (nm))
    ("catachrèse" (nf))
    ("cataplasme" (nm))
    ("cercopithèque" (nm))
    ("chauffard" (nm))
    ("chenapan" (nm))
    ("choléra" (nm))
    ("chouette mal empaillée" (gnf))
    ("cloporte" (nm))
    ("clysopompe" (nm))
    ("coléoptère" (nm))
    ("coloquinte" (nf))
    ("coquin" (n a))
    ("cornemuse" (nf))
    ("cornichon" (nm))
    ("corsaire" (nm))
    ("coupe­jarret" (nm))
    ("cow­boy de la route" (gnm))
    ("crétin des alpes" (gnm))
    ("Cro­magnon" (np))
    ("cyanure" (nm))
    ("cyclone" (nm))
    ("cyclotron" (nm))
    ("Cyrano à quatre pattes" (gnm))
    ("diablesse" (nf))
    ("diplodocus" (nm))
    ("doryphore" (nm))
    ("dynamiteur" (nm))
    ("ecornifleur" (nm))
    ("ecraseur" (nm))
    ("ectoplasme" (nm))
    ("egoïste" (nm))
    ("emplatre" (nm))
    ("empoisonneur" (nm a))
    ("enragé" (nm a))
    ("épouvantail" (nm))
    ("équilibriste" (nm))
    ("esclavagiste" (nm))
    ("escogriffe" (nm))
    ("espèce de" (|...|))
    ("extrait de" (|...|))
    ("graine de" (|...|))
    ("tersorium" (nm))
    ("faux jeton" (nm))
    ("flibustier" (nm))
    ("forban" (nm))
    ("frères de la côte" (gnm))
    ("froussard" (nm a))
    ("galopin" (nm))
    ("gangster" (nm))
    ("garde­côte à la mie de pain" (gnm))
    ("gargarisme" (nm))
    ("garnement" (nm))
    ("gibier de potence" (nm))
    ("goujat" (nm))
    ("gredin" (nm))
    ("grenouille" (nf))
    ("gros plein de soupe" (gnm))
    ("gyroscope" (nm))
    ("hérétique" (n a))
    ("hors­la­loi" (nm))
    ("huluberlu" (nm))
    ("hydrocarbure" (nm))
    ("iconoclaste" (nm a))
    ("incas de carnaval" (gnmp))
    ("individou de général" (gnm))
    ("invertébré" (nm))
    ("ivrogne" (n))
    ("jet d'eau ambulant" (gnm))
    ("jocrisse" (nm))
    ("judas" (nm))
    ("jus de réglisse" (gnm))
    ("kroumir" (nm))
    ("ku klux klan" (gnm))
    ("lâche" (nm))
    ("lépidoptère" (nm))
    ("logarithme" (nm))
    ("loup­garou à la graisse de renoncule" (gnm))
    ("macaque" (nm))
    ("macrocéphale" (nm))
    ("malappris" (n a))
    ("malotru" (n))
    ("mamelouk" (nm))
    ("marchand de guano" (gnm))
    ("marchand de tapis" (gnm))
    ("marin d'eau douce" (gnm))
    ("marmotte" (nf))
    ("mégalomane" (nm a))
    ("mégacycle" (nm))
    ("mercanti" (nm))
    ("mercenaire" (nm a))
    ("mérinos" (nm))
    ("mille sabords" (gnmp))
    ("misérable" (a))
    ("mitrailleur à bavette" (gnm))
    ("moratorium" (nm))
    ("moricaud" (nm a))
    ("mouchard" (nm))
    ("moujik" (nm))
    ("moule à gaufres" (gnm))
    ("moussaillon" (nm))
    ("mrkrpxzkrmtfrz" (nm))
    ("mufle" (nm))
    ("Mussolini de carnaval" (nm))
    ("naufrageur" (nm))
    ("négrier" (nm))
    ("noix de coco" (gnm))
    ("nyctalope" (n a))
    ("olibrius" (nm))
    ("ophicléïde" (nm))
    ("ornithorynque" (nm))
    ("oryctérope" (nm))
    ("ostrogoth" (n a))
    ("ours mal lèché" (gnm))
    ("pacte à quatre" (gnm))
    ("paltoquet" (nm))
    ("pantoufle" (nf))
    ("Papous" (nm))
    ("paranoïaque" (nm a))
    ("parasite" (nm a))
    ("Patagon" (nm))
    ("patapouf" (nm))
    ("patate" (nf))
    ("péronnelle" (nf))
    ("perruche bavarde" (gnf))
    ("phénomène" (nm))
    ("phlébotome" (nm))
    ("phylactère" (nm))
    ("phylloxéra" (nm))
    ("pignouf" (nm))
    ("pirate" (nm))
    ("Polichinelle" (nm))
    ("polygraphe" (nm))
    ("porc­épic mal embouché" (gnm))
    ("potentat emplumé" (gnm))
    ("poussière" (nf))
    ("profiteur" (nm))
    ("projectile guidé" (gnm))
    ("protozoaire" (nm))
    ("pyromane" (nm))
    ("pyrophore" (nm))
    ("rapace" (nm))
    ("rat" (nm))
    ("Ravachol" (nm))
    ("renégat" (nm))
    ("rhizopode" (nm))
    ("Rocambole" (nm))
    ("sacripant" (nm))
    ("sajou" (nm))
    ("saltimbanque" (nm))
    ("sapajou" (nm))
    ("satané bazar de fourbi de truc" (gnm))
    ("satrape" (nm))
    ("sauvage" (n a))
    ("scélérat" (nm))
    ("schizophrène" (n a))
    ("scolopendre" (nf))
    ("scorpion" (nm))
    ("serpent" (nm))
    ("simili martien à la graisse de cabestan" (gnm))
    ("sinapisme" (nm))
    ("soulographe" (nm))
    ("squatter" (nm))
    ("tchouk­tchouk­nougat" (nm))
    ("technocrate" (nm))
    ("tête de lard" (gnf))
    ("tête de mule" (gnf))
    ("tigresse" (nf))
    ("tonnerre de Brest" (gnm))
    ("topinanbour" (nm))
    ("tortionnaire" (nm))
    ("traficant de chair humaine" (gnm))
    ("traine­potence" (nm))
    ("traitre" (nm a))
    ("troglodyte" (nm))
    ("trompe­la­mort" (nm))
    ("vampire" (nm))
    ("vandale" (nm a))
    ("va­nu­pieds" (nm))
    ("vaurien" (nm))
    ("végétarien" (nm))
    ("Vercingétorix de carnaval" (nm))
    ("ver de terre" (gnm))
    ("vermicelle" (nm))
    ("vermine" (nm))
    ("vipère" (nf))
    ("vivisectionniste" (nm))
    ("voleur" (nm))
    ("wisigoth" (n a))
    ("zapotèque" (nm))
    ("zèbre" (nm))
    ("zigomar" (nm))
    ("zouave" (nm))
    ("Zoulou" (nm))
    ));;insults

(defparameter nm (remove-if-not (lambda (x) (intersection '(n np nm gnm) (second x))) insults))
(defparameter nf (remove-if-not (lambda (x) (intersection '(nf np) (second x))) insults))
(defparameter ad (remove-if-not (lambda (x) (member 'a (second x))) insults))

(defun insulte ()
  (let* ((ga (format nil " ~A" (first (elt ad (random (length ad))))))
         (gn (let ((n (random (+ (length nf) (length nm)))))
               (if (>= n (length nm))
                 (prog1
                     (first (elt nf (- n (length nm))))
                   (cond
                    ((= 0 (length ga)))
                    ((string= "e" ga :start2 (1- (length ga))))
                    ((string= "eux" ga :start2 (- (length ga) 3))
                     (setf ga (concatenate 'string
                                (subseq ga 0 (- (length ga) 2)) "ille")))
                    ((string= "eur" ga :start2 (- (length ga) 3))
                     (setf ga (concatenate 'string
                                (subseq ga 0 (1- (length ga))) "se")))
                    (t
                     (setf ga (concatenate 'string ga "e")))))
                 (first (elt nm n)))))
         (conj (if (position (aref gn 0) "aeiouyh") "d'" "de "))
         (ins  (case (random 4)
                 ((0)       (concatenate 'string "espèce " conj gn ga))
                 ((1)       (concatenate 'string "bande "  conj gn ga))
                 (otherwise (concatenate 'string gn ga)))))
    (concatenate 'string
      (string-capitalize (subseq ins 0 1))
      (subseq ins 1)
      " !")))

(defun main (arguments)
  (declare (ignore arguments))
  (setf *random-state* (make-random-state t))
  (princ (insulte))
  (terpri)
  ex-ok)

;;;; THE END ;;;;
