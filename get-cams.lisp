;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;*****************************************************************************
;;;;FILE:              get-cams
;;;;LANGUAGE:          Common-Lisp
;;;;SYSTEM:            clisp/unix
;;;;USER-INTERFACE:    X window
;;;;DESCRIPTION
;;;;    This script periodically gather webcam pictures on the web and dispatch
;;;;    onto X displays.
;;;;USAGE
;;;;    get-cams --help
;;;;    Edit the cameras and displays tables in this script to match your needs.
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2003-03-02 <PJB> Added auto-delay feature.
;;;;    2003-02-22 <PJB> Converted to Common-Lisp.
;;;;    2002-??-?? <PJB> Creation.
;;;;BUGS
;;;;    You tell me.
;;;;LEGAL
;;;;    Copyright Pascal J. Bourguignon 2002 - 2003
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU  General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This script is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this library; see the file COPYING.LIB.
;;;;    If not, write to the Free Software Foundation,
;;;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;*****************************************************************************

;; Clean the packages imported into COMMON-LISP-USER:
(MAPC (LAMBDA (USED) (UNUSE-PACKAGE USED "COMMON-LISP-USER"))
      (REMOVE (FIND-PACKAGE "COMMON-LISP")
              (COPY-SEQ (PACKAGE-USE-LIST "COMMON-LISP-USER"))))

;; (SETQ *ARGS* '( "-t" "-c" "noumea" "-d" "thalassa-tl" "-c" "acapulco-2" "-d" "thalassa-bl" "-d" "galatea-tl" "-o" ))

(DEFUN DEF-LP-TRANS (HOST PATH &OPTIONAL (SUBPATH ""))
  ;; (PUSHNEW HOST *LOGICAL-HOSTS* :TEST (FUNCTION STRING-EQUAL))
  ;; If the HOST is already defined we don't change it.
  (UNLESS (HANDLER-CASE (LOGICAL-PATHNAME-TRANSLATIONS HOST) (ERROR () NIL))
    (LET ((DIRECTORY (APPEND (PATHNAME-DIRECTORY PATH)
                             (CDR (PATHNAME-DIRECTORY SUBPATH))
                             '( :WILD-INFERIORS ))))
      (SETF (LOGICAL-PATHNAME-TRANSLATIONS HOST)
            (LIST
             (LIST "**;*"     (MAKE-PATHNAME :DIRECTORY DIRECTORY
                                             :NAME :WILD))
             (LIST "**;*.*"   (MAKE-PATHNAME :DIRECTORY DIRECTORY
                                             :NAME :WILD :TYPE :WILD))
             (LIST "**;*.*.*" (MAKE-PATHNAME :DIRECTORY DIRECTORY
                                             :NAME :WILD :TYPE :WILD
                                             :VERSION :WILD)) )))))

(DEFPARAMETER +SHARE-LISP+
  (make-pathname :directory '(:absolute "USR" "LOCAL" "SHARE" "LISP")
                 :CASE :COMMON))
(DEF-LP-TRANS "PACKAGES"   +SHARE-LISP+ "packages/")

;;(print  (LOGICAL-PATHNAME-TRANSLATIONS "PACKAGES"))

(load  "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;PACKAGE")
(use-package    "COM.INFORMATIMAGO.COMMON-LISP.PACKAGE")
(load  "PACKAGES:COM;INFORMATIMAGO;CLISP;SCRIPT")
(add-nickname   "COM.INFORMATIMAGO.CLISP.SCRIPT" "SCRIPT")
(SCRIPT:INITIALIZE)

(DEFUN UNIX-PATH-EXTENSION (PATH &KEY REMOVE-DOT)
  "
PATH:       A string containing a unix path.
RETURN:     The extension of PATH.
            If REMOVE-DOT then the dot is not included.
"
  (LET ((DOT (POSITION (CHARACTER ".") PATH :FROM-END T)))
    (IF DOT
      (SUBSEQ PATH (+ (IF REMOVE-DOT 1 0) DOT))
      "")));;UNIX-PATH-EXTENSION


(DEFUN UNIX-PATH-ROOT (PATH)
  "
PATH:       A string containing a unix path.
RETURN:     A substring of PATH without the extension.
"
  (SUBSEQ PATH 0 (OR (POSITION (CHARACTER ".") PATH :FROM-END T)
                     (LENGTH PATH))));;UNIX-PATH-ROOT


(DEFUN PLIST-GET (PLIST KEY)
  (DO ((PLIST PLIST (CDDR PLIST)))
      ((OR (NULL PLIST)
           (EQUAL KEY (CAR PLIST))) (VALUES (CADR PLIST) (NOT (NULL PLIST))))));;PLIST-GET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Can you imagine it, COMMON-LISP CASE does not work on STRING!


(EVAL-WHEN (COMPILE LOAD EVAL)

  (DEFMACRO MEMQ (ITEM LIST)
    `(MEMBER ,ITEM ,LIST :TEST (FUNCTION EQ)))

  (DEFMACRO WHILE (CONDITION &BODY BODY)
    `(DO () ((NOT ,CONDITION) NIL) ,@BODY))


  (DEFCONSTANT CL-SIMPLE-FUNCS
    '(CAR CDR NTH AREF ELT IF AND OR + - 1+ 1- MIN MAX
          CAR-SAFE CDR-SAFE PROGN PROG1 PROG2))

  (DEFCONSTANT CL-SAFE-FUNCS
    '(* / % LENGTH MEMQ LIST VECTOR VECTORP < > <= >= = /= ERROR))


  ;; Things that are side-effect-free.
  (MAPCAR (FUNCTION (LAMBDA (Y) (SETF (GET Y 'SIDE-EFFECT-FREE) T)))
          '(FIRST SECOND THIRD FOURTH FIFTH SIXTH SEVENTH EIGHTH NINTH
                  TENTH REST ENDP PLUSP MINUSP CAAAR CAADR CADAR CADDR CDAAR
                  CDADR CDDAR CDDDR CAAAAR CAAADR CAADAR CAADDR CADAAR CADADR
                  CADDAR CADDDR CDAAAR CDAADR CDADAR CDADDR CDDAAR CDDADR
                  CDDDAR CDDDDR
                  ODDP EVENP SIGNUM LAST BUTLAST LDIFF PAIRLIS GCD LCM
                  ISQRT FLOOR CEILING TRUNCATE ROUND MOD REM SUBSEQ
                  LIST-LENGTH GET GETF))

  ;; Things that are side-effect-and-error-free.
  (MAPCAR (FUNCTION (LAMBDA (X) (SETF (GET X 'SIDE-EFFECT-FREE) 'ERROR-FREE)))
          '(EQL FLOATP-SAFE LIST SUBST ACONS EQUALP RANDOM-STATE-P
                COPY-TREE SUBLIS))

  ) ;;EVAL-WHEN


(DEFUN CL-SIMPLE-EXPR-P (X &OPTIONAL SIZE)
  (OR SIZE (SETQ SIZE 10))
  (IF (AND (CONSP X) (NOT (MEMQ (CAR X) '(QUOTE FUNCTION))))
    (AND (SYMBOLP (CAR X))
         (OR (MEMQ (CAR X) CL-SIMPLE-FUNCS)
             (GET (CAR X) 'SIDE-EFFECT-FREE))
         (PROGN
           (SETQ SIZE (1- SIZE))
           (WHILE (AND (SETQ X (CDR X))
                       (SETQ SIZE (CL-SIMPLE-EXPR-P (CAR X) SIZE))))
           (AND (NULL X) (>= SIZE 0) SIZE)))
    (AND (> SIZE 0) (1- SIZE))));;CL-SIMPLE-EXPR-P


(DEFMACRO STRING-CASE (EXPR &REST CLAUSES)
  "Eval EXPR and choose from CLAUSES on that value.
Each clause looks like (KEYLIST BODY...).  EXPR is evaluated and compared
against each key in each KEYLIST; the corresponding BODY is evaluated.
If no clause succeeds, case returns nil.  A single string may be used in
place of a KEYLIST of one string.  A KEYLIST of `t' or `otherwise' is
allowed only in the final clause, and matches if no other keys match.
Key values are compared by `string='."
  (LET* ((TEMP (IF (CL-SIMPLE-EXPR-P EXPR 3) EXPR (GENSYM)))
         (HEAD-LIST NIL)
         (BODY (CONS
                'COND
                (MAPCAR
                 (FUNCTION
                  (LAMBDA (C)
                    (CONS (COND ((MEMQ (CAR C) '(T OTHERWISE)) T)
                                ((EQ (CAR C) 'ECASE-ERROR-FLAG)
                                 (LIST 'ERROR "ecase failed: %s, %s"
                                       TEMP (LIST 'QUOTE (REVERSE HEAD-LIST))))
                                ((LISTP (CAR C))
                                 (SETQ HEAD-LIST (APPEND (CAR C) HEAD-LIST))
                                 (LIST 'MEMBER TEMP (LIST 'QUOTE (CAR C))
                                       :TEST '(FUNCTION STRING=)))
                                (T
                                 (IF (MEMBER (CAR C) HEAD-LIST
                                             :TEST (FUNCTION STRING=))
                                   (ERROR "Duplicate key in case: %s"
                                          (CAR C)))
                                 (PUSH (CAR C) HEAD-LIST)
                                 (LIST 'STRING= TEMP (LIST 'QUOTE (CAR C)))))
                          (OR (CDR C) '(NIL)))))
                 CLAUSES))))
    (IF (EQ TEMP EXPR) BODY
        (LIST 'LET (LIST (LIST TEMP EXPR)) BODY)))
  );;STRING-CASE


(DEFMACRO PJB-CL+INDENT (SYMBOL WIDTH)
  "PRIVATE, Emacs specific.
 Put on the SYMBOL  a 'lisp-indent-function property set to WIDTH.
"
  `(SETF (GET ',SYMBOL  'lisp-indent-function) ,WIDTH));;PJB-CL+INDENT

(PJB-CL+INDENT STRING-CASE 1)

;;(put 'MULTIPLE-VALUE-BIND 'lisp-indent-function 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BEGIN CONFIGURATION
;;;
;; camera names and displays names are used to build bash variable names.
;; Dashes are replaced by underlines. All other characters must be valid
;; for bash variable names, and care should be take to avoid name collision.


(DEFPARAMETER CAMTITLE
  '(
    ("Camera Name" "Zon" "Dly" "Wide" "High" "URL")
    ("--------------------" "---" "---" "----" "----"
     "----------------------------------------")
    "~20A ~3A ~3A ~4A ~4A  ~40A~%"));;CAMTITLE


(DEFPARAMETER CAMERAS
  '(
    (AFAA-CISCO
     +0   300 500 135
     "http://stats-clients.easynet.fr/AFAA/afaa-day.png"
     :DO-NOT-ZOOM T
     :CONVERT-FUNCTION (LAMBDA (IN-PATH)
                         (LET ((OUT-PATH (FORMAT NIL "~A.pnm"
                                                 (UNIX-PATH-ROOT IN-PATH))))
                           (UNLESS SCRIPT::TESTING
                             (SCRIPT:SHELL (FORMAT NIL "pngtopnm < ~A > ~A"
                                                   IN-PATH OUT-PATH)))
                           OUT-PATH)))
    (NOUMEA
     +12  60  768  512
     "http://webcam.kaori.nc/webcam.jpg")
    (SIDNEY
     +10  60  480  360
     "http://www.lss.com.au/webcam/webcaml.jpg")
    (KYOTO
     +9  60  320  240
     "http://www3.kyoto-kcg.ac.jp/daimon/current.jpg")
    (SETO-BRIDGE
     +9  60  320  240
     "http://info-bridge.santec.co.jp/setobridge/setob.jpg")
    (PERTH-1
     +8  60  320  240
     "http://www.aceonline.com.au/~livecam/cam1/livecam.jpg")
    (PERTH-2
     +8  60  320  240
     "http://www.aceonline.com.au/~livecam/cam2/livecam.jpg")
    (PERTH-3
     +8  60  320  240
     "http://www.aceonline.com.au/~livecam/cam3/livecam.jpg")
    (BANGKOK
     +7  60  320  240
     "http://www.cyberworksconsulting.com/images/webcam/capture.jpg")
    (NOVOSIBIRSK
     +5  60  320  240
     "http://webcam.telefun.ru/cgi-bin/w3cam.cgi")
    (MOSCOW
     +3  60  512  384
     "http://cards.mnc.ru/image/512/mos32.jpg")
    (GDANSK
     +2  60  320  240
     "http://www.gdansk.gda.pl/um_green/k_kamera/cam2.jpg")
    (JERUSALEM
     +2  60  320  240
     "http://aish1.com/wall/thewall92.jpg")
    (OXFORD-CIRCUS
     +0  60  320  240
     "http://www.fujiint.co.uk/street/FIP.jpg")
    (LA-MANGA
     +0  30  352  288
     "http://www.informatimago.com/webcams/webcam.jpeg")
    (GRAN-CANARIAS
     -1  60  640  472
     "http://www.condor.de/common/webcams/CF150312.jpg")
    (ISAFJORDUR
     -1  60  352  288
     "http://www.snerpa.is/vedur/isafjord.jpg")
    (ISUMERIT
     -2  60  320  240
     "http://iserit.greennet.gl/sajare/isummeritwebcam/webcam.jpg")
    (RIO-DE-JANEIRO
     -3  60  352  288
     "http://www.uol.com.br/aliwebcam2/pao_de_acucar.jpg")
    (BUENOS-AIRES
     -4  60  352  288
     "http://www.offspring.com.ar/webcam/buenos_aires01.jpg")
    (NEW-YORK
     -5  60  320  240
     "http://www.abc45online.com/skycpk.jpg")
    (MEXICO-PONIENTE-1
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg1.jpg"
      :TITLE "Periférico Norte, Paseo de las Palmas y Presidente Masarik"
      :LOCATION "Col. Chapultepec Morales(Zona Poniente)" )
    (MEXICO-SUR-1
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg5.jpg"
      :TITLE "Av. Patriotismo, pasando Extremadura (Eje 7 Sur)"
      :LOCATION "Col. Mixcoac(Zona Sur)" )
    (MEXICO-SUR-2
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg7.jpg"
      :TITLE "Periférico Sur, antes de Paseo de la Magdalena"
      :LOCATION "Col. Jardínes Pedregal(Zona Sur)" )
    (MEXICO-ORIENTE-1
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg8.jpg"
      :TITLE "Viaducto Miguel Alemán, entre Eje 3 Oriente y Añil"
      :LOCATION "Col. Granjas México(Zona Oriente)" )
    (MEXICO-SUR-3
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg9.jpg"
      :TITLE "Av. Universidad y Av. Coyoacan"
      :LOCATION "Col. del Valle(Zona Sur)" )
    (MEXICO-SUR-4
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg10.jpg"
      :TITLE "Av. Revolución y Dr. Zamora y Duque (antes de Viaducto)"
      :LOCATION "Col. Escandón(Zona Sur)" )
    (GUADALAJARA-1
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg11.jpg"
      :TITLE "Cruce de Av.Vallarta y Av. Chapultepec"
      :LOCATION "Col. Americana(Guadalajara, Jalisco)" )
    (GUADALAJARA-2
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg12.jpg"
      :TITLE "Federalismo Norte y Circunvalación"
      :LOCATION "Federalismo(Guadalajara, Jalisco)" )
    (MAZATLAN
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg13.jpg"
      :TITLE "21 de marzo 14 y 16 Pte."
      :LOCATION "(Mazatlán, Sinaloa)" )
    (LAPAZ
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg14.jpg"
      :TITLE "Clima"
      :LOCATION "Ocampo y Altamirano(La Paz, Baja California Sur)" )
    (MONTERREY-1
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg15.jpg"
      :TITLE "Av. Constitución, Centro"
      :LOCATION "(Monterrey, Nuevo León)" )
    (MONTERREY-2
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg16.jpg"
      :TITLE "Av. Gonzalitos y Nevado de Toluca Col. Urdiales"
      :LOCATION "Col. Urdiales(Monterrey, Nuevo León)" )
    (MEXICO-PONIENTE-2
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg17.jpg"
      :TITLE "Carretera México-Toluca km. 14.5"
      :LOCATION "Col. Lomas de Bezares(Zona Poniente)" )
    (MEXICO-SUR-5
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg19.jpg"
      :TITLE "Av. Insurgentes Sur"
      :LOCATION "Col. Copilco el Bajo(Zona Sur)" )
    (MEXICO-ORIENTE-2
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg20.jpg"
      :TITLE "Calzada Ignacio Zaragoza y Av.Telecomunicaciones"
      :LOCATION "U.H. Rotaria(Zona Oriente)" )
    (MEXICO-ORIENTE-3
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg21.jpg"
      :TITLE "Calzada Ermita Iztapalapa 1628"
      :LOCATION "Col. San Miguel(Zona Oriente)" )
    (ACAPULCO-1
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg22.jpg"
      :TITLE "Av. farallón del Obispo #2"
      :LOCATION "Frente a la Costera(Acapulco, Guerrero)" )
    (ACAPULCO-2
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg23.jpg"
      :TITLE "Costera Miguel Alemán y Av. del Mar"
      :LOCATION "Fracc. Club Deportivo(Acapulco, Guerrero)" )
    (CANCUN-1
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg24.jpg"
      :TITLE "Boulevard Kukulkan Km.10 Zona Hotelera, Frente al Centro de Convenciones"
      :LOCATION "(Cancún, Quintana Roo)" )
    (CANCUN-2
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg25.jpg"
      :TITLE "Boulevard Kukulkan Km.10 Zona Hotelera, Frente al Centro de Convenciones"
      :LOCATION "(Cancún, Quintana Roo)" )
    (NUEVOLAREDO
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg26.jpg"
      :TITLE "Avenida González y Benito Juárez Centro (Puente Internacional 1)"
      :LOCATION "(Nuevo Laredo, Tamaulipas)" )
    (MEXICO-SUR-6
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg27.jpg"
      :TITLE "Río Churubusco pasando Av. Universidad"
      :LOCATION "Col. del Valle(Zona Sur)" )
    (REYNOSA
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg28.jpg"
      :TITLE "Boulevard Hidalgo"
      :LOCATION "Col. Simón Rodríguez(Reynosa, Tamaulipas)" )
    (VERACRUZ
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg29.jpg"
      :TITLE "Zamora y Rayón"
      :LOCATION "Centro(Veracruz, Veracruz)" )
    (CHIHUAHUA-1
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg30.jpg"
      :TITLE "Av. Universidad No. 2731"
      :LOCATION "Col. Universidad(Chihuahua, Chihuahua)" )
    (MEXICO-SUR-7
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg32.jpg"
      :TITLE "Periférico Sur 3585"
      :LOCATION "(Zona Sur)" )
    (CHIHUAHUA-2
      -7  60  704  576
      "http://www.vialidad.telmex.net/fotos/fg33.jpg"
      :TITLE "El Paso Texas"
      :LOCATION "(Cd. Juárez, Chihuahua)" )
    ;; returns HTML instead of JPG.
;;     (VALLARTA
;;      -8  60  352  288
;;      "http://images.earthcam.com/affiliates/cheeseburger/puertovallarta.jpg")
    (SAN-FRANCISCO-BAY
     -8  60  400  300
     "http://webmarin.com/images/wc/Camera.jpg")
    (SAN-FRANCISCO-GOLDEN
     -8  60  320  240
     "http://www.mapwest.com/webcam/ggbridgecam.jpg")
    (VANCOUVER-ISLAND
     -9  60  352  288
     "http://www.shawniganlakecondo.com/webcam/webcam32.jpg")
    (RUBY
     -10  60  640  480
     "http://akweathercams.faa.gov/wxdata/Ruby/images/current/hugecam2.jpg")
    (KODIAK
     -10  60  640  480
     "http://akweathercams.faa.gov/wxdata/kodiak/images/current/hugecam4.jpg")
    ;; returns HTML instead of JPG.
;;     (HONOLULU-SMALL
;;      -11  60  128   96
;;      "http://www.hawaii-malinda.com/live/s1.jpg")
;;     (HONOLULU-BIG
;;      -11  60  640  480
;;      "http://www.hawaii-malinda.com/live/1.jpg")
    (HONOLULU-HARBOR
     -11  60  640  480
     "http://www.hawaiiocean.com/oceanonline/Channel.jpg")
    (CANCUN-NORTE-SMALL
     -6  60  352  288
     "http://www.vialidad.telmex.net/fotos/f24.jpg")
    (CANCUN-SUR-SMALL
     -6  60  352  288
     "http://www.vialidad.telmex.net/fotos/f25.jpg")
    (ACAPULCO-1-SMALL
     -7  60  352  288
     "http://www.vialidad.telmex.net/fotos/f22.jpg")
    (ACAPULCO-2-SMALL
     -7  60  352  288
     "http://www.vialidad.telmex.net/fotos/f23.jpg")
    ));;CAMERAS


#|

(defun gen-tiles (name display width height htiles vtiles)
  "EMACS"
  (let ( (hincrem (/ width  htiles))
         (vincrem (/ height vtiles)) )
    (for j 0 (1- vtiles)
         (for i 0 (1- htiles)
              (printf "    \"%-14s%-8s%5d%5d  %s\" \\\n"
                      (format "%s-%02d" name (+ i (* j htiles)))
                      (format "%d,%d" (* i hincrem) (* j vincrem))
                      hincrem vincrem display)
              ))))
;; (gen-tiles "galatea"   "galatea.informatimago.com:0.0"  800  600 5 5)
;; (gen-tiles "thalassa" "thalassa.informatimago.com:0.0" 1280 1024 5 5)

|#


(DEFPARAMETER DISTITLE
  '(
    ("Disp.Name" "Offset"  "Wide" "High"  "X screen")
    ("-------------" "--------" "----" "----"
     "---------------------------------------")
    "~13A ~8A ~4A ~4A ~39A~%"));;DISTITLE

(DEFPARAMETER DISPLAYS
  '(
    (PROTEUS-TL    "0,0"      400  300  "192.168.166.2:0.0")
    (PROTEUS-TR    "400,0"    400  300  "192.168.166.2:0.0")
    (PROTEUS-BL    "0,300"    400  300  "192.168.166.2:0.0")
    (PROTEUS-BR    "400,300"  400  300  "192.168.166.2:0.0")
    (THALASSA-FULL "0,0"     1280 1024  "thalassa.informatimago.com:0.0")
    (THALASSA-TL   "0,0"      640  512  "thalassa.informatimago.com:0.0")
    (THALASSA-TR   "640,0"    640  512  "thalassa.informatimago.com:0.0")
    (THALASSA-BL   "0,512"    640  512  "thalassa.informatimago.com:0.0")
    (THALASSA-BR   "640,512"  640  512  "thalassa.informatimago.com:0.0")
    (GALATEA-FULL  "0,0"      800  600  "galatea.informatimago.com:0.0")
    (GALATEA-TL    "0,0"      400  300  "galatea.informatimago.com:0.0")
    (GALATEA-TR    "400,0"    400  300  "galatea.informatimago.com:0.0")
    (GALATEA-BL    "0,300"    400  300  "galatea.informatimago.com:0.0")
    (GALATEA-CISCO-TR "310,0"  500  300  "galatea.informatimago.com:0.0")
    (GALATEA-CISCO "310,270"  500  300  "galatea.informatimago.com:0.0")
    (GALATEA-BR    "400,300"  400  300  "galatea.informatimago.com:0.0")
    (GALATEA-00    "0,0"      160  120  "galatea.informatimago.com:0.0")
    (GALATEA-01    "160,0"    160  120  "galatea.informatimago.com:0.0")
    (GALATEA-02    "320,0"    160  120  "galatea.informatimago.com:0.0")
    (GALATEA-03    "480,0"    160  120  "galatea.informatimago.com:0.0")
    (GALATEA-04    "640,0"    160  120  "galatea.informatimago.com:0.0")
    (GALATEA-05    "0,120"    160  120  "galatea.informatimago.com:0.0")
    (GALATEA-06    "160,120"  160  120  "galatea.informatimago.com:0.0")
    (GALATEA-07    "320,120"  160  120  "galatea.informatimago.com:0.0")
    (GALATEA-08    "480,120"  160  120  "galatea.informatimago.com:0.0")
    (GALATEA-09    "640,120"  160  120  "galatea.informatimago.com:0.0")
    (GALATEA-10    "0,240"    160  120  "galatea.informatimago.com:0.0")
    (GALATEA-11    "160,240"  160  120  "galatea.informatimago.com:0.0")
    (GALATEA-12    "320,240"  160  120  "galatea.informatimago.com:0.0")
    (GALATEA-13    "480,240"  160  120  "galatea.informatimago.com:0.0")
    (GALATEA-14    "640,240"  160  120  "galatea.informatimago.com:0.0")
    (GALATEA-15    "0,360"    160  120  "galatea.informatimago.com:0.0")
    (GALATEA-16    "160,360"  160  120  "galatea.informatimago.com:0.0")
    (GALATEA-17    "320,360"  160  120  "galatea.informatimago.com:0.0")
    (GALATEA-18    "480,360"  160  120  "galatea.informatimago.com:0.0")
    (GALATEA-19    "640,360"  160  120  "galatea.informatimago.com:0.0")
    (GALATEA-20    "0,480"    160  120  "galatea.informatimago.com:0.0")
    (GALATEA-21    "160,480"  160  120  "galatea.informatimago.com:0.0")
    (GALATEA-22    "320,480"  160  120  "galatea.informatimago.com:0.0")
    (GALATEA-23    "480,480"  160  120  "galatea.informatimago.com:0.0")
    (GALATEA-24    "640,480"  160  120  "galatea.informatimago.com:0.0")
    (THALASSA-00   "0,0"      256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-01   "256,0"    256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-02   "512,0"    256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-03   "768,0"    256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-04   "1024,0"   256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-05   "0,204"    256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-06   "256,204"  256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-07   "512,204"  256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-08   "768,204"  256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-09   "1024,204" 256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-10   "0,408"    256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-11   "256,408"  256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-12   "512,408"  256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-13   "768,408"  256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-14   "1024,408" 256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-15   "0,612"    256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-16   "256,612"  256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-17   "512,612"  256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-18   "768,612"  256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-19   "1024,612" 256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-20   "0,816"    256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-21   "256,816"  256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-22   "512,816"  256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-23   "768,816"  256  204  "thalassa.informatimago.com:0.0")
    (THALASSA-24   "1024,816" 256  204  "thalassa.informatimago.com:0.0")
    ));;DISPLAYS
;;;
;;; END CONFIGURATION
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SOME TABLE FUNCTIONS
;;;


(DEFUN TABLE-DISPLAY (TITLE DATA)
  "Prints the table."
  (LET ((FORM (ELT TITLE 2)))
    (FORMAT T "~%")
    (APPLY 'FORMAT T FORM  (ELT TITLE 1))
    (APPLY 'FORMAT T FORM  (ELT TITLE 0))
    (APPLY 'FORMAT T FORM  (ELT TITLE 1))
    (DOLIST (LINE DATA)
      (APPLY 'FORMAT T FORM LINE))
    (APPLY 'FORMAT T FORM  (ELT TITLE 1))
    (FORMAT T "~%")
    ));;TABLE-DISPLAY


;; Line accessors:

(DEFUN CAM-NAME    (CAM) (ELT CAM 0))
(DEFUN CAM-ZONE    (CAM) (ELT CAM 1))
(DEFUN CAM-PERIOD  (CAM) (ELT CAM 2))
(DEFUN CAM-WIDTH   (CAM) (ELT CAM 3))
(DEFUN CAM-HEIGHT  (CAM) (ELT CAM 4))
(DEFUN CAM-URL     (CAM) (ELT CAM 5))
(DEFUN CAM-OPTIONS (CAM) (CDDR (CDDDDR CAM)))

(DEFUN DIS-NAME    (DIS) (ELT DIS 0))
(DEFUN DIS-OFFSET  (DIS) (ELT DIS 1))
(DEFUN DIS-WIDTH   (DIS) (ELT DIS 2))
(DEFUN DIS-HEIGHT  (DIS) (ELT DIS 3))
(DEFUN DIS-SCREEN  (DIS) (ELT DIS 4))



;; Convert the table into hash-tables:

(DEFUN TABLE-CONVERT-INTO-HASHTABLE (TABLE)
  "Returns a hash-table containing the data from the table."
  (LET ((HASH (MAKE-HASH-TABLE :TEST (FUNCTION EQUAL)
                               :SIZE (LENGTH TABLE))))
    (DOLIST (LINE TABLE)
      (SETF (GETHASH (STRING-UPCASE (SYMBOL-NAME (CAR LINE))) HASH) LINE))
    HASH));;TABLE-CONVERT-INTO-HASHTABLE


(DEFUN H-TABLE-FIND (H-TABLE KEY)
  (GETHASH (STRING-UPCASE (IF (STRINGP KEY) KEY (SYMBOL-NAME KEY))) H-TABLE));;H-TABLE-FIND

;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ACTIVITIES
;;;

;; An activity  is some program to  be run repeatitively  with a given
;; period.

(DEFVAR *CURRENT-ACTIVITY* NIL
  "The current activity.");;*CURRENT-ACTIVITY*

(DEFVAR *ACTIVITY-QUEUE* NIL
  "A sorted queue of activities, in scheduled order.");;*ACTIVITY-QUEUE*


(DEFCLASS ACTIVITY ()
  (
   (PERIOD
    :ACCESSOR PERIOD
    :INITARG :PERIOD
    :INITFORM 0
    :TYPE     NUMBER
    :DOCUMENTATION "The period of this activity, expressed in seconds.")
   (CLOSURE
    :ACCESSOR CLOSURE
    :INITARG :CLOSURE
    :INITFORM (FUNCTION (LAMBDA ()))
    :TYPE     FUNCTION
    :DOCUMENTATION "The closure to be run periodically for this activity.")
   (STATE
    :ACCESSOR STATE
    :INITARG :STATE
    :INITFORM :SUSPENDED
    :TYPE     (OR :SUSPENDED :ACTIVE :TERMINATED)
    :DOCUMENTATION
    "The state of this actity:
      :SUSPENDED   kept in queue but won't be run (=> NEXT-TIME = :NEVER)
      :ACTIVE      kept in queue, run and rescheduled.
      :TERMINATED  removed from the queue and forgotten.")
   (NEXT-TIME
    :ACCESSOR NEXT-TIME
    :INITARG :NEXT-TIME
    :INITFORM :NEVER
    :TYPE     (OR :NEVER NUMBER)
    :DOCUMENTATION "The next time this activity will run.")
   (KIND
    :ACCESSOR KIND
    :INITARG :KIND
    :INITFORM :ACTIVITY
    :DOCUMENTATION "A label, reserved to the client.")
   (NAME
    :ACCESSOR NAME
    :INITARG :NAME
    :INITFORM "ACTIVITY"
    :TYPE    STRING
    :DOCUMENTATION "A name string, reserved to the client.")
   )
  (:DOCUMENTATION "An activity to be scheduled."));;ACTIVITY


(DEFMETHOD PRINT-OBJECT ((SELF ACTIVITY) STREAM)
  "
"
  (LET ((LEVEL *PRINT-LEVEL*))
    (MACROLET ((PRINT-FIELD
                (NAME)
                `(FORMAT STREAM ":~A ~A " ',NAME (,NAME SELF)) ))
              (PRINT-UNREADABLE-OBJECT
               (SELF STREAM :TYPE 'ACTIVITY :IDENTITY T)
               (PRINT-FIELD PERIOD)
               (PRINT-FIELD STATE)
               (PRINT-FIELD NEXT-TIME)
               (PRINT-FIELD KIND)
               (PRINT-FIELD NAME)
               (FORMAT STREAM "~&   ")
               (PRINT-FIELD CLOSURE)))
    (SETQ *PRINT-LEVEL* LEVEL))
  );;PRINT-OBJECT



(DEFMETHOD COMPUTE-SCHEDULE ((SELF ACTIVITY))
  "
DO:             COMPUTE THE NEW NEXT-TIME, TRYING TO AVOID TIME DRIFT,
                BUT ENSURING THAT NEXT-TIME IS IN THE PRESENT-FUTURE.
SEE ALSO:       RESCHEDULE.
PRE:            (= LAST (NEXT-TIME SELF))
                (= NOW  (GET-UNIVERSAL-TIME))
POST:           (STATE SELF) ==> (MAX NOW (= (NEXT-TIME SELF)
                                              (+ LAST (PERIOD SELF))))
                (NOT (STATE SELF)) ==> (EQ (NEXT-TIME SELF) :NEVER)
"
  (CASE (STATE SELF)
    (:SUSPENDED
     (SETF (NEXT-TIME SELF) :NEVER))
    (:ACTIVE
     (LET* ((NOW (GET-UNIVERSAL-TIME))
            (ONT (NEXT-TIME SELF))
            (NT  (+ (IF (EQ :NEVER ONT) NOW (+ ONT (PERIOD SELF))))) )
       (IF (< NT NOW)
         (SETF (NEXT-TIME SELF) NOW)
         (SETF (NEXT-TIME SELF) NT))))
    )
  );;COMPUTE-SCHEDULE



(DEFUN ACTIVITY-DEQUEUE (ACTIVITY)
  "
POST:           (NOT (MEMBER ACTIVITY *ACTIVITY-QUEUE*))
"
  (IF (EQ ACTIVITY (CAR *ACTIVITY-QUEUE*))
    (POP *ACTIVITY-QUEUE*)
    (SETQ *ACTIVITY-QUEUE*
          (DELETE ACTIVITY *ACTIVITY-QUEUE* :TEST (FUNCTION EQ))))
  );;ACTIVITY-DEQUEUE


(DEFUN ACTIVITY-ENQUEUE (ACTIVITY)
  "
PRE:            (NOT (MEMBER ACTIVITY *ACTIVITY-QUEUE*))
POST:           (MEMBER ACTIVITY *ACTIVITY-QUEUE*)
NOTE:           If the ACTIVITY has to be scheduled at the same time than
                other activities, then it's scheduled after them so as
                to implement a round-robin scheduling.
"
  (DO* ((PREVIOUS (CONS NIL *ACTIVITY-QUEUE*))
        (PLACE    PREVIOUS (CDR PLACE))
        (NT       (NEXT-TIME ACTIVITY))
        (ONT      (IF (CDR PLACE) (NEXT-TIME (CADR PLACE)) :NEVER)
                  (IF (CDR PLACE) (NEXT-TIME (CADR PLACE)) :NEVER)) )
       ((OR (EQ ONT :NEVER) (AND (NOT (EQ NT :NEVER)) (< NT ONT)))
        (PROGN (PUSH ACTIVITY (CDR PLACE))
               (SETQ *ACTIVITY-QUEUE* (CDR PREVIOUS)))) )
  );;ACTIVITY-ENQUEUE


(DEFMETHOD SCHEDULE ((SELF ACTIVITY))
  "
PRE:            TERMINATED <=> (EQ :TERMINATED (STATE SELF))
DO:             Compute the new schedule and reorder the *ACTIVITY-QUEUE*.
POST:           (NOT TERMINATED) <=> (MEMBER ACTIVITY *ACTIVITY-QUEUE*)
"
  (ACTIVITY-DEQUEUE SELF)
  (UNLESS (EQ :TERMINATED (STATE SELF))
    (COMPUTE-SCHEDULE SELF)
    (ACTIVITY-ENQUEUE SELF))
  );;SCHEDULE


(DEFUN ACTIVITY-RUN (&KEY ONE-STEP DEBUG)
  "
DO:             Periodically run the activities in *ACTIVITY-QUEUE*.
"
  (DO ((HEAD (CAR *ACTIVITY-QUEUE*) (CAR *ACTIVITY-QUEUE*)))
      ((NULL HEAD))
    (LET* ((NT    (NEXT-TIME HEAD))
           (NOW   (GET-UNIVERSAL-TIME)))
      (WHEN (MEMBER :ACTIVITY-TIME DEBUG)
        (FORMAT *TRACE-OUTPUT* "~&NOW IS ~D~%~S~%" NOW HEAD))
      (WHEN (< NOW NT)
        (WHEN (MEMBER :ACTIVITY-TIME DEBUG)
          (FORMAT *TRACE-OUTPUT* "~&SLEEPING FOR ~D~%"  (- NT NOW)))
        (SLEEP (- NT NOW)))
      (SETQ *CURRENT-ACTIVITY* HEAD)
      (FUNCALL (CLOSURE *CURRENT-ACTIVITY*))
      (SETQ *CURRENT-ACTIVITY* NIL)
      (WHEN (MEMBER :ACTIVITY-QUEUE DEBUG)
        (FORMAT *TRACE-OUTPUT* "~&QUEUE BEFORE RE-SCHEDULING:~%~S~%"
                *ACTIVITY-QUEUE*))
      (SCHEDULE HEAD)
      (WHEN (MEMBER :ACTIVITY-QUEUE DEBUG)
        (FORMAT *TRACE-OUTPUT* "~&QUEUE AFTER  RE-SCHEDULING:~%~S~%"
                *ACTIVITY-QUEUE*))
      )
    (WHEN ONE-STEP (SETQ *ACTIVITY-QUEUE* NIL)))
  );;ACTIVITY-RUN

;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GET-CAM SCRIPT
;;;


(DEFUN PRINT-USAGE ()
  (FORMAT T "~%~
             ~&~A usage:~%~
             ~&~%~
             ~&    ~A [ --help | --test | --one-shoot  ] \\~%~
             ~&    ~VA [ --camera CAMNAME [ --keep PATH ] \\~%~
             ~&    ~VA  --display DISPNAME [ --display DISPNAME ]... ]... \\~%~
             ~&~%~
             ~&The following options may be abreviated:~%~
             ~&   --camera    -camera    -cam  -c~%~
             ~&   --display   -display   -dis  -d~%~
             ~&   --one-shoot -one-shoot -one  -o~%~
             ~&   --keep      -keep            -k~%~
             ~&   --test      -test            -t~%~
             ~&   --help      -help            -h~%~
             ~&~%~
             ~&Takes a list of cameras, & displays them each on a ~
               list of displays.~%~
             ~&Order of arguments is important. The syntax is:~%~
             ~&  [oneshoot] ( camera [keep] display... )...~%~
             ~&--help list the available cameras and displays too.~%~
             ~&--test don't do anything but prints what would be done.~%~
             " SCRIPT:*NAME* SCRIPT:*NAME*
             (LENGTH SCRIPT:*NAME*) ""
             (LENGTH SCRIPT:*NAME*) ""))


(DEFPARAMETER H-DISPLAYS NIL)
(DEFPARAMETER H-CAMERAS  NIL)


;; Global options:
(DEFPARAMETER TMPDIR   (FORMAT NIL "/tmp/~A.~D/"  SCRIPT:*NAME* (SCRIPT:PID)))
(DEFPARAMETER TICK     0)
(DEFPARAMETER ONESHOOT NIL)
(DEFPARAMETER BGCOLOR  "#49C")
(DEFPARAMETER DEBUG    ())

;; came came came
;; keep keep
;; disp disp

(DEFPARAMETER CAMES ())
(DEFPARAMETER KEEPS ())
(DEFPARAMETER DISPS ())


;; (GET KEEP 'CAMERA) == CAME
;; (GET DISP 'CAMERA) == CAME

(DEFPARAMETER SCREENS    NIL)


(DEFUN SPUT (SYMBOL KEY VALUE)
  (UNLESS (SYMBOLP SYMBOL)
    (SETQ SYMBOL (INTERN (STRING-UPCASE SYMBOL))))
  (SETF (GET SYMBOL KEY) VALUE));;SPUT


(DEFUN SGET (SYMBOL KEY)
  (UNLESS (SYMBOLP SYMBOL)
    (SETQ SYMBOL (INTERN (STRING-UPCASE SYMBOL))))
  (GET SYMBOL KEY));;SGET


(DEFUN BRACKET (STRING)
  (FORMAT NIL "[~A]~A" (SUBSEQ STRING 0 1) (SUBSEQ STRING 1))
  );;BRACKET


(DEFUN PROCESS-ARGUMENTS (ARGUMENTS)
  ;; Argument processing:
  (MACROLET ((OTHER-ARG ()
                        `(PROGN
                           (WHEN (NULL (CDR ARGS))
                             (FORMAT *ERROR-OUTPUT*
                                     "~A: Missing argument after '~A'."
                                     SCRIPT:*NAME* ARG)
                             (PRINT-USAGE)
                             (SCRIPT:EXIT SCRIPT:EX-USAGE))
                           (POP ARGS))))
    (DO* ((ARGS ARGUMENTS (CDR ARGS))
          (ARG (CAR ARGS) (CAR ARGS))
          (CAME NIL) (CAMLINE) (DISLINE)
          (KEEP) (DISP) )
        ((NULL ARGS))
      ;;(WHEN (SYMBOLP ARG)
      ;;  (ERROR "ARGUMENTS LIKE ~S MUST BE STRINGS, NOT SYMBOLS." ARG))
      (IF (AND (< 8 (LENGTH (SYMBOL-NAME ARG)))
               (STRING-EQUAL "--DEBUG-" (SUBSEQ (SYMBOL-NAME ARG) 0 8)))
        (PUSH (INTERN (SUBSEQ (SYMBOL-NAME ARG) 8)
                      (FIND-PACKAGE "KEYWORD")) DEBUG)
        (CASE ARG
          ((-T -TEST --TEST)
           (SETQ SCRIPT::TESTING T))

          ((-P --PROCESSES)
           (SCRIPT:SHELL (FORMAT NIL "ps ax | egrep '~A' | grep -v ' -p'"
                                 (BRACKET SCRIPT:*NAME*)))
           (SCRIPT:EXIT 0))

          ((--KILL)
           (SCRIPT:SHELL
            (FORMAT NIL "kill `ps ax | egrep '~A' | awk '{print $1}'`"
                    (BRACKET SCRIPT:*NAME*)))
           (SCRIPT:EXIT 0))

          ((-C -CAM -CAME -CAMERA --CAMERA)
           (OTHER-ARG)
           (SETQ CAME (CAR ARGS))
           (SETQ CAMLINE (H-TABLE-FIND H-CAMERAS CAME))
           (UNLESS CAMLINE
             (FORMAT *ERROR-OUTPUT*
                     "~A: No camera named: '~A'. Use --help to get list."
                     SCRIPT:*NAME* CAME)
             (SCRIPT:EXIT SCRIPT:EX-DATAERR))
           (PUSH CAME CAMES))

          ((-K -KEEP --KEEP)
           (WHEN (NULL CAME)
             (FORMAT *ERROR-OUTPUT*
                     "~A: Syntax error: --keep before any --camera."
                     SCRIPT:*NAME*)
             (PRINT-USAGE)
             (SCRIPT:EXIT SCRIPT:EX-USAGE))
           (OTHER-ARG)
           (SETQ KEEP (CAR ARGS))
           (PUSH KEEP KEEPS)
           ;; We store the keep into the camera because keeps are PATHS.
           (SPUT CAME 'KEEP KEEP))

          ((-D -DIS -DISP -DISPLAY --DISPLAY)
           (WHEN (NULL CAME)
             (FORMAT *ERROR-OUTPUT*
                     "~A: Syntax error: --display before any --camera."
                     SCRIPT:*NAME*)
             (PRINT-USAGE)
             (SCRIPT:EXIT SCRIPT:EX-USAGE))
           (OTHER-ARG)
           (SETQ DISP (CAR ARGS))
           (SETQ DISLINE (H-TABLE-FIND H-DISPLAYS DISP))
           (WHEN (NULL DISLINE)
             (FORMAT *ERROR-OUTPUT*
                     "~A: No display named: '~A'. Use --help to get list."
                     SCRIPT:*NAME* DISP)
             (SCRIPT:EXIT SCRIPT:EX-DATAERR))
           (PUSH DISP DISPS)
           (SPUT DISP 'CAMERA CAME))

          ((-O -ONE-SHOOT --ONE-SHOOT)
           (SETQ ONESHOOT T))

          ((-H -HELP --HELP)
           (PRINT-USAGE)
           (TABLE-DISPLAY CAMTITLE CAMERAS)
           (TABLE-DISPLAY DISTITLE DISPLAYS)
           (SCRIPT:EXIT SCRIPT:EX-OK))

          (OTHERWISE
           (FORMAT *ERROR-OUTPUT*  "~A: Invalid option '~A'."
                   SCRIPT:*NAME* ARG)
           (PRINT-USAGE)
           (SCRIPT:EXIT SCRIPT:EX-USAGE))))
      )) ;;DO*;;MACROLET
  (UNLESS CAMES
    (FORMAT *ERROR-OUTPUT*  "~A: Missing a camera." SCRIPT:*NAME*)
    (PRINT-USAGE)
    (SCRIPT:EXIT SCRIPT:EX-USAGE))

  (UNLESS (OR KEEPS DISPS)
    (FORMAT *ERROR-OUTPUT*  "~A: Missing a display or a keep.~%" SCRIPT:*NAME*)
    (PRINT-USAGE)
    (SCRIPT:EXIT SCRIPT:EX-USAGE))
  );;PROCESS-ARGUMENTS


(DEFUN PREPARE (CAMES DISPS)
  (DO* ((REST-CAME CAMES (CDR REST-CAME))
        (CAME (CAR REST-CAME) (CAR REST-CAME))
        (CAMLINE) (URL) (EXT))
      ((NULL CAME))
    (SETQ CAMLINE (H-TABLE-FIND H-CAMERAS CAME))
    (SETQ URL     (CAM-URL CAMLINE))
    (SETQ EXT     (UNIX-PATH-EXTENSION URL))
    (SPUT CAME 'EXT EXT))
  (SETQ SCREENS (MAKE-HASH-TABLE :TEST (FUNCTION EQUAL)))
  ;; X-SCREEN --> (X-SCREEN DISP...)
  (DOLIST (DISP DISPS)
    (LET* ((DISLINE (H-TABLE-FIND H-DISPLAYS DISP))
           (DWIDTH  (DIS-WIDTH  DISLINE))
           (DHEIGHT (DIS-HEIGHT DISLINE))
           (CAME    (SGET DISP  'CAMERA))
           (CAMLINE (H-TABLE-FIND H-CAMERAS CAME))
           (CWIDTH  (CAM-WIDTH  CAMLINE))
           (CHEIGHT (CAM-HEIGHT CAMLINE))
           (XZOOM   (FLOAT (/ (* 100 DWIDTH ) CWIDTH )))
           (YZOOM   (FLOAT (/ (* 100 DHEIGHT) CHEIGHT)))

           (X-SCREEN (DIS-SCREEN DISLINE))
           (ENTRY    (GETHASH X-SCREEN SCREENS)) )
      (SPUT DISP :X-ZOOM XZOOM)
      (SPUT DISP :Y-ZOOM YZOOM)
      (IF ENTRY
        (PUSH DISP (CDR ENTRY))
        (SETF (GETHASH X-SCREEN SCREENS) (LIST X-SCREEN DISP))) ))
  );;PREPARE



(DEFUN CMD (&REST COMMAND)
  (IF SCRIPT::TESTING
    (FORMAT T "~&~A~%" COMMAND)
    (APPLY 'SCRIPT:EXECUTE COMMAND))
  );;CMD



(DEFUN GET-AND-DISPATCH (CAMES SCREENS ONE-SHOOT)
  ;; get activities:
  (DOLIST (CAME CAMES)
    (WHEN (MEMBER :GET-DISPLAY DEBUG)
      (FORMAT *TRACE-OUTPUT* "~&CAME=~S~%" CAME))
    (LET* ((CAMLINE      (H-TABLE-FIND H-CAMERAS CAME))
           (PERIOD       (CAM-PERIOD CAMLINE)))
      (SCHEDULE
       (MAKE-INSTANCE
        'ACTIVITY
        :PERIOD PERIOD
        :STATE :ACTIVE
        :NEXT-TIME 0
        :KIND :GET
        :CLOSURE
        (LET ((INDEX   0)
              (CAME    CAME)
              (CAMLINE CAMLINE))
          (LAMBDA ()
            (LET* ((OPTIONS      (CAM-OPTIONS CAMLINE))
                   (URL          (CAM-URL CAMLINE))
                   (EXT          (SGET CAME 'EXT))
                   (PICTURE-NUMB (FORMAT NIL "~6,'0D" (INCF INDEX)))
                   (KEEP         (SGET CAME 'KEEP))
                   (PICTURE-FILE
                    (IF KEEP
                      (FORMAT NIL "~A~A~A" KEEP PICTURE-NUMB EXT)
                      (FORMAT NIL "~A~A~A" TMPDIR CAME EXT))))
              (WHEN (MEMBER :GET-DISPLAY DEBUG)
                (FORMAT *TRACE-OUTPUT* "~&GETTING ~D ~A~%" INDEX CAMLINE)
                (FORMAT *TRACE-OUTPUT* "~&GETTING ~A~%" PICTURE-FILE))
              (CMD "wget"
                   "-o" (FORMAT NIL "~A.log" PICTURE-FILE)
                   "-O" PICTURE-FILE URL)
              (MULTIPLE-VALUE-BIND (FUN PRESENT)
                  (PLIST-GET OPTIONS :CONVERT-FUNCTION)
                (WHEN PRESENT
                  (SETQ PICTURE-FILE (FUNCALL (EVAL FUN) PICTURE-FILE))))
              (SPUT CAME 'PICTURE-FILE PICTURE-FILE)  )
            (WHEN ONE-SHOOT (SETF (STATE *CURRENT-ACTIVITY*) :TERMINATED))
            ))
        ))))
  ;; dispatch activities:
  ;; --------------------
  ;; TODO: For now, we just have one activity updating all the screens,
  ;; TODO: but we should have one activity per screen with possibly a
  ;; TODO: different period, depending on the period of the cameras.
  (SCHEDULE
   (MAKE-INSTANCE
    'ACTIVITY
    :PERIOD (APPLY (IF ONE-SHOOT (FUNCTION MAX) (FUNCTION MIN))
                   (MAPCAR (LAMBDA (CAME)
                             (CAM-PERIOD (H-TABLE-FIND H-CAMERAS CAME)))
                           CAMES))
    :STATE :ACTIVE
    :KIND :DISPLAY
    :CLOSURE
    (LAMBDA ()
      (LET ((SAS ()))
        ;; FIRST BUILD THE XLIARGS LISTS.
        (MAPHASH (LAMBDA (XSCREEN SLINE)
                   (LET ((XLIARGS (CONS XSCREEN NIL)))
                     (DOLIST (DISP (CDR SLINE))
                       (LET* ((DISLINE  (H-TABLE-FIND H-DISPLAYS DISP))
                              (OFFSET   (DIS-OFFSET DISLINE))
                              (CAME     (SGET DISP 'CAMERA))
                              (CAMLINE  (H-TABLE-FIND H-CAMERAS CAME))
                              (OPTIONS  (CAM-OPTIONS CAMLINE))
                              (X-ZOOM   (SGET DISP :X-ZOOM))
                              (Y-ZOOM   (SGET DISP :Y-ZOOM))
                              (PICTURE-FILE (SGET CAME 'PICTURE-FILE)))
                         (NCONC XLIARGS (LIST "-newoptions" "-at" OFFSET))
                         (NCONC XLIARGS
                                (IF (PLIST-GET OPTIONS :DO-NOT-ZOOM)
                                  (LIST "-xzoom" "100.0" "-yzoom" "100.0")
                                  (LIST
                                   "-smooth" "-smooth"
                                   "-xzoom" (FORMAT NIL "~,3F" X-ZOOM)
                                   "-yzoom" (FORMAT NIL "~,3F" Y-ZOOM))))
                         (NCONC XLIARGS (LIST PICTURE-FILE))))
                     (PUSH XLIARGS SAS)))
                 SCREENS)
        ;; THEN, RUN THE XLI COMMANDS.
        (DO* ((SAS SAS (CDR SAS))
              (XSCREEN (CAAR SAS) (CAAR SAS))
              (XLIARGS (CDAR SAS) (CDAR SAS)))
            ((NULL SAS))
          (WHEN (MEMBER :GET-DISPLAY DEBUG)
            (FORMAT *TRACE-OUTPUT* "~&DISPLAYING ~A~%"
                    (LIST  "xli" "-display" XSCREEN "-quiet"
                           "-onroot" "-border" BGCOLOR XLIARGS) ))
          (APPLY 'CMD "xli" "-display" XSCREEN "-quiet"
                 "-onroot" "-border" BGCOLOR XLIARGS)
          ))
      (WHEN (NOTANY (LAMBDA (ACTIVITY) (EQ :GET (KIND ACTIVITY)))
                    *ACTIVITY-QUEUE*)
        (SETF (STATE *CURRENT-ACTIVITY*) :TERMINATED))
      )
    ))
  (ACTIVITY-RUN :DEBUG DEBUG)
  );;GET-AND-DISPATCH



(DEFUN MAIN (ARGUMENTS)
  (CATCH :EXIT
    (SETQ SCRIPT::TESTING NIL)
    (SETQ TICK       0)
    (SETQ ONESHOOT   NIL)
    (SETQ CAMES      ())
    (SETQ KEEPS      ())
    (SETQ DISPS      ())
    (SETQ SCREENS    NIL)
    (SETQ H-DISPLAYS (TABLE-CONVERT-INTO-HASHTABLE DISPLAYS))
    (SETQ H-CAMERAS  (TABLE-CONVERT-INTO-HASHTABLE CAMERAS))
;;;     (SETQ DISPLAYS (SORT DISPLAYS (LAMBDA (A B)
;;;                                     (STRING-LESSP (SYMBOL-NAME (CAR A))
;;;                                                   (SYMBOL-NAME (CAR B))))))
    (SETQ CAMERAS  (SORT CAMERAS
                         (LAMBDA (A B)
                           (IF (= (CAM-ZONE A) (CAM-ZONE B))
                             (STRING-LESSP (SYMBOL-NAME (CAR A))
                                           (SYMBOL-NAME (CAR B)))
                             (< (CAM-ZONE A) (CAM-ZONE B))))))
    (PROCESS-ARGUMENTS ARGUMENTS)
    (PREPARE CAMES DISPS)
    (UNWIND-PROTECT
         (PROGN
           #+(or)(SCRIPT:MAKE-DIRECTORY TMPDIR)
           (ensure-directories-exist (make-pathname :name "TEST"
                                                    :defaults TMPDIR))
           ;; Let's start working for real...
           (GET-AND-DISPATCH CAMES SCREENS ONESHOOT))
      (SCRIPT:EXECUTE "rm" "-rf" TMPDIR) )
    (SCRIPT:EXIT SCRIPT:EX-OK)));;MAIN


(WHEN (SCRIPT:IS-RUNNING)
  (MAIN (MAPCAR (LAMBDA (A) (WITH-INPUT-FROM-STRING (S A) (READ S)))
                SCRIPT:*ARGUMENTS*)))

;;;
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; get-cams                         --                     --          ;;;;
