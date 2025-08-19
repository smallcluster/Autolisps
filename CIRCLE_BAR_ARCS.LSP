; Author: Pierre JAFFUER
; Date: 19/08/2025
; Version: 1.0
; License: The Unlicense (https://unlicense.org/)
; Description (FR) : 
;     Dessine des aciers circulaires (dans le sens trigonométrique) sous autocad en tenant compte de la longueur
;     de la barre, du centre du cercle et de la longueur de recouvrement.
;     La dernière barre est coupée si nécessaire ou une eclisse de recouvrement est ajoutée
;     si le recouvrement sur la première barre est insuffisant (une alerte est émise dans ce cas).
;     Testé sous AutoCAD 2014.
; Description (EN): 
;     Draws circular rebar (counter-clockwise) in AutoCAD considering the length of the bar,
;     the center of the circle, and the overlap length.
;     Last bar length is trimmed if necessary or an overlap splice is added
;     if the overlap on the first bar is insufficient (a wraning is issued in this case).
;     Tested under AutoCAD 2014.

(defun c:barc_r(/ pcenter pstart overlap len)
(setvar "cmdecho"0)
  
;----------------------------------------------------------------------------
; PROGRAM INPUTS
;----------------------------------------------------------------------------
(setq pcenter (getpoint"\nCentre du rayon : ")) ; Center point of the circle
(setq pstart  (getpoint"\nReference <Autre point sur le cercle - Depard de la barre> : ")) ; Reference point on the circle (start of the bar)
(setq overlap (getreal "\nLongueur de recouvrement (en metre) : ")) ; Length of the overlap (in meters)
(setq len     (getreal "\nLongueur de la barre (en metre) : ")) ; Length of the bar (in meters)


;----------------------------------------------------------------------------
; VARIABLES
;----------------------------------------------------------------------------
; Constants
(setq R (distance pcenter pstart))
(setq REC_ANGLE (/ overlap R))
(setq ARC_ANGLE (/ len R))
(setq ROT_ANGLE (- ARC_ANGLE REC_ANGLE))
(setq ROT_LEN (- len overlap))
(setq FINAL_ANGLE (+ (* 2 pi) REC_ANGLE))
(setq FINAL_LEN (* R FINAL_ANGLE))
(setq CIRCLE_LEN (* 2 (* pi R)))

; Mutables
(setq total_len 0)
(setq arc_start pstart)
(setq color_idx 1)

;----------------------------------------------------------------------------
; FUNCTIONS
;----------------------------------------------------------------------------
  
; Draws an colored arc around the user issued center point.
; Each call to this function will change the color index of the arc (ranging from 1 to 6).
(defun colored_arc (start end)
  (command "_arc" start "_c" pcenter end)
  (if (> (+ color_idx 1) 6)
		(setq color_idx 1)
		(setq color_idx (+ color_idx 1))
	)
	(command "_.ChProp" (entlast) "" "_C" color_idx "")
)

; Rotates a vector.
(defun vecrot (v angle_rad)
	(list 
		(- (* (car v) (cos angle_rad)) (* (cadr v) (sin angle_rad))) 
		(+ (* (car v) (sin angle_rad)) (* (cadr v) (cos angle_rad)))
	)
)
  
; Adds two vectors.
(defun vecadd (v1 v2)
	(list (+ (car v1) (car v2)) (+ (cadr v1) (cadr v2)))
)
  
; Substracts two vectors.
(defun vecsub (v1 v2)
	(list (- (car v1) (car v2)) (- (cadr v1) (cadr v2)))
)
  
; Rotate a point around the user issued center point.
(defun ptrot (point angle_rad)
	(vecadd pcenter (vecrot (vecsub point pcenter) angle_rad))
)

;----------------------------------------------------------------------------
; PROGRAM BODY
;----------------------------------------------------------------------------

; Draws all the arcs excluding the last one.
(while (<= (+ len total_len) CIRCLE_LEN)
  (setq arc_end (ptrot arc_start ARC_ANGLE))
  (colored_arc arc_start arc_end)
  ; Update the next arc start point.
	(setq arc_start (ptrot arc_start ROT_ANGLE))
	; Update the length of the total arcs drawn so far (ignoring overlaps).
	(setq total_len (+ total_len ROT_LEN))
)
  
; Now let's see how to place the last bar:
; 1st case: 
;    the missing arc produces an exact or longer overlap.
;    In this case, the bar is trimmed as needed.
; 2nd case: 
;    the missing arc does NOT overlap the first bar enough.
;    In this case, the bar is trimmed up to the start of the first bar,
;    then a splice is added with a uniform overlap before and after
;    the starting point (to be adjusted manually if needed)
  
(if ( >= (+ len total_len) FINAL_LEN)
	(progn	
		(setq arc_end (ptrot pstart REC_ANGLE))
    (colored_arc arc_start arc_end)
	)
	(progn
    (colored_arc arc_start pstart)
		(setq splice_start (ptrot pstart (- 0.0 REC_ANGLE)))
		(setq splice_end (ptrot pstart REC_ANGLE))
		(colored_arc splice_start splice_end)
		(alert "ATTENTION: eclisse de recouvrement ajoutée, merci de l'ajuster au besoin !")
	)
)
(setvar "cmdecho"1)
)
