;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 3)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 2)
(define BASE-DX 5)



(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body


(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 10))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; ListOfInvaders is one of:
;; - empty
;; - (cons Invader ListOfInvaders)
;; interp. a list of (make-invader Number Number Number)

#;
(define (fn-for-ListOfInvaders loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invaders (first loi)) (fn-for-ListOfInvaders (rest loi)))]))








(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))




;; ListOfMissile is one of:
;; - empty
;; - (cons Invader ListOfMissile)
;; interp. a list of (make-missile Number Number)

#;
(define (fn-for-ListOfMissile lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom)) (fn-for-ListOfMissile (rest lom)))]))






;; Functions


;; Functions:
;; game -> game
;; start the world with (make-game empty empty T0)
;; 	
(define (main game)
  (big-bang game ; game
    (on-tick update) ; game -> game
    (to-draw render) ; game -> Image
    (stop-when lose?) ; game -> Boolean
    (on-key key-handler))) ; game KeyEvent -> game
;; game -> game
;; produce the next game,
;; updating the invaders(x,y) and missiles(y), and removing invaders if hit, and adding new invaders

;(check-expect (update G0) (make-game empty empty (update-tank-position T0)))
;(check-expect (update G2) (make-game (add-random-inv (rm-if-hit (update-invaders-positions (list I1)) (list M1)))    (update-missiles-positions(list M1))   (update-tank-position T1)))

(define (update game) 
  (make-game
    (filter-inv (add-random-inv (rm-if-hit (update-invaders-positions (list I1)) (list M1))))
   (filter-mis(update-missiles-positions(list M1)))
   (update-tank-position T1)
  )
  )

;; ListOfInvader -> ListOfInvader
;; produce an update list of invaders, changing x, y positions
(check-expect (update-invaders-positions empty) empty)
(check-expect (update-invaders-positions (list (make-invader 150 100 BASE-DX))) (list (make-invader (+ 150 (* BASE-DX INVADER-X-SPEED)) (- 100 (abs (* BASE-DX INVADER-Y-SPEED))) BASE-DX)))
(check-expect (update-invaders-positions (list (make-invader WIDTH 100 BASE-DX))) (list (make-invader (- WIDTH (* BASE-DX INVADER-X-SPEED)) (- 100 (abs (* BASE-DX INVADER-Y-SPEED))) (- BASE-DX))))
(check-expect (update-invaders-positions (list (make-invader 0 100 (- BASE-DX)))) (list (make-invader (+ 0 (* BASE-DX INVADER-X-SPEED)) (- 100 (abs (* BASE-DX INVADER-Y-SPEED))) BASE-DX)))
(check-expect (update-invaders-positions (list (make-invader 150 100 BASE-DX) (make-invader WIDTH 100 BASE-DX)))
              (list (make-invader (+ 150 (* BASE-DX INVADER-X-SPEED)) (- 100 (abs (* BASE-DX INVADER-Y-SPEED))) BASE-DX) (make-invader (- WIDTH (* BASE-DX INVADER-X-SPEED)) (- 100 (abs (* BASE-DX INVADER-Y-SPEED))) (- BASE-DX))))


; (define (update-invaders-positions loi) loi)
(define (update-invaders-positions loi)
  (cond [(empty? loi) empty]
        [else
         (cons (update-inv-pos (first loi)) (update-invaders-positions (rest loi)))]))




;; Invader -> Invader
;; update the position of one invader
;(define (update-inv-pos inv) inv)
(define (update-inv-pos invader)
  (cond [(<= (invader-x invader) 0) (make-invader (+ (invader-x invader) (* BASE-DX INVADER-X-SPEED)) (- (invader-y invader) (abs (* BASE-DX INVADER-Y-SPEED))) (- (invader-dx invader)))]
        [(>= (invader-x invader) WIDTH) (make-invader (- (invader-x invader) (* BASE-DX INVADER-X-SPEED)) (- (invader-y invader) (abs (* BASE-DX INVADER-Y-SPEED))) (- (invader-dx invader)))]
        [else (make-invader (+ (invader-x invader) (* BASE-DX INVADER-X-SPEED)) (- (invader-y invader) (abs (* BASE-DX INVADER-Y-SPEED))) (invader-dx invader))]
        )
  )


;; ListOfInvader ListOfMissiles -> ListOfInvader
;; remove the invader if it got hit by a missile
(check-expect (rm-if-hit
               empty
               empty
               )
              empty
              )

(check-expect (rm-if-hit
               (list (make-invader 100 100 10))
               (list (make-missile 100 100))
               )
              empty
              )

(check-expect (rm-if-hit
               (list (make-invader 100 100 10))
               (list (make-missile 100 110))
               )
              empty
              )
(check-expect (rm-if-hit
               (list (make-invader 100 100 10))
               (list (make-missile (+ 100 HIT-RANGE) (+ 100 HIT-RANGE)))
               )
              empty
              )
(check-expect (rm-if-hit
               (list (make-invader 100 100 10))
               (list (make-missile (- 100 HIT-RANGE) (- 100 HIT-RANGE)))
               )
              empty
              )


(check-expect (rm-if-hit
               (list (make-invader 100 100 10))
               (list (make-missile 50 100))
               )
              (list (make-invader 100 100 10))
              )
(check-expect (rm-if-hit
               (list (make-invader 100 100 10))
               (list (make-missile 100 150))
               )
              (list (make-invader 100 100 10))
              )

(check-expect (rm-if-hit
               (list (make-invader 50 100 10) (make-invader 100 100 10))
               (list (make-missile 100 100))
               )
              (list (make-invader 50 100 10))
              )

; (define (rm-if-hit loi lom) loi)

(define (rm-if-hit loi lom)
  (cond [(or (empty? loi) (empty? loi)) loi]
        [else
         (if (in-range (first loi) lom)
             (rm-if-hit (rest loi) lom)
             (cons (first loi) (rm-if-hit (rest loi) lom))
             )]))

;; Invader ListOfMissile -> Boolean
;; produce true if the invader is in the missile's range

;(define (in-range inv lom) true)
(define (in-range inv lom)
  (cond [(empty? lom) false]
        [else
         (if (and (<= (abs (- (invader-x inv) (missile-x (first lom)))) HIT-RANGE)
                  (<= (abs (- (invader-y inv) (missile-y (first lom)))) HIT-RANGE)
                  )
             true
             (or false (in-range inv (rest lom)))
             )
         ]))


;; ListOfInvader -> ListOfInvader
;; randomly add an invader after a each while
;; !!!
(define (add-random-inv loi) loi)


;; ListOfInvader -> ListOfInvader
;; remove invader if it landed
(define (filter-inv loi) loi)


;; ListOfMissile -> ListOfMissile
;; remove missile if it Y > HEIGHT
(define (filter-mis lom) lom)

;; ListOfMissile -> ListOfMissile
;; update y position of all missiles
(define (update-missiles-positions lom) lom)

;; Tank -> Tank
;; update x position of the tank
(define (update-tank-position t) t)


;; game -> Image
;; render ... 	
;; !!!
(define (render game) ...)

; game -> Boolean
;; produce ...
;; !!!
(define (lose? game) ...)

;; game KeyEvent -> game
;; produce ...
;; !!!
(define (key-handler game key) ....)

;
;(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
;(define T1 (make-tank 50 1))            ;going right
;(define T2 (make-tank 50 -1))           ;going left

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))