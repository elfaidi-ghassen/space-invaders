;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1)
(define BASE-DX 1)

(define TANK-Y (- HEIGHT 12))


(define TANK-SPEED 3)
(define MISSILE-SPEED -10)

(define HIT-RANGE 10)

(define INVADE-RATE 2)  ; precentage

(define BACKGROUND (empty-scene WIDTH HEIGHT))
;(define BACKGROUND )

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

(define I1 (make-invader 150 100 -10))           ;not landed, moving right
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
   (add-random-inv (rm-if-hit (update-invaders-positions (game-invaders game)) (game-missiles game)))
   (filter-mis(update-missiles-positions (game-missiles game)))
   (update-tank-position (game-tank game))
   )
  )

;; ListOfInvader -> ListOfInvader
;; produce an update list of invaders, changing x, y positions
(check-expect (update-invaders-positions empty) empty)
(check-expect (update-invaders-positions (list (make-invader 150 100 BASE-DX))) (list (make-invader (+ 150 (* BASE-DX INVADER-X-SPEED)) (+ 100 (abs (* BASE-DX INVADER-Y-SPEED))) BASE-DX)))
(check-expect (update-invaders-positions (list (make-invader WIDTH 100 BASE-DX))) (list (make-invader (- WIDTH (* BASE-DX INVADER-X-SPEED)) (+ 100 (abs (* BASE-DX INVADER-Y-SPEED))) (- BASE-DX))))
(check-expect (update-invaders-positions (list (make-invader 0 100 (- BASE-DX)))) (list (make-invader (+ 0 (* BASE-DX INVADER-X-SPEED)) (+ 100 (abs (* BASE-DX INVADER-Y-SPEED))) BASE-DX)))
(check-expect (update-invaders-positions (list (make-invader 150 100 BASE-DX) (make-invader WIDTH 100 BASE-DX)))
              (list (make-invader (+ 150 (* BASE-DX INVADER-X-SPEED)) (+ 100 (abs (* BASE-DX INVADER-Y-SPEED))) BASE-DX) (make-invader (- WIDTH (* BASE-DX INVADER-X-SPEED)) (+ 100 (abs (* BASE-DX INVADER-Y-SPEED))) (- BASE-DX))))


; (define (update-invaders-positions loi) loi)
(define (update-invaders-positions loi)
  (cond [(empty? loi) empty]
        [else
         (cons (update-inv-pos (first loi)) (update-invaders-positions (rest loi)))]))




;; Invader -> Invader
;; update the position of one invader
;(define (update-inv-pos inv) inv)
(define (update-inv-pos invader)
  (cond [(<= (invader-x invader) 0) (make-invader (+ (invader-x invader) (* (abs(invader-dx invader)) INVADER-X-SPEED)) (+ (invader-y invader) (abs (* BASE-DX INVADER-Y-SPEED))) (abs(invader-dx invader)))]
        [(>= (invader-x invader) WIDTH) (make-invader (- (invader-x invader) (* (abs(invader-dx invader)) INVADER-X-SPEED)) (+ (invader-y invader) (abs (* BASE-DX INVADER-Y-SPEED))) (-(abs(invader-dx invader))))]
        [else (make-invader (+ (invader-x invader) (* (invader-dx invader) INVADER-X-SPEED)) (+ (invader-y invader) (abs (* BASE-DX INVADER-Y-SPEED))) (invader-dx invader))]
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

(define (add-random-inv loi)
  (cond [(> (random 100) INVADE-RATE) loi]
        [else (cons (make-invader (+ (/ WIDTH 5) (random (- WIDTH (/ WIDTH 2.5)))) 0 (randomPlusMinus BASE-DX)) loi)]
        )
  )

;; Number -> Number
;; randomy produce a negative or positive version of the given number
(define (randomPlusMinus n)
  (cond [(= (random 2) 1) n]
        [else (- n)])
  )



;; ListOfMissile -> ListOfMissile
;; remove missile if it Y > HEIGHT
(check-expect (filter-mis empty) empty)
(check-expect (filter-mis (list (make-missile 100 100))) (list (make-missile 100 100)))
(check-expect (filter-mis (list (make-missile 100 HEIGHT))) empty)
(check-expect (filter-mis (list (make-missile 100 HEIGHT) (make-missile 100 100))) (list (make-missile 100 100)))

; (define (filter-mis lom) lom)

(define (filter-mis lom)
  (cond [(empty? lom) empty]
        [else
         (cond [(>= (missile-y (first lom)) HEIGHT) (filter-mis (rest lom))]
               [else (cons (first lom) (filter-mis (rest lom)))])
         
         
         ]))


;; ListOfMissile -> ListOfMissile
;; update y position of all missiles
(check-expect (update-missiles-positions empty) empty)
(check-expect (update-missiles-positions (list (make-missile 100 100))) (list (make-missile 100 (+ 100 MISSILE-SPEED))))
(check-expect (update-missiles-positions (list (make-missile 100 100) (make-missile 50 70))) (list (make-missile 100 (+ 100 MISSILE-SPEED)) (make-missile 50 (+ 70 MISSILE-SPEED))))

;; (define (update-missiles-positions lom) lom)

(define (update-missiles-positions lom)
  (cond [(empty? lom) empty]
        [else
         (cons (update-one-mis (first lom)) (update-missiles-positions (rest lom)))]))


;; Missile -> Missile
;; update the position of one missile

; (define (update-one-mis m) m)

(define (update-one-mis m)
  (make-missile (missile-x m) (+ (missile-y m) MISSILE-SPEED)))

;; Tank -> Tank
;; update x position of the tank
(check-expect (update-tank-position (make-tank 50 1)) (make-tank (+ 50 (* TANK-SPEED 1)) 1))
(check-expect (update-tank-position (make-tank 50 -1)) (make-tank (+ 50 (* TANK-SPEED -1)) -1))
(check-expect (update-tank-position (make-tank WIDTH 1)) (make-tank WIDTH 1))
(check-expect (update-tank-position (make-tank 0 -1)) (make-tank 0 -1))

;; (define (update-tank-position t) t)

(define (update-tank-position t)
  (cond [(and (>= (tank-x t) WIDTH) (= (tank-dir t) 1)) t]
        [(and (<= (tank-x t) 0) (= (tank-dir t) -1))    t]
        [else (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]
        )
  )

;; game -> Image
;; render the next game scence, with updated Invaders, Missiles and Tank
(check-expect (render (make-game
                      empty
                      empty
                      (make-tank (/ WIDTH 2) 1)
                      ))
              (place-image TANK (/ WIDTH 2) TANK-Y BACKGROUND)
)
(check-expect (render (make-game
                      (list (make-invader 100 100 BASE-DX))
                      empty
                      (make-tank (/ WIDTH 2) 1)
                      ))
              (place-image INVADER 100 100 
               (place-image TANK (/ WIDTH 2) TANK-Y BACKGROUND))
)
(check-expect (render (make-game
                      (list (make-invader 100 100 BASE-DX) (make-invader 200 300 BASE-DX))
                      empty
                      (make-tank (/ WIDTH 2) 1)
                      ))
              (place-image INVADER 200 300 
               (place-image INVADER 100 100 
                 (place-image TANK (/ WIDTH 2) TANK-Y BACKGROUND)))
)

(check-expect (render (make-game
                      (list (make-invader 100 100 BASE-DX) (make-invader 200 300 BASE-DX))
                      (list (make-missile 200 100) (make-missile 50 75))
                      (make-tank (/ WIDTH 2) 1)
                      ))
              (place-image MISSILE 50 75
               (place-image MISSILE 200 100
                (place-image INVADER 200 300 
                 (place-image INVADER 100 100 
                  (place-image TANK (/ WIDTH 2) TANK-Y BACKGROUND))))))



(define (render game)
  (render-missiles (game-missiles game) (render-invaders (game-invaders game)
                                                         (render-tank (game-tank game) BACKGROUND)
                                                         ))
  )

;; Tank Image -> Image
;; render Tank on the background
(define (render-tank t img)
  (place-image TANK (tank-x t) TANK-Y img)
 )



;; ListOfInvader Image -> Image
;; render invaders on img
; (define (render-invaders loi img) img)

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (place-image INVADER (invader-x (first loi)) (invader-y (first loi)) (render-invaders (rest loi) img))]))




;; ListOfMissile Image -> Image
;; render missiles on img
;(define (render-missiles lom img) img)

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (place-image MISSILE (missile-x (first lom)) (missile-y (first lom)) (render-missiles (rest lom) img))]))




; game -> Boolean
;; produce true if any invader touches the ground

(check-expect (lose? (make-game empty (list M1) T1)) false)
(check-expect (lose? (make-game (list I1) (list M1) T1)) false)
(check-expect (lose?(make-game (list I2) (list M1) T1)) true)
(check-expect (lose?(make-game (list I3) (list M1) T1)) true)
;;(define (lose? game) false)
(define (lose? s)
  (beyond-height? (game-invaders s))
  )

;; ListOfInvaders -> Boolean
;; produce true if any invader's y position is >= HEIGHT

(define (beyond-height? loi)
  (cond [(empty? loi) false]
        [else
         (or (>= (invader-y (first loi)) HEIGHT) (beyond-height? (rest loi)))]))

;; game KeyEvent -> game
;; produce 
;; (define (key-handler game key) ...)

(check-expect (key-handler
               (make-game empty empty (make-tank 100 -1))
               "right"
               )
              (make-game empty empty (make-tank 100 1))
              )

(check-expect (key-handler
               (make-game empty empty (make-tank 100 1))
               "left"
               )
              (make-game empty empty (make-tank 100 -1))
              )
(check-expect (key-handler
               (make-game empty empty (make-tank 100 1))
               ""
               )
              (make-game empty empty (make-tank 100 1))
              )

(check-expect (key-handler
               (make-game empty empty (make-tank 100 1))
               " "
               )
              (make-game empty (list (make-missile 100 (- TANK-Y 20))) (make-tank 100 1))
              )




(define (key-handler s key)
  (make-game
       (game-invaders s)
       (if (string=? key " ")
           (shoot (game-missiles s) (tank-x (game-tank s)))
           (game-missiles s)
           )

       (cond
         [(string=? key "left")(set-direction (game-tank s) -1)]
         [(string=? key "right") (set-direction (game-tank s) 1)]
         [else  (game-tank s)]
         )
       ))



;; ListOfMissile Number Number -> ListOfMissiles
;; Add a missile at x position

;; (define (shoot lom x) lom)
(define (shoot lom x)
  (cons (make-missile x (- TANK-Y 20)) lom)
  )

;; Tank Number -> Tank
;; update the tank's position
;;(define (set-direction t dir)t)

(define (set-direction t dir)
  (make-tank (tank-x t) dir))


(define G0 (make-game empty empty T0))