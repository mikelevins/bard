;;; Nelson 1.0 puzzles

(define-puzzle "Medical Voyage: Navigation"
  (puzzle 
   name: "Medical Voyage: Navigation"
   title: "Medical Voyage: Navigation"
   backdrops: [(sprite 
                name: "backdrop"
                image: "medical-voyage-navigation-backdrop"
                center: (variant
                         iPad: (point x: 512 y: 384)
                         iPhone: (point x: 240 y: 180)))
               (sprite
                name: "scale"
                image: "navigation-scale"
                center: (variant
                         iPad: (point x: 882 y: 27)
                         iPhone: (point x: 441 y: 14)))]
   HUD: [(sprite
          name: "clock frame"
          image: "clock-frame01"
          center: (variant
                   iPad: (point x: 136 y: 710)
                   iPhone: (point x: 64 y: 285)))
         (sprite
          name: "main-ship-range"
          image: "clock-frame01"
          center: (variant
                   iPad: (point x: 336 y: 710)
                   iPhone: (point x: 160 y: 285)))]
   labels: [(label
             name: "clock display"
             text: "02:00"
             font: (variant
                    iPad: "digital-764"
                    iPhone: "digital-736")
             center: (variant
                      iPad: (point x: 136 y: 710)
                      iPhone: (point x: 64 y: 285)))
            (label
             name: "ship range display"
             text: "3000"
             font: (variant
                    iPad: "digital-748-red"
                    iPhone: "digital-736-red")
             center: (variant
                      iPad: (point x: 336 y: 710)
                      iPhone: (point x: 160 y: 285)))]
   actors: [(actor
             name: "medical-boat"
             icon: "medical-boat-icon"
             sprite: "medical-boat-sprite"
             port: "start port"
             spawn: (variant
                     iPad: (point x: 890 y: 495)
                     iPhone: (point x: 431 y: 220))
             action: "Medical Voyage Navigation: Boat Action")
            (actor
             name: "storm"
             icon: "storm-icon"
             sprite: "storm-sprite"
             spawn: (variant
                     iPad: (point x: 458 y: 295)
                     iPhone: (point x: 208 y: 114))
             action: "Medical Voyage Navigation: Storm Action")]
   buttons: [(button
              name: "start port"
              image: "navigation-port-start"
              center: (variant
                       iPad: (point x: 906 y: 516)
                       iPhone: (point x: 437 y: 229))
              dock: (variant
                     iPad: (point x: 890 y: 495)
                     iPhone: (point x: 431 y: 220))
              action: "Medical Voyage Navigation: Start Port")
             (button
              name: "leg1 port"
              image: "navigation-port-leg"
              center: (variant
                       iPad: (point x: 665 y: 431)
                       iPhone: (point x: 321 y: 188))
              dock: (variant
                     iPad: (point x: 657 y: 402)
                     iPhone: (point x: 316 y: 175))
              action: "Medical Voyage Navigation: Leg1 Port")
             (button
              name: "leg2 port"
              image: "navigation-port-leg"
              center: (variant
                       iPad: (point x: 285 y: 511)
                       iPhone: (point x: 129 y: 228))
              dock: (variant
                     iPad: (point x: 267 y: 489)
                     iPhone: (point x: 122 y: 215))
              action: "Medical Voyage Navigation: Leg2 Port")
             (button
              name: "goal port"
              image: "navigation-port-goal"
              center: (variant
                       iPad: (point x: 106 y: 392)
                       iPhone: (point x: 41 y: 166))
              dock: (variant
                     iPad: (point x: 112 y: 367)
                     iPhone: (point x: 42 y: 157))
              action: "Medical Voyage Navigation: Goal Port")
             (button
              name: "quit button"
              image: "quit-button01"
              center: (variant
                       iPad: (point x: 512 y: 32)
                       iPhone: (point x: 400 y: 34))
              action: "Common Actions: Quit")]
   timers: [(timer
             name: "main timer"
             init: 120.0
             enabled: #t
             interval: 1.0
             increment: -1
             display: "clock display")]))

