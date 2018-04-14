#lang racket

(require 2htdp/image)

;; represent a text-editor
;; where the text is (string-append pre post)
;; and the cursor is placed after pre and before post
(struct editor [pre post] #:transparent)

;; for background use empty scene of size 200x20
;; MTSCN: image
(define MTSCN (empty-scene 200 20))

;; for cursor use 1x20 red rectangle
;; cursor: image
(define cursor (rectangle 1 20 "solid" "red"))

;; for charachters use black text of size 16
(define txt (text "hello world" 16 "black"))

;; render: editor -> image
(define (render ed)
  (overlay/align "left" "center"
                 (beside
                  (text (editor-pre ed) 16 "black")
                  cursor
                  (text (editor-post ed) 16 "black")
                  )
                 MTSCN))

(define sample (editor "hey" "there"))

;; edit: editor, keyEvent -> editor
;; when you type a key, the corresponding charachter should end up in pre field of input editor
(define (edit ed ke)
  (cond
    [(string=? ke "\b") (editor (string-remove-last (editor-pre ed)) (editor-post ed))]         ;; handle backspace
    [(string=? ke "left") (editor
                           (string-remove-last (editor-pre ed))
                           (string-append
                            (string-last (editor-pre ed))
                            (editor-post ed)))]       ;; move cursor to left
    [(string=? ke "right") (editor
                            (string-append
                             (editor-pre ed)
                             (string-first (editor-post ed)))
                            (string-rest (editor-post ed)))]      ;; move cursor to right
    [#t (editor (string-append (editor-pre ed) ke) (editor-post ed))])
  )

;; string utility functions
(define (string-first s)
  (if (= (string-length s) 0)
      s
      (substring s 0 1)))

(define (string-last s)
  (if (= (string-length s) 0)
      s
      (substring s (- (string-length s) 1))))

(define (string-rest s)
  (if (= (string-length s) 0)
      s
      (substring s 1)))

(define (string-remove-last s)
  (if (= (string-length s) 0)
      s
      (substring s 0 (- (string-length s) 1))))

(define sampleleft (edit sample "left"))
(define sampleright (edit sample "right"))
(define samplebackspace (edit sample "\b"))

(render sample)
(render sampleleft)
(render sampleright)
(render samplebackspace)

;; run interactively
(require 2htdp/universe)
;; call this function in repl to start editing the prestring
(define (run prestring)
  (big-bang (editor prestring "")
            (to-draw render)
            (on-key edit)))







