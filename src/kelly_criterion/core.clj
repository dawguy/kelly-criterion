(ns kelly-criterion.core)

; Assumings odds work like this since I forget how they actually work.
; There are certainly multiple different type of odds, and I think what I'm describing
; here does count as one of them.
;
; Bet 100
; Winning total = 200
; Losing total = 0
; Odds 1:1
;
; Bet 100
; Winning total = 300
; Losing total = 0
; Odds 2:1

(defn expected-value [chances])



(comment
"A list of runnable helpful commands"
[]
  (prn "yo dog")
  (def chances [{:prob .15 :odds}])
)