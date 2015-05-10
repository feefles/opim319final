extensions[array]
;; ----------- SIMULATION PARAMETERS ---------- 


breed [ones one]
breed [twos two]
breed [threes three]

;; Global variables for keeping track of average score,
;; amount of cooperation and lying, and games played
globals [
  global-score   ; sum of all agents' individual scores
  global-num-games
  global-num-cooperating
  global-num-consulted   ; number of agents asked about partner
  global-num-lying   ; objective number of agents that lie
  global-num-accused-lying   ; subjective number thought to be lying
  global-num-turtles
  global-num-turtles-red
  global-num-turtles-yellow
  global-num-turtles-green
  
  global-num-cooperating-red
  global-num-cooperating-yellow
  global-num-cooperating-green
  
  
]

;; Define turtle-specific data
turtles-own [
  score
  partnered?
  partner   ; current partner in game
  
  ;; Boolean values corresponding to strategies in IPD --
  ;; true = defect; false = cooperate
  defect-now?
  partner-defected?
  
  
  ;; Values determining their cooperation and lying
  ;; parameters, each cohort has own
  coop-prob
  lie-prob
  forgiveness
  
  
  ;; Lists of integers representing agent's memory of previous
  ;; interactions with other agents -- neg bad, 0 neutral, pos good
  ;; and indexed by turtles' who value
  partner-defection-history
  partner-lying-history
  
  ;; Agentsets containing those consulted who said partner had a good
  ;; reputation, and those who said the partner had a bad reputation.

  positive-recommenders
  negative-recommenders
]


;; ----------- SETUP PROCEDURES -----------

to setup
  clear-all
  make-turtles
  reset-ticks
end

to make-turtles

  set global-num-turtles num-turtles-red + num-turtles-green + num-turtles-yellow
  set global-num-turtles-red num-turtles-red
  set global-num-turtles-yellow num-turtles-yellow
  set global-num-turtles-green num-turtles-green
  make-breeds  
  ;; Initialize turtles-own variables
  ask turtles [
    setxy random-xcor random-ycor
    set score 0
    set partnered? false
    set partner nobody
    set positive-recommenders nobody
    set negative-recommenders nobody
    
    ;; Initialize partner history lists -- default to
    ;; neutral reputation for every turtle
    set partner-defection-history array:from-list n-values (global-num-turtles + 1) [0]
    set partner-lying-history array:from-list n-values (global-num-turtles + 1) [0]
  ]
end

to make-breeds
  ;; Create 5 breeds of turtle and color them the same
  create-ones   (global-num-turtles-red) [ 
    set color red 
    set coop-prob (random-float .1) + prob-cooperate-red - .05 ; random float on range of .1 around default
    set lie-prob (random-float .1) + prob-lie-red - .05 
    set forgiveness forgiveness-red
    ]
  create-twos   (global-num-turtles-yellow) [
     set color yellow 
     set coop-prob (random-float .1) + prob-cooperate-yellow - .05 ; random integer on range of 10 around default prob
     set lie-prob (random-float .1) + prob-lie-yellow - .05 
     set forgiveness forgiveness-yellow
     ]
  create-threes (global-num-turtles-green) [
     set color green 
     set coop-prob (random-float .1) + prob-cooperate-green - .05 ; random integer on range of 10 around default prob
     set lie-prob (random-float .1) + prob-lie-green - .05     
     set forgiveness forgiveness-green
     ]
end


;;------------ EVOLUTION PROCEDURE -----------

to evolve
  let new-population-characteristics []
  ;; each entry in form: [breed#, coop-prob, lie-prob, forgiveness]
  
  let threshold global-num-turtles * genetic-pool-size ;;only breed with the top % of performers
  let ctr 0 
  let new-char []
  let breeding 1 / genetic-pool-size
  
  ;;this is pretty ridiculous, but I'm not sure how to do this properly
  let sorted_pool sort-on [(- score)] turtles;;get sorted pool, then convert to array
  let min-genetic-pool-score [score] of (array:item (array:from-list sorted_pool) threshold)
  let genetic-pool filter [[score] of ? > min-genetic-pool-score] sorted_pool ;;I'm not sure how to get the first k elems in list...
  
  foreach sort-on [(- score)] turtles [ ;; sort in decreasing order (highest scores first)
    if ctr < threshold [ 
      let breeding_ctr 0
      while [breeding_ctr < breeding] [
        let breeding-partner 0
        if breeding-style = "random-partner" [set breeding-partner one-of turtles ]   ;;random partner from population
        if breeding-style = "cohort" [set breeding-partner one-of [breed] of ?] ;;random partner of the same breed
        if breeding-style = "genetic-pool" [set breeding-partner one-of genetic-pool] ;;random partner from gene pool
        set new-char []
        (foreach (list ([breed] of ?)([lie-prob] of ?) ([coop-prob] of ?) ([forgiveness] of ?)) 
          (list ([breed] of breeding-partner)([lie-prob] of breeding-partner) ([coop-prob] of breeding-partner)([forgiveness] of breeding-partner)) [
          ifelse random-float 1 > .5 [
            set new-char lput ?1 new-char
          ] [
            set new-char lput ?2 new-char
          ]
        ])
        set new-population-characteristics lput (array:from-list new-char) new-population-characteristics
        set breeding_ctr breeding_ctr + 1
      ]     
      set ctr ctr + 1
    ]
  ]

  ;;print new-population-characteristics
  clear-turtles
  
 ;;NOW CREATE EVOLVED SPECIES
 
 set global-num-turtles-red 0
 set global-num-turtles-yellow 0
 set global-num-turtles-green 0
 
 ;; extra safety to ensure we only breed as many as global-num-population 
 let breeding-ctr 0
 foreach new-population-characteristics [
   if breeding-ctr < global-num-turtles [
     if array:item ? 0 = ones [
       create-ones   (1) [ 
         set color red 
         ifelse (random-float 1) < mutation-chance [ ;;mutate
           set coop-prob random-float 1
           set lie-prob random-float 1
           set forgiveness (random-float 100)
         ]
         [
           set coop-prob (random-float .1) + array:item ? 1 - .03 ; random float on range of .1 around default
           set lie-prob (random-float .1) + array:item ? 2 - .03 
           set forgiveness (random-float 10) + array:item ? 3 - 5 
         ]
       ]
        set global-num-turtles-red global-num-turtles-red + 1
        set breeding-ctr breeding-ctr + 1
     ]
     if array:item ? 0 = twos [
       create-twos   (1) [ 
         set color yellow 
         ifelse random-float 1 < mutation-chance [ ;;mutate
           set coop-prob random-float 1
           set lie-prob random-float 1
           set forgiveness (random-float 100)
         ]
         [
           set coop-prob (random-float .1) + array:item ? 1 - .03 ; random float on range of .1 around default
           set lie-prob (random-float .1) + array:item ? 2 - .03 
           set forgiveness (random-float 10) + array:item ? 3 - 5 
         ]
       ]
        set global-num-turtles-yellow global-num-turtles-yellow + 1
        set breeding-ctr breeding-ctr + 1
     ]
     if array:item ? 0 = threes [
       create-threes   (1) [ 
         set color green 
         ifelse random-float 1 < mutation-chance [ ;;mutate
           set coop-prob random-float 1
           set lie-prob random-float 1
           set forgiveness (random-float 100)
         ]
         [
           set coop-prob (random-float .1) + array:item ? 1 - .03 ; random float on range of .1 around default
           set lie-prob (random-float .1) + array:item ? 2 - .03 
           set forgiveness (random-float 10) + array:item ? 3 - 5 
         ]
       ]
        set global-num-turtles-green global-num-turtles-green + 1  
        set breeding-ctr breeding-ctr + 1 
     ]
     
    ]
  ]
  
  ask turtles [
    setxy random-xcor random-ycor
    set score 0
    set partnered? false
    set partner nobody
    set positive-recommenders nobody
    set negative-recommenders nobody
    
    set partner-defection-history array:from-list n-values (global-num-turtles + 1) [0]
    set partner-lying-history array:from-list n-values (global-num-turtles + 1) [0]
  ]

  reset-ticks
end


;; ----------- RUNTIME PROCEDURES -----------

to go
  clear-last-round
  ask turtles [partner-up]
  let partnered-turtles (turtles with [partnered?]) ;; all turtles that have partners
  ask partnered-turtles [select-action]
  ask partnered-turtles [play-a-round]
  do-scoring
  tick
end

to evolve-run
  tick
  while [ticks mod epoch-length != 0] [
    go
  ]
  evolve
  
end

to clear-last-round
  let partnered-turtles (turtles with [partnered?])
  ask partnered-turtles [release-partners]
  
  ;; Reset memory of agents that lied
  ask partnered-turtles [
    set positive-recommenders nobody
    set negative-recommenders nobody
  ]
end

;; Agents remove partner relationships and turn around to leave
to release-partners
  set partnered? false
  set partner nobody
  rt 180
  set label ""
end

to partner-up
  if (not partnered?) [
    ;; Move around randomly in search of a partner
    rt (random-float 90 - random-float 90) fd 1
    set partner one-of (turtles-at -1 0) with [ not partnered? ]
    
    if partner != nobody [
      set partnered? true
      set heading 270   ; turn to face partner
      
      ask partner [
        set partnered? true
        set partner myself
        set heading 90   ; turn to face partner
      ]
    ]
  ]  
end


;; Agents decide their actions according to the following heuristic:
;; 1. Consult specified agentset to determine partner's reputation
;;    which is a sum of their reputation for defecting and lying
;; 2. If partner's reputation is on average good or neutral, cooperate,
;;    else punish negative reputations by defecting against them
;; They also have forgiveness: they may have a negative recommendation, but a forgiving 
;; agent will still cooperate
to select-action
  let reputation (ask-reputation define-agentset)
  
  ifelse reputation < (- forgiveness) [ 
    set defect-now? true
  ][
    set defect-now? (random-float 1 > coop-prob)
  ]
  if (not defect-now?) [
    set global-num-cooperating (global-num-cooperating + 1)
    ;; This isn't beautiful, but it works
    if breed = ones [
      set global-num-cooperating-red (global-num-cooperating-red + 1)
    ]
    if breed = twos [
      set global-num-cooperating-yellow (global-num-cooperating-yellow + 1)
    ]
    if breed = threes [
      set global-num-cooperating-green (global-num-cooperating-green + 1)
    ]
  ]
end

;; Returns an agentset based on consult-agentset parameter -- options:
;; -> "own memory": agent consults only its own history lists
;; -> "breed cohort" agent consults turtles of the same breed 
;;      (if no breeds exist, then consults all turtles)
;; -> "all turtles": consults all turtles
to-report define-agentset
  if consult-agentset = "own memory" [
    report turtle-set self
  ]
  if consult-agentset = "breed cohort" [
    report turtles with [breed = [breed] of myself]
  ]
  if consult-agentset = "all turtles" [
    report turtles
  ]
end

to-report ask-reputation [agents]
  let reputation []
  foreach sort agents [
    
    let defection-recommendation array:item ([partner-defection-history] of ?) ([who] of partner)
    let lying-recommendation array:item ([partner-lying-history] of ?) ([who] of partner)
    
    ifelse (lie-to-me? ?) [ ;;lie to the agent
      ifelse defection-recommendation > 0 [ ;; do the opposite 
        set negative-recommenders (turtle-set negative-recommenders ?)
        set reputation lput -1 reputation
      ] [
        set positive-recommenders (turtle-set positive-recommenders ?)
        set reputation lput 1 reputation
      ]
      ifelse lying-recommendation > 0 [ ;; do the opposite 
        set negative-recommenders (turtle-set negative-recommenders ?)
        set reputation lput -1 reputation
      ] [
        set positive-recommenders (turtle-set positive-recommenders ?)
        set reputation lput 1 reputation
      ]
      set global-num-lying (global-num-lying + 1)

    ] [
      ifelse (defection-recommendation < 0) [
         set negative-recommenders (turtle-set negative-recommenders ?)
         set reputation lput -1 reputation
      ]
      [
          set positive-recommenders (turtle-set positive-recommenders ?)
          set reputation lput 1 reputation
      ]
       ifelse (lying-recommendation) < 0 [
         set negative-recommenders (turtle-set negative-recommenders ?)
         set reputation lput -1 reputation
      ]
      [
          set positive-recommenders (turtle-set positive-recommenders ?)
          set reputation lput 1 reputation
      ]
    ]
  ]
  report sum reputation
end

;; Returns true criteria for lying-heuristic are met -- options:
;; -> "never": always returns false
;; -> "randomly": returns true with certain probability
;; -> "lie to outsiders": returns true questioner breed != agent breed

to-report lie-to-me? [agent]
  if lying-heuristic = "never" [
    report false
  ]
  if lying-heuristic = "randomly" [
      report random-float 1 < lie-prob
  ]

  if lying-heuristic = "lie to outsiders" [
    report breed != [breed] of agent
  ]
end

to play-a-round
  get-payoff
  update-histories
  set global-num-games (global-num-games + 1)
end

;; Calculate payoffs according to generic PD rules
to get-payoff
  set partner-defected? ([defect-now?] of partner)
  ifelse partner-defected? [
    ifelse defect-now? [
      ;; Defect-Defect
      set score (score + 1) 
      set label 1
    ][
      ;; Cooperate-Defect
      set score (score + 0) 
      set label 0
    ]
  ][
    ifelse defect-now? [
      ;; Defect-Cooperate
      set score (score + 5) 
      set label 5
    ][
      ;; Cooperate-Cooperate
      set score (score + 3)
      set label 3
    ]
  ]
end

;; Update agent's memory of others' cooperation and lying
to update-histories

  let old-defection-history array:item partner-defection-history ([who] of partner)
  let partner-index ([who] of partner)
  
  ifelse partner-defected? [
    ;; Remember that partner defected
    let new-defection-history (old-defection-history - 1)
    ;; using array set means you are just modifying the old array, not making a new one!
    array:set partner-defection-history partner-index new-defection-history
    
     ;; Remember that the positive recommenders lied...
    if positive-recommenders != nobody [
      foreach sort positive-recommenders [
        let old-lying-history array:item partner-lying-history ([who] of ?)
        array:set partner-lying-history ([who] of ?) (old-lying-history - 1)
      ]
    ]
    
    ;; ... and the negative recommenders didn't
    if negative-recommenders != nobody [
      foreach sort negative-recommenders [
        let old-lying-history array:item partner-lying-history ([who] of ?)
        array:set partner-lying-history ([who] of ?) (old-lying-history + 1)
      ]
    ]
    
  ][ ;; else partner cooperated
    ;; Remember that partner cooperated
    let new-defection-history (old-defection-history + 1)
    array:set partner-defection-history partner-index new-defection-history
    
    ;; Remember that the negative recommenders lied...
    if negative-recommenders != nobody [
      foreach sort negative-recommenders [
        let old-lying-history array:item partner-lying-history ([who] of ?)
        array:set partner-lying-history ([who] of ?) (old-lying-history - 1)
      ]
    ]
    
    ;; ... and the positive recommenders didn't
    if positive-recommenders != nobody [
      foreach sort positive-recommenders [
        let old-lying-history array:item partner-lying-history ([who] of ?)
        array:set partner-lying-history ([who] of ?) (old-lying-history + 1)
      ]
    ]
  ]
end


;; ----------- ANALYSIS PROCEDURES -----------

to do-scoring
  set global-score (sum ([score] of turtles))
end

to-report percent-cooperating
  ifelse global-num-games > 0 [
    report global-num-cooperating / global-num-games
  ][
    report 0
  ]
end

;; is there a better way to do this in netlogo? 
to-report red-cooperating
  ifelse global-num-games > 0 [
    report global-num-cooperating-red / global-num-games
  ][
    report 0
  ]
end
to-report yellow-cooperating
  ifelse global-num-games > 0 [
    report global-num-cooperating-yellow / global-num-games
  ][
    report 0
  ]
end
to-report green-cooperating
  ifelse global-num-games > 0 [
    report global-num-cooperating-green / global-num-games
  ][
    report 0
  ]
end

to-report percent-lying
  ifelse global-num-consulted > 0 [
    report global-num-lying / global-num-consulted
  ][
    report 0
  ]
end

to-report avg-cooperation
  ;; in form [red, yellow, green]
  let ret []
  foreach (list ones twos threes) [
    ifelse count ? = 0 [
      set ret lput 0 ret
    ] [
      set ret lput (precision (mean [coop-prob] of ?) 3) ret
    ]
  ]
  report ret
end

to-report avg-lie
    ;; in form [red, yellow, green]
  let ret []
  foreach (list ones twos threes) [
    ifelse count ? = 0 [
      set ret lput 0 ret
    ] [
      set ret lput (precision (mean [lie-prob] of ?) 3) ret
    ]
  ]
  report ret
end

to-report avg-forgiveness
    ;; in form [red, yellow, green]
  let ret []
  foreach (list ones twos threes) [
    ifelse count ? = 0 [
      set ret lput 0 ret
    ] [
      set ret lput (precision (mean [forgiveness] of ?) 3) ret
    ]
  ]
  report ret
end
@#$#@#$#@
GRAPHICS-WINDOW
750
25
1133
429
16
16
11.303030303030303
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
12
259
78
292
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
98
259
161
292
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
11
484
175
529
consult-agentset
consult-agentset
"own memory" "breed cohort" "all turtles"
1

SLIDER
14
51
186
84
num-turtles-red
num-turtles-red
0
100
50
5
1
NIL
HORIZONTAL

CHOOSER
9
549
179
594
lying-heuristic
lying-heuristic
"never" "randomly" "lie to outsiders"
1

PLOT
13
313
213
463
Cooperation
Iterations
% cooperating
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot percent-cooperating"
"pen-1" 1.0 0 -2674135 true "" "plot red-cooperating"
"pen-2" 1.0 0 -4079321 true "" "plot yellow-cooperating"
"pen-3" 1.0 0 -13840069 true "" "plot green-cooperating"

SLIDER
12
104
245
137
prob-cooperate-red
prob-cooperate-red
0
1
0.2
0.05
1
NIL
HORIZONTAL

SLIDER
12
155
189
188
prob-lie-red
prob-lie-red
0
.95
0.8
0.05
1
NIL
HORIZONTAL

BUTTON
185
259
256
292
NIL
evolve
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
250
50
423
83
num-turtles-yellow
num-turtles-yellow
0
100
50
1
1
NIL
HORIZONTAL

SLIDER
505
50
677
83
num-turtles-green
num-turtles-green
0
100
50
1
1
NIL
HORIZONTAL

SLIDER
250
104
497
137
prob-cooperate-yellow
prob-cooperate-yellow
0
1
0.8
.05
1
NIL
HORIZONTAL

SLIDER
504
105
748
138
prob-cooperate-green
prob-cooperate-green
0
1
0.5
.05
1
NIL
HORIZONTAL

SLIDER
250
157
448
190
prob-lie-yellow
prob-lie-yellow
0
1
0.2
.05
1
NIL
HORIZONTAL

SLIDER
502
157
698
190
prob-lie-green
prob-lie-green
0
1
0.5
.05
1
NIL
HORIZONTAL

MONITOR
231
313
309
358
red turtles
global-num-turtles-red
17
1
11

MONITOR
229
375
311
420
yellow turtles
global-num-turtles-yellow
17
1
11

MONITOR
227
431
314
476
green turtles
global-num-turtles-green
17
1
11

CHOOSER
572
269
710
314
genetic-pool-size
genetic-pool-size
0.1 0.25 0.5
2

SLIDER
573
330
745
363
mutation-chance
mutation-chance
0
1
0
.01
1
NIL
HORIZONTAL

INPUTBOX
572
375
727
435
epoch-length
50
1
0
Number

BUTTON
278
259
378
292
NIL
evolve-run
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
573
451
723
496
breeding-style
breeding-style
"random-partner" "cohort" "genetic-pool"
1

SLIDER
14
208
186
241
forgiveness-red
forgiveness-red
0
100
0
1
1
NIL
HORIZONTAL

SLIDER
251
209
423
242
forgiveness-yellow
forgiveness-yellow
0
100
0
1
1
NIL
HORIZONTAL

SLIDER
501
211
673
244
forgiveness-green
forgiveness-green
0
100
0
1
1
NIL
HORIZONTAL

MONITOR
342
312
469
357
NIL
avg-cooperation
2
1
11

MONITOR
342
376
468
421
NIL
avg-lie
17
1
11

MONITOR
344
434
520
479
NIL
avg-forgiveness
17
1
11

@#$#@#$#@
# Evolving a Socially Conscious Population in the Multi-Player Iterated Prisoner's Dilemma

Evelyn Fifi Yeung - May 10, 2015
OPIM 319 Term Project

## Background

This is an extension of Max McCarthy's OPIM 319 Project from Spring, 2014. To summarize, his project added social awareness to the IPD game. Agents can play against other agents in their vicinity, but they draw on the opinions of others to decide on their strategy. They could choose who to draw information from (in this version, I have restricted it to their own breed, all the turtles, or their own memory). 

His original model includes a lying heuristic, in which you can define when an agent will lie about the reputation of another. While it is included, I always ran the "randomly" lie option, since it emphasizes the importance of that trait in the population. 

This project builds upon this model by adding evolution. It aims to see how cooperation can evolve to dominance in a population. There is a simple genetic algorithm that helps breed the turtles. The "genome" of the turtles is encapsulated in 3 properties: their cooperation probability, lying probability, and forgiveness. The initial states can be set. On each iteration, the entire population is evaluated by their total score. A group of that will be taken and bred, according to various methods. The algorithm attempts to mimic a traditional evolutionary algorithm, in that there is crossover: there will always be two parents for a new agent, and there can be mutation introduced. 

## The Game

### THE ITERATED PRISONER'S DILEMMA
(Adapted from Max)

The game played by each agent when they are `partnered` (determined spatially based on random movements) is a generic form of the prisoner's dilemma, using Robert Axelrod's standard payoff scheme:

>
<table align=center style="">
<thead><td></td> <td style="padding:5px"><b>Cooperate</b></td> <td  style="padding:5px"><b>Defect</b></td></thead>
<tr><td align=right  style="padding:5px"><b>Cooperate</b></td><td  style="padding:5px" align=center>3,3</td><td align=center  style="padding:5px">0,5</td></tr>
<tr><td align=right  style="padding:5px"><b>Defect</b></td><td align=center  style="padding:5px">5,0</td><td align=center  style="padding:5px">1,1</td></tr>
</table>

Each agent keeps track of which agents have cooperated or defected when playing them, and which agents have lied to them. Agents also have default probabilities for cooperating. If the agent hears a positive recommendation from others, it will cooperate according to its default probability. 

You can play this without evolution to see how the population would change on its own, then you can play with `evolve-run`, in which case the population will evolve every `epoch-length` ticks.


### CONSULTING A COHORT
(Adapted from Max)
Agents can be programmed to ask a certain `agentset` about the reputation of their `partner`, and then choose strategies according to the feedback they receive. There are several options for the `agentset` to interrogate:

>**`"own memory"`**
Agents do not consult any other individuals when determining their action, only their own memory.

>**`"breed cohort"`**
Agents consult only those individuals of the same breed as themself. This could represent some sort of cultural, genetic, or linguistic relation amongst agents, simulating the role that smaller networks play in sharing information.

>**`"all turtles"`**
As implied, agents consult all other `turtles` for information.

Agents gather information from the specified agentset. However, they also have a forgiveness factor built in. Even in the event that the recommendation was negative, the agent may still cooperate, depending on how forgiving they are. In this case, forgiveness is a threshold for how badly recommended someone can be before they decide to defect. It would also be interesting to explore forgiveness as a percentage - an agent could forgive a bad player a certain number of times.

Also, this differs from the original model, because the lying heuristic no longer has randomness. Once an agent decides to lie (according to the lie prob), they will purposely do the opposite of what they think would do. This makes the game more interesting to look at through an evolutionary lens, since populations are now more deliberate in their lying, and these traits are passed on. 

### EVOLVING
There are several parameters governing the evolution. 

####genetic-pool-size
When breeding, this is the percentage of the population that is guaranteed to be a parent. It is always taken as the top n scorers according to the fitness function, which is the score in this case. Depending on the size, these agents may be bred multiple times with various partners. This represents how 'strict' the evolution is.  

####mutation-chance
When breeding, there is a chance of mutation. This is good, since it introduces variation into the population, much like true evolution. This is the percent chance that a child is mutated. In this case, the mutation implies all the genomes for the child are completely randomized.


####breeding-style
When breeding, there are various methods of choosing a partner 
>**`"random-partner"`**
The partner is randomly chosen from the entire turtle population

>**`"cohort"`**
The partner is randomly chosen from the same cohort

>**`"genetic-pool"`**
The partner is randomly chosen from the pool, which is defined by the `genetic-pool-size`


## Conclusions

There are a number of things, which I outline in a separate presentation. The breeding process is set up to represent actual things that happen in the real world. Using evolution definitely seems to help cooperation the vast majority of the time, but there are a lot of different factors that could not be tested extensively. 
Across all experiments, characteristics evolved to reflect what we expected: much higher rates of cooperation. However, lying also increased, which is somewhat surprising, since we might expect that people should lie less in order to help their teammates. Also, we may have expected that people would lie less, so that they would be punished less in future interactions. This may be why the `epoch time` comes in: if there is a short epoch time, a liar or defector may not be punished as much, so their negative traits could continue in the pool.  

## Extensions

Having only 3 genomes may not be the best for a genetic algorithm. It would definitely be worth it to explore the addition of more genomes. One that Max discussed in his original model would also apply here very well: the idea of limited social memory. An agent may not actually remember all past encounters, so having a memory length that is shorter than the epoch time could be interesting to observe in terms of evolution. We may think that having a long memory is good, but it may over punish some agents based on initial interactions. Since there is so much randomness, agents that defect in the beginning are penalized more harshly in the long run than those that defect later in the game (merely based on percentages). 





## Related Models

* NetLogo PD N-Person Iterated model.  (See credits)

## Credits and References
* Max McCarthy for providing a lot of starter code for this project
* Axelrod, R. (1984). The Evolution of Cooperation. Basic Books, NY. p. 23.
* Wilensky, U. (2002). NetLogo PD N-Person Iterated model. http://ccl.northwestern.edu/netlogo/models/PDN-PersonIterated. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.5
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="undetectability of lying" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <metric>percent-cooperating</metric>
    <metric>percent-lying</metric>
    <metric>percent-accused-lying</metric>
    <enumeratedValueSet variable="default-prob-cooperate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defection-heuristic">
      <value value="&quot;defection reputation&quot;"/>
      <value value="&quot;lying reputation&quot;"/>
      <value value="&quot;defection or lying&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-turtles">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consult-agentset">
      <value value="&quot;all turtles&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lying-heuristic">
      <value value="&quot;randomly&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="default-prob-coopeation" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>percent-cooperating</metric>
    <enumeratedValueSet variable="lying-heuristic">
      <value value="&quot;never&quot;"/>
      <value value="&quot;randomly&quot;"/>
      <value value="&quot;lie to outsiders&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-turtles">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defection-heuristic">
      <value value="&quot;defection reputation&quot;"/>
      <value value="&quot;lying reputation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consult-agentset">
      <value value="&quot;own memory&quot;"/>
      <value value="&quot;breed cohort&quot;"/>
      <value value="&quot;proximal cohort&quot;"/>
      <value value="&quot;all turtles&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="default-prob-cooperate">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="xenophobia" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>percent-cooperating</metric>
    <enumeratedValueSet variable="lying-heuristic">
      <value value="&quot;lie to outsiders&quot;"/>
      <value value="&quot;never&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-turtles">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defection-heuristic">
      <value value="&quot;defection reputation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consult-agentset">
      <value value="&quot;all turtles&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="default-prob-cooperate" first="0" step="0.1" last="1"/>
  </experiment>
  <experiment name="social memory" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>percent-cooperating</metric>
    <enumeratedValueSet variable="consult-agentset">
      <value value="&quot;own memory&quot;"/>
      <value value="&quot;breed cohort&quot;"/>
      <value value="&quot;proximal cohort&quot;"/>
      <value value="&quot;all turtles&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-turtles">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="default-prob-lie">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="default-prob-cooperate">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defection-heuristic">
      <value value="&quot;defection reputation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lying-heuristic">
      <value value="&quot;never&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
