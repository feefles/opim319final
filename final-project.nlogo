extensions[array]
;; ----------- SIMULATION PARAMETERS ---------- 

;; Define breeds. Can be used to simulate genetic, 
;; linguistic, or other relatedness amongst a group 
;; of turtles (aka a "cohort")
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
  
  
  ;; Lists of integers representing agent's memory of previous
  ;; interactions with other agents -- neg bad, 0 neutral, pos good
  ;; and indexed by turtles' who value
  partner-defection-history
  partner-lying-history
  
  ;; Agentsets containing those consulted who said partner had a good
  ;; reputation, and those who said the partner had a bad reputation.
  ;; Used in order to determine which agents lied about the partner
  ;; (even if unintentional--could be misinformed but no subjective way
  ;; of knowing)
  ;; Warning: this is a bit hacky and likely very inefficient
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
    set partner-defection-history []
    set partner-lying-history []
    repeat (count turtles) [
      set partner-defection-history (lput 0 partner-defection-history)
      set partner-lying-history (lput 0 partner-lying-history)
    ]
  ]
end

to make-breeds
  ;; Create 5 breeds of turtle and color them the same
  create-ones   (global-num-turtles-red) [ 
    set color red 
    set coop-prob (random-float .1) + prob-cooperate-red - .05 ; random float on range of .1 around default
    set lie-prob (random-float .1) + prob-lie-red - .05 
    ]
  create-twos   (global-num-turtles-yellow) [
     set color yellow 
     set coop-prob (random-float .1) + prob-cooperate-yellow - .05 ; random integer on range of 10 around default prob
     set lie-prob (random-float .1) + prob-lie-yellow - .05 
     ]
  create-threes (global-num-turtles-green) [
     set color green 
     set coop-prob (random-float .1) + prob-cooperate-green - .05 ; random integer on range of 10 around default prob
     set lie-prob (random-float .1) + prob-lie-green - .05     
     ]
end


;;------------ EVOLUTION PROCEDURES -----------
;;breed each of the top half twice with a random partner
to evolve
  let new-population-characteristics []
  ;; each entry in form: [breed#, coop-prob, lie-prob]
  
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
        (foreach (list ([breed] of ?)([lie-prob] of ?) ([coop-prob] of ?)) (list ([breed] of breeding-partner)([lie-prob] of breeding-partner) ([coop-prob] of ?)) [
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
 
  
  foreach new-population-characteristics [
    if array:item ? 0 = ones [
       create-ones   (1) [ 
         set color red 
         ifelse (random-float 1) < mutation-chance [ ;;mutate
           set coop-prob random-float 1
           set lie-prob random-float 1
         ]
         [
           set coop-prob (random-float .1) + array:item ? 1 - .03 ; random float on range of .1 around default
           set lie-prob (random-float .1) + array:item ? 2 - .03 
         ]
       ]
      set global-num-turtles-red global-num-turtles-red + 1
    ]
    if array:item ? 0 = twos [
       create-twos   (1) [ 
         set color yellow 
         ifelse random-float 1 < mutation-chance [ ;;mutate
           set coop-prob random-float 1
           set lie-prob random-float 1
         ]
         [
           set coop-prob (random-float .1) + array:item ? 1 - .03 ; random float on range of .1 around default
           set lie-prob (random-float .1) + array:item ? 2 - .03 
         ]
       ]
       set global-num-turtles-yellow global-num-turtles-yellow + 1
    ]
    if array:item ? 0 = threes [
       create-threes   (1) [ 
         set color green 
         ifelse random-float 1 < mutation-chance [ ;;mutate
           set coop-prob random-float 1
           set lie-prob random-float 1
         ]
         [
           set coop-prob (random-float .1) + array:item ? 1 - .03 ; random float on range of .1 around default
           set lie-prob (random-float .1) + array:item ? 2 - .03 
         ]
       ]
       set global-num-turtles-green global-num-turtles-green + 1   
    ]
  ]
  
  ask turtles [
    setxy random-xcor random-ycor
    set score 0
    set partnered? false
    set partner nobody
    set positive-recommenders nobody
    set negative-recommenders nobody
    
    ;; Initialize partner history lists -- default to
    ;; neutral reputation for every turtle
    set partner-defection-history []
    set partner-lying-history []
    repeat (count turtles) [
      set partner-defection-history (lput 0 partner-defection-history)
      set partner-lying-history (lput 0 partner-lying-history)
    ]
  
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
;;    which can be their reputation for cooperating, for telling the truth,
;;    or a combination (sum) of both
;; 2. If partner's reputation is on average good or neutral, cooperate,
;;    else punish negative reputations by defecting against them
to select-action
  if defection-heuristic = "defection reputation" [
    let partner-defection-reputation (ask-defection-reputation define-agentset)
     ifelse partner-defection-reputation < 0 [
      set defect-now? true
    ][
      set defect-now? (random-float 1 > coop-prob)
    ]
  ]
  
  if defection-heuristic = "lying reputation" [
    let partner-lying-reputation (ask-lying-reputation define-agentset)  
    ifelse partner-lying-reputation < 0 [
      set defect-now? true
    ][
      set defect-now? (random-float 1 > coop-prob)
    ]
  ]
  
  if defection-heuristic = "defection or lying" [
    let partner-defection-reputation (ask-defection-reputation define-agentset)
    let partner-lying-reputation (ask-lying-reputation define-agentset)
    let combined-reputation partner-defection-reputation + partner-lying-reputation
    
    ifelse combined-reputation < 0 [
      set defect-now? true
    ][
      set defect-now? (random-float 1 > coop-prob)
    ]
  ]
  
  if (not defect-now?) [
    set global-num-cooperating (global-num-cooperating + 1)
  ]
end

;; Returns an agentset based on consult-agentset parameter -- options:
;; -> "own memory": agent consults only its own history lists
;; -> "breed cohort" agent consults turtles of the same breed 
;;      (if no breeds exist, then consults all turtles)
;; -> "proximal cohort": consults turtles within a certain radius
;; -> "all turtles": consults all turtles
to-report define-agentset
  if consult-agentset = "own memory" [
    report turtle-set self
  ]
  if consult-agentset = "breed cohort" [
    report turtles with [breed = [breed] of myself]
  ]
  if consult-agentset = "proximal cohort" [
    report turtles in-radius 8
  ]
  if consult-agentset = "all turtles" [
    report turtles
  ]
end

to-report ask-defection-reputation [agents]
  let defection-reputation []
  
  foreach sort agents [
    ifelse (lie-to-me? ?) [
      let false-value random 3 - 1   ; random integer on [-1, 1]
      
      ;; Add to agentset based on recommendation
      if false-value < 0 [
        set negative-recommenders (turtle-set negative-recommenders ?)
      ]
      if false-value > 0 [
        set positive-recommenders (turtle-set positive-recommenders ?)
      ]
      set defection-reputation (lput false-value defection-reputation)
      set global-num-lying (global-num-lying + 1)
    ][
      let true-value (item ([who] of partner) ([partner-defection-history] of ?))
      
      ;; Add to agentset based on recommendation
      if true-value < 0 [
        set negative-recommenders (turtle-set negative-recommenders ?)
      ]
      if true-value > 0 [
        set positive-recommenders (turtle-set positive-recommenders ?)
      ]
      set defection-reputation (lput true-value defection-reputation)
    ]
    
    set global-num-consulted (global-num-consulted + 1)
  ]
  
  report sum defection-reputation
end

to-report ask-lying-reputation [agents]
  let lying-reputation []
  
  foreach sort agents [
    ifelse (lie-to-me? ?) [
      let false-value random 3 - 1   ; random integer on [-1, 1]
      
      ;; Add to agentset based on recommendation
      if false-value < 0 [
        set negative-recommenders (turtle-set negative-recommenders ?)
      ]
      if false-value > 0 [
        set positive-recommenders (turtle-set positive-recommenders ?)
      ]
      
      set lying-reputation (lput false-value lying-reputation)
      set global-num-lying (global-num-lying + 1)
    ][
      let true-value (item ([who] of partner) ([partner-lying-history] of ?))
      
      ;; Add to agentset based on recommendation
      if true-value < 0 [
        set negative-recommenders (turtle-set negative-recommenders ?)
      ]
      if true-value > 0 [
        set positive-recommenders (turtle-set positive-recommenders ?)
      ]
      set lying-reputation (lput true-value lying-reputation)
    ]
    
    set global-num-consulted (global-num-consulted + 1)
  ]
  
  report sum lying-reputation
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
  let old-defection-history (item ([who] of partner) partner-defection-history)
  ifelse partner-defected? [
    ;; Remember that partner defected
    let new-defection-history (old-defection-history - 1)
    set partner-defection-history (replace-item ([who] of partner)
                                   partner-defection-history new-defection-history)
    
    ;; Remember that the positive recommenders lied...
    if positive-recommenders != nobody [
      foreach sort positive-recommenders [
        let old-lying-history (item ([who] of ?) partner-lying-history)
        let new-lying-history (old-lying-history - 1)
        set partner-lying-history (replace-item ([who] of ?) 
                                   partner-lying-history new-lying-history)
        set global-num-accused-lying (global-num-accused-lying + 1)
      ]
    ]
    
    ;; ... and the negative recommenders didn't
    if negative-recommenders != nobody [
      foreach sort negative-recommenders [
        let old-lying-history (item ([who] of ?) partner-lying-history)
        let new-lying-history (old-lying-history + 1)
        set partner-lying-history (replace-item ([who] of ?) 
                                   partner-lying-history new-lying-history)
      ]
    ]
    
  ][
    ;; Remember that partner cooperated
    let new-defection-history (old-defection-history + 1)
    set partner-defection-history (replace-item ([who] of partner) 
                                   partner-defection-history new-defection-history)
    
    ;; Remember that the negative recommenders lied...
    if negative-recommenders != nobody [
      foreach sort negative-recommenders [
        let old-lying-history (item ([who] of ?) partner-lying-history)
        let new-lying-history (old-lying-history - 1)
        set partner-lying-history (replace-item ([who] of ?) 
                                   partner-lying-history new-lying-history)
        set global-num-accused-lying (global-num-accused-lying + 1)
      ]
    ]
    
    ;; ... and the positive recommenders didn't
    if positive-recommenders != nobody [
      foreach sort positive-recommenders [
        let old-lying-history (item ([who] of ?) partner-lying-history)
        let new-lying-history (old-lying-history + 1)
        set partner-lying-history (replace-item ([who] of ?) 
                                   partner-lying-history new-lying-history)
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

to-report percent-lying
  ifelse global-num-consulted > 0 [
    report global-num-lying / global-num-consulted
  ][
    report 0
  ]
end

to-report percent-accused-lying
  ifelse global-num-consulted > 0 [
    report global-num-accused-lying / global-num-consulted
  ][
    report 0
  ]
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
215
78
248
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
215
161
248
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
42
443
206
488
consult-agentset
consult-agentset
"own memory" "breed cohort" "proximal cohort" "all turtles"
0

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
40
508
210
553
lying-heuristic
lying-heuristic
"never" "randomly" "lie to outsiders"
2

CHOOSER
39
572
213
617
defection-heuristic
defection-heuristic
"defection reputation" "lying reputation" "defection or lying"
2

PLOT
44
272
244
422
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
"default" 1.0 0 -5298144 true "" "plot percent-cooperating"

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
0.6
0.1
1
NIL
HORIZONTAL

BUTTON
185
215
256
248
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
0.2
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
0.15
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
0.8
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
0.75
.05
1
NIL
HORIZONTAL

MONITOR
262
272
423
317
NIL
global-num-turtles-red
17
1
11

MONITOR
260
334
438
379
NIL
global-num-turtles-yellow
17
1
11

MONITOR
258
390
433
435
NIL
global-num-turtles-green
17
1
11

CHOOSER
466
273
604
318
genetic-pool-size
genetic-pool-size
0.1 0.25 0.5
2

SLIDER
467
334
639
367
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
466
379
621
439
epoch-length
1000
1
0
Number

BUTTON
278
215
378
248
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
467
455
617
500
breeding-style
breeding-style
"random-partner" "cohort" "genetic-pool"
2

@#$#@#$#@
# Social Memory, Reputation, and Deception in the Multi-Player Iterated Prisoner's Dilemma

Max McCarthy - 9 May 2014
OPIM 319 Term Project, University of Pennsylvania

## WHAT IS IT?

This is an extension of the *n*-player iterated prisoner's dilemma, where agents' decisions about cooperation and defection are informed not just by their own memory of their `partner`'s history of play (as in a tit-for-tat strategy), but also have access to the social knowledge of a set of other agents, all of whom are also playing the game.

The precise specification of this set (or "cohort") is determined by the user, who can select among consulting no other agents, all other agents, or agents determined by rules like same breed or geographic distance, which are rudimentary ways of defining small societies. 

Agents make strategic decisions based on the input of their cohort, so it is vital that the signal/noise ratio is as high as possible. Unfortunately, however, other agents may have various motivations for being dishonest—they may have faulty memories, be apprehensive or unable to communicate with thouse outside of their communities, or might hold a grudge based on previous interactions.

This model traces the amount of cooperation that is sustained, and the amount of deceit that takes place, as these parameters are varied. 

## HOW IT WORKS

### THE ITERATED PRISONER'S DILEMMA

The game played by each agent when they are `partnered` (determined spatially based on random movements) is a generic form of the prisoner's dilemma, using Robert Axelrod's standard payoff scheme:

>
<table align=center style="">
<thead><td></td> <td style="padding:5px"><b>Cooperate</b></td> <td  style="padding:5px"><b>Defect</b></td></thead>
<tr><td align=right  style="padding:5px"><b>Cooperate</b></td><td  style="padding:5px" align=center>3,3</td><td align=center  style="padding:5px">0,5</td></tr>
<tr><td align=right  style="padding:5px"><b>Defect</b></td><td align=center  style="padding:5px">5,0</td><td align=center  style="padding:5px">1,1</td></tr>
</table>

Each agent has a its own `partner-defection-history`, which is a `who`-indexed list of integers representing the number of times the `partner` has cooperated or defected as a net positive or net negative amount. Each agent's history is initialized to `0`, and following each interaction the history is incremented by `1` if the agent cooperated, and decremented by `1` if the agent defected. The other `turtles-own` value `partner-lying-history` acts similarly, but traces the reputation of other agents for accurately predicting the behavior of the `partner`.

### CONSULTING A COHORT

Agents can be programmed to ask a certain `agentset` about the reputation of their `partner`, and then choose strategies according to the feedback they receive. There are several options for the `agentset` to interrogate:

>**`"own memory"`**
Agents do not consult any other individuals when determining their action, only their own memory.

>**`"breed cohort"`**
Agents consult only those individuals of the same breed as themself. This could represent some sort of cultural, genetic, or linguistic relation amongst agents, simulating the role that smaller networks play in sharing information.

>**`"proximal cohort"`**
Agents consult only those individuals within a certain radius, who are likely to have interacted with each other previously. This could simulate geographic proximity, where inviduals who are not necessarily otherwise related can exchange information for mutual benefit.

>**`"all turtles"`**
As implied, agents consult all other `turtles` for information.

Agents will sum the feedback from each agent in the `agentset` in order to select an action. If the sum is a net positive (or neutral) recommendation then the agent will play according to its `default-prob-cooperate`, otherwise it will defect.

The agent remembers which members of its cohort gave a positive recommendation and which gave a negative recommendation, and based on the `partner`'s move, will update its internal history of the agents who lied according to whether they accurately predicted the `partner`'s move. 

They key feature here is that reputation does not predict present behavior, especially when each agent is acting based on incomplete information about its `partner`. Agents who lie may nonetheless accurately predict behavior, and agents who tell the truth may be wrong. There is no way to determine intention outside observed behavior, so the agents who are found to be "lying" are determined based on *subjective* and *random* factors. The inability of any agent to access an objective viewpoint is a key reason why lying is so destabilizing, since it is impossible to perfectly know when it occurs.

### LYING HEURISTICS

When consulted by an agent about its `partner`, members of the particular `consult-agentset` are asked about their history with `partner` and decide whether to tell the truth, or to lie to the inquiring agent according to the following heuristics:

>**`"never"`**
Agents will always tell the truth.

>**`"randomly"`**
Agents will decide to lie with probability 0.5, and will select with equal likelihood one of -1, 0, or +1 to report as their history with the `partner`. Decisions are made independently of other agents. (Note: if the `consult-agentset` is `"own memory"` then the agent will randomly lie to itself, which could present strange results.)

>**`"lie to outsiders"`**
Agents will lie if they are of a different breed than the agent asking, and will select with equal likelihood one of -1, 0, or +1 to report as their history with the `partner`. Decisions are made independently of other agents.

>**`"previously defected"`**
Agents will lie if they remember that the agent asking has defected against them a net positive number of times, and will select with equal likelihood one of -1, 0, or +1 to report as their history with the `partner`. Decisions are made independently of other agents.

>**`"previously lied"`**
Agents will lie if they remember that the agent asking has lied to them a net positive number of times, and will select with equal likelihood one of -1, 0, or +1 to report as their history with the `partner`. Decisions are made independently of other agents.


### DEFECTION HEURISTICS

>**`"defection reputation"`**
Agents will consider the `partner`'s reputation for defection, and punish `partner`s that defect often with a defection.

>**`"lying reputation"`**
Agents will consider the `partner`'s reputation for lying, and punish `partner`s that lie often (subjectively determined) with a defection.

>**`"defection or lying"`**
Agents will consider both the `partner`'s reputation for defection and for lying, and will add the two values together to determine the overall reputation of their `partner`, punishing accordingly.

## THINGS TO NOTICE

The default rate of cooperation does affect the initial levels of trust between agents, but is not an accurate predictor of the equilibrium rate of cooperation (as *t* approaches infinity) in all cases. Under some conditions, cooperation is achieved regardless of the default rate, and under others the smallest deviation from 100% cooperation is enough to sow mistrust and collapse the system of cooperativity.

The speed at which an equilibrium is approached varies across parameters as well, demonstrating that lying can have a destabilizing influence on a society. In particular, when agents are consulting a larger cohort, for example, `all turtles`, it can take longer for the turtles to have had a sufficient number of interactions to benefit from consulting one another. 

Punishing defection, rather than lying or a combination of the two, is most effective at maintaining a cooperative equilibrium in the face of random lying. Agents are more likely to be accused of lying (regardless of whether they are actually lying) when `partner`s' reputation for dishonesty is asked when compared to reputaton for defection. This is likely because for every interaction that results in information about one agent's defection, there is an entire cohort of agents whose reputations for lying are updated.

Finally, when agents are programmed to lie to outsiders (i.e. those of a different breed) there is a significant limit to the equilibrium rate of cooperation in the world, when compared to the rate of cooperation when no lying takes place (simulating interactions *within* a society, rather than *between* societies). 

## EXTENDING THE MODEL

This model's biggest shortcoming is that it doesn’t account for strategic uses of lying. Agents randomly select an integer from { -1, 0, 1 } and report that, but this limits both the magnitude of lying that can take place; e.g. an agent could report that `partner` has defected against it 5 times rather than just once. In addiiton, the quality of the lie might be strategically varied, such as agents always informing others that a member of their own cohort has a good reputation, in an attempt to exploit agents outside of its cohort. Extending the types and magnitudes of lies that can take place would likely have interesting results and more accurately reflect the strategic value of lying in various agent-agent and agent-group relations.

Agents also have no opportunity to express contrition or to make up for a previous lie or defection. In nature, we observe a strong instinctive pressure to atone for wrongdoing, usually as a result of social exclusion or punishment. If agents were able to apologize or otherwise repair their relations with others, there may be increased cooperation instead of the mutual defections ad-infinitum that tit-for-tat strategies may encourage.

In this model, agents have infinite memory space and duration, which is an unrealistic assumption. It is unlikely that amongst larger groups of agents that each agent could keep track of all of its previous partners, and so the amount of information available in the "social memory" is likely larger than would be found in the real world. There also might be a limit to the number of previous interactions that an agent remembers, so that an agent who defects on tick 1 might not be punished by that agent when they meet again during tick 99. (However, the social memory makes reputation last much longer than it would amongst agents who did not communicate with each other, since the agent can be immediately punished by another partner on tick 2.)

Finally, another strategic use of partner reputation might be the ability to balk, or shun the other agent, or otherwise refuse to play the game for fear of receiving the "sucker's payoff" if an agent has a strong reputation for defection. This might afford cooperative agents an advantage since they can choose to cooperate amongst themselves only, and leave the noncooperative agents to undermine each other.


## RELATED MODELS

* NetLogo PD N-Person Iterated model.  (See credits)

## CREDITS AND REFERENCES

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
