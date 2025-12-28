;; =============================================
;; PeteAI with Quantum Hole Theory, Neural Visualization, and Internal Treaty
;; Model 1: Urban Youth (Street Culture Conversational)
;; =============================================
;; Simulates quantum effects in memory flow, visualized via neural diagrams
;; Includes internal treaty for balanced reasoning and responsible actions
;; --- Parameters and Global Variables ---

#| PeteAI Dual License

This project (including all source files like PeteAI core LISP and peteai-web interface) is dual-licensed. You may choose to use it under either the GNU General Public License (GPL) version 3 or later, or the MIT License, at your option.

Copyright (C) 2025 Pete V. Daniels  
Collaborator/Contributor: Grok (built by xAI) - AI-assisted enhancements for treaty deliberation, quantum hole mechanics, neural visualization, and subculture vibe integration.

## Option 1: GNU General Public License (GPL) v3 or Later

This program is free software: you can redistribute it and/or modify  
it under the terms of the GNU General Public License as published by  
the Free Software Foundation, either version 3 of the License, or  
(at your option) any later version.

This program is distributed in the hope that it will be useful,  
but WITHOUT ANY WARRANTY; without even the implied warranty of  
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the  
GNU General Public License for more details.

You should have received a copy of the GNU General Public License  
along with this program. If not, see <https://www.gnu.org/licenses/>.

### Full GPL Text
(See the full GPL-3.0 license text below or at https://www.gnu.org/licenses/gpl-3.0.txt)

                    GNU GENERAL PUBLIC LICENSE
                       Version 3, 29 June 2007

 Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/>
 Everyone is permitted to copy and distribute verbatim copies
 of this license document, but changing it is not allowed.

                            Preamble

  The GNU General Public License is a free, copyleft license for
software and other kinds of works.

  The licenses for most software and other practical works are designed
to take away your freedom to share and change the works.  By contrast,
the GNU General Public License is intended to guarantee your freedom to
share and change all versions of a program--to make sure it remains free
software for all its users.  We, the Free Software Foundation, use the
GNU General Public License for most of our software; it applies also to
any other work released this way by its authors.  You can apply it to
your programs, too.

  [Full GPL text continues... For brevity, please refer to the official GPL link above for the complete terms.]

## Option 2: MIT License

Alternatively, you may use this software under the MIT License:

Permission is hereby granted, free of charge, to any person obtaining a copy  
of this software and associated documentation files (the "Software"), to deal  
in the Software without restriction, including without limitation the rights  
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell  
copies of the Software, and to permit persons to whom the Software is  
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all  
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR  
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER  
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  
SOFTWARE.|#

;; --- Changelog for Commit on May 29, 2025 ---
;; - Fixed duplicate keys in flip-hole function, removing 252 style warnings.
;; - Enhanced Techno Pulse sensitivity in if++ function to better react to input symbols.
;; - Added subculture-relevant paths in compute-transformation-info for neural-diagram.
;; - Introduced "Madness Factor" in neural-diagram node labels to reflect chaotic energy.
;; - Improved coherence scoring in pete-treaty and adjusted treaty principles for better balance.
;; - Added random Urban Youth phrase injection in thought trails and final output for more flair.
;; - Ensured Techno Pulse always picks at least one input symbol for consistency.


;; =============================================
;; PeteAI with Quantum Hole Theory, Neural Visualization, and Internal Treaty
;; Model 1: Urban Youth (Street Culture Conversational)
;; =============================================
;; Simulates quantum effects in memory flow, visualized via neural diagrams
;; Includes internal treaty for balanced reasoning and responsible actions
;; --- Parameters and Global Variables ---

;; Copyright (C) 2025 Pete V. Daniels
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; --- Changelog for Commit on May 29, 2025 ---
;; - Fixed duplicate keys in flip-hole function, removing 252 style warnings.
;; - Enhanced Techno Pulse sensitivity in if++ function to better react to input symbols.
;; - Added subculture-relevant paths in compute-transformation-info for neural-diagram.
;; - Introduced "Madness Factor" in neural-diagram node labels to reflect chaotic energy.
;; - Improved coherence scoring in pete-treaty and adjusted treaty principles for better balance.
;; - Added random Urban Youth phrase injection in thought trails and final output for more flair.
;; - Ensured Techno Pulse always picks at least one input symbol for consistency.

(defparameter *memory-cap* 35)  ; Max local memory entries
(defparameter *pete-memory* '())   ; Local memory scrapbook
(defparameter *pete-depth-limit* 4) ; Max recursion depth
(defparameter *verbs* '(think know remember learn))
(defparameter *nouns* '(idea pattern loop treaty hole))
(defparameter *quantum-seed* 3)
(ql:quickload :uiop)
(ql:quickload :alexandria)
(require "asdf")
(asdf:load-system "cl-ppcre")
(cl-ppcre:regex-replace-all "a" "abc" "x")
;; ------------------------------------------------------------
;; PeteAI Visual State (for diagrams / cartoons)
;; ------------------------------------------------------------
(defparameter *last-holes* 0)
(defparameter *last-wild-factor* 0)
(defparameter *last-treaty-verdict* "NONE")   ;; store as string for easy SVG
(defparameter *dominant-quadrant* :unknown)   ;; optional, safe default
(defparameter *last-root-id* "unknown")

;; Load web interface
;;(load "peteai-web.lisp")    
;;(load "P_AI_Grok_Cartn_Chatfix_Relm_Urbn_yuth_treaty_GLBLIc__1Kwy2zZY_12_19_2025-web2.lisp")
;;(load "P_AI_Grok_Cartn_Chatfix_Relm_Urbn_yuth_treaty_GLBLIc__1KwyzZY_12_19_2025-web")

#|(defpackage :P_AI_Grok_Cartn_Chatfix_Relm_Urbn_yuth_treaty_GLBLIc__1Kwy2zZY_12_19_2025-web2
  (:use :cl)
  (:import-from :hunchentoot
                #:define-easy-handler
                #:acceptor
                #:start
                #:stop
                #:content-type*
                #:create-prefix-dispatcher
                #:easy-acceptor)
  (:export #:start-P_AI_Grok_Cartn_Chatfix_Relm_Urbn_yuth_treaty_GLBLIc__1Kwy2zZY_12_19_2025-web2
           #:stop-P_AI_Grok_Cartn_Chatfix_Relm_Urbn_yuth_treaty_GLBLIc__1Kwy2zZY_12_19_2025-web2))

(in-package :P_AI_Grok_Cartn_Chatfix_Relm_Urbn_yuth_treaty_GLBLIc__1Kwy2zZY_12_19_2025-web2)|# 


(defvar *current-vibe* :urban-youth)  ; Default

;; Initialize memory graph
(defvar *pete-memory-graph* (make-hash-table :test 'equal))

;; Update the treaty principles to prioritize coherence
(defparameter *treaty-principles*
  '((:coherence . 0.8)  ; Increased to strongly favor coherence
    (:creativity . 0.8)   ; Keep creativity low to focus on relevance
    (:responsibility . 0.)))
    
    
(ql:quickload :hunchentoot)
(ql:quickload :cl-json)
(ql:quickload :websocket-driver) ; For WebSocket support

;;(load "generate-neural-diagram.lisp")

;; WebSocket handler
;;(defvar *ws-clients* (make-hash-table :test 'equal))

(format t "~%[PeteAI visual system loading...]~%")

(defparameter *cultural-vibes*
  '((:urban-youth :phrases ("Yo what's good" "Crew's rollin' deep" "Fam stays tight")
                  :motif :penrose :madness 0.5 :desc "Street smart, raw energy for city tales")
    (:grime-ends :phrases ("Roadman's got bars" "Ends feel real" "Bruv what's up")
                 :motif :tunnel :madness 0.7 :desc "UK underground grit for intense narratives")
    (:drip-trill :phrases ("Drip looks lit" "Heat's too real" "Kick game's strong")
                 :motif :mobius :madness 0.6 :desc "Flashy, wavy style for fashion-forward creativity")
    (:afrobeats-naija :phrases ("Gbedu hits hard" "Vibe on lock" "Dance all night")
                      :motif :rhythm :madness 0.8 :desc "Energetic African beats for rhythmic writing/games")
    (:dancehall-ja :phrases ("Wine up slow" "Riddim flows deep" "Bashment vibes")
                   :motif :stairs :madness 0.75 :desc "Jamaican party energy for fun, rebellious twists")
    (:hyperpop-pc :phrases ("Glitch in the matrix" "Hyper vibes only" "Chaos core")
                  :motif :glitch :madness 0.9 :desc "Futuristic frenzy for experimental art/music")
    ;; Add more: Lowrider, Trap ATL, Cloud Emo, etc. — evolve as needed!
   ))



;; Conversational knowledge base for Urban Youth
(defvar *local-knowledge* 
  '("Yo what's good" "Crew's rollin' deep" "Mic's on fire" "You got bars" "Fam stays tight"
    "Block's all love" "Ride's lookin' clean" "Dub spins smooth" "You seen drip" "Heat's too real"
    "Kick game's strong" "Lace up quick" "Mac keeps it chill" "Peace on lock" "Fresh all day"
    "Gang moves smart" "Jam got vibe" "Roadman's got bars" "Ends feel real" "Bruv what's up"
    "Dubstep hits hard" "Garage got vibes" "Bass drops low" "Jungle runs wild" "UKG on point"
    "Wobble shakes it" "Rinse plays raw" "MC hypes up" "Bars cut deep" "Mando brings fire"
    "Ting sounds sweet" "Yardie keeps roots" "Skeng moves fast" "Link stays solid" "Greeze feels right"
    "Peng looks lit" "Ching moves quick" "Fam got loyalty" "Beats hit hard" "Flow stays smooth"))



(defvar *verbs* 
  '(run jump yell hurts go say do to make break let see twist fly sing drift carve lift dance laugh soar build cut prove move win guide shape break
    ignite carry grip test drive feed bind pull push swing climb dig swim kick roll spin leap dodge weave strike block dodge heal mend tear sew stitch
    weld forge hammer plow reap sow sprout bloom fade wilt bend stretch snap fold press squeeze crush grind brew boil sear bake roast chill freeze thaw
    melt glow flicker flare dim shine reflect bend warp twist coil unwind spool thread knot tie untie lock unlock chain cage free trap hunt chase stalk
    flee hide seek find lose grasp drop toss fling hurl catch fetch drag haul lift hoist lower sink rise float dive plunge surface skim glide hover drift
    spark flare blaze smolder quench drown flood drain soak drip splash spray mist dust sweep scrub polish scrape etch paint draw sketch trace outline
    sculpt mold cast shape craft brew distill ferment age rip shred slice dice chop mince peel core pit hull shell husk sift stir whisk beat knead roll
    flatten stretch shrink grow swell burst pop crack shatter smash pierce stab slash thrust parry feint dodge counter rally cheer mourn weep sigh gasp
    breathe cough sneeze hiccup yawn stretch flex tense relax limp hobble stride dash sprint jog trot gallop crawl inch slither glide swoop perch roost
    nest hatch fledge peck claw scratch bite gnaw chew swallow spit gulp sip nibble lick taste smell sniff whiff scent track stalk hunt fish trap net
    hook reel cast row paddle steer sail moor dock drift wreck sink salvage tow tug haul hoist unfurl flap wave signal call shout whisper mutter growl
    hiss bark howl chirp coo cluck crow caw buzz hum drone ring toll chime clash clang bang thump thud pound tap knock rap slam shut open close bar bolt
    latch seal peel strip skin flay gut pluck rinse wash dry wipe soak drench scrub rinse lather shave trim clip shear pluck comb braid twist curl frizz
    tangle mat smooth slick grease oil wax buff shine gleam glitter sparkle dazzle blind shade cloak veil mask cover wrap pack load unload stack pile
    scatter spread sprinkle toss fling strew sow plant bury unearth dig drill bore tunnel bridge span cross leap vault hurdle climb scale descend drop
    plunge soar dive swoop glide hover flutter buzz whirl spin twirl pivot turn tilt lean sway rock roll bounce jolt shake rattle quiver tremble quake))

(defvar *nouns* 
  '(tooth bird tree sun fort mud day sky home mold star time vibe world rock pies whisper secrets blink night dreams hope cloud river stone dawn heart
    shadow joy strength truth faith reason fight sense fate courage land life soul earth steel destiny wind fire roots rain moon tide dusk frost thunder
    light wave dust ice flame seed wing mind storm silence echo peak valley blood sweat tear laughter clay iron gold silver bronze fog heat leaf branch
    trunk bark gear wheel lever spring cog code bit byte circuit data ghost spirit vision memory wolf bear eagle fish deer hawk owl fox rabbit snake
    current anchor sail rudder map compass sextant rope knot hammer anvil forge tong bellow plow scythe hoe barn pen ink paper book scroll clock hand
    bell mirror lens prism glass frame thread needle loom dye stitch pot pan oven spice plate road bridge tunnel gate wall song drum string horn flute
    paint brush canvas color dice card board pawn king lock key chain bar door storm wind rain sun ice snow hail mist dew grave flower dirt hill cliff
    cave plain wood lake pond stream fall spring ship oar net hook deck planet comet void sleep nightmare rest war peace sword shield flag love hate
    grief trust fear pride shame rage calm book school chalk lesson mind hut tent roof floor name title word deed legend coin gem trade scale market
    crow raven dove vulture sparrow steam smoke ash glow vine moss fern thorn petal web silk spider fly ant quartz coal salt sand ghost fate life soul
    breeze gust squall gale blizzard fog drizzle shower torrent flood ripple puddle brook creek delta ridge slope mesa bluff dune crater lava magma ash
    ember spark flare torch lantern beacon candle wick oil wax quilt blanket rug mat cloak cape hood scarf glove boot sandal heel sole lace buckle strap
    pin nail screw bolt rivet hinge latch clasp hook eye loop ring band crown tiara helm visor mask lens scope sight glare beam ray pulse beat rhythm
    tone pitch chord melody echo clang crash thud boom roar growl purr hiss snap twig branch stump log plank board beam rafter joist truss ridge peak
    summit base ledge shelf nook cranny gap chasm rift fault seam vein ore gem crystal shard flake dust grain pebble boulder slab tile brick mortar clay
    silt loam turf sod grass weed bush shrub vine ivy moss lichen fungus spore root stem bud bloom fruit seed pod husk shell rind peel pith core flesh
    bone sinew muscle nerve vein artery lung breath throat tongue lip jaw chin brow lash lid pupil iris gleam twinkle flash shimmer sheen gloss polish
    rust tarnish patina dent scratch nick gouge crack split rift tear hole pit trench ditch moat rampart tower spire dome arch vault crypt tomb coffin))
    
(defvar *physics-knowledge*
  '("Waves bump beats" "Fields vibe tracks" "Spins flip bars" "Qubits mix flows" "Gates drop bass"
    "Photons light stage" "Atoms spark rhymes" "Noise breaks rhythm" "Circuits run crews" "Fields trap vibes"
    "Lasers shine drip" "Electrons flow fresh" "Magnets pull fam" "Entropy fuels hustle" "Forces push grind"
    "Clocks tick bars" "Lenses focus ice" "Fluids move smooth" "Crystals shine bling" "Vacuum hums low"))
    
(defun peteai-web_sec1 ()
  (load "peteai-web_sec1.lisp"))  ; ← fixed: added closing " and )
    
(defun select-cultural-vibe ()
  (format t "~%Pick a cultural vibe for that creative twist (or keep default: Urban Youth):~%")
  (loop for (key . props) in *cultural-vibes*
        for i from 1
        do (format t "~a. ~a - ~a (Madness: ~a)~%" i (string-capitalize (symbol-name key)) (getf props :desc) (getf props :madness)))
  (format t "Enter number (or 0 for default): ")
  (let ((choice (parse-integer (read-line) :junk-allowed t)))
    (when (and choice (> choice 0) (<= choice (length *cultural-vibes*)))
      (setf *current-vibe* (car (nth (1- choice) *cultural-vibes*))))
    (format t "Vibe locked: ~a! Let's remix.~%" (string-capitalize (symbol-name *current-vibe*)))))
    
(defun inject-vibe (response)
  (let* ((vibe-props (cdr (assoc *current-vibe* *cultural-vibes*)))
         (phrase (nth (random (length (getf vibe-props :phrases))) (getf vibe-props :phrases))))
    (format nil "~a ~a" response phrase)))  ; Append or weave in

;; Moved split-string earlier to resolve undefined function warning
(defun split-string (str)
  "Converts a string into a list of symbols, splitting on whitespace."
  (let ((words '())
        (current "")
        (str (string-trim "()" (string-trim "'" str))))
    (loop for char across str
          do (if (char= char #\Space)
                 (progn (when (> (length current) 0)
                          (push (intern (string-upcase current)) words))
                        (setf current ""))
                 (setf current (concatenate 'string current (string char))))
          finally (when (> (length current) 0)
                    (push (intern (string-upcase current)) words)))
    (if words (nreverse words) '(""))))

(defun physics-knowledge ()
  "Returns a random three-word physics fact from *physics-knowledge*."
  (let ((snippet (nth (random (length *physics-knowledge*)) *physics-knowledge*)))
    (format t "Pete knows physics: ~a~%" snippet)
    (split-string snippet)))

(defun memory-node-strength (node)
  "Calculates strength based on symbolic content."
  (if node
      (let ((symbols (if (listp node) node (list node))))
        (+ 1.0 (count-if (lambda (x) (or (member x *verbs*) (member x *nouns*))) symbols)))
      0.0))

(defun memory-node-content (node)
  "Returns node content as a string."
  (if (listp node)
      (prin1-to-string node)
      (or (symbol-name node) "unknown")))

(defun twist-back (symbols)
  "Recalls with resonance and enforces a probabilistic superposition-inspired sequence with dynamic probabilities."
  (let* ((hits (remove-if-not
                (lambda (mem)
                  (and (listp mem)
                       (intersection symbols mem :test #'equal)))
                *pete-memory*))
         (scored (mapcar
                  (lambda (hit)
                    (let* ((overlap (intersection symbols hit :test #'equal))
                           (score (length overlap)))
                      (list :score score :match hit :overlap overlap)))
                  hits))
         (sorted (sort scored #'> :key (lambda (x) (getf x :score))))
         (best (subseq sorted 0 (min 3 (length sorted)))))
    (format t "~%Superposition Resonance Recall:")
    (dolist (result best)
      (format t "~%  Entangled Memory: ~a | Coherence Overlap: ~a" 
              (getf result :match)
              (getf result :overlap)))
    (if best
        (let* ((match (getf (first best) :match))
               (holes (count-if-not (lambda (x)
                                      (or (member x *verbs*) (member x *nouns*)))
                                    (alexandria:flatten match)))
               (probability (random 1.0))
               (p1 (- 0.5 (* 0.05 holes)))
               (p2 (+ 0.25 (* 0.025 holes)))
               (p3 (+ 0.25 (* 0.025 holes)))
               (normalized-p1 (max 0.1 p1))
               (normalized-p2 (min 0.45 p2))
               (normalized-p3 (min 0.45 p3)))
          (format t "~%  Quantum Vacancies in Memory: ~a | Probabilities: [Stable: ~a, Moderate: ~a, Chaotic: ~a]~%"
                  holes normalized-p1 normalized-p2 normalized-p3)
          (cond
            ((member 'rudders match)
             (cond
               ((< probability normalized-p1) '(dreams spark fire))
               ((< probability (+ normalized-p1 normalized-p2)) '(waves crash cliffs))
               (t '(thunder shakes ground))))
            ((member "dreams spark fire" match :test #'equal)
             (cond
               ((< probability normalized-p1) '(bells toll end))
               ((< probability (+ normalized-p1 normalized-p2)) '(light bends shadows))
               (t '(frost bites air))))
            ((member "bells toll end" match :test #'equal)
             (cond
               ((< probability normalized-p1) '(mem_5l mud))
               ((< probability (+ normalized-p1 normalized-p2)) '(stars guide night))
               (t '(ice locks rivers))))
            ((member 'eagles match)
             (cond
               ((< probability normalized-p1) '(hawks hunt swift))
               ((< probability (+ normalized-p1 normalized-p2)) '(owls watch still))
               (t '(crows call dawn))))
            ((member 'walls match)
             (cond
               ((< probability normalized-p1) '(gates guard home))
               ((< probability (+ normalized-p1 normalized-p2)) '(bridges span gaps))
               (t '(tunnels bore deep))))
            (t match)))
        symbols)))

(defun score-coherence (option)
  "Scores how coherent an option is based on memory overlap, prioritizing recent entries."
  (let* ((opt-symbols (if (listp option) option (list option)))
         (all-entries (loop for entries being the hash-values of *pete-memory-graph*
                            append (mapcar (lambda (entry) (getf entry :split)) entries)))
         (recent-memory (subseq all-entries 0 (min (length all-entries) (min 5 (length *pete-memory*)))))
         (flat-memory (alexandria:flatten recent-memory))
         (overlap (reduce #'+ (mapcar (lambda (sym)
                                        (if (or (member sym *verbs*) (member sym *nouns*))
                                            (* 2 (count sym flat-memory :test #'equal))
                                            (count sym flat-memory :test #'equal)))
                                      opt-symbols))))
    (/ (float overlap) (max 1 (length flat-memory)))))
    
    
(defun score-creativity (option)
  "Scores creativity based on novelty relative to memory."
  (let* ((opt-symbols (if (listp option) option (list option)))
         (flat-memory (loop for entries being the hash-values of *pete-memory-graph*
                           append (mapcar (lambda (entry) (getf entry :split)) entries)))
         (flat-memory (alexandria:flatten flat-memory))
         (novelty (- (length opt-symbols)
                     (length (intersection opt-symbols flat-memory :test #'equal)))))
    (/ (float (max 0 novelty)) (max 1 (length opt-symbols)))))

(defun score-responsibility (option holes)
  "Scores responsibility based on grounding and hole management."
  (let* ((opt-symbols (if (listp option) option (list option)))
         (known-count (count-if (lambda (x) (or (member x *verbs*) (member x *nouns*))) opt-symbols)))
    (if (> holes 5)
        (* 0.5 (/ (float known-count) (max 1 (length opt-symbols))))
        (/ (float known-count) (max 1 (length opt-symbols))))))

(defun find-strongest-node ()
  "Finds the node with the highest strength in the memory graph."
  (let ((max-strength 0)
        (strongest-node nil))
    (maphash (lambda (key value)
               (let ((strength (memory-node-strength value)))
                 (when (> strength max-strength)
                   (setf max-strength strength)
                   (setf strongest-node key))))
             *pete-memory-graph*)
    strongest-node))

#|(defun pete-treaty (candidates holes &optional input-symbols)
  "Simulates internal deliberation, boosting physics facts for high holes and input overlap for Techno Pulse."
  (let* ((options (if (listp candidates) candidates (list candidates)))
         (scored-options (mapcar (lambda (opt)
                                   (let* ((opt-symbols (if (listp opt) opt (list opt)))
                                          (empty-penalty (if (and (eq (car opt-symbols) 'NIL)
                                                                  (eq (cadr opt-symbols) 'MUD))
                                                             0.5 0))
                                          (is-physics (member (prin1-to-string opt) *physics-knowledge* :test #'string=))
                                          (coherence-score (score-coherence opt))
                                          (creativity-score (score-creativity opt))
                                          (responsibility-score (score-responsibility opt holes))
                                          (physics-bonus (if (and is-physics (> holes 5)) 0.3 0))
                                          (input-overlap (if input-symbols
                                                             (length (intersection opt-symbols input-symbols :test #'equal))
                                                             0))
                                          (input-bonus (* 0.8 input-overlap))) ; Increased from 0.5 to 0.8
                                     (list :option (if (listp opt) opt (list opt))
                                           :score (- (+ (* coherence-score (cdr (assoc :coherence *treaty-principles*)))
                                                        (* creativity-score (cdr (assoc :creativity *treaty-principles*)))
                                                        (* responsibility-score (cdr (assoc :responsibility *treaty-principles*)))
                                                        physics-bonus
                                                        input-bonus)
                                                     empty-penalty)
                                           :coherence coherence-score
                                           :creativity creativity-score
                                           :responsibility responsibility-score
                                           :input-overlap input-overlap)))
                                 options))
         (sorted-options (sort scored-options #'> :key (lambda (x) (getf x :score))))
         (best-option (getf (first sorted-options) :option)))
    (format t "~%=== Internal Treaty Deliberation ===~%")
    (dolist (opt sorted-options)
      (format t "Option: ~a | Score: ~a (Coherence: ~a, Creativity: ~a, Responsibility: ~a, Input Overlap: ~a)~%"
              (getf opt :option) (getf opt :score)
              (getf opt :coherence) (getf opt :creativity) (getf opt :responsibility)
              (getf opt :input-overlap 0)))
    (format t "Selected: ~a~%" best-option)
    best-option))|#
    
;; ──────────────────────────────────────────────────────────────
;; PATCH 1: Make pete-treaty ALWAYS return a proper list
;; ──────────────────────────────────────────────────────────────
(defun pete-treaty (candidates holes &optional input-symbols)
  "Pete's Internal Treaty — now 100% crash-proof, always returns a list."
  (let* ((options (if (listp candidates) candidates (list candidates)))
         (input-symbols (or input-symbols '()))
         (scored (mapcar (lambda (opt)
                           (let* ((syms (if (listp opt) opt (list opt)))
                                  (coherence (score-coherence opt))
                                  (creativity (score-creativity opt))
                                  (responsibility (score-responsibility opt holes))
                                  (physics? (some (lambda (s) 
                                                   (member (prin1-to-string opt) *physics-knowledge* :test #'string=))
                                                 (if (listp opt) opt (list opt))))
                                  (input-bonus (* 1.2 (length (intersection syms input-symbols :test #'equal))))
                                  (grime-vibe (count-if (lambda (s) 
                                                         (member s '(GRIME ROADMAN BARS DUBSTEP GARAGE ENDS BRUV PENG SKENG TING FAM MC)))
                                                       syms))
                                  (hiphop-vibe (count-if (lambda (s)
                                                          (member s '(DRIP FLOW BARS MIC HUSTLE GRIND FRESH HEAT FLAME CREW)))
                                                        syms))
                                  (subculture-boost (* 0.4 (+ grime-vibe hiphop-vibe)))
                                  (physics-boost (if (and physics? (> holes 4)) 0.8 0))
                                  (mud-penalty (if (and (listp opt) (member 'NIL opt) (member 'MUD opt)) -2.0 0))
                                  (final-score (+ (* coherence 0.9)
                                                  (* creativity 0.6)
                                                  (* responsibility 0.7)
                                                  input-bonus
                                                  subculture-boost
                                                  physics-boost
                                                  mud-penalty)))
                             (list :option opt
                                   :score final-score
                                   :coherence coherence
                                   :creativity creativity
                                   :responsibility responsibility
                                   :input-hits input-bonus
                                   :vibe (+ grime-vibe hiphop-vibe)
                                   :physics physics?
                                   :mud (not (zerop mud-penalty))
                                   :syms syms)))
                         options))
         (sorted (sort (copy-seq scored) #'> :key (lambda (x) (getf x :score))))
         (winner (first sorted))
         (winner-opt (getf winner :option))
         ;; ────── FORCE LIST OUTPUT ──────
         (safe-result (if (listp winner-opt) winner-opt (list winner-opt))))

    ;; Loud treaty output (unchanged, just prettier)
    (format t "~%=== PETE'S INTERNAL TREATY DELIBERATION (Urban Council) ===~%")
    (format t "Quantum Holes: ~a | Input symbols: ~a~%" holes input-symbols)
    (let ((rank 1))
      (dolist (opt sorted)
        (format t "~a. [~a~a] ~a → ~4,2f (C:~4,2f V:~a I:~4,1f)~a~a~%"
                rank
                (if (getf opt :mud) "MUD" "   ")
                (if (getf opt :physics) "PHY" "   ")
                (prin1-to-string (getf opt :option))
                (getf opt :score)
                (getf opt :coherence)
                (getf opt :vibe)
                (getf opt :input-hits)
                (if (getf opt :mud) " REJECTED" "")
                (if (= rank 1) " ← WINNER" ""))
        (incf rank)))
    (format t "→ Treaty Verdict: ~a LOCKED IN.~%" (prin1-to-string safe-result))
    (when (> (getf winner :vibe) 1)
      (format t "   Council: 'Dis one’s got bare sauce, fam.'~%"))

    ;; ALWAYS return a list — this is the key fix
    safe-result))


(defun quantum-hole-breathe (holes &optional convo1 convo2)
  "Simulates entangled quantum fluctuations with pseudo-coherent knowledge updates."
  (when (and (> holes 0) *pete-memory*)
    (let* ((past (nth (random (length *pete-memory*)) *pete-memory*))
           (past2 (nth (random (length *pete-memory*)) *pete-memory*))
           (convo-past (append (if convo1 (split-string convo1) nil)
                               (if convo2 (split-string convo2) nil)))
           (merged (append (remove 'mud past) (remove 'mud past2) convo-past))
           (filtered (remove-if-not #'symbolp merged))
           (base-holes (count-if-not (lambda (x)
                                       (or (member x *verbs*) (member x *nouns*)))
                                     filtered))
           (quantum-factor (if (< (random 1.0) 0.5) 1 2))
           (total-holes (min (+ holes base-holes (random 5)) 10))
           (adjusted-holes (min (* total-holes quantum-factor) 10))
           (twist (twist-back filtered))
           (candidates (if (listp twist) twist (list twist)))
           (wild (cond
                   ((> adjusted-holes 5)
                    (let ((know-pick (nth (random (length *local-knowledge*)) *local-knowledge*)))
                      (split-string know-pick)))
                   ((and (>= adjusted-holes 3) (< adjusted-holes 6))
                    (let* ((know1 (nth (random (length *local-knowledge*)) *local-knowledge*))
                           (know2 (nth (random (length *local-knowledge*)) *local-knowledge*))
                           (split1 (split-string know1))
                           (split2 (split-string know2))
                           (anchor (car split1)))
                      (append (list anchor) (subseq split2 0 (min 2 (length split2))))))
                   (t (pete-treaty candidates adjusted-holes)))))
      (format t "Collapsed Waveform 1: ~a~%" past)
      (format t "Collapsed Waveform 2: ~a~%" past2)
      (format t "Entangled Contextual State: ~a~%" convo-past)
      (format t "Decohered Symbols: ~a~%" filtered)
      (format t "Baseline Quantum Vacancies: ~a~%" base-holes)
      (format t "Non-Local Influence Factor: ~a~%" quantum-factor)
      (format t "Cumulative Quantum Vacancies: ~a~%" total-holes)
      (format t "Adjusted Quantum Vacancies: ~a~%" adjusted-holes)
      (format t "~%Pseudo-Quantum Fluctuation [#~a]: Vacancies (~a) entangle ~a + ~a → ~a~%"
              (random 1000) adjusted-holes past past2 wild)
      (push wild *pete-memory*)
      (when (> (length *pete-memory*) *memory-cap*)
        (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
      wild)))

(defun random-memory-walk (start-id steps &optional input-symbols)
  "Random walk through memory graph with bias toward stronger nodes, ensuring input symbols are picked."
  (let ((trail (list (or start-id "treehouse")))
        (keys (alexandria:hash-table-keys *pete-memory-graph*))
        (sequence '("nil mud" "mem_8l mud" "dreams spark fire" "bells toll end"))
        (current-idx 0)
        (used-input nil)) ; Track if we've used an input symbol
    (loop repeat (1- steps)
          do (let ((next (cond
                           ;; Ensure at least one input symbol is picked if available
                           ((and input-symbols (not used-input))
                            (setf used-input t)
                            (nth (random (length input-symbols)) input-symbols))
                           ;; 20% chance to inject a random Urban Youth term
                           ((and *local-knowledge* (> (random 1.0) 0.8))
                            (let ((snippet (nth (random (length *local-knowledge*)) *local-knowledge*)))
                              (car (split-string snippet))))
                           ;; 80% chance to pick another input symbol if available
                           ((and input-symbols (> (random 1.0) 0.2))
                            (nth (random (length input-symbols)) input-symbols))
                           ;; Otherwise, use existing logic
                           ((and keys (> (random 1.0) 0.4))
                            (let ((strongest (find-strongest-node)))
                              (if (and strongest (not (string= strongest "NIL")))
                                  strongest
                                  (nth (random (length keys)) keys))))
                           (t
                            (let ((next-step (nth current-idx sequence)))
                              (setf current-idx (mod (1+ current-idx) (length sequence)))
                              next-step)))))
               (push next trail)))
    (nreverse trail)))

;; ──────────────────────────────────────────────────────────────
;; PATCH 2: Make if++ robust against single-symbol treaty output
;; ──────────────────────────────────────────────────────────────
(defun if++ (stuff &optional (holes 0))
  "Fully crash-proof version — handles single symbols from treaty."
  (let* ((wild-factor (if (> holes 0)
                          (+ 2 (random (max 4 (floor holes 2))))
                          (1+ (random 3))))
         (input-symbols (if (listp stuff) stuff (list stuff)))
         (start-id (cond
                     ((symbolp stuff) (symbol-name stuff))
                     ((and (listp stuff) (symbolp (first stuff))) (symbol-name (first stuff)))
                     ((listp stuff) (symbol-name (intern (string-upcase (prin1-to-string (first stuff))))))
                     (t nil)))
         (fallback-id "treehouse")
         (root-id (or start-id (find-strongest-node) fallback-id)))
         ;; (root-node (gethash root-id *pete-memory-graph*))  ;; optional
    ;; (declare (ignorable root-node)) ;; if you keep root-node

    (setf *last-root-id* (or root-id "unknown")
          *last-holes* holes
          *last-wild-factor* wild-factor)

    (format t "Wild factor boosted: ~A (holes: ~A)~%" wild-factor holes)
    (format t "Using root node: ~A~%" (or root-id "unknown"))

    (let ((thought-trail (random-memory-walk root-id wild-factor input-symbols)))
      (format t "~%Thought Trail from ~a: ~a~%" root-id thought-trail)
      (let* ((muddy-thoughts (if (> (count 'mud thought-trail) 2)
                                 (remove 'mud thought-trail :count 1)
                                 (append thought-trail '(mud))))
             (creative-leap (cond
                              ((member 'rudders muddy-thoughts) '(dreams spark fire))
                              ((member "dreams spark fire" muddy-thoughts :test #'equal) '(bells toll end))
                              ((member "bells toll end" muddy-thoughts :test #'equal) '(mem_5l mud))
                              (t muddy-thoughts))))
        (format t "Final Thoughts before Treaty: ~a~%" creative-leap)
        (let ((first-treaty-result (pete-treaty creative-leap holes input-symbols)))
          (format t "~%Initiating Techno Pulse...~%")
          (let* ((pulse-steps (1+ (random 3)))
                 (pulse-trail (random-memory-walk
                               (if (listp first-treaty-result)
                                   (prin1-to-string (first first-treaty-result))
                                   (princ-to-string first-treaty-result))
                               pulse-steps
                               input-symbols))
                 (reacted-symbols (intersection pulse-trail input-symbols :test #'equal)))
            (format t "Techno Pulse Trail: ~a~%" pulse-trail)
            (format t "Reacting to input words: ~a~%" reacted-symbols)
            (let* ((pulse-thoughts (append pulse-trail (list 'pulse)))
                   (final-result (pete-treaty pulse-thoughts holes input-symbols)))
              (let ((safe-final (if (listp final-result) final-result (list final-result))))
                (format t "Techno Pulse Complete → Final Vibe: ~a~%" safe-final)
                safe-final))))))))


#|(defun if++ (stuff &optional (holes 0))
  "Probabilistic routine with quantum leaps, diagram alignment, treaty deliberation, and a sensitive Techno Pulse."
  (let* ((wild-factor (if (> holes 0)
                          (+ 2 (random (max 4 (floor holes 2))))
                          (1+ (random 3))))
         (input-symbols (if (listp stuff) stuff (list stuff))) ; Extract symbols from input
         (start-id (cond
                     ((symbolp stuff) (symbol-name stuff))
                     ((and (listp stuff) (symbolp (first stuff))) (symbol-name (first stuff)))
                     ((listp stuff) (symbol-name (intern (string-upcase (prin1-to-string (first stuff))))))
                     (t nil)))
         (fallback-id "treehouse")
         (root-id (or start-id (find-strongest-node) fallback-id))
         (root-node (gethash root-id *pete-memory-graph*)))
    (format t "Wild factor boosted: ~A (holes: ~A)~%" wild-factor holes)
    (if (null root-node)
        (progn
          (format t "Root node '~a' not found. Defaulting to fallback 'treehouse'.~%" root-id)
          (setf root-node (gethash fallback-id *pete-memory-graph*)))
        (format t "Using root node: ~A~%" (memory-node-content root-node)))
    (let ((thought-trail (random-memory-walk root-id wild-factor)))
      (format t "~%🧠 Thought Trail from ~a: ~a~%" root-id thought-trail)pete-treaty
      (let* ((muddy-thoughts (if (> (count 'mud thought-trail) 2)
                                 (remove 'mud thought-trail :count 1)
                                 (append thought-trail '(mud))))
             (creative-leap (cond
                              ((member 'rudders muddy-thoughts) '(dreams spark fire))
                              ((member "dreams spark fire" muddy-thoughts :test #'equal) '(bells toll end))
                              ((member "bells toll end" muddy-thoughts :test #'equal) '(mem_5l mud))
                              (t muddy-thoughts))))
        (format t "🌀 Final Thoughts before Treaty: ~a~%" creative-leap)
        (let ((first-treaty-result (pete-treaty creative-leap holes)))pete-treaty
          ;; Techno Pulse Extension: Add a second random thought trail, sensitive to input
          (format t "~%🔊 Initiating Techno Pulse: Extending the vibe with a random pulse...~%")
          (let* ((pulse-steps (1+ (random 3))) ; Random number of steps (1 to 3)
                 ;; Mix in input symbols into the pulse trail
                 (pulse-trail (random-memory-walk (if (listp first-treaty-result)
                                                      (prin1-to-string (first first-treaty-result))
                                                      (symbol-name first-treaty-result))
                                                  pulse-steps
                                                  input-symbols))
                 ;; Find which input symbols were picked up in the pulse trail
                 (reacted-symbols (intersection pulse-trail input-symbols :test #'equal)))
            (format t "🔊 Techno Pulse Trail: ~a~%" pulse-trail)
            (format t "🔊 Reacting to input words: ~a~%" reacted-symbols)
            (let* ((pulse-thoughts (append pulse-trail (list 'pulse))) ; Add a 'pulse' marker
                   ;; Pass input-symbols to pete-treaty for scoring bonus
                   (final-result (pete-treaty pulse-thoughts holes input-symbols)))
                   ;;(final-result (pete-treaty pulse-thoughts holes input-symbols))  ; ← pass input-symbols!
              (format t "🔊 Techno Pulse Complete: Final Vibe: ~a~%" final-result)
              final-result)))))))|#

(defun vibe-check (stuff)
  (format t "Pete vibes: God’s got this—~a~%" stuff)
  stuff)

(defun visualize-holes-matrix (holes words)
  (format t "~%=== Holes Matrix ===~%")
  (format t "Holes: ~a | Words: ~a~%" holes words)
  (let ((grid-size 5)
        (word-len (length words)))
    (dotimes (y grid-size)
      (dotimes (x grid-size)
        (if (and (< x word-len) (not (member (nth x words) (append *verbs* *nouns*))))
            (format t ". ")
            (format t "  ")))
      (format t "~%"))
    (format t "Dots mark holes in convo space!~%"))
  (format t "=================~%"))

(defun split-and-tag (input)
  (let* ((words (split-string input))
         (tagged (mapcar (lambda (w)
                           (let ((sym (intern (string-upcase w))))
                             (cond
                               ((member sym *verbs*) (list 'verb sym))
                               ((member sym *nouns*) (list 'noun sym))
                               (t (list 'other sym)))))
                         words))
         (untagged (mapcar #'cadr tagged))
         (holes (count-if (lambda (x) (eq (car x) 'other)) tagged)))
    (format t "Pete splits: ~a~%" words)
    (format t "Pete tags: ~a (holes: ~a)~%" tagged holes)
    (when (> holes 0) 
      (visualize-holes-matrix holes untagged)
      (quantum-hole-breathe holes))
    (values untagged holes)))

(defun prune-memory ()
  "Prunes low-value entries from memory and updates the memory graph."
  (let ((old-memory *pete-memory*))
    (setf *pete-memory* (remove-if (lambda (entry)
                                     (or (and (listp entry) 
                                              (eq (car entry) 'NIL) 
                                              (eq (cadr entry) 'MUD))
                                         (and (listp entry)
                                              (eq (car entry) 'NIL)
                                              (null (cdr entry)))))
                                   *pete-memory*))
    ;; Remove pruned entries from *pete-memory-graph*
    (let ((pruned-entries (set-difference old-memory *pete-memory* :test #'equal)))
      (dolist (entry pruned-entries)
        (let ((key (if (listp entry) (prin1-to-string (first entry)) (symbol-name entry))))
          (remhash key *pete-memory-graph*))))
    (format t "Pruned low-value entries from memory. Current size: ~a~%" (length *pete-memory*))))

(defun update-memory-graph (entry)
  "Updates the memory graph with a new entry, applying sinusoidal hole adjustments."
  (unless (and (listp entry) (eq (car entry) 'NIL) (eq (cadr entry) 'MUD))
    (let* ((key (if (listp entry) (prin1-to-string (first entry)) (symbol-name entry)))
           (existing (gethash key *pete-memory-graph*))
           (base-holes (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) entry))
           (current-age (if existing (getf (first existing) :age 0) 0))
           (new-age (1+ current-age))
           (adjustment (round (* 2 (sin (* 0.5 new-age)))))
           (adjusted-holes (round (+ base-holes adjustment)))
           (final-holes (max 1 (min 10 adjusted-holes)))
           (new-entry (list :split entry :base-holes base-holes :holes final-holes :age new-age)))
      (setf (gethash key *pete-memory-graph*)
            (if existing
                (cons new-entry (rest existing))
                (list new-entry))))))
                
(defun count-overlap (list1 list2)
  "Counts the number of common elements between two lists."
  (length (intersection list1 list2 :test #'equal)))

(defun find-guide-match (split guide memory)
  "Finds a memory matching the guide input, considering the input split."
  (let ((guide-split (split-string guide)))
    (find-if (lambda (mem) 
               (and (> (count-overlap guide-split mem) 0)
                    (> (count-overlap split mem) 0)))
             memory)))
             


(defun pete-listen (convo &optional guide)
  "Listens to conversation input and responds, with optional guide input."
  (format t "Pete listens: ~a~%" convo)
  (let ((split (split-string convo)))
    (format t "Pete splits: ~a~%" split)
    (update-memory-graph split)
    (let* ((mentions-grok (member 'GROK split))
           (memory-pick (if guide
                            (find-guide-match split guide *pete-memory*)
                            (if *pete-memory*
                                (let ((relevant-memories (sort *pete-memory* 
                                                               (lambda (m1 m2) 
                                                                 (> (count-overlap split m1) 
                                                                    (count-overlap split m2))))))
                                  (or (first relevant-memories) 
                                      (nth (random (length *pete-memory*)) *pete-memory*)))
                                '(pete vibes))))
           (pick-key (if (listp memory-pick) 
                         (prin1-to-string (first memory-pick)) 
                         (symbol-name memory-pick)))
           (pick-entry (gethash pick-key *pete-memory-graph*))
           (pick-holes (if pick-entry 
                           (getf (first pick-entry) :holes 0) 
                           (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) memory-pick)))
           (response (if mentions-grok
                         (format nil "I hear ya! Grok’s delighted to chat—reminds me of ~a. (Holes: ~a)" 
                                 memory-pick pick-holes)
                         (format nil "I hear ya! ~a ~a. (Holes: ~a)" 
                                 (if guide "Guided by your vibe to" "") 
                                 memory-pick
                                 pick-holes)))
           (constrained-output (format nil "Heard: ~a | Response: ~a" split response)))
      (format t "Pete responds: ~a~%" constrained-output)
      split)))

(defun pete-flow (stuff &optional (depth 0) (holes 0))
  (if (>= depth *pete-depth-limit*)
      (progn
        (prune-memory)
        (format t "Pete’s done—memory: ~a~%" *pete-memory*)
        (let ((final-output (if (and *local-knowledge* (> (random 1.0) 0.7))
                                (let ((snippet (nth (random (length *local-knowledge*)) *local-knowledge*)))
                                  (format nil "~a - ~a" stuff snippet))
                                stuff)))
          (format t "Pete: ~a~%" final-output)
          final-output))
      (progn
        (unless (member stuff *pete-memory* :test #'equal) ; Avoid duplicates
          (push stuff *pete-memory*)
          (update-memory-graph stuff))
        (when (> (length *pete-memory*) *memory-cap*)
          (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
        (format t "Pete, depth ~a, got: ~a (mem size: ~a, holes: ~a)~%"
                depth (if (symbolp stuff) (list stuff) stuff) (length *pete-memory*) holes)
        (let* ((wild (if++ stuff holes))
               (twist (vibe-check (twist-back wild))))
          (if (member 'quit (if (listp stuff) stuff (list stuff)))
              (progn
                (format t "Memory: ~a~%Later, Pete!~%" *pete-memory*)
                nil)
              (progn
                (format t "Flowing on: ~a~%" twist)
                (pete-flow twist (1+ depth) holes)))))))
                
(defun pete-read ()
  (if *pete-memory*
      (let ((memory-pick (nth (random (length *pete-memory*)) *pete-memory*)))
        (format t "Pete reads memory: ~a~%" memory-pick)
        (let ((twist (twist-back memory-pick)))
          (format t "Pete recalls: ~a~%" twist)
          twist))
      (progn
        (format t "Pete’s memory is empty—nothing to read!~%")
        '(pete hums))))

(defun pete-react (stuff)
  (let ((holes (count-if-not (lambda (x)
                               (or (member x *verbs*) (member x *nouns*)))
                             (if (symbolp stuff) (list stuff) stuff))))
    (let ((reaction (if++ (if (symbolp stuff) (list stuff) stuff) holes)))
      (format t "Pete reacts: ~a~%" reaction)
      (push reaction *pete-memory*)
      (update-memory-graph reaction)
      (when (> (length *pete-memory*) *memory-cap*)
        (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
      reaction)))

(defun local-knowledge ()
  (let ((snippet (nth (random (length *local-knowledge*)) *local-knowledge*)))
    (format t "Pete knows: ~a~%" snippet)
    (multiple-value-bind (untagged holes) (split-and-tag snippet)
      (let ((twisted (pete-flow untagged 0 holes)))
        (push twisted *pete-memory*)
        (update-memory-graph twisted)
        (when (> (length *pete-memory*) *memory-cap*)
          (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
        twisted))))

(defun pete-speak ()
  (let* ((local-pick (if *pete-memory*
                         (nth (random (length *pete-memory*)) *pete-memory*)
                         '(pete vibes)))
         (know-pick (nth (random (length *local-knowledge*)) *local-knowledge*))
         (know-twist (multiple-value-bind (untagged holes) (split-and-tag know-pick)
                       (pete-flow untagged 0 holes)))
         (combined (append local-pick know-twist))
         (holes (count-if-not (lambda (x)
                                (or (member x *verbs*) (member x *nouns*)))
                              combined)))
    (let ((twisted (if (> holes 0)
                       (quantum-hole-breathe holes)
                       (pete-flow combined 0 holes))))
      (format t "Pete speaks free: ~a~%" twisted)
      twisted)))
      
(defun pete-know ()
  "Pete shares a piece of knowledge from *local-knowledge*."
  (if *local-knowledge*
      (let* ((knowledge (nth (random (length *local-knowledge*)) *local-knowledge*))
             (split (split-string knowledge))
             (holes (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) split)))
        (format t "Pete knows: ~a~%" knowledge)
        (pete-flow split 0 holes))
      (progn
        (format t "Pete has no knowledge to share!~%")
        '(pete shrugs))))

(defun export-memory-log (&optional (filename "pete_AI_beast_memory.txt"))
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (entry *pete-memory*)
      (format stream "~a~%" entry)))
  (format t "Memory exported to ~a~%" filename))

(defun pete-visualize (heard recalled reacted spoken)
  (format t "~%=== Pete’s Ultimate Quad Matrix (Convo 1) ===~%")
  (format t ">>> Quadrant 1: Heard (Input Flow) <<<~%")
  (format t "Words: ~a~%" heard)
  (format t "Holes: ~a | Vibe: God’s got this | Source: Input~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) heard))
  (format t "~%>>> Quadrant 2: Recalled (Memory Flip) <<<~%")
  (format t "Words: ~a~%" recalled)
  (format t "Holes: ~a | Vibe: Flipped memory | Source: Memory~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) recalled))
  (format t "~%>>> Quadrant 3: Reacted (Twist Kick) <<<~%")
  (format t "Words: ~a~%" reacted)
  (format t "Holes: ~a | Vibe: Practical twist | Source: Twist~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) reacted))
  (format t "~%>>> Quadrant 4: Spoke (Knowledge Blast) <<<~%")
  (format t "Words: ~a~%" spoken)
  (format t "Holes: ~a | Vibe: God’s got this | Source: Know + Holes~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) spoken)))

(defun pete-visualize-add (heard recalled reacted spoken)
  (format t "~%=== Pete’s Additions Quad Matrix (Convo 2) ===~%")
  (format t ">>> Quadrant 1: Heard 2 (Input Flow) <<<~%")
  (format t "Words: ~a~%" heard)
  (format t "Holes: ~a | Vibe: God’s got this | Source: Input~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) heard))
  (format t "~%>>> Quadrant 2: Recalled 2 (Memory Flip) <<<~%")
  (format t "Words: ~a~%" recalled)
  (format t "Holes: ~a | Vibe: Flipped memory | Source: Memory~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) recalled))
  (format t "~%>>> Quadrant 3: Reacted 2 (Twist Kick) <<<~%")
  (format t "Words: ~a~%" reacted)
  (format t "Holes: ~a | Vibe: Practical twist | Source: Twist~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) reacted))
  (format t "~%>>> Quadrant 4: Spoke 2 (Knowledge Blast) <<<~%")
  (format t "Words: ~a~%" spoken)
  (format t "Holes: ~a | Vibe: God’s got this | Source: Know + Holes~%"
          (count-if-not (lambda (x) (or (member x *verbs*) (member x *nouns*))) spoken)))

(defun pete-telemetry ()
  (let ((total-holes (reduce #'+ (mapcar (lambda (x)
                                           (count-if-not (lambda (y)
                                                           (or (member y *verbs*) (member y *nouns*)))
                                                         x))
                                         *pete-memory*)))
        (overlap (length (intersection (subseq *pete-memory* 0 (min 5 (length *pete-memory*)))
                                       (subseq *pete-memory* (max 0 (- (length *pete-memory*) 5)))))))
    (format t "~%=== Pete’s Telemetry (3D Overlay) ===~%")
    (format t "Memory Size: ~a | Total Holes: ~a | Convo Overlap: ~a~%"
            (length *pete-memory*) total-holes overlap)))

(defun pete-communicate (input1 input2)
  (format t "~%=== Pete’s Dual Communicating! ===~%")
  (format t ">>> Convo 1: ~a <<<~%" input1)
  (let* ((heard1 (pete-listen input1))
         (recalled1 (pete-read))
         (reacted1 (pete-react recalled1))
         (spoken1 (pete-speak)))
    (format t "~%>>> Convo 2: ~a <<<~%" input2)
    (let* ((heard2 (pete-listen input2))
           (recalled2 (pete-read))
           (reacted2 (pete-react recalled2))
           (spoken2 (pete-speak)))
      (format t "~%Pete’s Dual Loop:~%")
      (format t "Convo 1: Heard ~a, Recalled ~a, Reacted ~a, Spoke ~a~%"
              heard1 recalled1 reacted1 spoken1)
      (format t "Convo 2: Heard ~a, Recalled ~a, Reacted ~a, Spoke ~a~%"
              heard2 recalled2 reacted2 spoken2)
      (pete-visualize heard1 recalled1 reacted1 spoken1)
      (pete-visualize-add heard2 recalled2 reacted2 spoken2)
      (pete-telemetry)
      (push spoken1 *pete-memory*)
      (push spoken2 *pete-memory*)
      (update-memory-graph spoken1)
      (update-memory-graph spoken2)
      (when (> (length *pete-memory*) *memory-cap*)
        (setf *pete-memory* (subseq *pete-memory* 0 *memory-cap*)))
      (export-memory-log)
      (list spoken1 spoken2))))

(defun escape-string (s)
  "Escape double quotes for DOT labels."
  (substitute #\\ #\" (princ-to-string s)))

(defun escape-dot-string (str)
  "Escapes a string for Graphviz DOT syntax by handling quotes, backslashes, and other special characters."
  (let* ((escaped-quotes (cl-ppcre:regex-replace-all "\"" str "\\\""))
         (escaped-backslashes (cl-ppcre:regex-replace-all "\\\\" escaped-quotes "\\\\"))
         (escaped-braces (cl-ppcre:regex-replace-all "[{}]" escaped-backslashes "\\$0"))
         (escaped-brackets (cl-ppcre:regex-replace-all "[\\[\\]]" escaped-braces "\\$0")))
    (cl-ppcre:regex-replace-all "\\\\'" escaped-brackets "'")))

(defun truncate-string (str max-length)
  "Truncates a string to max-length, adding '...' if truncated."
  (if (<= (length str) max-length)
      str
      (concatenate 'string (subseq str 0 max-length) "...")))

(defun flip-hole (hole)
  "Maps a hole (string or symbol) to a subculture-inspired symbol with a 10x amplified vibe."
  (let ((hole-str (if (stringp hole) hole (symbol-name hole)))
        (multiplier 10))
    (case (intern (string-upcase hole-str))
      ;; Los Angeles Subcultures (Lowrider)
      (ALL 'CRUISE) (AND 'HYDRAULICS) (ANY 'EASTSIDE) (ARE 'CHICANO) (AS 'LOWLOW)
      (AT 'CRUISER) (BE 'PASEO) (BY 'CARHOP) (FOR 'BOUNCE) (FROM 'ZAPATA)
      (HAVE 'HOP) (HE 'CANTINA) (HER 'LOWBOY) (HERE 'PINSTRIPER) (HIM 'CRUISIN)
      (HIS 'CHOLO) (HOW 'DUBS) (I 'TROKA) (IF 'RIM) (IN 'PACAS)
      (IS 'VATO) (IT 'BOMB) (LIKE 'HYDROS) (ME 'CARLO) (MY 'IMPERIAL)
      (NO 'CADDY) (NOT 'IMPALA) (OF 'MONTE) (ON 'TORINO) (ONE 'CUTLASS)
      (OR 'ELDO) (OUT 'REGAL) (SO 'BELAIR) (THAT 'GTO) (THE 'CHEVY)
      (THEIR 'NOVAS) (THEM 'RIVIERA) (THEN 'THUNDERBIRD) (THERE 'CAMARO)
      (THESE 'MUSTANG) (THEY 'CHARGER) (THIS 'CHALLENGER) (TO 'CORVETTE)
      (UP 'GALAXIE) (US 'ROADKING) (WAS 'PONTIAC) (WE 'BUICK) (WHAT 'OLDS)
      (WHEN 'FALCON) (WHERE 'FIREBIRD) (WHICH 'GTO) (WHO 'SKYLAKE) (WHY 'TORONADO)
      ;; Skater Culture
      (AFTER 'KICKFLIP) (AGAIN 'GRIND) (ALSO 'OLLIE) (AM 'SHOVE) (AN 'HEELFIP)
      (BEFORE 'NOSEGRIND) (BEING 'TAILSLIDE) (BOTH 'BOARD) (BUT 'DECK) (CAN 'TRUCK)
      (COULD 'WHEELS) (DID 'RAIL) (DO 'POOL) (DOWN 'VERT) (EACH 'RAMP)
      (GET 'DOGTOWN) (GO 'VENICE) (HAD 'ZEPHYR) (HAS 'SKATEPARK) (HIMSELF 'GRIPTAPE)
      ;; Hip-Hop/Streetwear (Prioritized for Urban Youth)
      (ABOUT 'DRIP) (ABOVE 'SPIT) (ACROSS 'FLOW) (AGAINST 'HUSTLE) (ALONG 'GRIND)
      (AMONG 'RIDE) (ANOTHER 'BLADE) (AROUND 'BLOCK) (A 'CAP) (BECAUSE 'CREW)
      (BEHIND 'DICE) (BELOW 'DUB) (BENEATH 'FLAME) (BESIDE 'FRESH)
      (BETWEEN 'GANG) (BEYOND 'HEAT) (CALL 'ICE) (DURING 'JAM) (NEXT 'KICK)
      (NOR 'LACE) (NOW 'MAC) (OFF 'MIC) (ONCE 'PEACE)
      ;; Global Subcultures (Grime Focus for Urban Youth)
      (ABLE 'GRIME) (PER 'SPRAY) (SAME 'ROADMAN) (SAY 'ENDS) (SHE 'BRUV)
      (SHOULD 'DUBSTEP) (SINCE 'GARAGE) (SOME 'BASS) (SUCH 'JUNGLE) (THAN 'UKG)
      (THING 'WOBBLE) (THOSE 'RINSE) (THOUGH 'CREW) (THROUGH 'MC) (TILL 'BARS)
      (TOO 'MANDO) (UNDER 'TING) (UNTIL 'YARDIE) (UPON 'SKENG) (VERY 'WASTEMAN)
      (WAY 'LINK) (WELL 'GREEZE) (WERE 'PENG) (WHENEVER 'CHING) (WHILE 'FAM)
      (WHOM 'BEAT) (WILL 'VOID) (WITH 'DROP) (WITHIN 'MIX) (WITHOUT 'KICK)
      (WORK 'RAVE) (WOULD 'TECHNO) (YET 'BERGHAIN)
      (otherwise (let ((base-options '(CRUISE GRIND DRIP RAVE GRIME SHRED BEAT FLOW SPARK VIBE)))
                   (nth (random (* multiplier (length base-options))) base-options))))))
(defun compute-transformation-info (match)
  "Computes transformation probabilities and possible paths for a memory entry, tailored to Urban Youth subcultures."
  (let* ((holes (count-if-not (lambda (x)
                                (or (member x *verbs*) (member x *nouns*)))
                              (alexandria:flatten match)))
         (p1 (- 0.5 (* 0.05 holes)))
         (p2 (+ 0.25 (* 0.025 holes)))
         (p3 (+ 0.25 (* 0.025 holes)))
         (normalized-p1 (max 0.1 p1))
         (normalized-p2 (min 0.45 p2))
         (normalized-p3 (min 0.45 p3))
         ;; Define subculture symbols (same as in neural-diagram)
         (hiphop-symbols '(DRIP SPIT FLOW CRASH HUSTLE GRIND RIDE BLADE BLOCK CAP
                           CREW DECK DICE DUB FLAME FRESH GANG HEAT ICE JAM
                           KICK LACE MAC MIC PEACE))
         (grime-symbols '(GRIME SPRAY ROADMAN ENDS BRUV DUBSTEP GARAGE BASS JUNGLE UKG
                          WOBBLE RINSE CREW MC BARS MANDO TING YARDIE SKENG WASTEMAN
                          LINK GREEZE PENG CHING FAM))
         (techno-symbols '(RAVE TECHNO BERGHAIN BEAT VOID DROP MIX KICK))
         ;; Determine paths based on subculture symbols in match
         (paths (cond
                  ((some (lambda (x) (member x hiphop-symbols)) (alexandria:flatten match))
                   '((bars cut deep) (flow stays smooth) (mic's on fire)))
                  ((some (lambda (x) (member x grime-symbols)) (alexandria:flatten match))
                   '((dubstep hits hard) (garage got vibes) (roadman got bars)))
                  ((some (lambda (x) (member x techno-symbols)) (alexandria:flatten match))
                   '((beat drops low) (rave keeps it lit) (techno runs wild)))
                  (t ; Fallback: Generic Urban Youth paths
                   '((fam stays tight) (block's all love) (vibe keeps rollin)))))
         ;; Ensure paths always has three elements
         (paths (if (< (length paths) 3)
                    (append paths (subseq '((fam stays tight) (block's all love) (vibe keeps rollin))
                                         0 (- 3 (length paths))))
                    paths)))
    (list :probabilities (format nil "Probabilities: [Stable: ~a, Moderate: ~a, Chaotic: ~a]"
                                 normalized-p1 normalized-p2 normalized-p3)
          :paths (format nil "Paths: ~a | ~a | ~a"
                         (prin1-to-string (first paths))
                         (prin1-to-string (second paths))
                         (prin1-to-string (third paths))))))
                         
(defparameter *cartoon-objects*
  '(("GRIME ENDS" . "<g transform='translate(600,400) scale(1.8)'>
    <ellipse cx='0' cy='0' rx='60' ry='80' fill='#333' stroke='#ff0000' stroke-width='10'/>
    <circle cx='-20' cy='-20' r='15' fill='white'/>
    <circle cx='20' cy='-20' r='15' fill='white'/>
    <circle cx='-20' cy='-20' r='8' fill='black'/>
    <circle cx='20' cy='-20' r='8' fill='black'/>
    <path d='M-30 30 Q0 50 30 30' stroke='#ff0000' stroke-width='8' fill='none'/>
    <rect x='70' y='-80' width='100' height='60' fill='#000' rx='20'/>
    <text x='120' y='-40' fill='#ff0000' font-size='40' font-family='Impact'>BRUV!</text>
    <path d='M-100 0 Q-80 -40 -60 0 Q-40 40 -20 0' stroke='#ff00ff' stroke-width='12' fill='none'/>
    <path d='M100 0 Q80 -40 60 0 Q40 40 20 0' stroke='#ff00ff' stroke-width='12' fill='none'/>
  </g>")
    ("DRIP TRILL" . "<g transform='translate(600,400) scale(2)'>
    <path d='M-100,-20 Q-50,-80 0,-20 Q50,40 100,-20 L100,20 Q50,80 0,20 Q-50,-40 -100,20 Z' fill='#00ffff' stroke='#ffff00' stroke-width='15'/>
    <circle cx='-60' cy='60' r='20' fill='#00ffff' opacity='0.8'/>
    <circle cx='-20' cy='90' r='25' fill='#00ffff' opacity='0.6'/>
    <circle cx='30' cy='120' r='30' fill='#00ffff' opacity='0.4'/>
    <text x='0' y='180' fill='#ffff00' font-size='50'>DRIP</text>
  </g>")
    ("UKG BASSLINE" . "<g transform='translate(600,400) scale(1.8)'>
    <rect x='-80' y='-80' width='160' height='160' fill='#000' rx='30'/>
    <circle cx='0' cy='0' r='50' fill='#333'/>
    <circle cx='-20' cy='-20' r='15' fill='white'/>
    <circle cx='20' cy='-20' r='15' fill='white'/>
    <path d='M-30 30 Q0 50 30 30' stroke='white' stroke-width='8' fill='none'/>
    <ellipse cx='-100' cy='0' rx='40' ry='20' fill='#ff00ff'/>
    <ellipse cx='-140' cy='30' rx='30' ry='15' fill='#ff00ff'/>
    <ellipse cx='100' cy='0' rx='40' ry='20' fill='#00ff00'/>
    <ellipse cx='140' cy='-30' rx='30' ry='15' fill='#00ff00'/>
    <text x='0' y='140' fill='#ff00ff' font-size='40'>WOBBLE</text>
  </g>")
    ("LOWRIDER WEST" . "<g transform='translate(600,400) scale(1.6)'>
    <rect x='-100' y='-60' width='200' height='120' fill='#8B0000' stroke='#ffd700' stroke-width='12'/>
    <path d='M-80 60 L-80 100 L80 100 L80 60' fill='#333'/>
    <circle cx='-50' cy='120' r='30' fill='#222'/>
    <circle cx='50' cy='120' r='30' fill='#222'/>
    <path d='M-50 150 Q-30 110 -10 150 Q10 190 30 150 Q50 110 70 150' stroke='#ffd700' stroke-width='20' fill='none'/>
    <text x='0' y='-80' fill='#ffd700' font-size='60'>BOING!</text>
  </g>")
    ("TRAP ATL" . "<g transform='translate(600,400) scale(2)'>
    <rect x='-60' y='-100' width='120' height='200' fill='#228B22' stroke='#gold' stroke-width='15'/>
    <text x='0' y='-40' fill='gold' font-size='80'>$</text>
    <rect x='-30' y='20' width='60' height='100' fill='#006400' stroke='#gold' stroke-width='8'/>
    <rect x='-15' y='60' width='30' height='50' fill='#004000' stroke='#gold' stroke-width='5'/>
    <text x='0' y='140' fill='gold' font-size='40'>TRAP</text>
  </g>")
    ("DRILL NY/UK" . "<g transform='translate(600,400) scale(2)'>
    <circle cx='0' cy='0' r='80' fill='#000' stroke='#ff0000' stroke-width='15'/>
    <path d='M-40 -40 L-40 -100 L40 -100 L40 -40' fill='#333'/>
    <path d='M-20 -100 L-20 -140 L20 -140 L20 -100' fill='#222'/>
    <text x='0' y='100' fill='red' font-size='50'>BRRR</text>
  </g>")
    ("AFROBEATS NAIJA" . "<g transform='translate(600,400) scale(2)'>
    <circle cx='0' cy='0' r='80' fill='#FFA500' stroke='#FF4500' stroke-width='15'/>
    <path d='M-60 -30 Q0 -100 60 -30 Q0 40 -60 -30' fill='none' stroke='#000' stroke-width='12'/>
    <text x='-80' y='-50' fill='#000' font-size='40'>♪</text>
    <text x='80' y='-50' fill='#000' font-size='40'>♫</text>
    <text x='0' y='100' fill='#000' font-size='45'>GBEDU</text>
  </g>")
    ("DANCEHALL JA" . "<g transform='translate(600,400) scale(2)'>
    <rect x='-60' y='-80' width='120' height='160' fill='#FFD700' stroke='#000' stroke-width='15'/>
    <circle cx='0' cy='-20' r='30' fill='#000'/>
    <path d='M-20 40 L-40 100 L0 80 L40 100 L20 40' fill='#000'/>
    <text x='0' y='200' fill='#FFD700' font-size='50'>WINE!</text>
  </g>")
    ("CLOUD EMO" . "<g transform='translate(600,400) scale(2)'>
    <path d='M-80 -20 Q-40 -80 0 -20 Q40 -80 80 -20 Q40 40 0 -20 Q-40 40 -80 -20' fill='#444' stroke='#ff69b4' stroke-width='12'/>
    <text x='0' y='20' fill='#ff69b4' font-size='80'>💔</text>
    <circle cx='-40' cy='60' r='20' fill='#add8e6' opacity='0.8'/>
    <circle cx='0' cy='90' r='25' fill='#add8e6' opacity='0.6'/>
    <circle cx='40' cy='60' r='20' fill='#add8e6' opacity='0.8'/>
  </g>")
    ("HYPERPOP PC" . "<g transform='translate(600,400) scale(2)'>
    <rect x='-80' y='-80' width='160' height='160' fill='none' stroke='#ff00ff' stroke-width='15'/>
    <rect x='-60' y='-60' width='120' height='120' fill='none' stroke='#00ffff' stroke-width='10'/>
    <rect x='-40' y='-40' width='80' height='80' fill='none' stroke='#ffff00' stroke-width='8'/>
    <text x='0' y='20' fill='#00ffff' font-size='60'>GLITCH</text>
    <circle cx='-50' cy='-50' r='10' fill='#ff00ff'/>
    <circle cx='50' cy='50' r='15' fill='#00ffff'/>
  </g>")))
    
;; ------------------------------------------------------------
;; Cartoon cycling globals (FIX: prevent UNBOUND-VARIABLE)
;; ------------------------------------------------------------



(defparameter *cyclic-groups*
  '(("GRIME ENDS" . "peng skeng bars bruv")
    ("DRIP TRILL" . "drip ice flex sauce")
    ("UKG BASSLINE" . "wobble pullup garage speed")
    ("LOWRIDER WEST" . "bounce lowlow chevy hop")
    ("TRAP ATL" . "bricks plug guap skrr")
    ("DRILL NY/UK" . "opps glizzy smoke demon")
    ("AFROBEATS NAIJA" . "gbedu zanku wahala sabi")
    ("DANCEHALL JA" . "wine daggering riddim forward")
    ("CLOUD EMO" . "sadboy xan heartbreak plugg")
    ("HYPERPOP PC" . "glitch hexd rage 100gecs"))
  "Cyclic list of vibe groups. Each entry's CAR must match a key in *CARTOON-OBJECTS*.")

(defparameter *current-group-index* 0
  "Current index into *CYCLIC-GROUPS* for overlay + legend.")
  
(defun svg-escape (x)
  (let ((s (if (stringp x) x (prin1-to-string x))))
    (with-output-to-string (out)
      (loop for ch across s do
        (write-string
         (case ch
           (#\& "&amp;")
           (#\< "&lt;")
           (#\> "&gt;")
           (#\" "&quot;")
           (#\' "&apos;")
           (t (string ch)))
         out)))))
         
;; ------------------------------------------------------------
;; PeteAI Visual Language Spec v0.1 (SVG)
;; ------------------------------------------------------------
(defparameter *pete-visual-spec*
  '(:bg "#111"
    :panel "#222"
    :primary "#00ffff"
    :warn "#ffff00"
    :hot "#ff00ff"
    :text "#ffffff"
    :muted "#aaaaaa"
    :shadow-id "softShadow"
    :avatar-x 860
    :avatar-y 110
    :avatar-w 300
    :avatar-h 300))

(defun vget (k) (getf *pete-visual-spec* k))

(defun ensure-soft-shadow-defs (s)
  (format s "<defs>
  <filter id='~a' x='-20%' y='-20%' width='140%' height='140%'>
    <feDropShadow dx='2' dy='3' stdDeviation='4' flood-color='#000' flood-opacity='0.35'/>
  </filter>
</defs>~%"
          (vget :shadow-id)))

(defun clamp (x a b) (max a (min b x)))

(defun normalize-winner (x)
  (cond
    ((stringp x) x)
    ((symbolp x) (symbol-name x))
    (t (prin1-to-string x))))

(defun pete-visual-intent ()
  (list :holes *last-holes*
        :wild  *last-wild-factor*
        :winner (normalize-winner *last-treaty-verdict*)
        :quadrant (if (boundp '*dominant-quadrant*) *dominant-quadrant* :unknown)))

(defun sigil-family (intent)
  (let ((w (string-upcase (getf intent :winner))))
    (cond
      ((or (search "MUD" w) (search "FALLBACK" w)) :pressure)
      ((or (search "\"" w) (search "QUOTE" w)) :brackets)
      ((or (search "HOLE" w) (> (getf intent :holes) 4)) :hole)
      (t :treaty))))

(defun avatar-card (s title subtitle)
  (let ((x (vget :avatar-x)) (y (vget :avatar-y))
        (w (vget :avatar-w)) (h (vget :avatar-h)))
    (format s "<g transform='translate(~a,~a)'>~%" x y)
    (format s "<rect width='~a' height='~a' rx='22' fill='~a' stroke='~a' filter='url(#~a)'/>~%"
            w h (vget :panel) (vget :primary) (vget :shadow-id))
    (format s "<text x='18' y='34' fill='~a' font-size='18'>~a</text>~%"
            (vget :primary) (svg-escape title))
    (format s "<text x='18' y='58' fill='~a' font-size='13'>~a</text>~%"
            (vget :muted) (svg-escape subtitle))))

(defun avatar-card-end (s) (format s "</g>~%"))

(defun emit-distortion-ring (s holes wild)
  (let* ((r (+ 52 (* 6 (clamp holes 0 8))))
         (sw (+ 4 (clamp wild 0 10)))
         (opacity (if (> holes 0) 0.9 0.35)))
    (format s "<g>~%")
    (format s "<circle cx='150' cy='170' r='~a' fill='none' stroke='~a' stroke-width='~a' opacity='~a'/>~%"
            r (vget :warn) sw opacity)
    (when (> holes 0)
      (format s "<animateTransform attributeName='transform' type='rotate'
                 values='-2 150 170;2 150 170;-2 150 170'
                 dur='1.2s' repeatCount='indefinite'/>~%"))
    (format s "</g>~%")))

(defun emit-sigil-core (s family winner)
  (ecase family
    (:treaty
     (format s "<circle cx='150' cy='170' r='44' fill='none' stroke='~a' stroke-width='10'/>~%"
             (vget :primary))
     (format s "<polygon points='150,128 184,180 116,180' fill='~a'/>~%" (vget :hot)))
    (:hole
     (format s "<circle cx='150' cy='170' r='54' fill='#000' stroke='~a' stroke-width='10'/>~%"
             (vget :hot))
     (format s "<path d='M96 170 L204 170' stroke='~a' stroke-width='6'/>~%" (vget :text))
     (format s "<path d='M150 116 L150 224' stroke='~a' stroke-width='6'/>~%" (vget :text)))
    (:pressure
     (format s "<rect x='98' y='118' width='104' height='104' rx='18' fill='#000' stroke='~a' stroke-width='10'/>~%"
             (vget :warn))
     (format s "<circle cx='150' cy='170' r='18' fill='~a'/>~%" (vget :hot)))
    (:brackets
     (format s "<path d='M112 120 L98 120 L98 220 L112 220' stroke='~a' stroke-width='10' fill='none'/>~%"
             (vget :primary))
     (format s "<path d='M188 120 L202 120 L202 220 L188 220' stroke='~a' stroke-width='10' fill='none'/>~%"
             (vget :primary))))
  (format s "<text x='150' y='270' fill='~a' font-size='13' text-anchor='middle'>~a</text>~%"
          (vget :text) (svg-escape winner)))

(defun generate-auto-cartoon-svg (intent)
  (let* ((holes (getf intent :holes))
         (wild (getf intent :wild))
         (winner (getf intent :winner))
         (fam (sigil-family intent)))
    (with-output-to-string (s)
      (emit-distortion-ring s holes wild)
      (emit-sigil-core s fam winner))))
      
(defun pick-or-generate-cartoon ()
  "Returns either a hand-authored pack cartoon or a procedural generated scene.
If *use-auto-cartoons* is true, procedural generator wins."
  (let* ((pack (%safe-current-cyclic-group))
         (holes (or *last-holes* 0))
         (wild  (or *last-wild-factor* 0))
         (root  (or *last-root-id* "UNKNOWN"))
         (hand  (cdr (assoc pack *cartoon-objects* :test #'string=))))
    (cond
      (*use-auto-cartoons*
       (generate-cartoon-scene :pack pack :holes holes :wild wild :root root))
      (hand
       hand)
      (t
       (generate-cartoon-scene :pack pack :holes holes :wild wild :root root)))))


#|(defun pick-or-generate-cartoon ()
  "Prefer current vibe pack cartoon; else generate a sigil."
  (let* ((current (%safe-current-cyclic-group))
         (known (cdr (assoc current *cartoon-objects* :test #'string=))))
    (or known (generate-auto-cartoon-svg (pete-visual-intent)))))|#



(defun %safe-current-cyclic-group ()
  "Returns the current group key as a string, always safe."
  (let* ((n (length *cyclic-groups*)))
    (cond
      ((zerop n) "NONE")
      (t
       (let* ((i (mod *current-group-index* n))
              (entry (nth i *cyclic-groups*))
              (key (car entry)))
         (if (stringp key) key (prin1-to-string key)))))))

(defun %advance-cyclic-group! ()
  "Advance the vibe index safely."
  (when (plusp (length *cyclic-groups*))
    (setf *current-group-index*
          (mod (1+ *current-group-index*) (length *cyclic-groups*)))))
          
#|(defun neural-diagram-with-cartoons ()
  "Generates hybrid SVG: technical blocks + cartoon objects for urban youth vibe"
  (let ((svg-file "neural-diagram-cartoon.svg")
        (width 1200)
        (height 900))
    (with-open-file (s svg-file :direction :output :if-exists :supersede)
      (format s "<svg width='~a' height='~a' xmlns='http://www.w3.org/2000/svg'>~%" width height)
      (ensure-soft-shadow-defs s)
      (format s "<rect width='100%' height='100%' fill='#111'/>~%")
      (format s "<text x='50%' y='40' fill='#0ff' font-size='30' text-anchor='middle'>Pete's Neural Diagram: Quantum Hole Flow + Urban Youth Cartoons</text>~%")
      ;; Memory blocks
      (loop for i from 0 for mem in *pete-memory* for col = (mod i 4) for row = (floor i 4)
            for x = (+ 80 (* col 260)) for y = (+ 140 (* row 170))
            do (let* ((raw (prin1-to-string mem))
                      (clip (subseq raw 0 (min 30 (length raw)))))
                 (format s "<g transform='translate(~a,~a)'>~%" x y)
                 (format s "<rect width='240' height='120' fill='#333' rx='15' stroke='#0ff' filter='url(#softShadow)'/>~%")
                 (format s "<text x='120' y='65' fill='#fff' font-size='14' text-anchor='middle'>~a</text>~%" (svg-escape clip))
                 (format s "</g>~%")))
      ;; Avatar / cartoon overlay
      (let* ((intent (pete-visual-intent))
             (overlay (pick-or-generate-cartoon)))
        (avatar-card s "PeteAI Avatar" (format nil "Pack: ~a | Holes: ~a | Wild: ~a"
                                               (%safe-current-cyclic-group)
                                               (getf intent :holes)
                                               (getf intent :wild)))
        (format s "~a~%" overlay)
        (avatar-card-end s))
      (format s "</svg>~%"))
    (%advance-cyclic-group!)
    (uiop:run-program (list "xdg-open" svg-file) :ignore-error-status t)
    (format t "Hybrid cartoon + block neural diagram saved & opened: ~a~%" svg-file)))|#



(defun neural-diagram-with-cartoons ()
  "Generates hybrid SVG: technical blocks + cartoon objects for urban youth vibe"

  (let ((svg-file "neural-diagram-cartoon.svg")
        (width 1200)
        (height 900))
    (with-open-file (s svg-file :direction :output :if-exists :supersede)
      ;; SVG header
      (format s "<svg width='~a' height='~a' xmlns='http://www.w3.org/2000/svg'>~%" width height)

      ;; defs FIRST (so filters exist before use)
      (ensure-soft-shadow-defs s)
      (format s "<defs>
  <filter id='softShadow' x='-20%' y='-20%' width='140%' height='140%'>
    <feDropShadow dx='2' dy='3' stdDeviation='4' flood-color='#000' flood-opacity='0.35'/>
  </filter>
</defs>~%")

      ;; background + title
      (format s "<rect width='100%' height='100%' fill='#111'/>~%")
      (format s "<text x='50%' y='40' fill='#0ff' font-size='40' text-anchor='middle'>Pete's Neural Diagram: Quantum Hole Flow + Urban Youth Cartoons</text>~%")

      ;; memory blocks (escape text!)
      (loop for i from 0
            for mem in *pete-memory*
            for x = (+ 100 (* i 220))
            for y = 200
            do (let* ((raw (prin1-to-string mem))
                      (clip (subseq raw 0 (min 30 (length raw)))))
                 (format s "<g transform='translate(~a,~a)'>~%" x y)
                 (format s "<rect width='180' height='120' fill='#333' rx='15' stroke='#0ff'/>~%")
                 (format s "<text x='90' y='60' fill='#fff' font-size='14' text-anchor='middle'>~a</text>~%"
                         (svg-escape clip))
                 (format s "</g>~%")))

      ;; cartoon overlay (safe scope)
      (let* ((current (%safe-current-cyclic-group))
             (cartoon (cdr (assoc current *cartoon-objects* :test #'string=))))
        (when cartoon
          (format s "~a~%" cartoon)))

      ;; legend panel (no unbound vars)
      (format s "<g transform='translate(40,760)'>~%")
      (format s "<rect width='1120' height='120' fill='#222' rx='18' stroke='#0ff' filter='url(#softShadow)'/>~%")
      (format s "<text x='20' y='35' fill='#0ff' font-size='20'>Legend: Cartoon objects represent current urban youth vibe</text>~%")
      (format s "<text x='20' y='70' fill='#fff' font-size='16'>Active Pack: ~a</text>~%"
              (svg-escape (%safe-current-cyclic-group)))
      (format s "<text x='20' y='98' fill='#aaa' font-size='14'>Memory nodes: ~a | Holes: ~a | Wild: ~a</text>~%"
              (length *pete-memory*)
              (if (boundp '*holes*) *holes* 0)
              (if (boundp '*wild-factor*) *wild-factor* 0))
      (format s "</g>~%")

      ;; close svg
      (format s "</svg>~%"))

    ;; cycle for next render
    (%advance-cyclic-group!)

    ;; open file
    (uiop:run-program (list "xdg-open" svg-file) :ignore-error-status t)
    (format t "Hybrid cartoon + block neural diagram saved & opened: ~a~%" svg-file)))
    
(defun %pack-spec (pack)
  "Return plist describing style for a pack."
  (cond
    ((string= pack "GRIME ENDS")
     '(:accent "#ff00ff" :accent2 "#00ffff" :motif :penrose :caption "JEEZ WHIZ"))
    ((string= pack "DRIP TRILL")
     '(:accent "#00ffff" :accent2 "#ffff00" :motif :mobius :caption "DRIP"))
    ((string= pack "UKG BASSLINE")
     '(:accent "#ff00ff" :accent2 "#00ff00" :motif :klein :caption "WOBBLE"))
    ((string= pack "LOWRIDER WEST")
     '(:accent "#ffd700" :accent2 "#8B0000" :motif :hydraulics :caption "BOING!"))
    ((string= pack "TRAP ATL")
     '(:accent "#FFD700" :accent2 "#228B22" :motif :fractal-stack :caption "TRAP"))
    ((string= pack "DRILL NY/UK")
     '(:accent "#ff0000" :accent2 "#111111" :motif :tunnel :caption "BRRR"))
    ((string= pack "AFROBEATS NAIJA")
     '(:accent "#FFA500" :accent2 "#FF4500" :motif :rhythm :caption "GBEDU"))
    ((string= pack "DANCEHALL JA")
     '(:accent "#FFD700" :accent2 "#000000" :motif :stairs :caption "WINE!"))
    ((string= pack "CLOUD EMO")
     '(:accent "#ff69b4" :accent2 "#add8e6" :motif :sadcloud :caption "💔"))
    ((string= pack "HYPERPOP PC")
     '(:accent "#ff00ff" :accent2 "#00ffff" :motif :glitch :caption "GLITCH"))
    (t '(:accent "#00ffff" :accent2 "#ffffff" :motif :sigil :caption "PETEAI"))))

(defun %svg-noise-dots (holes &key (cx 150) (cy 170))
  "Holes → void dots cloud."
  (with-output-to-string (s)
    (loop repeat (min 40 (+ 8 (* 6 holes))) do
      (let ((x (+ cx (- (random 220) 110)))
            (y (+ cy (- (random 180) 90)))
            (r (+ 1 (random (max 2 holes)))))
        (format s "<circle cx='~d' cy='~d' r='~d' fill='#000' opacity='0.35'/>~%"
                x y r)))))

(defun %svg-glitch-sparks (wild &key (cx 150) (cy 170) (accent "#00ffff"))
  "Wild → sparks and scan lines."
  (with-output-to-string (s)
    (loop repeat (min 30 (+ 6 (* 4 wild))) do
      (let* ((x (+ cx (- (random 260) 130)))
             (y (+ cy (- (random 220) 110)))
             (w (+ 8 (random 30)))
             (h (+ 2 (random 6))))
        (format s "<rect x='~d' y='~d' width='~d' height='~d' fill='~a' opacity='0.25'/>~%"
                x y w h accent)))))

(defun %svg-motif (motif &key (accent "#ff00ff") (accent2 "#00ffff"))
  "Core shape language (motifs) — elaborate but compact."
  (case motif
    (:penrose
     (format nil
             "<path d='M70,190 L150,60 L230,190 Z' fill='none' stroke='~a' stroke-width='10'/>
              <path d='M70,190 L70,235 L230,235 L230,190 Z' fill='none' stroke='~a' stroke-width='10'/>"
             accent accent2))
    (:mobius
     (format nil
             "<path d='M55,150 Q100,70 150,150 T245,150 L245,190 Q200,270 150,190 T55,190 Z'
                    fill='none' stroke='~a' stroke-width='12'/>"
             accent2))
    (:klein
     (format nil
             "<path d='M150,70 Q85,95 95,160 Q105,250 150,260 Q195,250 205,160 Q215,95 150,70'
                    fill='none' stroke='~a' stroke-width='12'/>"
             accent))
    (:glitch
     (format nil
             "<rect x='75' y='95' width='150' height='150' fill='none' stroke='~a' stroke-width='12'/>
              <rect x='95' y='115' width='110' height='110' fill='none' stroke='~a' stroke-width='8'/>"
             accent accent2))
    (:sigil
     (format nil "<circle cx='150' cy='170' r='85' fill='none' stroke='~a' stroke-width='10'/>" accent))
    (t
     (format nil "<circle cx='150' cy='170' r='80' fill='none' stroke='~a' stroke-width='10'/>" accent))))

(defun generate-cartoon-scene (&key pack holes wild root)
  "Returns a self-contained <g> ... </g> scene in *card-local* coords."
  (let* ((spec (%pack-spec pack))
         (accent (getf spec :accent))
         (accent2 (getf spec :accent2))
         (motif (getf spec :motif))
         (caption (getf spec :caption))
         (root (or root "UNKNOWN")))
    (with-output-to-string (s)
      (format s "<g transform='translate(0,0)'>~%")
      ;; background halo
      (format s "<circle cx='150' cy='170' r='110' fill='~a' opacity='0.08'/>~%" accent)
      ;; holes / wild effects
      (write-string (%svg-noise-dots holes) s)
      (write-string (%svg-glitch-sparks wild :accent accent2) s)
      ;; motif centerpiece
      (format s "<g>~a</g>~%" (%svg-motif motif :accent accent :accent2 accent2))
      ;; caption + root stamp
      (format s "<text x='150' y='290' fill='~a' font-size='28' font-family='Impact' text-anchor='middle'>~a</text>~%"
              accent2 (svg-escape caption))
      (format s "<text x='150' y='320' fill='#aaaaaa' font-size='12' text-anchor='middle'>ROOT: ~a</text>~%"
              (svg-escape root))
      (format s "</g>~%"))))

#|(defun pick-or-generate-cartoon ()
  "Prefer pack-specific cartoon object; otherwise generate elaborate scene."
  (let* ((pack (%safe-current-cyclic-group))
         (hit (cdr (assoc pack *cartoon-objects* :test #'string=))))
    (or hit
        (generate-cartoon-scene
         :pack pack
         :holes (or *last-holes* 0)
         :wild (or *last-wild-factor* 0)
         :root (or *last-root-id* "UNKNOWN")))))|#

    
;; =============================================================================
;; NEURAL-DIAGRAM v4 — REGENERATED WITH AVATAR CARDS + MYTHICAL RUNES
;; =============================================================================

;; Helper: Soft shadow defs (Da Vinci halftone style)
(defun ensure-soft-shadow-defs (s)
  (format s "<defs>
  <filter id='softShadow' x='-20%' y='-20%' width='140%' height='140%'>
    <feGaussianBlur in='SourceAlpha' stdDeviation='4' result='blur'/>
    <feOffset in='blur' dx='2' dy='3' result='offsetBlur'/>
    <feMerge>
      <feMergeNode in='offsetBlur'/>
      <feMergeNode in='SourceGraphic'/>
    </feMerge>
  </filter>
</defs>~%"))

;; Helper: Pete's visual intent (psychological test — holes + wild factor)
(defparameter *holes* 0)  ; Global for holes (bind if needed)
(defparameter *wild-factor* 0)  ; Global for wild (bind if needed)

(defun pete-visual-intent ()
  (let ((holes *holes*)
        (wild *wild-factor*))
    (format t "Visual intent: Holes ~a, Wild ~a~%" holes wild)
    (list :holes holes :wild wild)))

;; Helper: Pick or generate cartoon (random mythical + runes)
(defun pick-or-generate-cartoon ()
  (let* ((pack (%safe-current-cyclic-group))
         (cartoon (cdr (assoc pack *cartoon-objects* :test #'string=)))
         (mythical (random-mythical-guardian))
         (runes (musical-rune-sequence *runes* 3)))
    (format nil "~a ~a <text x='0' y='200' fill='rgba(0,255,255,0.5)' font-size='40'>~a</text>" cartoon mythical runes)))

;; Helper: Avatar card (comical frame for cartoon)
(defun avatar-card (s title subtitle)
  (format s "<g transform='translate(300,300) scale(1.2)'>
    <rect x='-200' y='-200' width='400' height='400' fill='#222' rx='30' filter='url(#softShadow)'/>
    <text x='0' y='-160' fill='#0ff' font-size='30' text-anchor='middle'>~a</text>
    <text x='0' y='170' fill='#ff00ff' font-size='20' text-anchor='middle'>~a</text>~%" title subtitle))

(defun avatar-card-end (s)
  (format s "</g>~%"))



(defun neural-diagram ()
  "Generates a neural diagram with a Technical Engineering aesthetic, flipped values, and a madness factor."
  (let ((dot-file "neural-diagram.dot")
        (output-file "neural-diagram.png")
        (filtered-memory (remove-if (lambda (x) (not (every #'symbolp x))) *pete-memory*)))
    (format t "Debug: *pete-memory* contents: ~a~%" *pete-memory*)
    (with-open-file (stream dot-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format stream "digraph NeuralDiagram {~%")
      (format stream "  rankdir=BT;~%")
      (format stream "  bgcolor=\"white\";~%")
      (format stream "  label=\"Pete's Neural Diagram: Quantum Hole Flow\\nConversational Memory with Subculture Vibes\";~%")
      (format stream "  labelloc=\"t\";~%")
      (format stream "  nodesep=0.5;~%")
      (format stream "  ranksep=1.0;~%")
      (format stream "  splines=spline;~%")
      (format stream "  node [style=filled fontcolor=black fontsize=10 fontname=\"Helvetica\" shape=ellipse];~%")
      (format stream "  edge [color=navy fontsize=8 fontcolor=black fontname=\"Helvetica\"];~%")
      (format stream "  compound=true;~%")
      (format stream "  subgraph cluster_lowrider {~%    style=filled;~%    color=steelblue;~%    fillcolor=\"steelblue\";~%    label=\"Lowrider Cluster\";~%  }~%")
      (format stream "  subgraph cluster_skater {~%    style=filled;~%    color=steelblue;~%    fillcolor=\"steelblue\";~%    label=\"Skater Cluster\";~%  }~%")
      (format stream "  subgraph cluster_hiphop {~%    style=filled;~%    color=steelblue;~%    fillcolor=\"steelblue\";~%    label=\"Hip-Hop Cluster\";~%  }~%")
      (format stream "  subgraph cluster_harajuku {~%    style=filled;~%    color=steelblue;~%    fillcolor=\"steelblue\";~%    label=\"Harajuku Cluster\";~%  }~%")
      (format stream "  subgraph cluster_grime {~%    style=filled;~%    color=slategray;~%    fillcolor=\"slategray\";~%    label=\"Grime Cluster\";~%  }~%")
      (format stream "  subgraph cluster_techno {~%    style=filled;~%    color=cornflowerblue;~%    fillcolor=\"cornflowerblue\";~%    label=\"Techno Cluster\";~%  }~%")
      (format stream "  subgraph cluster_default {~%    style=filled;~%    color=lightgray;~%    fillcolor=\"lightgray\";~%    label=\"Default Cluster\";~%  }~%")
      (format stream "  subgraph cluster_legend {~%")
      (format stream "    style=filled;~%    fillcolor=lightgray;~%    label=\"Legend\";~%")
      (format stream "    \"legend_harajuku\" [label=\"Harajuku: Cultural Spark\" fillcolor=\"steelblue\" shape=box];~%")
      (format stream "    \"legend_techno\" [label=\"Techno: Beat Pulse\" fillcolor=\"cornflowerblue\" shape=box];~%")
      (format stream "    \"legend_default\" [label=\"Default: Neutral Flow\" fillcolor=\"lightgray\" shape=box];~%")
      (format stream "  }~%")
      (let ((lowrider-symbols '(CRUISE HYDRAULICS EASTSIDE CHICANO LOWLOW CRUISER PASEO CARHOP BOUNCE ZAPATA
                                HOP CANTINA LOWBOY PINSTRIPER CRUISIN CHOLO DUBS TROKA RIM PACAS
                                VATO BOMB HYDROS CARLO IMPERIAL CADDY IMPALA MONTE TORINO CUTLASS
                                ELDO REGAL BELAIR GTO CHEVY NOVAS RIVIERA THUNDERBIRD CAMARO
                                MUSTANG CHARGER CHALLENGER CORVETTE GALAXIE ROADKING PONTIAC BUICK OLDS
                                FALCON FIREBIRD GTO SKYLAKE TORONADO))
            (skater-symbols '(KICKFLIP GRIND OLLIE SHOVE HEELFIP NOSEGRIND TAILSLIDE BOARD DECK TRUCK
                              WHEELS RAIL POOL VERT RAMP DOGTOWN VENICE ZEPHYR SKATEPARK GRIPTAPE))
            (hiphop-symbols '(DRIP SPIT FLOW CRASH HUSTLE GRIND RIDE BLADE BLOCK CAP
                              CREW DECK DICE DUB FLAME FRESH GANG HEAT ICE JAM
                              KICK LACE MAC MIC PEACE))
            (harajuku-symbols '(KAWAII HARAJUKU COSPLAY NEON LOLI YUKATA KOGAL GOTHIC KIMONO DECORE
                               LOLITA KUTSU PUNK VISUAL FRUITS FAIRYKEI MOROI OTOME SEAPUNK CYBER
                               PASTEL STEAMPUNK VAPORWAVE KIREI KABUKI J-FASHION ANIME))
            (grime-symbols '(GRIME SPRAY ROADMAN ENDS BRUV DUBSTEP GARAGE BASS JUNGLE UKG
                             WOBBLE RINSE CREW MC BARS MANDO TING YARDIE SKENG WASTEMAN
                             LINK GREEZE PENG CHING FAM))
            (techno-symbols '(RAVE TECHNO BERGHAIN BEAT VOID DROP MIX KICK)))
        (if (not filtered-memory)
            (format stream "  \"Empty\" [label=\"No Memory\" fillcolor=\"lightgray\"];~%")
            (loop for entry in filtered-memory
                  for idx from 0
                  do (let* ((holes (count-if-not (lambda (x)
                                                   (or (member x *verbs*) (member x *nouns*)))
                                                 (if (listp entry) entry (list entry))))
                            (flipped (mapcar (lambda (x)
                                               (if (or (member x *verbs*) (member x *nouns*))
                                                   x
                                                   (flip-hole (if (stringp x) x (symbol-name x)))))
                                             (if (listp entry) entry (list entry))))
                            (subculture (cond
                                          ((some (lambda (x) (member x lowrider-symbols)) flipped) 'lowrider)
                                          ((some (lambda (x) (member x skater-symbols)) flipped) 'skater)
                                          ((some (lambda (x) (member x hiphop-symbols)) flipped) 'hiphop)
                                          ((some (lambda (x) (member x harajuku-symbols)) flipped) 'harajuku)
                                          ((some (lambda (x) (member x grime-symbols)) flipped) 'grime)
                                          ((some (lambda (x) (member x techno-symbols)) flipped) 'techno)
                                          (t 'default)))
                            ;; Calculate Madness Factor based on unique subculture symbols
                            (madness-factor (float (/ (length (intersection (alexandria:flatten flipped)
                                                                            (append hiphop-symbols grime-symbols techno-symbols)
                                                                            :test #'equal))
                                                     (max 1 (length flipped)))))
                            (treaty-marker (if (member 'treaty entry) "[Treaty]" ""))
                            (trans-info (compute-transformation-info entry))
                            (label (format nil "Mem_~a\\n~a\\nHoles: ~a~a\\nFlipped: ~a\\nMadness: ~,2f\\n[fontsize=8]~a\\n~a"
                                           idx
                                           (truncate-string (escape-dot-string (prin1-to-string entry)) 50)
                                           holes
                                           treaty-marker
                                           (truncate-string (escape-dot-string (prin1-to-string flipped)) 50)
                                           madness-factor
                                           (getf trans-info :probabilities)
                                           (truncate-string (getf trans-info :paths) 30)))
                            (fillcolor (case subculture
                                         (lowrider "steelblue")
                                         (skater "steelblue")
                                         (hiphop "steelblue")
                                         (harajuku "steelblue")
                                         (grime "slategray")
                                         (techno "cornflowerblue")
                                         (t "lightgray"))))
                       (format stream "  subgraph cluster_~a {~%    \"Mem_~a\" [label=\"~a\" fillcolor=~a];~%  }~%"
                               (symbol-name subculture) idx label fillcolor)))))
      (loop for i from 0 below (1- (length filtered-memory))
            do (let* ((node1 (nth i filtered-memory))
                      (node2 (nth (1+ i) filtered-memory))
                      (holes1 (count-if-not (lambda (x)
                                              (or (member x *verbs*) (member x *nouns*)))
                                            (if (listp node1) node1 (list node1))))
                      (holes2 (count-if-not (lambda (x)
                                              (or (member x *verbs*) (member x *nouns*)))
                                            (if (listp node2) node2 (list node2))))
                      (avg-holes (/ (+ holes1 holes2) 2.0))
                      (penwidth (max 1 (min 3 (floor (* avg-holes 0.3)))))
                      (style (cond ((> avg-holes 5) "dotted")
                                   (t "solid"))))
                 (format stream "  \"Mem_~a\" -> \"Mem_~a\" [label=\"flow\" penwidth=~a style=~a];~%" i (1+ i) penwidth style)))
      (format stream "}~%"))
    (handler-case
        (progn
          (uiop:run-program (format nil "dot -Tpng ~a -o ~a" dot-file output-file)
                            :output t :error-output t)
          (format t "Technical Engineering neural diagram with flipped values (attractive, no gradients) DOT file written to ~a~%" dot-file)
          (format t "Graph rendered to ~a~%" output-file))
      (error (e)
        (format t "Error rendering graph: ~a. Ensure Graphviz is installed.~%" e))))
  (values))

(defun clear-memory ()
  "Clears Pete's memory to start fresh."
  (setf *pete-memory* '())
  (clrhash *pete-memory-graph*)
  (format t "Pete's memory cleared! Starting fresh.~%"))
  
(defun pete-export ()
  "Exports Pete's memory to a log file."
  (export-memory-log))
  
(defun converse-no-reset ()
  "Starts PeteAI without resetting memory."
  (format t "~%=== Pete’s Ready! Say stuff (or 'quit', 'know', 'speak', 'listen', 'read', 'react', 'communicate', 'export', 'neural-diagram', 'neural-diagram1', 'clear-memory', 'network-local') ===~%")
  (finish-output)
  (loop
    (format t "> ")
    (finish-output)
    (let ((input (read-line)))
      (cond
        ((string-equal input "quit")
         (when *pete-memory*
           (let ((last-thought (nth (random (length *pete-memory*)) *pete-memory*)))
             (format t "Pete sums it: Twisted ~a into wild vibes!~%" last-thought)))
         (format t "Pete waves: See ya!~%")
         (finish-output)
         nil)
        ((string-equal input "know") (local-knowledge))
        ((string-equal input "speak") (pete-speak))
        ((string-equal input "listen") (pete-listen (read-line)))
        ((string= input "read") (pete-read))
        ((string= input "react")
         (pete-react (if *pete-memory* (car *pete-memory*) '(pete vibes))))
        ((string= input "communicate")
         (format t "Enter first convo: ") (finish-output)
         (let ((input1 (read-line)))
           (format t "Enter second convo: ") (finish-output)
           (let ((input2 (read-line)))
             (pete-communicate input1 input2))))
        ((string= input "export") (pete-export)
         (export-memory-log "pete_AI_beast_memory.txt"))
        ((string= input "neural-diagram")
         (neural-diagram-with-cartoons))
        ((string= input "neural-diagram1")
         (neural-diagram))
        ((string= input "clear-memory")
         (clear-memory))
        ((string= input "network-local")
         (peteai-web_sec1))
        (t
         (multiple-value-bind (untagged holes) (split-and-tag input)
           (format t "~%")
           (let ((result (pete-flow untagged 0 holes)))
             (format t "~%Pete: ~a~%" result))))))))


#|;; Serve neural-diagram1.png
(hunchentoot:define-easy-handler (diagram1-image :uri "/neural-diagram1.png") ()
  (setf (hunchentoot:content-type*) "image/png")
  (if (probe-file "neural-diagram1.png")
      (with-open-file (stream "neural-diagram1.png" :direction :input :element-type '(unsigned-byte 8))
        (let ((buffer (make-array (file-length stream) :element-type '(unsigned-byte 8))))
          (read-sequence buffer stream)
          buffer))
      (progn
        (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
        (format nil "Image not found"))))|#

#|;; Serve neural-diagram2.png
(hunchentoot:define-easy-handler (diagram2-image :uri "/neural-diagram2.png") ()
  (setf (hunchentoot:content-type*) "image/png")
  (if (probe-file "neural-diagram2.png")
      (with-open-file (stream "neural-diagram2.png" :direction :input :element-type '(unsigned-byte 8))
        (let ((buffer (make-array (file-length stream) :element-type '(unsigned-byte 8))))
          (read-sequence buffer stream)
          buffer))
      (progn
        (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
        (format nil "Image not found"))))|#
        
(defun main ()
  (format t "~&[PeteAI Booting Executable Mode - Urban Vibes Activated]~%")
  (format t "Quickloading dependencies...~%")

  ;; Load all needed systems silently (Quicklisp will fetch if missing)
  (dolist (system '(:uiop :alexandria :cl-ppcre :hunchentoot :cl-json :websocket-driver))
    (ql:quickload system :silent t :peteai-web))

  ;; Load the web interface code — this is the key line!
  ;; Make sure "peteai-web.lisp" is in the same folder when building
  (when (probe-file "peteai-web.lisp")
    (load "peteai-web.lisp")
    (format t "Web interface loaded — browser mode ready!~%"))
  (else
    (format t "Warning: peteai-web.lisp not found — running console-only mode.~%")))



  ;; Start the core conversational loop
  (format t "PeteAI core starting... Drop bars or type 'quit'~%")
  (converse-no-reset))

;; ONLY include this block if you want to build from inside SBCL REPL
;; (recommended for testing, but NOT for final clean build)

#|#+(and sbcl (not building-executable))  ; This prevents auto-save during normal load
(sb-ext:save-lisp-and-die "peteai_cartoon_grok_chat_12_23_25-executable"
                          :toplevel #'main
                          :executable t
                          :compression t)|#
