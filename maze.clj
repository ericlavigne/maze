(ns maze
	(:use clojure.set))

(def simple-board-string
".###..###
..###..##
..####.##
.#####.#.
..####.#.
#......#.
#.#####..")

(def panic-drafta-board-string
"..........................................#....................................
########...#######..##....##.####.#########...#############....................
##.....##.##.....##.###...##.####....##..............###.......................
##.....##.##.....##.####..##..##.....##................########................
##.....##.##.....##.##.##.##.##......##..........#####....#....................
##.....##.##.....##.##..####.........##.....####..........#....................
##.....##.##.....##.##...###.........##........###############################.
########...#######..##....##.........##............#...........................
...................................................#..........#######..........
.........###########################################...........................
.....................................................##########################
####################################################...........................
.....................................................##########################
########.....###....##....##.####..######.....####.############################
##.....##...##.##...###...##..##..##....##....####.............................
##.....##..##...##..####..##..##..##..........####.............................
########..##.....##.##.##.##..##..##...........##..............................
##........#########.##..####..##..##...........................................
##........##.....##.##...###..##..##....##....####.............................
##........##.....##.##....##.####..######.....####.............................
.........................#........................#############################
........#########################################.......#######################
...............................................................................
.#############################################################################.
..###########################################################################..
...########################################################################..##
....######################################################################..###
.....####################################################################..####
......##################################################################.......
.......################################################################....####
........##############################################################...######
.........############################################################...#######
.###################################################################...........
...............................................................................
.##############################################################################
...............................................................................
##############################################################################.
...............................................................................
.##############################################################################
...............................................................................")

(defn cartesian-product [seq1 seq2]
	(mapcat 
		(fn [x1] 
			(map 
				(fn [x2] (list x1 x2)) 
				seq2))
		seq1))

; make board from string where obstacles are '#' and empty spaces are '.'
; board is set of allowed positions as [x y]
(defn board-and-goal-from-string [s]
	(let 
		[rows (filter not-empty (map (fn [s] (.trim s)) (.split s "\n")))
		 num-rows (count rows)
		 num-cols (count (first rows))]
		[(set
			(filter
				(fn [pos] 
					(= \. 
						(nth
							(nth rows (- (first pos) 1))
							(- (second pos) 1))))
				(cartesian-product 
					(range 1 (+ 1 num-rows)) 
					(range 1 (+ 1 num-cols)))))
		 [num-rows num-cols]]))

(defn board-from-string [s]
	(get (board-and-goal-from-string s) 0))
	
(defn goal-from-string [s]
	(get (board-and-goal-from-string s) 1))

(def simple-board (board-from-string simple-board-string))

(def simple-goal (goal-from-string simple-board-string))

(def panic-drafta-board (board-from-string panic-drafta-board-string))

(def panic-drafta-goal (goal-from-string panic-drafta-board-string))

(defn maze-step [start]
	(map 
		(fn [direction] (map + start direction))
		(cartesian-product [-1 0 1] [-1 0 1])))

(defn maze-search 
	([old-paths allowed-states goal-state]
		(when (not (empty? old-paths))
			(let [next-path (first old-paths)
			      next-state (first next-path)
			      new-states (filter (fn [s] (contains? allowed-states s)) (maze-step next-state))]
				(if (= goal-state next-state)
					(reverse next-path)
					(recur
						(concat
							(rest old-paths)
							(map (fn [s] (cons s next-path)) new-states))
						(difference allowed-states (set new-states))
						goal-state)))))
	([allowed-states goal-state]
		(maze-search [[[1 1]]] allowed-states goal-state)))

(def simple-result 
	(maze-search 
		simple-board 
		simple-goal))
		
(def panic-drafta-result 
	(maze-search 
		panic-drafta-board 
		panic-drafta-goal))

(defn mis-step? [path board goal]
	(cond 
		(empty? path)	nil
		(not (contains? board (first path))) (first path)
		:default (recur (rest path) board goal)))

(defn contains-start? [path board goal]
	(= [1 1] (first path)))

(defn contains-goal? [path board goal]
	(contains? (set path) goal))

(defn long-step? [path board goal]
	(when (and (first path) (second path))
		(if (contains? (set (maze-step (first path))) (second path))
			(recur (rest path) board goal)
			[(first path) (second path)])))

(defn solution-mistake [path board goal]
	(cond
		(not (contains-start? path board goal)) {:include-start [1 1]}
		(not (contains-goal? path board goal)) {:include-goal goal}
		(mis-step? path board goal) {:mis-step (mis-step? path board goal)}
		(long-step? path board goal) {:long-step (long-step? path board goal)}
		(> (count path) (count (maze-search board goal))) {:solution-too-long (count path)}))

(def simple-result-skip-start
	'((2 1) (3 1) (4 1) (5 2) (6 3) (6 4) (6 5) (6 6) (6 7) (7 8) (7 9)))

(def simple-result-skip-goal
	'([1 1] (2 1) (3 1) (4 1) (5 2) (6 3) (6 4) (6 5) (6 6) (6 7) (7 8)    ))

(def simple-result-mis-step
	'([1 1] (2 1) (3 1)   (4 2)   (5 2) (6 3) (6 4) (6 5) (6 6) (6 7) (7 8) (7 9)))

(def simple-result-skip-mid
	'([1 1] (2 1) (3 1) (4 1)     (6 3) (6 4) (6 5) (6 6) (6 7) (7 8) (7 9)))

(def simple-result-inefficient
	'([1 1] (2 1) (3 1) (4 1) (5 1) (6 2) (6 3) (6 4) (6 5) (6 6) (6 7) (7 8) (7 9)))

