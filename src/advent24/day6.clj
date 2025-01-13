(ns advent24.day6
  (:require [clojure.string :as str]))


(def ex6
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(defn turn90 [dir]
  ({:up :right
    :right :down
    :down :left
    :left :up} dir))

(defn step1 [[r c dir]]
  (case dir
    :up [(dec r) c]
    :right [r (inc c)]
    :left [r (dec c)]
    :down [(inc r) c]))

(defn at-room [room r c]
  (let [height (count room)
        width (count (peek room))]
    (if (and (< -1 r height) (< -1 c width))
      (nth (nth room r) c)
      :out)))

(defn step [room-state]
  (let [room (pop room-state)
        rcd (peek room-state)
        [r2 c2] (step1 rcd)
        loc2 (at-room room r2 c2)]
    (case loc2
      :out (conj room [r2 c2 :out])
      true (conj room [r2 c2 (peek rcd)])
      nil (conj (assoc-in room [r2 c2] true) [r2 c2 (peek rcd)])
      false (conj room (conj (pop rcd) (turn90 (peek rcd)))))))
                 


    
(defn parse-room [s]
  (mapv (fn [line] (mapv #(case % \. nil \# false \^ true) line))
        (str/split-lines s)))

(defn find-start [room]
  (first (for [r (range (count room))
               c (range (count (peek room)))
               :when (true? (nth (nth room r) c))]
           [r c])))

(defn day6-part1 [s]
  (let [room (parse-room s)
        [r c] (find-start room)]
    (loop [room-state (conj room [r c :up])]
      (if (= :out (peek (peek room-state)))
        ;;room-state ; really need to count true
        (count (mapcat #(filter true? %) (pop room-state)))
        (recur (step room-state))))))



(def input24-6
  "..#....##....................#...#...#..........................................................................................#.
.....#..............................................#....#.....#.....#..............................#.......#...#.............#...
............#.#.........................................#...................#..#...........................###..........#.....#...
.....#.....#.............#...........................................................#.#.................#.........#..............
.......................#...........................#................#...........#......#.....................#...#.......#....#...
...#........................................#....................#.......#........##.##.........#.........................#.......
......#...........#....................#...........#.............................#........................#....................#..
...........................#...................................#...............#.........................................#........
........#...........#................................................................#............#...............................
.........................#......#.......#..............#......#.................#..................#...................#..........
............#......................................................#..............................#........#....#.................
........#.....................................................................#..............#....................................
..#............................#.................#.#............#.................................................................
...#................#.....................#.................................................#...................#.................
......#...................#........................................................................................#..#...........
.....................##..#........................................#...................#............#.....#........................
.......#.................#..........#...........................#.......#..............#..........................................
.............#........#........#..........#................#.........#.##.................................#.......#...........#...
.#..............................................................#..#.....................................#.................##.....
.........................#.#.......#.#..................................................#....................#.......#.........#..
.............#................................................#..#................#.............#.....#...........................
.............................................#..............#.....#..............#.........#......................................
................#............#............................#...........#.#.........................................................
...#.................................#.......#.........................................#..........................................
...............................#.....................................................................#.....#.........##...........
...................#...................................#.....................#.............................#..#...#...............
......#.......................................................#............................#......#.................#.#...........
...........................................#........................#.........#........................##......#..................
....#.......................#...................#.....#....................#......................................................
.....#......#....#..........#............#........#...............................................................................
.........#.................................................................................#........#.........................#...
........................#................................#........#...#......#........#..........#.#.......#.........#.........#.#
.##......#..#......#............#.#.................................#..............................#.........#....................
......#..............................#..........#.........#.....#....................................#...........................#
..#.........................#.#...#............................................#.#.......................................##.......
#.........................#......................#.............#..#.#...#....#.....#.#...#......#........................#........
.#............................#.................................#....#..............................#........#..........#.........
..........#.........#.....#.....#......................#............................................#.............................
.........................#...#.............................#..............................................#......#.#..............
....................#................................................................................................#............
...#........#......................#..#...........#..#.........#.................................................#................
...................#....................#..............#......................#..............#........#......................#....
.............#..................................................................................#................#.....##.........
.#.............................#........#................#..#..#.....#...........###..............................................
.......................................................#.........................#.............................#..................
...........................##.............#.........................................................#.......................#.....
......#.......##.........#..................................................................#.#..#.........#.........#.......#....
..#...........#..........#........#....#..........#..................#.............#.............#................................
#................................##...........#......#....##......#..#.....................................#.........#..#.........
...#...#.#.......................#......................................#.........#......#...............#........................
..#...#.......................................................#..................#.........................................#......
...#.....#.....#..........#....................#..................#.........................#...............#...#..#....#.........
..........#................................#...................#..#...........#.........#....................................#....
..............#........#............................#..................................................................#........#.
............................................................#.....##.................................#............................
...##..............................................#..........................................#.............................#.##..
...#...........................................................................#......#.....................#.#...................
.........#.............................................................................##.....................................#.#.
...............#.....#........#..#................................................................................................
#...........................#.#..............................#........^......#.................................#.....#............
...#...#.........................................#......................#.#.......................................................
.#......#...........................#.............................................................................................
.....#.........#.......#..#........#.....#.....................#......#........................................#...#..............
....#....................................................................................................#........................
#..............#.......................#.........................................#...............................#.#..............
................................#..............................#.......................................................#....#.....
....#............................#............................................................#.....#........#...................#
...........#...#............................#....................#.......................................................#........
.#.....................................................................................................#.....#....................
.#.......#..............#.....................#.....................#............#........................#.......................
............................#...#................................................#................................#...............
..........#...............#..........................#................#.........................#.......#.#.......................
..............................#.............................................................................................#.....
..............................................#....#...............................................#.....#........................
.........#...................................................#...............#.....#................#.............................
...........................................#..............#....................................................#........#.#.......
..........................#...........#.#...#.....................................#......................#...........#..#.........
....#...............#..............#.............................#....#.....#....#................................................
....#.#...........#..............#.................................................................#..............................
.....#.......#..........................................................................................#.#.......................
................................................#..#..............................................................................
.#....#..............................#..........#................#.............................................................#..
....#.......................#........#.....#..............#...............#..#........................................#...........
...........................................................................#......................................................
...#.........#..#.......................................................#..#..............................#.......................
.............#...................................................................#.......................#...#...........#........
.....#........................#....................#...................#................................#.........................
.........#......#...........................................#.......#....................#........................................
...........................................................#...........................#.............................#............
..........................#..............#........................................#......#.........#.#.............#........#.....
.........#..............#..#...........................#......#.......................#..#..#.............#.......................
...#.................#...............................................................................................#............
................#.....#.#...........................#......................#....................................#...#.#...........
..................#.....#...............................................................#............................#....#..#....
...............................#................................................#.............#..........................#........
..#...........#................................................#.........................................#...............##......#
...............................#..................##..#...........................................#................#..............
...........................#........#.........#....................................................#......#......#................
..................................#................#..................#.....#..................#...................#.....#........
.......#...#................................................................##...........................................#........
..............#..#.......#............................#...................#....#...........................#..#......#............
...............#............#................................................................................#..............#.....
...............#.....#........................#.........#.........#...#................#....#.........#...........................
................................#........................................................................#.........#..............
.............#..................#...#................................#..............#................................#.........#..
.....................#.#.............................#......#..................................#.........##....#.................#
......#.........#...#.................................#..........#.#......................#.......#........#......................
.....#....................#......................#.........#.....................#.........................................#......
.........##.....................#....#.#....#....................................................................................#
.................................................#.#....#....................#...............##..#................................
..##.......#........#.......................#.....................................................................#...........#.##
..#...........##........##......#..........#....................#.....#.....#..................#.....#............................
..........................#.......................#.....#.........................................................................
.#...........................#..............#...#...........#.....#.......#....#............#.....#..................#....#......#
.................#.......................#....#...#..................#......#............................#.....................#..
.....##..#........#........................#......#........................#....#......................##.........................
...#....................................................................................................#.......................#.
.......................#...#..............#..................#............#.#.........#....#...#..................................
...........#.......................#.......................#.......#....#.....#..................#...................#............
..................#......................................................................#.......#..........................#.....
.......#..................#.............#..#........##........#..##........##.....##...#................#..#.......#..............
........#................#..............................#..#......................................................................
...................#....#.........................#...........#.#......##.........#.#..................#..#.#....#........#.......
.............#......#.....................................................#.......................................................
.........................#.......................#..................#.........................#...............................#..#
..#...........................#............#.................##...#................#.................#................#...........
.....#...............#.....#..........#..................................#.#........................#.....###.#...................
...............................................#.............#......................................................#.............
......#......................#..............#.................#.#........#.................#......................................
........................#.......................................#....................#.......#.#..#...............................")



;; (day6-part1 input24-6)
;; 5095


;;; part 2
;;; add one obstacle to cause a loop
;;; need to keep track of travel direction and find loops
;;; room at val should be dir (not just true)
;;; loop detected when next step is same as current dir -- quit with :loop
;;; OK to overwrite with different dir

;;; for each location in original path, next step is possible inserted obstacle.  Or 90
;;; degree turn instead of straight




(defn step2 [room-state]
  (let [room (pop room-state)
        rcd (peek room-state)
        dir (peek rcd)
        [r2 c2] (step1 rcd)
        loc2 (at-room room r2 c2)]
    (if (= dir loc2)
      (conj room [r2 c2 :loop])
      (case loc2
        :out (conj room [r2 c2 :out])
        false (conj room (conj (pop rcd) (turn90 dir)))
        (conj (assoc-in room [r2 c2] dir) [r2 c2 dir])))))

                 


(defn run-loop [room-state]
  (let [res (peek (peek room-state))]
    (case res
      :loop :loop
      :out :out
      (recur (step2 room-state)))))

(defn day6-part2 [s]
  (let [room (parse-room s)
        [r c] (find-start room)]
    (loop [room-state (conj room [r c :up]) loopers 0]
      (let [dir (peek (peek room-state))]
        (if (= dir :out)
          loopers
          (if (= dir :loop)
            (assert "unexpected original loop")
            (let [rcd (peek room-state)
                  [ro co] (step1 rcd)]
              (if (nil? (at-room (pop room-state) ro co))
                ;; must already be empty, not obstacle false or visited
                (let [room-ob-state (conj (assoc-in (pop room-state) [ro co] false)
                                          (conj (pop rcd) (turn90 (peek rcd))))
                      res (run-loop room-ob-state)]
                  (recur (step2 room-state) (if (= res :loop) (inc loopers) loopers)))
                (recur (step2 room-state) loopers)))))))))


;; (day6-part2 input24-6)
;; 1933
