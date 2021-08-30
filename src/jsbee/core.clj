(ns jsbee.core
  (:require [random-seed.core :as rand])
  (:refer-clojure :exclude [rand-nth])
  (:require [clojure.string :as string])
  (:require [clojure.java.io :as io])
  (:require [clojure.java.shell :as shell]))

(def SEED (rand-int 65536))
(rand/set-random-seed! SEED)

(def OUTPUT_PATH "output")
(def OUTPUT_FILE (str "/first-species-" SEED))
(def PC [0 4 7])
(def IC [2 5])
; (def DISSONANCE [1 3 6])
(def CF [10 11 13 12 14 13 12 11 10])
(def NOTE_MAP 
  (into {} (mapv
            (fn [note]
              [note
               (str
                (char (+ (int (Math/floor (rem note 7))) 97))
                (if (> (mod note 7) 1)
                  (apply str (repeat (int (Math/floor (/ note 7))) "'"))
                  (apply str (repeat (int (- (Math/floor (/ note 7)) 1)) "'"))))])
            (range 2 24))))

(defn write-to-file [text]
  (let [wrtr (io/writer (str OUTPUT_PATH OUTPUT_FILE ".ly") :append true)]
    (.write wrtr text)
    (.close wrtr)))

(defn write-lines [lines]
  (doseq [line lines] (write-to-file (str line "\n"))))

(defn generate-config []
  (write-lines ["\\version \"2.16.1\""
                "#(set-default-paper-size \"letter\")"
                "#(set-global-staff-size 19)"]))

(defn generate-headers [title subtitle]
  (write-lines (concat
                ["" "\\header {"]
                (mapv
                 #(str "  " (% 0) " = \"" (% 1) "\"")
                 [["title" title]
                  ["subtitle" subtitle]
                  ["copyright" "Creative Commons Attribution-ShareAlike 3.0"]
                  ["composer" "Ed Ryan"]])
                ["}"])))

(defn get-valid-pitch [pitch validPitches]
  (->>
   (map
    (fn [operand]
      (map
       (fn [interval] (operand pitch interval))
       validPitches))
    [+ -])
   flatten
   distinct
   sort))

(defn filter-by-range [v operand]
  [(filter (fn [note] (operand note (get v 1))) (get v 0)) (get v 1)])

(defn get-valid-notes [cf]
  (concat
   [[(get-valid-pitch (first cf) PC) (first cf)]]
   (pop (mapv #(vector (get-valid-pitch % (concat IC PC)) %) (rest CF)))
   [[(get-valid-pitch (last cf) PC) (last cf)]]))

(defn generate-first-species [cf operand]
  ; phase zero:
  ; - all pitches must be consonants
  ; - all pitches must be above the cf
  ; - first and last notes must be unisons or octaves
  ; phase one: 
  ; - prefer stepwise motion
  ; phase 2:
  ; - no perfect consonants approached by similar motion
  ; - second to last note should be stepwise
  ; - no consecutive perfect consonants
  ; - follow leaps with stepwise motion
  ; phase 3
  ; - no tritones between lines
  ; - avoid more than 3 consecutive imperfect consonants
  ; - contour should fit the CF
  ; - contour should not be too flat
  (->>
   (get-valid-notes cf)
   (map #(filter-by-range % operand))
   (map #(rand/rand-nth (get % 0)))))
   

(defn pitch-int-to-str [pitches]
  (mapv #(get NOTE_MAP %) pitches))

(defn generate-pitches [id pitches]
  (write-lines
   [""
    (str "  " id "= {")
    (str "  " (string/join "1 " (pitch-int-to-str pitches)) " \\bar\"|.\"")
    "}"]))

(defn generate-staff [staffId staffName clef]
   [(str "     \\context Staff = \"" staffName "\" {")
    (str "       \\set Staff.instrumentName = #\"" staffName "  \"")
    (str "       \\clef " clef
         "       \\time 2/2"
         "       \\overrideProperty Score.NonMusicalPaperColumn.line-break-system-details #'((Y-offset . 13) (alignment-distances . (10)))")
    (str "       <<\\" staffId ">>  }")])

(defn generate-score []
  (write-lines (concat
                [""
                 "\\score {"
                 "  \\context StaffGroup << "
                 "    \\set StaffGroup.midiInstrument = \"piano\""]
                (generate-staff "firstSpeciesAbove" "Sp." "treble")
                (generate-staff "cantusFirmus" "Cf." "alto")
                (generate-staff "firstSpeciesBelow" "Ba." "bass")
                ["  >>"
                 "  \\layout { }"
                 "  \\midi { \\tempo 4 = 140 }"
                 "}"])))

(defn generate-file []
  (generate-config)
  (generate-headers "Generated Counterpoint" (str "First species - " SEED))
  (generate-pitches "firstSpeciesAbove" (generate-first-species CF >))
  (generate-pitches "firstSpeciesBelow" (generate-first-species CF <))
  (generate-pitches "cantusFirmus" CF)
  (generate-score))

(defn build []
  (generate-file)
  (shell/sh "lilypond" (str (System/getProperty "user.dir") "/" OUTPUT_PATH OUTPUT_FILE))
  (shell/sh "mv" (str "." OUTPUT_FILE ".midi") (str "./" OUTPUT_PATH OUTPUT_FILE ".midi"))
  (shell/sh "mv" (str "." OUTPUT_FILE ".pdf") (str "./" OUTPUT_PATH OUTPUT_FILE ".pdf"))
  (shell/sh "rsync" "./output/" "/mnt/f/personal/jsbee/sync" "-r")) ; TODO Remove this

(build)
