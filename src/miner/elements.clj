(ns miner.elements
  (:require (clojure [string :as str]))
  (:use clojure.repl))

;; Inspired by [Br]eaking [Ba]d credits

;; more element info at:  http://php.scripts.psu.edu/djh300/cmpsc221/p3s11-pt-data.htm


(def element-info {
  "H" ["Hydrogen" 1]
  "He" ["Helium" 2]
  "Li" ["Lithium" 3]
  "Be" ["Beryllium" 4]
  "B" ["Boron" 5]
  "C" ["Carbon" 6]
  "N" ["Nitrogen" 7]
  "O" ["Oxygen" 8]
  "F" ["Fluorine" 9]
  "Ne" ["Neon" 10]
  "Na" ["Sodium" 11]
  "Mg" ["Magnesium" 12]
  "Al" ["Aluminium" 13]
  "Si" ["Silicon" 14]
  "P" ["Phosphorus" 15]
  "S" ["Sulfur" 16]
  "Cl" ["Chlorine" 17]
  "Ar" ["Argon" 18]
  "K" ["Potassium" 19]
  "Ca" ["Calcium" 20]
  "Sc" ["Scandium" 21]
  "Ti" ["Titanium" 22]
  "V" ["Vanadium" 23]
  "Cr" ["Chromium" 24]
  "Mn" ["Manganese" 25]
  "Fe" ["Iron" 26]
  "Co" ["Cobalt" 27]
  "Ni" ["Nickel" 28]
  "Cu" ["Copper" 29]
  "Zn" ["Zinc" 30]
  "Ga" ["Gallium" 31]
  "Ge" ["Germanium" 32]
  "As" ["Arsenic" 33]
  "Se" ["Selenium" 34]
  "Br" ["Bromine" 35]
  "Kr" ["Krypton" 36]
  "Rb" ["Rubidium" 37]
  "Sr" ["Strontium" 38]
  "Y" ["Yttrium" 39]
  "Zr" ["Zirconium" 40]
  "Nb" ["Niobium" 41]
  "Mo" ["Molybdenum" 42]
  "Tc" ["Technetium" 43]
  "Ru" ["Ruthenium" 44]
  "Rh" ["Rhodium" 45]
  "Pd" ["Palladium" 46]
  "Ag" ["Silver" 47]
  "Cd" ["Cadmium" 48]
  "In" ["Indium" 49]
  "Sn" ["Tin" 50]
  "Sb" ["Antimony" 51]
  "Te" ["Tellurium" 52]
  "I" ["Iodine" 53]
  "Xe" ["Xenon" 54]
  "Cs" ["Caesium" 55]
  "Ba" ["Barium" 56]
  "La" ["Lanthanum" 57]
  "Ce" ["Cerium" 58]
  "Pr" ["Praseodymium" 59]
  "Nd" ["Neodymium" 60]
  "Pm" ["Promethium" 61]
  "Sm" ["Samarium" 62]
  "Eu" ["Europium" 63]
  "Gd" ["Gadolinium" 64]
  "Tb" ["Terbium" 65]
  "Dy" ["Dysprosium" 66]
  "Ho" ["Holmium" 67]
  "Er" ["Erbium" 68]
  "Tm" ["Thulium" 69]
  "Yb" ["Ytterbium" 70]
  "Lu" ["Lutetium" 71]
  "Hf" ["Hafnium" 72]
  "Ta" ["Tantalum" 73]
  "W" ["Tungsten" 74]
  "Re" ["Rhenium" 75]
  "Os" ["Osmium" 76]
  "Ir" ["Iridium" 77]
  "Pt" ["Platinum" 78]
  "Au" ["Gold" 79]
  "Hg" ["Mercury" 80]
  "Tl" ["Thallium" 81]
  "Pb" ["Lead" 82]
  "Bi" ["Bismuth" 83]
  "Po" ["Polonium" 84]
  "At" ["Astatine" 85]
  "Rn" ["Radon" 86]
  "Fr" ["Francium" 87]
  "Ra" ["Radium" 88]
  "Ac" ["Actinium" 89]
  "Th" ["Thorium" 90]
  "Pa" ["Protactinium" 91]
  "U" ["Uranium" 92]
  "Np" ["Neptunium" 93]
  "Pu" ["Plutonium" 94]
  "Am" ["Americium" 95]
  "Cm" ["Curium" 96]
  "Bk" ["Berkelium" 97]
  "Cf" ["Californium" 98]
  "Es" ["Einsteinium" 99]
  "Fm" ["Fermium" 100]
  "Md" ["Mendelevium" 101]
  "No" ["Nobelium" 102]
  "Lr" ["Lawrencium" 103]
  "Rf" ["Rutherfordium" 104]
  "Db" ["Dubnium" 105]
  "Sg" ["Seaborgium" 106]
  "Bh" ["Bohrium" 107]
  "Hs" ["Hassium" 108]
  "Mt" ["Meitnerium" 109]
  "Ds" ["Darmstadtium" 110]
  "Rg" ["Roentgenium" 111]
  "Cn" ["Copernicium" 112]
  "Uut" ["Ununtrium" 113]
  "Uuq" ["Ununquadium" 114]
  "Uup" ["Ununpentium" 115]
  "Uuh" ["Ununhexium" 116]
  "Uus" ["Ununseptium" 117]
  "Uuo" ["Ununoctium" 118]
  })

(def element-symbols (keys element-info))

(def two-char-symbols (filter #(== (count %) 2) element-symbols))

(def single-char-symbols (filter #(== (count %) 1) element-symbols))

(defn element-name [symbol]
  (first (get element-info symbol)))

(defn atomic-number [symbol]
  (second (get element-info symbol)))

(defn badstr [word symbol at]
  (str (subs word 0 at) "[" symbol "]" (subs word (+ at (count symbol)))))

(defn upword [word]
  (vec (seq (str/upper-case word))))

(defn lower-char [^Character ch]
  (Character/toLowerCase ch))


  
;; ignore case
(defn subsIgnoreCase? [word start target]
  (.equalsIgnoreCase ^String target (subs word start (+ start (count target)))))

;; element is a vector of capital chars
(defn cap-element [element]
  (case (count element)
    1 (str element)
    2 (str (first element) (lower-char (second element)))
    (str/capitalize (apply str element))))

;; about half the speed of the other one
(defn badize-symbol-slow
  ([word symbol] (badize-symbol-slow word symbol 0))
  ([word symbol at]
     (let [slen (count symbol)
           end (+ at slen)
           len (count word)]
       (when (<= end len)
         (let [ww (str/capitalize (subs word at end))]
           (if (= ww symbol)
             (vector at symbol)
             (recur word symbol (inc at))))))))


(defn badize-symbol
  ([word symbol] (badize-symbol word symbol 0))
  ([word symbol at]
     (let [slen (count symbol)
           end (+ at slen)
           len (count word)]
       (when (<= end len)
           (if (subsIgnoreCase? word at symbol)
             (vector at symbol)
             (recur word symbol (inc at)))))))


(defn badize-symbols [word symbols]
  (let [results (keep #(badize-symbol word %) symbols)]
    (when (seq results)
      results)))

;; bug stops at first possibilty, should use at to keep looking
(defn badize [word]
  (map #(badstr word (second %) (first %))
       (or (badize-symbols word two-char-symbols)
           (badize-symbols word single-char-symbols))))



(defn badize-element
  ;; word and element are vectors of up char
  ([word element] (badize-element word element 0))
  ([word element at]
     (let [elen (count element)
           end (+ at elen)
           len (count word)]
       (when (<= end len)
         (if (= (subvec word at end) element)
           (vector at (cap-element element))
           (recur word element (inc at)))))))

(defn badize-elements [word elements]
  (let [results (keep #(badize-element word %) elements)]
    (when (seq results)
      results)))


(def two-char-elements (map upword two-char-symbols))
(def single-char-elements (map upword single-char-symbols))

;; maybe 10% faster by using vectors of chars
(defn badize2 [word]
  (let [vword (upword word)]
    (map #(badstr word (second %) (first %))
         (or (badize-elements vword two-char-elements)
             (badize-elements vword single-char-elements)))))




;; keep last
;(prn `(in-ns '~(ns-name *ns*)))


