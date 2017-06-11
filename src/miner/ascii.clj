;; http://www.rgagnon.com/javadetails/java-0456.html

  

;; import java.text.Normalizer;
;; import java.util.regex.Pattern;
;; 
;; public class StringUtils {
;;   private StringUtils() {}
;; 
;;   public static String unAccent(String s) {
;;       String temp = Normalizer.normalize(s, Normalizer.Form.NFD);
;;       Pattern pattern = Pattern.compile("\\p{InCombiningDiacriticalMarks}+");
;;       return pattern.matcher(temp).replaceAll("");
;;   }
;; 
;;   public static void main(String args[]) throws Exception{
;;       String value = "é à î _ @";
;;       System.out.println(StringUtils.unAccent(value));
;;       // output : e a i _ @
;;   }
;; }



(defn asciify [s]
  (let [normalized (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFD)
        diacriticals #"\p{InCombiningDiacriticalMarks}+"]
    (clojure.string/replace normalized diacriticals "")))

  
(def sample "Sample text: È,É,Ê,Ë,Û,Ù,Ï,Î,À,Â,Ô,è,é,ê,ë,û,ù,ï,î,à,â,ô,ç abc")


(def accented-char-map {\È \E,\É \E, \Ê \E ,\Ë \E,\Û \U,\Ù \U,\Ï \I,\Î \I,\À \A,\Â \A,\Ô \O,\è \e,\é \e,\ê \e,\ë \e,\û
                        \u,\ù \u,\ï \i,\î \i,\à \a,\â \a, \ô \o,\ç \c})

(def cheap-asciify [s]
  (clojure.string/escape s accented-char-map))

(defn ^String casciify 
  [^CharSequence s]
  (loop [index (int 0)
         buffer (StringBuilder. (.length s))]
    (if (= (.length s) index)
      (.toString buffer)
      (let [ch (.charAt s index)
            ascii (case ch
                        (\È \É \Ê \Ë) \E
                        (\Û \Ù) \U
                        (\Ï \Î) \I
                        (\À \Â) \A
                        \Ô \O
                        (\è \é \ê \ë) \e
                        (\û \ù) \u
                        (\ï \î) \i
                        (\à \â) \a
                        \ô \o
                        \ç \c
                        ch)]
        (.append buffer ascii)
        (recur (inc index) buffer)))))

  

