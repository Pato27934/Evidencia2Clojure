(def listaCalorias
  {"granulated sugar" 773
   "all-purpose flour" 455
   "cocoa powder" 196
   "powdered sugar" 467
   "chocolate chips" 805
   "egg" 72
   "canola oil" 1927
   "extra-virgin olive oil" 1910
   "almond flour" 640
   "baking powder" 53
   "kosher salt" 0
   "vanilla extract" 288
   "lemon zest" 6
   "lemon juice" 54
   "fettuccine pasta" 220
   "butter" 1627
   "heavy cream" 821
   "romano cheese" 431
   "parmesan cheese" 431
   "salt" 0
   "garlic salt" 5
   "vegetable oil" 1927
   "garlic" 203
   "fresh rosemary" 11
   "olive oil" 1910
   "white wine vinegar" 5
   "red pepper flakes" 49
   "smoked paprika" 19
   "parsley" 22})

(def listaConversiones
  {"granulated sugar" 200
   "all-purpose flour" 120
   "cocoa powder" 85
   "powdered sugar" 120
   "chocolate chips" 175
   "egg" 50
   "canola oil" 218
   "extra-virgin olive oil" 216
   "almond flour" 96
   "baking powder" 192
   "kosher salt" 288
   "vanilla extract" 208
   "lemon zest" 96
   "lemon juice" 240
   "fettuccine pasta" 100
   "butter" 227
   "heavy cream" 240
   "romano cheese" 100
   "parmesan cheese" 100
   "salt" 273
   "garlic salt" 288
   "vegetable oil" 218
   "garlic" 136
   "fresh rosemary" 28
   "olive oil" 216
   "white wine vinegar" 239
   "red pepper flakes" 45
   "smoked paprika" 92
   "parsley" 60})

(def listaRecetas
  ["recetas/Best Homemade Brownies-1.txt"
   "recetas/Lemon Cake-1.txt"
   "recetas/Fettuccine Alfredo.txt"
   "recetas/Pan-Seared Steak with Garlic Butter.txt"
   "recetas/Chimichurri Sauce.txt"])


(defn split [texto separador]
  (vec (.split texto separador)))

(defn splitLines [texto]
  (split texto "\n"))

(defn lowerCase [texto]
  (.toLowerCase texto))

(defn contienePalabra [linea palabra]
  (let [contenido (lowerCase linea)
        p (lowerCase palabra)]
    (not (nil? (re-find (re-pattern (str "(?i)" p)) contenido)))))

(defn contieneCaracter [linea]
  (not= linea (apply str (repeat (count linea) \space))))


(defn trim [s]
  (let [start (count (re-find #"^\s*" s))
        end (- (count s) (count (re-find #"\s*$" s)))]
    (subs s start end)))

(defn leerOptions [ruta]
  (let [lineas (splitLines (slurp ruta))]
    (into {}
          (map (fn [linea]
                 (let [pos (or (.indexOf linea ":") -1)
                       clave (keyword (trim (subs linea 0 pos)))
                       valor (trim (subs linea (inc pos)))]
                   [clave (lowerCase valor)]))
               lineas))))



(defn filtrarRecetas [archivos keyword]
  (filter (fn [ruta]
            (let [contenido (slurp ruta)
                  lineas (splitLines contenido)
                  titulo (first lineas)
                  start (->> lineas
                             (map-indexed vector)
                             (filter (fn [[_ l]] (re-find #"(?i)ingredients" l)))
                             first
                             first)]
              (if (= (lowerCase keyword) "all")
                true
                (or
                 ;; Buscar en el título
                 (contienePalabra titulo keyword)
                 ;; Buscar en los ingredientes
                 (when start
                   (let [seccion (subvec lineas (inc start))
                         ingredientes (take-while #(not (re-find #"(?i)instructions" %)) seccion)]
                     (some #(contienePalabra % keyword) ingredientes)))))))
          archivos))

;Obtener titulo receta
(defn obtenerTitulo [ruta]
  (first (splitLines (slurp ruta))))

(defn obtenerInstrucciones [ruta temp]
  (let [lineas (splitLines (slurp ruta ))
        start (->> lineas
                   (map-indexed vector)
                   (filter (fn [[_ l]] (re-find #"(?i)instructions" l)))
                   first
                   first)]
    (if start
      (->> (subvec lineas (inc start))
           (filter #(re-find #"\S" %))) ; solo líneas no vacías
      [])))

(defn obtenerAutor [texto]
  (let [lineas (splitLines texto)
        patrones [#"(?i)^\s*by\s+([^\n]+)"
                  #"(?i)^\s*from\s+([^\n]+)"
                  #"(?i)^\s*author\s*:\s*([^\n]+)"
                  #"(?i)^\s*submitted\s+by\s+([^\n]+)"]
        ;; Solo considerar líneas que empiezan con letra (ignorando espacios)
        empieza-con-letra? (fn [linea]
                             (boolean (re-find #"^\s*[A-Za-z]" linea)))
        buscar-autor (fn [linea]
                       (when (empieza-con-letra? linea)
                         (some (fn [pat]
                                 (let [m (re-find pat linea)]
                                   (when m
                                     (trim (last m)))))
                               patrones)))]
    (or
     (some #(buscar-autor %) lineas)
     "anonimo")))

(defn obtenerPorcion [texto]
  (let [lineas (splitLines texto)
        patrones [#"(?i)^\s*servings?\s*[:-]?\s*([0-9]+)"
                  #"(?i)^\s*serves?\s*[:-]?\s*([0-9]+)"]
        buscar-porciones (fn [linea]
                           (some (fn [pat]
                                   (let [m (re-find pat linea)]
                                     (when m
                                       (trim (last m)))))
                                 patrones))]
    (or
     (some #(buscar-porciones %) lineas)
     "N/A")))


(defn split-slash [s]
  (loop [chars (seq s) current "" acc []]
    (if (empty? chars)
      (conj acc current)
      (let [c (first chars)]
        (if (= c \/)
          (recur (rest chars) "" (conj acc current))
          (recur (rest chars) (str current c) acc))))))

(defn split-espacio [s]
  (loop [chars (seq s) current "" acc []]
    (if (empty? chars)
      (conj acc current)
      (let [c (first chars)]
        (if (= c \space)
          (recur (rest chars) "" (conj acc current))
          (recur (rest chars) (str current c) acc))))))

(defn parse-fraccion [s]
  (try
    (let [parts (split-espacio s)]
      (if (= 2 (count parts))
        (+ (Double/parseDouble (first parts))
           (let [[num den] (split-slash (second parts))]
             (/ (Double/parseDouble num) (Double/parseDouble den))))
        (if (not= -1 (.indexOf s "/"))
          (let [[num den] (split-slash s)]
            (/ (Double/parseDouble num) (Double/parseDouble den)))
          (Double/parseDouble s))))
    (catch Exception _ nil)))


(defn exportado [titulo autor porciones ingredientes instrucciones caloriasTotales porcionesRecetas porcionesOpciones]
  (let [nombre-archivo (str "salidas/" (.replaceAll (lowerCase titulo) "[^a-z0-9]+" "_") ".html")
        porciones-html (if (not= porciones "N/A")
                         (str "<div style=\"font-size:0.9em;color:#555;\">Porciones: " porciones "</div>\n")
                         "")
        ingredientes-html
        (str "<h2>Ingredientes</h2>\n<ul>\n"
             (apply str
                    (for [{:keys [cantidad unidad ingrediente]} ingredientes]
                      (str "<li>" (or cantidad "") " " (or unidad "") " " (or ingrediente "") "</li>\n")))
             "</ul>\n")
        instrucciones-html
        (str "<h2>Instrucciones</h2>\n<ol>\n"
             (apply str
                    (for [inst instrucciones]
                      (let [sin-num (clojure.string/replace inst #"^\s*\d+\.\s*" "")]
                        (str "<li>" sin-num "</li>\n"))))
             "</ol>\n")
        html (str "<!DOCTYPE html>\n<html>\n<head>\n<title>" titulo "</title>\n</head>\n"
                  "<body>\n"
                  "<h1>" titulo "</h1>\n"
                  "<div style=\"font-size:0.9em;color:#555;margin-bottom:1em;\">-- " autor "</div>\n"
                  porciones-html
                  ingredientes-html
                  instrucciones-html
                  "</body>\n</html>")]
    (spit nombre-archivo html)))

(defn calculoCalorias [cantidadGramos ingri]
  (let [kcal-por-100g (get listaCalorias ingri)]
    (if kcal-por-100g
      (* (/ cantidadGramos 100.0) kcal-por-100g)
      nil)))

(defn caloriasTotales [ingredientes]
  (reduce
   (fn [total ingr]
     (+ total (or (:calorias ingr) 0)))
   0
   ingredientes))



(defn conversionGramosTabla [cantidad unidad ingrediente]
  (let [u (when unidad (lowerCase unidad))
        ingr (when ingrediente (lowerCase ingrediente))
        factor (get listaConversiones ingr)]
    (cond
      (or (= u "gram") (= u "grams") (= u "g")) cantidad

      (and (or (= u "cup") (= u "cups")) factor) (* cantidad factor)

      (or (= u "ounce") (= u "ounces")) (* cantidad 28.35)

      (and (or (= u "pint") (= u "pints")) factor) (* cantidad factor 2)

      (and (or (= u "teaspoon") (= u "tsp")) factor) (* cantidad factor 0.0208)

      (and (or (= u "tablespoon") (= u "tablespoons") (= u "tbsp")) factor) (* cantidad factor 0.0625)

      (or (= u "dash") (= u "dashes")) (* cantidad 0.6)

      :else cantidad)))


(defn mainReceta [ruta tipoConversion porReceta porOpciones]
  (let [lineas (splitLines (slurp ruta))
        start (->> lineas
                   (map-indexed vector)
                   (filter (fn [[_ l]] (re-find #"(?i)ingredients" l)))
                   first
                   first)
        seccion (subvec lineas (inc start))
         ingredientes (->> seccion
                           (take-while #(not (re-find #"(?i)instructions" %)))
                           (filter #(re-find #"\S" %)))
         unidad-regex #"(?i)\b(cups?|pints?|ounces?|dashes?|tablespoons?|tbsp?|teaspoons?|tsp?|grams?|kgs?|ml|liters?)\b"]
    (mapv
     (fn [ing]
       (let [escalado (/ (Integer/parseInt porOpciones) (Integer/parseInt porReceta))
             cant-str (re-find #"\d+\s+\d+/\d+|\d+/\d+|\d+" ing)
             cant (when cant-str (* (parse-fraccion cant-str) escalado))
             ingri-match (re-find #"(?i)(?:[\d/\.]+\s*)?(?:cups?|pints?|ounces?|dashes?|tablespoons?|tbsp?|tsp?|teaspoons?|grams?|kgs?|ml|liters?)?\s*(.+)" ing)
             ingri-orig (when ingri-match (lowerCase (second ingri-match)))
             ingri (some (fn [[k _]]
                           (when (re-find (re-pattern (str "(?i).\\b" k "s?\\b.")) ingri-orig)
                             k))
                         listaConversiones)
             unidad-match (re-find unidad-regex ing)
             unidad (when unidad-match (lowerCase (second unidad-match)))
             cantidadCalorias (conversionGramosTabla cant unidad ingri)
             cantidad-final (if (= (lowerCase tipoConversion) "metric")
                              (cantidadCalorias)
                              cant)
             calorias (calculoCalorias cantidadCalorias ingri)
             unidad-final (if (nil? unidad) unidad (if (= (lowerCase tipoConversion) "metric") "grams" unidad))]
         {:cantidad cantidad-final
          :unidad unidad-final
          :ingrediente ingri-orig
          :calorias calorias
          ;; aquí puedes agregar más campos si lo necesitas
          }))
     ingredientes)))

(defn -main []
  (let [opciones (leerOptions "options.txt")
        keywordReceta (:filtra opciones)
        tipoConversion (:sistema opciones)
        porcionesOpciones (:porciones opciones)
        tempConversion (:temp opciones)
        recetasFiltradas (filtrarRecetas listaRecetas keywordReceta)]
    (println "Recetas encontradas con filtro:" keywordReceta)
    (if (empty? recetasFiltradas)
      (println "> Ninguna receta coincide con el filtro.")
      (doseq [ruta recetasFiltradas]
        (let [titulo (obtenerTitulo ruta)
              autor (obtenerAutor (slurp ruta))
              porcionesRecetas (obtenerPorcion (slurp ruta))
              ingredientes (mainReceta ruta tipoConversion porcionesRecetas porcionesOpciones)
              instrucciones (obtenerInstrucciones ruta tempConversion)
              caloriasTotales (caloriasTotales ingredientes)]
          (exportado titulo autor porcionesRecetas ingredientes instrucciones caloriasTotales porcionesRecetas porcionesOpciones))))))

(-main)