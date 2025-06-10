(def listaCalorias ; gramos a kcal
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

(def listaConversiones ;taza a gramos
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

(def listaCostos
  {"granulated sugar" 4.5     ; azúcar estándar
   "all-purpose flour" 3.0    ; harina de trigo
   "cocoa powder" 12.0
   "powdered sugar" 5.0
   "chocolate chips" 18.0
   "egg" 3.0                  ; por unidad promedio
   "canola oil" 6.5
   "extra-virgin olive oil" 22.0
   "almond flour" 25.0
   "baking powder" 2.0
   "kosher salt" 0.5
   "vanilla extract" 48.0     ; extracto real
   "lemon zest" 2.0
   "lemon juice" 1.5
   "fettuccine pasta" 4.0
   "butter" 17.0
   "heavy cream" 12.0
   "romano cheese" 30.0
   "parmesan cheese" 35.0
   "salt" 0.5
   "garlic salt" 2.5
   "vegetable oil" 5.5
   "garlic" 3.5
   "fresh rosemary" 10.0
   "olive oil" 20.0
   "white wine vinegar" 3.0
   "red pepper flakes" 8.0
   "smoked paprika" 6.5
   "parsley" 4.0})


(def listaRecetas
  (->> (file-seq (java.io.File. "recetas"))
       (filter #(and (.isFile %) (.endsWith (.getName %) ".txt")))
       (map #(.getPath %))
       (into [])))


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

(defn convertir-temp [linea temp-opcion]
  (let [matcher (re-matcher #"(\d+(?:\.\d+)?)\s*°?\s*([fFcC])\b" linea)]
    (loop [result "" last-end 0]
      (if (.find matcher)
        (let [start (.start matcher)
              end (.end matcher)
              num (Double/parseDouble (.group matcher 1))
              unidad (.group matcher 2)
              unidad-lc (if (or (= unidad "c") (= unidad "C")) "c" "f")
              nueva-temp (cond
                           (and (= temp-opcion "c") (= unidad-lc "f"))
                           (str (format "%g" (* (- num 32) (/ 5.0 9.0))) "°C")
                           (and (= temp-opcion "f") (= unidad-lc "c"))
                           (str (format "%g" (+ (* num (/ 9.0 5.0)) 32)) "°F")
                           :else (.group matcher 0))]
          (recur (str result (subs linea last-end start) nueva-temp) end))
        (str result (subs linea last-end))))))

;Obtener titulo receta
(defn obtenerTitulo [ruta]
  (first (splitLines (slurp ruta))))

(defn obtenerInstrucciones [ruta temp]
  (let [lineas (splitLines (slurp ruta))
        start (->> lineas
                   (map-indexed vector)
                   (filter (fn [[_ l]] (re-find #"(?i)instructions" l)))
                   first
                   first)]
    (if start
      (->> (subvec lineas (inc start))
           (filter #(re-find #"\S" %))
           (map #(convertir-temp % temp))) ; aplicar conversión aquí
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

(defn caloriasTotales [ingredientes]
  (reduce
   (fn [total ingr]
     (+ total (or (:calorias ingr) 0)))
   0
   ingredientes))

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
                         (str "<div class=\"porciones\">Porciones (" porcionesOpciones ")</div>\n")
                         "")
        calorias-html (str "<div class=\"calorias\">Calorías totales: "
                           (int caloriasTotales) " kcal</div>\n")
        ingredientes-html
        (str "<h2 class=\"ingredientes\">Ingredientes</h2>\n<ul>\n"
             (apply str
                    (for [{:keys [cantidad unidad ingrediente]} ingredientes]
                      (let [cantidad-str (if cantidad
                                           (str "<span class=\"cantidad\">" cantidad "</span>")
                                           "")
                            unidad-str (or unidad "")
                            ingrediente-str (or ingrediente "")]
                        (str "<li>" cantidad-str " " unidad-str " " ingrediente-str "</li>\n"))))
             "</ul>\n")
        instrucciones-html
        (str "<h2>Instrucciones</h2>\n<ol>\n"
             (apply str
                    (for [inst instrucciones]
                      (let [sin-num (.replaceAll inst "^\\s*\\d+\\.\\s*" "")
                            temp-html (.replaceAll sin-num
                                                   "(\\d+(?:\\.\\d+)?\\s*°\\s*[CFcf])"
                                                   "<span class=\"temp\">$1</span>")]
                        (str "<li>" temp-html "</li>\n"))))
             "</ol>\n")
        style "
  <style>
    body {
      min-height: 100vh;
      margin: 0;
      font-family: 'Quicksand', 'Segoe UI', Arial, sans-serif;
      background: linear-gradient(135deg, #fffbe6 0%, #ffe6f0 100%);
      color: #222;
    }
    h1 {
      font-family: 'Playfair Display', 'Georgia', serif;
      font-size: 2.7em;
      font-weight: 800;
      text-align: center;
      margin-top: 1.1em;
      margin-bottom: 0.5em;
      letter-spacing: 0.04em;
      color: #a67c52;
      text-shadow: 0 2px 8px #f3e9d2;
    }
    .porciones {
      background: #fff3e0;
      color: #b26a00;
      font-weight: bold;
      font-size: 1.25em;
      padding: 0.4em 1.2em;
      border-radius: 1.2em;
      display: inline-block;
      margin: 1em auto 1.5em auto;
      text-align: center;
      box-shadow: 0 2px 8px #f3e9d2;
      letter-spacing: 0.03em;
      display: block;
      width: fit-content;
    }
    .ingredientes {
      color: #b26a00;
      background: #fff3e0;
      padding: 0.2em 1em;
      border-radius: 1em;
      display: inline-block;
      font-size: 1.4em;
      font-weight: bold;
      margin-bottom: 0.5em;
      margin-top: 1.5em;
      box-shadow: 0 2px 8px #f3e9d2;
    }
    h2 {
      font-size: 1.3em;
      font-weight: 700;
      letter-spacing: 0.02em;
      margin-top: 1.5em;
    }
    ul, ol {
      font-size: 1.08em;
    }
    .cantidad {
      color: #b8860b;
      font-weight: bold;
    }
    .temp {
      color: #cc0000;
      font-weight: bold;
    }
    .calorias {
      font-style: italic;
      color: #888;
      margin-bottom: 1em;
      font-size: 1.05em;
    }
    .autor {
      font-size:0.9em;
      color:#555;
      margin-bottom:1em;
      text-align:center;
    }
  </style>
  <link href=\"https://fonts.googleapis.com/css?family=Quicksand:400,700|Playfair+Display:700,800&display=swap\" rel=\"stylesheet\">
"
        html (str "<!DOCTYPE html>\n<html>\n<head>\n<title>" titulo "</title>\n"
                  style
                  "</head>\n"
                  "<body>\n"
                  "<h1>" titulo "</h1>\n"
                  "<div class=\"autor\">-- " autor "</div>\n"
                  porciones-html
                  ingredientes-html
                  calorias-html
                  instrucciones-html
                  "</body>\n</html>")]
    (spit nombre-archivo html)))

(defn calculoCalorias [cantidadGramos ingri]
  (let [kcal-por-100g (get listaCalorias ingri)]
    (if kcal-por-100g
      (* (/ cantidadGramos 100.0) kcal-por-100g)
      nil)))


(defn analisisCosto [ingri cantidad]
  (let [costo100g (get listaCostos ingri)]
     (if costo100g
       (* cantidad costo100g)
       nil)))

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
                              cantidadCalorias
                              cant)
             calorias (calculoCalorias cantidadCalorias ingri)
             costo (analisisCosto ingri cantidad-final)
             unidad-final (if (nil? unidad) unidad (if (= (lowerCase tipoConversion) "metric") "grams" unidad))]
         {:cantidad cantidad-final
          :unidad unidad-final
          :ingrediente ingri-orig
          :calorias calorias
          :costo costo}))
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
      (doall
       (pmap
        (fn [ruta]
          (let [start-time (System/nanoTime)
                titulo (obtenerTitulo ruta)
                autor (obtenerAutor (slurp ruta))
                porcionesRecetas (obtenerPorcion (slurp ruta))
                ingredientes (mainReceta ruta tipoConversion porcionesRecetas porcionesOpciones)
                instrucciones (obtenerInstrucciones ruta tempConversion)
                caloriasTotales (caloriasTotales ingredientes)]
            (exportado titulo autor porcionesRecetas ingredientes instrucciones caloriasTotales porcionesRecetas porcionesOpciones)))
        recetasFiltradas)))))

(-main)