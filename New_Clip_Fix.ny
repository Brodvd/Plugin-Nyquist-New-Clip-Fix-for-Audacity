;nyquist plug-in
;version 4
;type process
;name "Riduzione Clipping"

;; Algoritmo avanzato di Clip Fix di Benjamin Schwartz
;; Clip Fix è un correttore di clipping digitale avanzato
;; L'algoritmo è abbastanza semplice:
;; 1. Trova tutte le regioni tagliate
;; 2. Ottieni la pendenza immediatamente su entrambi i lati della regione
;; 3. Esegui un'interpolazione bicubica avanzata.
;; 4. Passa alla regione successiva

$control GAIN (_ "Riduci l'ampiezza per consentire il ripristino dei picchi (dB)") float "" -9 -30 0

(setf thresh-ratio 1.0)  ; Soglia predefinita al 100%
(setf gain-lin (db-to-linear GAIN))
(setf buffersize 100000)
(setf slopelength 4)  ; numero di campioni utilizzati per calcolare la pendenza di uscita / rientro

(defun declip (sig thresh peak)
  (let* ((thresh (* thresh peak))
         (ln (truncate len))
         (finalbufsize (rem ln buffersize)))
    ;; Calcola il numero di buffer che possiamo elaborare.
    ;; se il buffer finale non è abbastanza grande per il de-clipping
    ;; lo aggiungeremo semplicemente alla fine così com'è.
    (if (>= finalbufsize slopelength)
        (setf buffercount (1+ (/ ln buffersize))) 
        (setf buffercount (/ ln buffersize)))
    ;;; Crea una sequenza di output dai buffer elaborati
    (setf out
      (seqrep (i buffercount)
        (let* ((step (min buffersize (- ln (* i buffersize))))
               (buffer (snd-fetch-array sig step step))
               (processed (process buffer thresh step)))
          (cue (mult gain-lin
                    (snd-from-array 0 *sound-srate* processed))))))
    ;;; Se rimane audio non elaborato, aggiungilo alla fine
    (if (and (> finalbufsize 0)(< finalbufsize slopelength))
        (seq out (cue (getfinalblock sig finalbufsize gain-lin)))
        out)))

(defun getfinalblock (sig step gain-lin)
  (let ((block (snd-fetch-array sig step step)))
    (mult gain-lin (snd-from-array 0 *sound-srate* block))))

(defun process (buffer thresh bufferlength)
  ;;; Trova le soglie di attraversamento
  (setf exit-list ())         ; elenco dei tempi in cui la forma d'onda supera la soglia
  (setf return-list ())       ; elenco dei tempi in cui la forma d'onda torna sotto la soglia
  ;; Limitazione dell'algoritmo: i primi e gli ultimi 'slopelength' alle estremità del buffer vengono ignorati
  ;; in modo da avere abbastanza campioni oltre l'attraversamento della soglia per calcolare la pendenza.
  (let ((last-sample (- bufferlength slopelength)))
    (do ((i slopelength (1+ i)))
        ((>= i last-sample))
      (if (>= (abs (aref buffer i)) thresh)
          (when (< (abs (aref buffer (- i 1))) thresh)   ; abbiamo appena attraversato la soglia
            (push (- i 1) exit-list))
          (when (>= (abs (aref buffer (- i 1))) thresh)  ; siamo appena rientrati nell'intervallo
            (push i return-list)))))
  ;; Inverti gli elenchi in ordine cronologico.
  ;; Questo è più veloce che aggiungere valori in ordine cronologico.
  (setf exit-list (reverse exit-list))
  (setf return-list (reverse return-list))
  ;; Se l'audio inizia in una regione tagliata, scarta il primo ritorno
  (when (>= (abs (aref buffer (1- slopelength))) thresh)
    (setq return-list (cdr return-list)))
  ;; Interpola tra ciascuna coppia di punti di uscita / ingresso
  (let ((slopelen (1- slopelength)))
    (mapc (lambda (t0 t1)
            (interpolate buffer t0 t1 slopelen))
          exit-list return-list))
  buffer)

(defun interpolate (buffer t0 t1 dur)
  "Interpolazione bicubica avanzata"
  (let* ((d0 (/ (- (aref buffer t0) (aref buffer (- t0 dur))) (float dur))) ; pendenza all'inizio
         (d1 (/ (- (aref buffer (+ t1 dur)) (aref buffer t1)) (float dur))) ; pendenza alla fine
         (a0 (aref buffer t0))
         (a1 (aref buffer t1))
         (a2 (/ (- d0 d1) 2.0))
         (a3 (/ (+ d0 d1) 2.0)))
    (do ((j (1+ t0) (1+ j)))
        ((= j t1))
      (setf (aref buffer j)
        (+ a0 (* a1 (- j t0)) (* a2 (expt (- j t0) 2.0)) (* a3 (expt (- j t0) 3.0)))))))

;; (get '*selection* 'peak) introdotto in Audacity 2.1.3
(multichan-expand #'declip *track* thresh-ratio (get '*selection* 'peak))
