;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;;
;;                                              file: cbr-play-rec.lisp  ;;
;;                                                                       ;;
;;                                   author: Claudio Baccigalupo @ IIIA  ;;
;;                                                                       ;;
;;                                                                       ;;
;;                                                     date: 2006.03.29  ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; A Case-based Reasoning Playlist Recommender - version 7
;;
;; This code implements the algorithm presented in the paper
;; "Case-based Sequential Ordering of Songs for Playlist Recommendation"
;; (C. Baccigalupo, E. Plaza - 2006).
;;
;; To use the program: (run ID) returns a playlist for the song ID.
;; For example: (run 3861136) returns a playlist for "Plug In Baby" (Muse).
;;

;; version 7 - changed www.strands.com to mystrands.com/internalservices
;; version 6 - changed popularity with logarithm /// naaaah bad idea!
;; version 5 - web added (titles xml)
;; version 4 - added support for sameSongs.txt file  
;; version 3 - everything is loaded in MEMORY, no hard disk support


;;-------------------------------------------------------------------------
;;
;; SETTING UP THE CASE BASE
;;
;;-------------------------------------------------------------------------



;; The original Repository is formed by ".rpf" files, provided by 
;; MusicStrands. These files must be loaded before running the recommender
;; with the function load-rpf-files ().

 
     
(defun rpf-files ()
  "Return the list of provided repository files to load (Case Base)."
  (directory (merge-pathnames "rpf/")))

(defvar *CB-SAME-SONGS* (make-hash-table :test #'eql) )


(defun load-rpf-files ()
  "Load the repository files (Case Base)."
  (when (eql 0 (hash-table-count *CB-SAME-SONGS*))
    (load-same-songs-file))
  (loop for rpf-file in (rpf-files)
        do (with-open-file (in rpf-file)
             (dbg :Load "~&;; Loading file ~a" rpf-file)
             (do* ((line (read-line in nil) (read-line in nil)))
                  ((null line))
               (load-rpf-line line)))))

(defun load-same-songs-file ()
  (with-open-file (in (make-pathname :name "sameSong.txt"))
    (dbg :Load "~&;; Loading same songs file")
    (do* ((line (read-line in nil) (read-line in nil)))
         ((null line))
      (load-same-songs-line line))))

(defun load-same-songs-line (line)
  (loop with duplicates = (string->list (substitute #\SPACE #\: line))
        for song in duplicates
        do (setf (gethash song *CB-SAME-SONGS*)
                 (first duplicates))))

;; When loading playlists from the Case Base, we skip playlists that have
;; less than *LAMBDA-MIN* songs, more than *LAMBDA-MAX* songs, a variety 
;; smaller than *VARIETY-MIN* and (to be done) an alphabetical order. 
;; All these playlists are considered 'noisy'.

(defvar *LAMBDA-MIN* 4
  "Playlists with less songs are considered noisy.")
(declaim (type fixnum *LAMBDA-MIN*))

(defvar *LAMBDA-MAX* 45
  "Playlists with more songs are considered noisy.")
(declaim (type fixnum *LAMBDA-MAX*))

(defvar *VARIETY-MIN* 0.001
  "Playlists with a smaller variety are considered noisy.")
;(declaim (type float *VARIETY-MIN*))



;; The playlists from the Case Base are saved in the hash table
;; *CB-PLAYLISTS*, using their unique ID as the hash-key.

(defvar *CB-PLAYLISTS* (make-hash-table :test #'eql)
   "The playlists loaded from the repository (Case Base).")



;; For every playlist, we also save the unique song-id and artist-id of 
;; every track in the hash table *CB-TRACKS*, (key: song-id, value: 
;; artist-id).

(defvar *CB-TRACKS* (make-hash-table :test #'eql)
   "The tracks loaded from the repository (Case Base).")

(defun artist (song)
  "Returns the artist of SONG."
  (declare (type integer song))
  (the integer (gethash song *CB-TRACKS*)))



;; For every playlist, we also save the relevant patterns in *CB-PATTERNS*
;; in order to have them available in the CBR process. We only load those
;; patterns that are no longer than *THETA*.

(defvar *THETA* 4
  "Maximum length for the relevant patterns.")
(declaim (type fixnum *THETA*))

(defvar *CB-PATTERNS* (make-hash-table :test #'eql)
  "The relevant patterns loaded from the repository (Case Base).")



;; We load the ".rpf" files line by line, where every line contains an
;; ordered list of triplets, each representing a track as (song-id, 
;; artist-id, album-id). Actually, we ignore the album-id. 

(defun same-song (song)
  (let ((same (gethash song *CB-SAME-SONGS*)))
    (if same same song)))

(defun load-rpf-line (line)
  "Read a line from a .rpf file and load the relative playlists,
   skipping noisy ones."
  (let* ((id (read-from-string (subseq line 0 (position '#\, line))))
         (tracks 
          (string->list 
           (concatenate 'string "("
            (substitute #\SPACE #\: 
             (replace-substring (subseq line (+ (position '#\| line) 1))
                                "," ") (")) ")")))                
         (songs (mapcar #'first tracks))
         (songs (mapcar #'same-song songs))
         (artists (mapcar #'second tracks))
         (length (length songs))
         (variety))
    (loop for i from 0 to (1- length)
          do (setf (gethash (elt songs i) *CB-TRACKS*)
                   (elt artists i))
          finally (setf variety (songs-variety songs)))
    (when (and (>= length *LAMBDA-MIN*) (<= length *LAMBDA-MAX*)
               (>= variety *VARIETY-MIN*))
      (loop for i from 0 to (1- (length songs))
            do (pushnew (list (ssubseq songs (- i (1- *THETA*)) i)
                              (ssubseq songs (+ i 1) (+ i *THETA*))) 
                        (gethash (elt songs i) *CB-PATTERNS*)))
      (setf (gethash id *CB-PLAYLISTS*)
            (make-instance 'playlist :songs songs :variety variety)))))



;; PLAYLIST is the structure we use for the values of *cb-playlists*.
;; SONGS is the list of songs; VARIETY is their variety.

(defclass playlist ()
  ((songs :initarg :songs :initform nil 
          :reader songs :type cons)
   (variety :initarg :variety :initform 1 
            :reader pl-variety))) ; :type (float 0.0 1.0))))

(defmethod print-object ((p playlist) stream)
  "Print a playlist as the list of songs it contains."
  (format stream "<~{~A~^,~}>" (songs p)))

(defmethod songs-count ((p playlist)) 
  "Return the number of songs included in a playlist."
  (length (songs p)))


(defun songs-from-id (playlist-id)
  "Return the songs contained in the ID playlist of the Case Base." 
  (declare (type integer playlist-id))
  (songs (gethash playlist-id *CB-PLAYLISTS*)))


(defun variety (playlist-id)
  "Return the variety of the ID playlist of the Case Base."
  (declare (type integer playlist-id))
  (pl-variety (gethash playlist-id *CB-PLAYLISTS*)))




;; VARIETY is a factor between 0 (low variety) and 1 (high variety).
;; that measures how many times the same song or artists is repeated in a
;; playlist, and how close are the repetitions.
;; The variety of a playlist is < 1 whenever it contains tracks that are:
;; - the same song, and not more distant than *GAMMA-SONG* songs;
;; - from the same artist, and not more distant than *GAMMA-ARTIST* songs.

(defvar *GAMMA-SONG* 200
  "The variety of a playlist is affected if two same songs appear in the
   playlist at a shorter distance.")
(declaim (type fixnum *GAMMA-SONG*))

(defvar *GAMMA-ARTIST* 40
  "The variety of a playlist is affected if two songs of the same artist
   appear in the playlist at a shorter distance.")
(declaim (type fixnum *GAMMA-ARTIST*))

(defun songs-variety (songs)
  "Returns the variety of a list of songs."
;  (the (float 0.0 1.0) (variety-helper songs 1)))
  (variety-helper songs 1))

(defun variety-helper (songs temp)
  "Support function for the tail recursive songs-variety function."	
  (cond ((<= temp *VARIETY-MIN*) 0)
        ((<= (length songs) 1) temp)
        (t (variety-helper 
            (rest songs)
;	    (the (float 0.0 1.0)
             (* temp (song-repetition-factor (first songs) 
                                              (rest songs)))))))
;)

(defun song-repetition-factor (song songs &optional (from-end? nil))
  "Returns a value between 0 and 1 that measures how close to the beginning
  / the end (along from-end?) of SONG (or its track properties) is the 
  first repetition of SONG (or its track properties)."
  (let* ((seq (if from-end? (reverse songs) songs))
         (len (length seq))
         (rep-song (position song seq 
                              :end (min len *GAMMA-SONG*)))
         (rep-artist (position (artist song) (mapcar #'artist seq) 
                              :end (min len *GAMMA-ARTIST*))))
    (* (if rep-song (/ (1+ rep-song) *GAMMA-SONG*) 1)
       (if rep-artist (/ (1+ rep-artist) *GAMMA-ARTIST*) 1))))
 
;;-------------------------------------------------------------------------






;;-------------------------------------------------------------------------
;;
;; THE RETRIEVE PROCESS
;;
;;-------------------------------------------------------------------------



;; The Retrieve process examines the Case Base playlists and returns the
;; lists of songs from the best *K* ones, where each playlist is ranked 
;; according to its VARIETY and its COHERENCE to the input song *S*.

;(defvar *S* 
;  "The input song.")
;(declaim (type fixnum *S*))

(defvar *K* 30
  "The number of retrieved playlists.")
(declaim (type fixnum *K*))

(defun retrieve-process (s)
  "Return the lists of songs of the *K* best-ranked Case Base playlists,
   according to S."
  (let ((ranked-playlists (rank-playlists-with s)))
    (dbg :Run "~&;; ~a playlists contain the input song." 
         (length ranked-playlists))
    ; Note: when *beta* = 0, several playlists can have the same rating
    ; value, in this case we don't WHICH ones are taken as first-n.
    (first-n 
     (remove-duplicates 
      (mapcar #'songs-from-id
              (mapcar #'first (sort ranked-playlists '> :key #'second)))
      :test #'equal) *K*)))

(defun rank-playlists-with (song)
  "Return a list with every playlist that contains SONG and its rating."
  (loop for id being the hash-keys in *cb-playlists* 
        using (hash-value pl)
        when (position song (songs pl)) 
        collect (list id (rating id (pattern-triplets pl song)))))


(defun rating (playlist-id patterns-triplets)
  "Return the RATING of the ID playlist from the Case Base in relation to 
   a song s. This is, in fact, a combination of the VARIETY of the playlist
   and of its COHERENCE to the song s."
  (* (variety playlist-id) 
     (coherence patterns-triplets)))

;; The main component of the Rating is the COHERENCE of a playlist to a 
;; song. To rapidly calculate this value, we separate the calculation of
;; the pattern count with the combination with the paramters.
;; So, patterns-relevances just contain triplets of (LAMBDA PHI PSI) values
;; (in other words; length of the pattern, pattern count and partial
;; pattern count).
;; Then, we combine them together with the combine-relevances function with
;; *ALPHA* and *BETA*, the parameters designed to control the effects of
;; the length and the popularity of patterns over COHERENCE.

(defvar *ALPHA* 0.5
  "Pattern-length parameter.")
;(declaim (type float *ALPHA*))

(defvar *BETA* 0.5
  "Pattern-popularity parameter.")
;(declaim (type float *BETA*))

(defun coherence (patterns-triplets)
  "Return the coherence of a playlist to a song combining the information
   of its patterns with the parameters *ALPHA* and *BETA*."
  (reduce '+ 
      (mapcar #'combine-relevances patterns-triplets)))

(defun combine-relevances (pattern-triplet)
  "Return the relevance of a pattern as PSI * (ALPHA^(THETA-LAMBDA)) /
   (PSI^BETA) where PATTERN-TRIPLET contains (LAMBBDA PSI PHI)."
  (let ((lambda (first pattern-triplet))
        (phi (second pattern-triplet))
        (psi (third pattern-triplet)))
    (* phi
       (/ (expt *alpha* (- *theta* lambda))
          (expt psi *beta*)))))

;; These PATTERN-TRIPLETS are calculated run-time from *CB-PATTERNS*
;; taking into account only the patterns that contain S and occur
;; at least twice in the Case Base and are not longer than *THETA*

(defun valid-patterns (sequence pivot)
  "Return all the patterns contained in SEQUENCE that include PIVOT and
   are not longer than *THETA*."
  (remove nil 
   (loop with index = (position pivot sequence)
         for i from 1 to 3
         append (loop for j from 0 to i
                      collect (ssubseq2 sequence (+ (- index i) j)
                                        (+ (1+ index) j))))))

(defun pattern-triplets (playlist song)
  "Return a triplet for every valid pattern of PLAYLIST that include SONG.
   This triplet contains the length of the pattern, the pattern count and
   the partial pattern count."
  (loop for pattern in (valid-patterns (songs playlist) song)
        for phi = (pattern-count pattern)
        when (> phi 1)
        collect (list (length pattern) phi
                      (pattern-count (substitute '? song pattern)))))

(defun pattern-count (pattern)
  "Returns the number of occurrences of PATTERN in the Case Base (PHI).
   PATTERN can contain a ? as a wild card (used to calculate the partial
   pattern count PSI)."                                            
  (let ((index (position '? pattern)))
    (if (null index)
        ; Calculate the pattern count - PHI
        (count (rest pattern) 
               (mapcar (n-first (1- (length pattern))) 
                       (mapcar #'second 
                               (gethash (first pattern) *cb-patterns*))) 
               :test #'equal)
        ; Calculate the partial pattern count - PSI
      (if (> index 0)
          ; Equivalent to the PATTERN COUNT excluding the '? song
          (count (rest pattern) 
                 (mapcar 
                  #'(lambda (complete-pattern)
                      (setf (elt complete-pattern (1- index) ) '?
                            complete-pattern complete-pattern))
                  (remove index  
                    (mapcar (n-first (1- (length pattern))) 
                      (remove nil 
                        (mapcar #'second 
                          (gethash (first pattern) *cb-patterns*))))
                    :test #'> :key #'length)) 
                 :test #'equal)
          ; Evaluate the PATTERN COUNT from the second song, but
          ; on the condition that at least there is one predecessor
        (count (rest (rest pattern))
               (mapcar 
                (n-first (- (length pattern) 2)) 
                (mapcar #'second 
                        (remove nil
                                (gethash (second pattern) *cb-patterns*)
                                :key #'first))) 
               :test #'equal)))))



 
;;-------------------------------------------------------------------------




;;-------------------------------------------------------------------------
;;
;; THE REUSE PROCESS
;;
;;-------------------------------------------------------------------------

;; The Reuse process combines the retrieved playlists to generate a 
;; single playlist, that contains the song S and has length *LAMBDA*.

(defvar *LAMBDA* 10
  "The length of the playlist to recommend.")
(declaim (type fixnum *LAMBDA*))

(defvar *RETRIEVED-PLAYLISTS* nil
  "The list of *k* retrieved playlists.")

(defun reuse-process (*retrieved-playlists* s *lambda*)
  (dbg :Run "~&;; Combining ~a retrieved playlists." 
       (length *retrieved-playlists*)) 
  (beam-search (new-node s) #'complete? #'expand #'heuristics))


;;-------------------------------------------------------------------------
;; REUSE (1): The Search Process
;;-------------------------------------------------------------------------

;; Actually, we implement the search as a Beam-Search. The code for the
;; search is taken from Paradigms of AI Programming - 1991 Peter Norvig

(defvar *BEAM-WIDTH* 20
  "The maximum number of paths saved in memory in the beam search.")
(declaim (type fixnum *BEAM-WIDTH*))

(defconstant fail nil)

(defun beam-search (start goal-p successors cost-fn)
  "Search highest scoring states first until goal is reached,
  but never consider more than beam-width states at a time."
  (tree-search 
   (list start) goal-p successors 
   #'(lambda (new old)
       (let ((sorted (funcall (sorter cost-fn) new old)))
         (dbg :Search "~&;; Sorted: ~a~%~%" (first-n sorted 4))
         (if (> *beam-width* (length sorted))
           sorted (subseq sorted 0 *beam-width*))))))

(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner."
  (dbg :Search "~&~%;; Search: ~a" (first-n states 4))
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search
             (funcall combiner
                      (funcall successors (first states)) (rest states))
             goal-p successors combiner))))

(defun sorter (cost-fn)
  "Return a combiner function that sorts according to cost-fn."
  #'(lambda (new old)
; 1. (slow) Best-first search: append new and old, then sort everything
;      (sort (append new old) #'> :key cost-fn)))
; 2. (fast) Depth-first-search: append sorted new to old
      (append (sort new #'> :key cost-fn) old)))


;; The NODES of the tree search are playlists. In the structure NODE we
;; add the sum of the COHERENCES of the songs added at every step to the
;; playlist, in order to rapidly estimate the quality of each node.

(defclass node (playlist)
  ((coherences-sum :initarg :coh-sum :initform 0
                   :reader coh-sum))) ; :type float)))

;; The Reuse process is implemented as a Depth-First Search process.
;; It terminates when a playlist with length *LAMBDA* is found.

(defmethod complete? ((p node))
  "Return true if PLAYLIST is complete."
  (= (songs-count p) *LAMBDA*))

;; The search starts from the playlist that contains the input song

(defun new-node (first-song)
  "Create a partial playlist with only FIRST-ELEMENT."
  (make-instance 'node :songs (list first-song) :variety 1))




;;-------------------------------------------------------------------------
;; REUSE (2): The Hypotheses Generation
;;-------------------------------------------------------------------------

;; To generate the branches of the tree search, we use an EXPAND function
;; that considers the successors and predecessors of the first and last
;; song of a playlist, using the retrieved playlists.
  
(defun get-song-successors (song)
  "Return the songs that occur after SONG in any retrieved playlist."
  (remove-duplicates 
   (loop for pl in *RETRIEVED-PLAYLISTS*
         append (loop for index from 0 to (- (length pl) 2)
                      when (equal (nth index pl) song)
                      collect (nth (1+ index) pl)))))

(defun get-song-predecessors (song)
  "Return the songs that occur before SONG in any retrieved playlist."
  (remove-duplicates 
   (loop for pl in *RETRIEVED-PLAYLISTS*
         append (loop for index from 1 to (- (length pl) 1)
                      when (equal (nth index pl) song)
                      collect (nth (1- index) pl)))))

(defun song-coherence-factor (song songs &optional (from-end? nil))
  "Return the coherence of the node with the new song added. This value
   is a SUM: first we check if the pattern of lenght 2 has a coherence
   higher than 0, then the pattern of length 3, and so on."
  ; to be done! here I should STOP if I find a coherence = 0.
  (/ (loop for i from 1 to (min (length songs) (1- *THETA*))
           for pattern = 
           (if from-end?
               (append (subseq songs (- (length songs) i)) (list song))
             (cons song (subseq songs 0 i)))
           sum (* (expt *ALPHA* (- *THETA* i -1))
                  (pattern-count pattern)))
     (expt (pattern-count (list song)) *BETA*)))

(defun make-node (p &optional (from-end? t))
  "Return a new node, where SONG has been added to the songs of P."
  #'(lambda(song)
      (make-instance 
       'node
       :songs (if from-end?
                  (append (songs p) (list song))
                (cons song (songs p)))
       :variety (* (pl-variety p) 
                   (song-repetition-factor song (songs p) from-end?))
       :coh-sum (+ (coh-sum p)
                     (song-coherence-factor song (songs p) from-end?)))))

(defmethod expand ((p node))
  "Return the playlists that expand P with a new coherent song added
   either at the end or at the beginning of the songs of P."
  (let ((post (get-song-successors (first (last (songs p)))))
        (pre (get-song-predecessors (first (songs p)))))
    (remove nil (append (mapcar (make-node p nil) pre)
                        (mapcar (make-node p t) post)))))




;;-------------------------------------------------------------------------
;; REUSE (3): The Hypotheses Ordering
;;-------------------------------------------------------------------------

;; To evaluate the branches of the tree search, we use an HEURISTIC
;; function takes into account its VARIETY and the COHERENCE.
;; This function is recursively called *L* times to act as a look-ahead.

(defvar *L* 0
  "The look-ahead level for the heuristic function." )
(declaim (type fixnum *L*))

(defmethod heuristics ((p node)  &optional (look-ahead *L*))
  ; IMPORTANT: to be re-written as tail recursive
  "Return an heuristic value for the playlist P as a good branch in the
   tree search for the generation of a good recommended playlist."
  (if (or (= look-ahead 0) (>= (songs-count p) *lambda*))
      ; here I should use song-coherence-factor instead of coh-sum but
      ; I think that since this is just for rating, it should have the
      ; same effect
    (* (coh-sum p) (pl-variety p))
    (let ((h (remove nil
                     (mapcar #'(lambda(new-p)
                                 (heuristics new-p (1- look-ahead)))
                             (expand p)))))
      (if h (reduce #'max h) 0))))





;;-------------------------------------------------------------------------
;;
;; AUXILIARY FUNCTIONS (FOR RETRIEVING THE NAMES OF THE SONGS)
;;
;;-------------------------------------------------------------------------

(defun execute-telnet (in out)
  #+lispworks
  (sys:call-system 
   (concatenate 'string "expect < " (namestring in) " > "
                (namestring out)))
  #+cmu
  (ext:process-output
   (ext:run-program "expect" nil :input (namestring in)
                    :output (namestring out) :wait t))
  )
   

(defun tracks (&key id (max *lambda*) (retr-size *k*) (len-factor *alpha*)
                    (pop-factor *beta*) (look-ahead *L*))
  (let* ((playlist (run id :*lambda* max :*k* retr-size :*alpha* len-factor 
                       :*beta* pop-factor :*L* look-ahead))
         (session (gensym))
         (request-file (merge-pathnames 
                        (with-output-to-string(s) 
                          (format s "openstrands-req-~a" session))))
         (response-file (merge-pathnames 
                         (with-output-to-string(s) 
                           (format s "openstrands-resp-~a" session))))
         (http-buffer (make-string 0)))
    (unless playlist
      (error "No results."))
    (prepare-curl-file request-file			 
            (if playlist (songs playlist) (list 0))) 
    (when (probe-file response-file)
      (delete-file (probe-file response-file)))
    (handler-case 
        (execute-telnet request-file response-file)
      (error ()
             (error "OpenStrands services are not available.")))
    (setf http-buffer (read-file-to-string 
                       response-file))
    (when (probe-file request-file)
      (delete-file (probe-file request-file)))
    (when (probe-file response-file)
      (delete-file (probe-file response-file)))
    (let* (
	  ; (http-ok (search "HTTP/1.1 200 OK" http-buffer))
           (xml-start (search "<?xml" http-buffer))
           (xml-end-tag (if playlist "</SimpleTrackList>" "/>"))
           (xml-end (search xml-end-tag http-buffer)))
     ; 30.08.07 check removed, because curl does not return HTTP status
     ; (unless http-ok
     ;   (error "OpenStrands error."))
      (unless xml-start
        (error "OpenStrands XML format unrecognised."))
      (subseq http-buffer xml-start
              (when xml-end (+ xml-end (length xml-end-tag)))))))


(defun prepare-curl-file (file external-ids)
  (with-open-file (out file :direction :output :if-exists :supersede)
;    (write-string "spawn curl --fail -s --connect-timeout 5 --max-time 10 \"http://labs.mystrands.com/services/lookup/tracks?" out)   
;    (write-string "spawn curl --fail -s --connect-timeout 5 --max-time 10 \"http://labs.strands.com/services/lookup/tracks?" out)   
    (write-string "spawn curl --fail -s --connect-timeout 5 --max-time 10 \"https://www.mystrands.com/services/lookup/tracks?" out)
    (write-string "subscriberId=292R65MK2" out)
    (loop for id in external-ids
	  do (write-string (format nil "&id=~d" id) out))
    (write-line "\"" out)
    (write-line "expect \"</SimpleTrackList>\"" out)
    (write-line "puts \"$expect_out(buffer)\"" out)))



  
        


;;-------------------------------------------------------------------------
;;
;; RUNNING THE RECOMMENDER
;;
;;-------------------------------------------------------------------------

;; The Recommender needs two parameters from the user: the INPUT SONG S
;; and the the desired length *LAMBDA* for the recommended playlist.


(defun run (s &key (*lambda* *LAMBDA*) (*k* *K*) (*alpha*  *ALPHA*)
                   (*beta*   *BETA*) (*l* *L*))
  "Return a recommended playlist long *LAMBDA* from the input song *S*."
  (let ((s (same-song s)))
    (reuse-process (retrieve-process s) s *lambda*)))

(defun show-params ()
  "Show the parameters that can be modified by the user."
  (list '*lambda* *LAMBDA* '*k* *K* '*alpha*  *ALPHA* 
        '*beta* *BETA* '*l* *L*))
;;-------------------------------------------------------------------------


(load-rpf-files)
