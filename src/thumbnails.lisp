;;;; thumbnails.lisp - audio file thumbnail functionality.

(in-package #:thundersnow/common)

(defvar *thumbnail-directory* (uiop:xdg-cache-home "thundersnow/thumbnails/")
  "The directory to store thumbnails generated by `audio-file-spectrogram' in.")

(defun thumbnail-directory (&optional subdirectory)
  "Get the full path of Thundersnow's thumbnail directory. With SUBDIRECTORY, get the specified subdirectory of it.

Examples:

;; (thumbnail-directory) ;=> \"/home/user/.cache/thundersnow/thumbnails/\"
;; (thumbnail-directory \"spectrograms\") ;=> \"/home/user/.cache/thundersnow/thumbnails/spectrograms/\""
  (file-path
   (if subdirectory
       (join-path-components *thumbnail-directory* subdirectory)
       *thumbnail-directory*)))

(defun (setf thumbnail-directory) (directory &optional subdirectory)
  (when subdirectory
    (error "Cannot set subdirectories of ~S." 'thumbnail-directory))
  (setf *thumbnail-directory* (uiop:native-namestring directory)))

;;; thumbnail utility functions

;; FIX: should this be a general utility? mutility/files perhaps?
(defun subpathp (maybe-subpath &optional (directory *configuration-directory*)) ;; FIX: still wrong for (subpathp "/home/modula/f" "/home/modula")
  "True if MAYBE-SUBPATH is a subdirectory of DIRECTORY.

Note that this is basically just a convenience wrapper around `uiop:subpathp'."
  (uiop:subpathp (pathname maybe-subpath) (pathname directory)))

(defun thumbnail-cache-clear ()
  "Clear all data from Thundersnow's thumbnail cache.

See also: `thumbnail-cache-cleanup'"
  (error "thumbnail-cache-clear is not yet implemented."))

;; FIX: also add these options:
;; - LAST-ACCESSED - Remove all thumbnails last accessed N or more days ago.
(defun thumbnail-cache-cleanup (&key (broken-links t))
  "Clear \"stale\" data from Thundersnow's thumbnail cache. Each argument specifies what items to remove:

- BROKEN-LINKS - Remove all thumbnails that point to files that no longer exist.

See also: `thumbnail-cache-clear'"
  (error "thumbnail-cache-cleanup is not yet implemented."))

;;; thumbnail generation

(defun audio-file-thumbnail-output-filename (audio-file &key size (type :spectrogram))
  "Generate a default filename to write a spectrogram image for AUDIO-FILE to."
  (concat (thumbnail-directory (concat (ecase type
                                         (:spectrogram "spectrograms")
                                         (:waveform "waveforms"))
                                       "/" (first size) "x" (second size)))
          (file-path audio-file)
          ".png"))

(defun audio-file-thumbnail-ffmpeg-invocation (&key audio-file output-file show-axes (width 1024) (height 256) (mode :spectrogram) &allow-other-keys)
  `("ffmpeg"
    "-loglevel" "warning"
    "-y" ; overwrite existing files
    "-i" ,audio-file
    "-lavfi" ; number of inputs and outputs differ
    ,(concat (ecase mode
               (:spectrogram "showspectrumpic")
               (:waveform "showwavespic"))
             "=size=" width "x" height
             ":legend=" (if show-axes "enable" "disable"))
    ,output-file))

(defun audio-file-thumbnail-sox-invocation (&key audio-file output-file show-axes (width 1024) (height 256) (mode :spectrogram) &allow-other-keys)
  `("sox"
    ,audio-file
    "-n"
    "channels" "1" ; mix to mono
    ,(ecase mode
       (:spectrogram "spectrogram"))
    "-x" ,(concat width)
    "-Y" ,(concat height)
    ,@(unless show-axes '("-r"))
    "-o" ,output-file))

(defun audio-file-thumbnail (audio-file &rest args &key output-file (show-axes nil) (width 1024) (height 256) (mode :spectrogram) force (generator :ffmpeg))
  "Generate a spectrogram for FILE and return the path to the spectrogram image.

Keyword arguments:

- OUTPUT-FILE - File name to write the generated image to. If not specified, a default file path is generated in the Thundersnow thumbnails directory.
- SHOW-AXES - Whether to draw the axes in the resulting image.
- WIDTH - Width of the resulting image.
- HEIGHT - Height of the resulting image.
- MODE - Type of image to generate; :spectrogram (default) or :waveform.
- FORCE - If true, always (re)generate the thumbnail even if OUTPUT-FILE exists. If false (default), only (re)generate the thumbnail if OUTPUT-FILE does not exist or is older than the modification time of AUDIO-FILE.
- GENERATOR - What software to use to generate the thumbnail; accepted values are :ffmpeg (default) or :sox."
  (declare (ignore show-axes))
  (let ((audio-file (file-path audio-file)))
    (assert (file-exists-p audio-file) (audio-file) "File ~S does not exist" audio-file) ; FIX: make this a general condition
    (assert (member generator (list :ffmpeg :sox)) (generator) "Unknown generator ~S" generator)
    (assert (member mode (list :spectrogram :waveform)) (mode) "Unknown mode ~S" mode)
    (assert (not (and (eql generator :sox)
                      (eql mode :waveform)))
            (generator mode)
            "SoX does not support drawing waveforms")
    (let ((output-file (or output-file
                           (audio-file-thumbnail-output-filename audio-file :size (list width height) :type mode))))
      (unless (or force
                  (not (file-exists-p output-file))
                  (>= (file-write-date audio-file)
                      (file-write-date output-file)))
        (return-from audio-file-thumbnail output-file))
      (uiop:ensure-all-directories-exist (file-directory output-file))
      (values-list (list* output-file
                          (multiple-value-list
                           (uiop:run-program (apply (case generator
                                                      (:ffmpeg 'audio-file-thumbnail-ffmpeg-invocation)
                                                      (:sox 'audio-file-thumbnail-sox-invocation))
                                                    :audio-file audio-file
                                                    :output-file output-file
                                                    (remove-from-plist args :output-file))
                                             :output '(:string :stripped t)
                                             :error-output '(:string :stripped t))))))))

(defun view-image (image-file)
  "Open IMAGE-FILE in an external image viewer."
  (uiop:launch-program (list "open" image-file)))
