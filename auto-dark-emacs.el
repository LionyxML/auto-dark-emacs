;; Auto-Dark-Emacs is an auto changer between 2 themes, dark/light, respecting the
;; overall settings of MacOS

;; Set your themes here
(set 'darktheme 'wombat)
(set 'lightheme 'leuven)

(set 'thebeforestate "initial") 

(run-with-timer 0 1 (lambda ()			   
			   ;; Get's MacOS dark mode state
			   (if (string= (shell-command-to-string "printf %s \"$( osascript -e \'tell application \"System Events\" to tell appearance preferences to return dark mode\' )\"") "true")
			       (progn
				 (set 'thenowstate t)
				 )
			     (set 'thenowstate nil)
			     )

			   ;; Verifies if Darkmode is changed since last checked
			   (if (string= thenowstate thebeforestate)
			       ;; If nothing is changed
			       (progn

				 )
			     ;; If something is changed

			     (if (string= thenowstate "t")
				 (progn
				   (load-theme darktheme t)
				   (disable-theme lightheme)
				   )
			       (load-theme lightheme t)
			       (disable-theme darktheme) 
			       )
			     )
			   (set 'thebeforestate thenowstate)
			   )
		     )
