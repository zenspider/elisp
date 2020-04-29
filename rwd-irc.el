(require 'erc)
(require 'erc-track)
(require 'erc-match)
(require 'easymenu)

;;;###autoload
(defun rwd-irc-freenode1 ()
  (interactive)
  (erc-select :server "localhost" :port 16668
              :password (read-file-to-string "~/.erc_password")
              :nick "zenspider"))

;;;###autoload
(defun rwd-irc-freenode2 ()
  (interactive)
  (erc-select :server "localhost" :port 16668
              :password (concat "zenspider/freenode:"
                                (read-file-to-string "~/.erc_password"))
              :nick "zenspider"))

;;;###autoload
(defalias 'my-irc 'rwd-irc-freenode2)

;;;###autoload
(defun rwd-irc-direct ()
  (interactive)
  (erc-select :server "irc.freenode.net" :port 6667 :nick "zenspider"))

(defun erc-cmd-BAN (nick)
  (let* ((chan (erc-default-target))
         (who (erc-get-server-user nick))
         (host (erc-server-user-host who))
         (user (erc-server-user-login who)))
    (erc-server-send (format "MODE %s +b *!%s@%s" chan user host))))

(defun erc-cmd-FIXBAN (nick)
  (let* ((chan (erc-default-target))
         (who (erc-get-server-user nick))
         (host (erc-server-user-host who))
         (user (erc-server-user-login who)))
    (erc-server-send (format "MODE %s +b *!%s@%s ##fix-your-connection" chan user host))))

(defun erc-cmd-UNBAN (nick)
  (let* ((chan (erc-default-target)))
    (erc-server-send (format "MODE %s -b %s" chan nick))))

(defun erc-cmd-VOICE (nick)
  (let* ((chan (erc-default-target)))
    (erc-server-send (format "MODE %s +v %s" chan nick))))

(defun erc-cmd-DEVOICE (nick)
  (let* ((chan (erc-default-target)))
    (erc-server-send (format "MODE %s -v %s" chan nick))))

(defun erc-cmd-KICKBAN (nick &rest reason)
  (setq reason (mapconcat #'identity reason " "))
  (and (string= reason "")
       (setq reason nil))
  (erc-cmd-BAN nick)
  (erc-server-send (format "KICK %s %s %s"
                           (erc-default-target)
                           nick
                           (or reason
                               "Kicked (kickban)"))))

(defun erc-cmd-RB (nick)
  (erc-send-message (format "!ban %s !T 1d" nick)))

(defun erc-cmd-TROLL (nick)
  (erc-send-message (format "!ban %s !T 1d trolls go home" nick)))

(defun erc-cmd-FOOL (nick)
  (customize-push-and-save 'erc-fools (list nick)))

(defun erc-cmd-UNFOOL (nick)
  (setq erc-fools (remove nick erc-fools))
  (customize-push-and-save 'erc-fools (list)))

(defun erc-cmd-FIX (nick)
  (erc-send-message (format "!ban %s !T 1h please fix your connection" nick)))

(defun erc-cmd-SPAM (nick)
  (erc-send-message (format "!ban %s !T 1d spammers go home" nick)))

(defun erc-cmd-MUTE (nick)
  (erc-send-message (format "!mute %s" nick)))

(defun erc-cmd-STFU (nick)
  (erc-cmd-MUTE nick))

(defun erc-cmd-LAST (len)
  "Recall the last LEN lines from dIRCproxy for the current channel"
  (let* ((chan (erc-default-target)))
    (erc-server-send (format "dircproxy recall %s %s" chan len))))

(defun erc-cmd-PLAY ()
  "Replay the savebuffer in ZNC. Not as smart as dircproxy but..."
  (erc-server-send (format "PRIVMSG *status PlayBuffer %s" (erc-default-target))))

(defun erc-cmd-CLEAR ()
  "Replay the savebuffer in ZNC. Not as smart as dircproxy but..."
  (erc-server-send (format "znc ClearBuffer %s" (erc-default-target))))

(defun erc-cmd-CLEARALL ()
  "Replay the savebuffer in ZNC. Not as smart as dircproxy but..."
  (erc-server-send (format "znc ClearAllChannelBuffers")))

(defun erc-cmd-PROXY (line)
  "Say shit to dircproxy."

  (erc-server-send (format "dircproxy %s" (erc-trim-string line))))
(put 'erc-cmd-PROXY 'do-not-parse-args t)

(defun erc-cmd-UNPROXY ()
  "Quit dircproxy."
  (erc-server-send "dircproxy quit"))

(provide 'rwd-irc)
