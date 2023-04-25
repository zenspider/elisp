# integration/bash --- Bash integration ... rwd style

# PROMPT_COMMAND="prefix_command${PROMPT_COMMAND:+; $PROMPT_COMMAND}"
# PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }postfix_command"

# vs:

# PROMPT_COMMAND+="; postfix_command"
# PROMPT_COMMAND="prefix_command; $PROMPT_COMMAND"

# copied (with modifications to hostname) from /etc/bashrc_Apple_Terminal
if [ -n "${INSIDE_EMACS:-}" ] && [ -n "${EAT_SHELL_INTEGRATION_DIR:-}" ]; then
    update_terminal_cwd() {
        # Identify the directory using a "file:" scheme URL, including
        # the host name to disambiguate local vs. remote paths.
        
        # Percent-encode the pathname.
        local url_path=''
        {
	    # Use LC_CTYPE=C to process text byte-by-byte and
	    # LC_COLLATE=C to compare byte-for-byte. Ensure that
	    # LC_ALL and LANG are not set so they don't interfere.
	    local i ch hexch LC_CTYPE=C LC_COLLATE=C LC_ALL= LANG=
	    for ((i = 0; i < ${#PWD}; ++i)); do
	        ch="${PWD:i:1}"
	        if [[ "$ch" =~ [/._~A-Za-z0-9-] ]]; then
		    url_path+="$ch"
	        else
		    printf -v hexch "%02X" "'$ch"
		    # printf treats values greater than 127 as
		    # negative and pads with "FF", so truncate.
		    url_path+="%${hexch: -2:2}"
	        fi
	    done
        }
        printf '\e]7;%s\a' "file://$(hostname)$url_path"
    }

    [ -n "${EAT_SHELL_INTEGRATION_DIR:-}" ] && \
        PROMPT_COMMAND+="; update_terminal_cwd"
fi

# Local Variables:
# mode: sh
# sh-shell: bash
# End:
