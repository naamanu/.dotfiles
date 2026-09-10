function dev --description "Open dev workspace: two vim panes on left, claude on right"
    # tmux rejects '.' and ':' in session names.
    set -l session (basename (pwd) | string replace -a -r '[.:]' _)
    set -l cwd (pwd)

    # Reattach if session already exists
    if tmux has-session -t $session 2>/dev/null
        if test -n "$TMUX"
            tmux switch-client -t $session
        else
            tmux attach-session -t $session
        end
        return
    end

    # Create detached session
    tmux new-session -d -s $session -c $cwd

    # Split right for claude (40%), left (60%) gets vim
    tmux split-window -h -t "$session:1" -c $cwd
    tmux resize-pane -t "$session:1.2" -x 40%

    # pane 1.1 = left (vim), pane 1.2 = right (claude)
    tmux send-keys -t "$session:1.1" "nvim" Enter
    tmux send-keys -t "$session:1.2" "claude" Enter

    # Focus left pane
    tmux select-pane -t "$session:1.1"

    if test -n "$TMUX"
        tmux switch-client -t "$session:1.1"
    else
        tmux attach-session -t "$session:1.1"
    end
end
