function fish_user_key_bindings
    command -q fzf; or return
    fzf --fish | source
end
