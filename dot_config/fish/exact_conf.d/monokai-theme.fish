# Monokai for fish, matching Ghostty's "Monokai Classic", Neovim's
# monokai.nvim and Emacs's monokai-theme.
#
# The terminal stays dark in both theme modes, so there is one palette:
# theme-mode(1) still sets the universal __theme_mode (its --check reads it),
# but the colours, bat and delta do not change with it.
#
# fish 4.x writes conf.d/fish_frozen_theme.fish on upgrade to pin its stock
# ANSI theme; this file sorts after "fish_..." so it still wins if that file
# ever reappears. conf.d is an exact_ directory in chezmoi, so the frozen file
# is removed on apply.

function __monokai_apply --description 'Apply the Monokai palette'
    set -g fish_color_normal         f8f8f2             # foreground
    set -g fish_color_command        a6e22e             # functions: the command word
    set -g fish_color_keyword        f92672             # keywords
    set -g fish_color_quote          e6db74             # strings
    set -g fish_color_param          fd971f             # parameters
    set -g fish_color_option         ae81ff             # constants: flags
    set -g fish_color_redirection    f92672             # operators
    set -g fish_color_end            f92672             # ; and &
    set -g fish_color_escape         ae81ff
    set -g fish_color_comment        75715e             # comments
    set -g fish_color_error          f92672 --underline
    set -g fish_color_operator       f8f8f2
    set -g fish_color_autosuggestion 75715e             # ghost text, quiet like comments
    set -g fish_color_selection      f8f8f2 --background=49483e
    set -g fish_color_search_match   --background=49483e
    set -g fish_color_cwd            66d9ef
    set -g fish_color_cwd_root       f92672
    set -g fish_color_user           a6e22e
    set -g fish_color_host           f8f8f2
    set -g fish_color_host_remote    e6db74
    set -g fish_color_status         f92672
    set -g fish_color_valid_path     --underline
    set -g fish_color_history_current --bold
    set -g fish_color_cancel         -r
    set -g fish_pager_color_progress            75715e
    set -g fish_pager_color_prefix              66d9ef --bold
    set -g fish_pager_color_completion          f8f8f2
    set -g fish_pager_color_description         75715e
    set -g fish_pager_color_selected_background --background=49483e

    # Dark syntax themes for bat and delta (git's pager) in both modes; the
    # light starship config theme-mode generates is the same Monokai palette.
    set -e STARSHIP_CONFIG
    set -gx BAT_THEME "Monokai Extended"
    set -e DELTA_FEATURES
end

__monokai_apply
