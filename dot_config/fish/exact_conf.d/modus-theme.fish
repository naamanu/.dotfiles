# Modus for fish -- both variants, switched as a set by theme-mode(1).
#
# None of these hexes are guessed. Each is the colour Neovim actually renders
# for the matching highlight group, read out of nvim_get_hl under
# modus_vivendi (tinted) and modus_operandi, so the shell and the editors are
# the same numbers and drift together or not at all.
#
# fish 4.x writes conf.d/fish_frozen_theme.fish on upgrade to pin its stock
# ANSI theme; this file sorts after "fish_..." so it still wins if that file
# ever reappears. conf.d is an exact_ directory in chezmoi, so the frozen file
# is removed on apply.

set -g __modus_starship_light (
    test -n "$XDG_CACHE_HOME"; and echo $XDG_CACHE_HOME; or echo $HOME/.cache
)/starship/light.toml

# The universal variable is how theme-mode(1) reaches shells that are already
# open. A shell on a fresh machine has no universal variable yet, so it falls
# back to the state file every other tool reads -- otherwise fish would come up
# dark while tmux, started from the same file, comes up light.
if not set -q __theme_mode
    set -l state_file (
        test -n "$XDG_STATE_HOME"; and echo $XDG_STATE_HOME; or echo $HOME/.local/state
    )/theme-mode
    if test -r $state_file; and test (string trim < $state_file) = light
        set -g __theme_mode light
    else
        set -g __theme_mode dark
    end
end

function __modus_dark --description 'modus-vivendi-tinted palette'
    set -g fish_color_normal         ffffff             # Normal
    set -g fish_color_command        dfaf7a             # Function: the command word
    set -g fish_color_keyword        79a8ff --italics   # Keyword, italic as in Neovim
    set -g fish_color_quote          2fafff             # String
    set -g fish_color_param          4ae2f0             # Identifier: arguments
    set -g fish_color_option         feacd0             # Special: flags
    set -g fish_color_redirection    ffffff             # Operator
    set -g fish_color_end            b6a0ff             # Statement: ; and &
    set -g fish_color_escape         9ac8e0             # SpecialChar
    set -g fish_color_comment        ff9f80 --italics   # Comment, italic as in Neovim
    set -g fish_color_error          ff7f9f             # DiagnosticError
    set -g fish_color_operator       ffffff
    set -g fish_color_autosuggestion 989898             # NonText, not Comment -- see below
    set -g fish_color_selection      ffffff --background=7030af   # Visual
    set -g fish_color_search_match   --background=2f822f          # Search
    set -g fish_color_cwd            2fafff
    set -g fish_color_cwd_root       ff5f59
    set -g fish_color_user           6ae4b9
    set -g fish_color_host           ffffff
    set -g fish_color_host_remote    fec43f
    set -g fish_color_status         ff5f59
    set -g fish_pager_color_progress            989898
    set -g fish_pager_color_prefix              79a8ff --bold
    set -g fish_pager_color_completion          ffffff
    set -g fish_pager_color_description         989898
    set -g fish_pager_color_selected_background --background=7030af
end

function __modus_light --description 'modus-operandi palette'
    set -g fish_color_normal         000000             # Normal
    set -g fish_color_command        721045             # Function
    set -g fish_color_keyword        531ab6 --italics   # Keyword
    set -g fish_color_quote          3548cf             # String
    set -g fish_color_param          005e8b             # Identifier
    set -g fish_color_option         8f0075             # Special
    set -g fish_color_redirection    000000             # Operator
    set -g fish_color_end            531ab6             # Statement
    set -g fish_color_escape         005077             # SpecialChar
    set -g fish_color_comment        595959 --italics   # Comment
    set -g fish_color_error          a0132f             # DiagnosticError
    set -g fish_color_operator       000000
    set -g fish_color_autosuggestion 595959             # NonText
    set -g fish_color_selection      000000 --background=dfa0f0   # Visual
    set -g fish_color_search_match   --background=8adf80          # Search
    set -g fish_color_cwd            0031a9
    set -g fish_color_cwd_root       a60000
    set -g fish_color_user           005f5f
    set -g fish_color_host           000000
    set -g fish_color_host_remote    884900
    set -g fish_color_status         a60000
    set -g fish_pager_color_progress            595959
    set -g fish_pager_color_prefix              3548cf --bold
    set -g fish_pager_color_completion          000000
    set -g fish_pager_color_description         595959
    set -g fish_pager_color_selected_background --background=dfa0f0
end

# Autosuggestions are ghost text, not comments. Neovim renders that as NonText,
# and the upstream modus fish extra maps it to the comment salmon -- which makes
# a suggestion louder than the command it trails.
function __modus_apply --description 'Apply the palette for the current theme mode'
    set -g fish_color_valid_path --underline
    set -g fish_color_history_current --bold
    set -g fish_color_cancel -r

    # bat and delta (git's pager) pick a syntax theme by name, so they are
    # switched here alongside starship rather than left dark-on-light.
    if test "$__theme_mode" = light
        __modus_light
        set -gx STARSHIP_CONFIG $__modus_starship_light
        set -gx BAT_THEME GitHub
        set -gx DELTA_FEATURES +light
    else
        __modus_dark
        set -e STARSHIP_CONFIG
        set -gx BAT_THEME ansi
        set -e DELTA_FEATURES
    end
end

# A universal variable change reaches every running fish, so `theme-mode light'
# repaints shells that are already open rather than only new ones.
function __modus_on_mode_change --on-variable __theme_mode
    __modus_apply
end

__modus_apply
