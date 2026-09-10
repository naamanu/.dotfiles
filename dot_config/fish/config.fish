# ======================
# PATH
# ======================
# fish_add_path is idempotent and skips directories that do not exist, so a
# nested shell never accumulates duplicates. Listed in priority order: the
# first entry ends up first on PATH.
set -gx BUN_INSTALL "$HOME/.bun"
set -gx PNPM_HOME "$HOME/.local/share/pnpm"
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set -gx GHCUP_INSTALL_BASE_PREFIX $HOME
fish_add_path -g \
    $HOME/.ghcup/bin $HOME/.cabal/bin \
    $PNPM_HOME $BUN_INSTALL/bin \
    $HOME/.cargo/bin \
    $HOME/.local/bin

# fnm's per-shell multishell dir is ephemeral, so long-running apps that
# inherit PATH once at launch (Emacs via exec-path-from-shell) can end up
# pointing at a node that no longer exists. Append the stable default alias
# as a lower-priority fallback; per-project versions still take precedence.
fish_add_path -ga $HOME/.local/share/fnm/aliases/default/bin

# ======================
# Platform-specific paths
# ======================
# Tool-specific snippets (antigravity, orbstack, rustup, texlive) live in
# conf.d/; only the entries with no file of their own are here.
switch (uname)
    case Darwin
        fish_add_path -ga "$HOME/Library/Application Support/Coursier/bin"
    case Linux
        fish_add_path -ga $HOME/.local/share/coursier/bin
        fish_add_path -g $HOME/.opencode/bin
end

# ======================
# Language/Runtime Configs
# ======================

# opam: adds the active switch to PATH and installs opam completions.
test -r "$HOME/.opam/opam-init/init.fish"; and source "$HOME/.opam/opam-init/init.fish" >/dev/null 2>&1; or true

# Everything below only matters when a person is typing at the prompt.
# Scripts, `fish -c`, and the editors' shell probes skip it, which keeps
# them fast and free of prompt/alias side effects.
status is-interactive; or exit

# ======================
# Shell Integrations
# ======================
command -q zoxide; and zoxide init fish | source
command -q fnm; and fnm env --use-on-cd --shell fish | source
command -q atuin; and atuin init fish | source
command -q starship; and starship init fish | source

# ======================
# Abbreviations
# ======================
# Abbreviations, not aliases: they expand visibly at the prompt and never
# leak into functions or scripts. An alias is a function, so `alias cat=bat`
# would silently rewrite every `cat` inside the helpers in functions/.

# Modern replacements
if command -q eza
    abbr -a ls 'eza --icons'
    abbr -a ll 'eza -l --icons'
    abbr -a la 'eza -la --icons'
    abbr -a lt 'eza --tree --icons'
    abbr -a tree 'eza --tree --icons'
end
command -q bat; and abbr -a cat bat
command -q rg; and abbr -a grep rg

# Git
abbr -a g git

# Navigation
abbr -a .. 'cd ..'
abbr -a ... 'cd ../..'
abbr -a .... 'cd ../../..'
abbr -a wks 'cd ~/workspace'
abbr -a work 'cd ~/workspace/work'

# Utility
abbr -a c clear
abbr -a h history
abbr -a vim nvim
abbr -a v nvim

# ML/AI & scientific. `uv tool install jupyterlab` exposes `jupyter-lab`.
abbr -a jl jupyter-lab
abbr -a tb 'tensorboard --logdir'
