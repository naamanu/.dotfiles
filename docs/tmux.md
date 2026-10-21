# tmux Setup

**Prefix**: `C-a`
**Plugin manager**: TPM
**Theme**: static Modus Vivendi-style status colors
**Config**: `~/.tmux.conf`

---

## Plugins

| Plugin | Purpose |
| :--- | :--- |
| tmux-plugins/tpm | Plugin manager |
| tmux-plugins/tmux-sensible | Sensible defaults |
| tmux-plugins/tmux-yank | System clipboard integration in copy mode |
| tmux-plugins/tmux-resurrect | Manual session save and restore |

---

## Options

| Option | Value |
| :--- | :--- |
| History limit | 100,000 lines |
| Display time | 2000ms |
| Escape time | 0ms |
| Mouse | on |
| Window/pane base index | 1 |
| Renumber windows | on |
| Activity alerts | on, silent |
| Copy mode | vi keys |
| Resurrect pane contents | on |
| Resurrect nvim strategy | session |

Terminal overrides enable true color for xterm-256color, Alacritty, and Ghostty, plus undercurl and strikethrough support for Neovim diagnostics.

---

## Keybindings

All bindings use prefix `C-a` unless noted as no-prefix.

### Session and Config

| Key | Action |
| :--- | :--- |
| `prefix + R` | Reload `~/.tmux.conf` |
| `prefix + C-s` | Save session with resurrect |
| `prefix + C-r` | Restore session with resurrect |

### Popovers

Floating tools over the current pane (in its directory), closing with the program.

| Key | Action |
| :--- | :--- |
| `prefix + g` / `prefix + G` | lazygit / gh-dash |
| `prefix + t` | Scratch shell |
| `prefix + f` | yazi |
| `prefix + s` | sesh: switch/create a session (tmux sessions + zoxide dirs) |

### Windows

| Key | Action |
| :--- | :--- |
| `prefix + c` | New window |
| `prefix + p` | Previous window |
| `prefix + n` | Next window |
| `prefix + ,` | Rename window |
| `prefix + &` | Kill window |

### Panes

| Key | Action |
| :--- | :--- |
| `prefix + \|` | Split horizontal in current path |
| `prefix + -` | Split vertical in current path |
| `prefix + h/j/k/l` | Select pane left/down/up/right |
| `prefix + H/J/K/L` | Resize pane left/down/up/right, repeatable |
| `prefix + m` | Toggle pane zoom |
| `prefix + x` | Kill pane |

### Neovim-Aware Navigation

No-prefix `C-h/j/k/l` and `M-h/j/k/l` are intercepted. If the active pane is running Neovim, keys are forwarded to Neovim; otherwise tmux handles pane navigation or resizing.

| Key | In tmux | In Neovim |
| :--- | :--- | :--- |
| `C-h/j/k/l` | Select pane | Move window |
| `M-h/j/k/l` | Resize pane by 3 | Resize window |

### Copy Mode

Enter copy mode with `prefix + [`.

| Key | Action |
| :--- | :--- |
| `v` | Begin selection |
| `C-v` | Toggle rectangle selection |
| `y` | Copy selection and exit |
| `q` | Exit copy mode |

### Plugin Management

| Key | Action |
| :--- | :--- |
| `prefix + I` | Install plugins |
| `prefix + U` | Update plugins |
| `prefix + alt+u` | Remove unused plugins |

---

## Validation

```sh
tmux -L test -f ~/.tmux.conf start-server
```
