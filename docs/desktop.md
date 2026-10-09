# Desktop Setup (GNOME)

**Platform**: Linux + GNOME Shell (tested on Ubuntu 24.04, GNOME 46, X11)
**Look**: macOS-inspired, native GNOME underneath
**Apply**: `desktop-theme`
**Undo**: `desktop-theme --reset`

The goal is a desktop that *feels* like macOS — a floating bottom dock,
translucency, rounded corners, a thin top bar, a Spotlight-style launcher —
without installing a third-party GTK theme. Shell themes like WhiteSur break
on every GNOME release; the stock Yaru/Adwaita theme does not. So the shell
theme stays stock and the macOS feel comes from extensions, icons, cursors,
fonts and spacing.

---

## The two scripts

| Script | Role |
| :--- | :--- |
| `setup.sh` | Installs the **parts**: extensions, icon and cursor themes, fonts, ImageMagick |
| `desktop-theme` | Writes the **configuration**: every gsettings key, and which extensions are on |
| `desktop-wallpaper` | Generates and sets the gradient wallpapers |
| `theme-mode` | Owns light/dark across every themed surface, desktop included |
| `menu` / `keys` | Action menu and keybinding cheatsheet, on Super+Alt+Space and Super+K |

This split matches the rest of the repo: `setup.sh` installs packages and
never writes config. `desktop-theme` is idempotent — re-run it any time the
desktop drifts back towards stock Ubuntu.

All of it is **Linux-only**. macOS already is a Mac, so `.chezmoiignore` keeps
`desktop-theme` and `desktop-wallpaper` off Darwin machines entirely, and
`setup.sh` installs the parts inside its Linux branch only.

### What this does NOT own

Colour scheme, GTK theme and icon theme belong to the light/dark mode, and
`theme-mode(1)` is their only owner. `desktop-theme` deliberately sets none of
the three — an earlier version pinned them dark, which meant switching to
light left dark icons on a light panel, and re-running `desktop-theme` yanked
the whole desktop back to dark. It now re-applies the *current* mode instead.

```sh
desktop-theme              # apply the look (also sets a wallpaper if none is set)
desktop-theme --check      # report which parts are installed
desktop-theme --dry-run    # print every setting without writing one
desktop-theme --reset      # hand every key it manages back to its default
```

`--reset` resets exactly the keys the script sets, so unrelated preferences in
the same schemas are left alone.

---

## Extensions

| Extension | What it contributes |
| :--- | :--- |
| ubuntu-dock | The dock. Ubuntu's fork of Dash to Dock, on the same `dash-to-dock` schema |
| blur-my-shell | Translucent panel, overview, dock, and terminal |
| just-perfection | Thin top bar, clock moved right, no Activities button, boots to desktop |
| rounded-window-corners-reborn | 12px rounded corners on every window |
| search-light | Spotlight-style centred search overlay |
| tilingshell | Tiling layouts and snap assist |

Ubuntu's own `tiling-assistant` is **disabled**: it and Tiling Shell both grab
the drag-to-edge gesture and fight over it.

Installing an extension does not make GNOME notice it. `gnome-extensions
enable` talks to the running shell, which only knows about extensions it has
already scanned — so on the run that installs them it reports "not installed"
and quietly does nothing. `desktop-theme` therefore writes
`org.gnome.shell enabled-extensions` directly, which is what the shell reads
at startup.

---

## Menu and cheatsheet

Omarchy's `Super+Alt+Space` menu and `Super+K` cheatsheet, rebuilt on what is
already here — fzf, the desktop scripts, and GNOME's own settings panels. Both
open in a small undecorated Ghostty window so they read as overlays.

| Command | Key | What it is |
| :--- | :--- | :--- |
| `menu` | `Super+Alt+Space` | Theme, wallpaper, settings panels, session actions |
| `keys` | `Super+K` | Every keybinding, fuzzy-searchable; Enter copies one |

`menu` hides any action whose command is missing, so it never offers something
this machine cannot do. `keys` reads the README's Keybindings section rather
than keeping a second copy — a cheatsheet that can disagree with the docs is
worse than none. Both heading levels become the filter column (`Neovim/LSP`,
`Emacs/Org`), and Neovim's three-column tables are handled by finding the cell
that holds the backtick rather than assuming a column.

Set `MENU_KEYS=0 desktop-theme` to skip registering the two hotkeys.

## Wallpapers

Generated, not shipped — a dotfiles repo has no business carrying tens of
megabytes of image, and generating locally means each machine renders at its
own resolution.

```sh
desktop-wallpaper --list        # show the six palettes
desktop-wallpaper monterey      # switch to one
desktop-wallpaper --regenerate  # re-render (e.g. after attaching a 4K monitor)
```

| Name | Description |
| :--- | :--- |
| `aurora` | Indigo and violet with a teal horizon (default) |
| `monterey` | Deep ocean blue drifting into cyan |
| `ember` | Charcoal with a low ember glow |
| `abyss` | Near-black with a single cold blue swell |
| `sequoia` | Deep forest teal and emerald |
| `dusk` | Magenta sunset falling into deep purple |

Each has a dark and a light rendering, wired to `picture-uri-dark` and
`picture-uri`, so the desktop follows the system colour scheme.

The renderer draws a mesh gradient from six colour control points on a 64x36
canvas via ImageMagick's `-sparse-color Shepards`, then scales it up — the
upscale is what makes the blend smooth. A vignette adds depth, and a little
grain is deliberate: a smooth 8-bit gradient across 1920px bands visibly, and
noise dithers that away while reading as texture. Output is JPEG because the
grain defeats PNG compression (11 MB a piece against 300 KB).

---

## What changes, and what deliberately does not

**Changed**

| Area | Setting |
| :--- | :--- |
| Dock | Bottom, floating (not full width), autohide + intellihide, 44px icons, dots for running apps |
| Top bar | 30px, clock on the right, no Activities button, boots to the desktop |
| Window buttons | Traffic lights on the **left**, macOS order |
| Fonts | Inter 11 for UI and titles, SauceCodePro Nerd Font 12 (Source Code Pro) for monospace |
| Icons / cursor | Colloid-Dark, Bibata-Modern-Classic |
| Windows | 12px rounded corners, new windows centred, modal dialogs attached as sheets |
| Trackpad | Natural scrolling, tap to click |
| Desktop | Home icon hidden, icons start top-right |

**Left alone**

Keybindings. No Super-as-Cmd remapping, so `Ctrl+C`/`Ctrl+V` keep working in
terminals and Emacs. The one exception is `Super+Space`: GNOME binds it to
"switch input source", which with a single keyboard layout switches between
one layout and itself, so Search Light claims it as the Spotlight key.
`desktop-theme --reset` gives it back.

Window buttons are the other judgement call, since left-hand traffic lights
are muscle memory rather than looks:

```sh
TRAFFIC_LIGHTS=0 desktop-theme    # keep Ubuntu's right-hand buttons
```

---

## Terminal

Ghostty is translucent (`background-opacity = 0.92`) so the desktop blur reads
through it. The blur is **not** ghostty's `background-blur` — that is a macOS
and KDE/Wayland path and does nothing on GNOME/X11. Blur My Shell provides it,
whitelisted to `com.mitchellh.ghostty` so only the terminal pays the cost
rather than every window on screen.

---

## Troubleshooting

**Extensions not visible after applying.** They load at shell startup. Log out
and back in, or on X11 press `Alt+F2`, type `r`, Enter.

**An extension broke after a GNOME upgrade.** Extensions are pinned to a shell
version by extensions.gnome.org. Re-run `setup.sh` to fetch builds for the new
version, or remove the offending UUID from `ENABLE_EXTENSIONS` in
`desktop-theme`.

**Wallpaper looks soft on an external monitor.** It was rendered for whatever
was attached at the time. `desktop-wallpaper --regenerate` re-renders at the
largest display currently connected (capped at 4K).

**btop's colours do not match.** `theme-mode` generates `modus-dark.theme` and
`modus-light.theme` from the ghostty theme files and points `btop.conf` at
them. btop reads its theme once at startup, so restart it after switching.

**Everything looks wrong and you want out.** `desktop-theme --reset` restores
Ubuntu's defaults, including its own extension set.
