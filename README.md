# Keybindings Documentation

This document outlines the keybindings configured for Neovim and tmux.

---

## Neovim Keybindings

**Leader Key:** The Neovim leader key is set to `<Space>`.

### General

| Mode | Keybinding     | Description                           |
| :--- | :------------- | :------------------------------------ |
| n    | `<Esc>`        | Clear search highlights               |
| n    | `<C-h>`        | Move to left window                   |
| n    | `<C-j>`        | Move to bottom window                 |
| n    | `<C-k>`        | Move to top window                    |
| n    | `<C-l>`        | Move to right window                  |
| n    | `<C-Up>`       | Increase window height                |
| n    | `<C-Down>`     | Decrease window height                |
| n    | `<C-Left>`     | Decrease window width                 |
| n    | `<C-Right>`    | Increase window width                 |
| n    | `<leader>bn`   | Next buffer                           |
| n    | `<leader>bp`   | Previous buffer                       |
| n    | `<leader>bd`   | Delete buffer                         |
| v    | `<`            | Indent left                           |
| v    | `>`            | Indent right                          |
| v    | `J`            | Move text down                        |
| v    | `K`            | Move text up                          |
| n    | `<C-d>`        | Scroll down and center                |
| n    | `<C-u>`        | Scroll up and center                  |
| n    | `n`            | Next search result                    |
| n    | `N`            | Previous search result                |
| x    | `<leader>p`    | Paste without yanking                 |
| n    | `<C-s>`        | Save file                             |
| n    | `<leader>q`    | Quit                                  |
| n    | `<leader>sv`   | Split window vertically               |
| n    | `<leader>sh`   | Split window horizontally             |
| n    | `<leader>se`   | Make splits equal size                |
| n    | `<leader>sx`   | Close current split                   |

### Completion (nvim-cmp & luasnip)

| Mode | Keybinding | Description                             |
| :--- | :--------- | :-------------------------------------- |
| i    | `<C-k>`    | Select previous completion item         |
| i    | `<C-j>`    | Select next completion item             |
| i    | `<C-b>`    | Scroll completion docs up               |
| i    | `<C-f>`    | Scroll completion docs down             |
| i    | `<C-Space>`| Trigger completion                      |
| i    | `<C-e>`    | Abort completion                        |
| i    | `<CR>`     | Confirm completion                      |
| i, s | `<Tab>`    | Select next item or expand/jump snippet |
| i, s | `<S-Tab>`  | Select previous item or jump back in snippet |

### Editor (Comment.nvim & nvim-autopairs)

| Mode | Keybinding | Plugin         | Description                 |
| :--- | :--------- | :------------- | :-------------------------- |
| n    | `gcc`      | Comment.nvim   | Toggle comment on current line |
| v    | `gbc`      | Comment.nvim   | Toggle block comment        |
| n    | `gc`       | Comment.nvim   | Comment line                |
| v    | `gb`       | Comment.nvim   | Block comment               |
| n    | `gcO`      | Comment.nvim   | Comment above               |
| n    | `gco`      | Comment.nvim   | Comment below               |
| n    | `gcA`      | Comment.nvim   | Comment to end of line      |
| i    | `<M-e>`    | nvim-autopairs | Fast wrap                   |

### Git (gitsigns.nvim & lazygit.nvim)

| Mode | Keybinding  | Plugin       | Description            |
| :--- | :---------- | :----------- | :--------------------- |
| n    | `]h`        | gitsigns     | Next git hunk          |
| n    | `[h`        | gitsigns     | Previous git hunk      |
| n    | `<leader>gs`| gitsigns     | Stage hunk             |
| n    | `<leader>gr`| gitsigns     | Reset hunk             |
| n    | `<leader>gS`| gitsigns     | Stage buffer           |
| n    | `<leader>gu`| gitsigns     | Undo stage hunk        |
| n    | `<leader>gR`| gitsigns     | Reset buffer           |
| n    | `<leader>gp`| gitsigns     | Preview hunk           |
| n    | `<leader>gb`| gitsigns     | Blame line             |
| n    | `<leader>gd`| gitsigns     | Diff this              |
| n    | `<leader>gg`| lazygit.nvim | Open LazyGit           |

### NvimTree

| Mode | Keybinding  | Description             |
| :--- | :---------- | :---------------------- |
| n    | `<leader>ee`| Toggle file explorer    |
| n    | `<leader>ef`| Find file in explorer   |
| n    | `<leader>ec`| Collapse file explorer  |
| n    | `<leader>er`| Refresh file explorer   |

### Telescope

| Mode | Keybinding  | Description                                 |
| :--- | :---------- | :------------------------------------------ |
| i    | `<C-k>`     | Move selection previous (in Telescope)      |
| i    | `<C-j>`     | Move selection next (in Telescope)          |
| i    | `<C-q>`     | Send selected to quickfix list and open (in Telescope) |
| n    | `<leader>ff`| Find files (Telescope)                      |
| n    | `<leader>fr`| Find recent files (Telescope)               |
| n    | `<leader>fg`| Live grep (Telescope)                       |
| n    | `<leader>fc`| Find string under cursor (Telescope)        |
| n    | `<leader>fb`| Find buffers (Telescope)                    |
| n    | `<leader>fh`| Find help (Telescope)                       |
| n    | `<leader>fk`| Find keymaps (Telescope)                    |
| n    | `<leader>ft`| Find colorschemes (Telescope)               |

### Treesitter

| Mode          | Keybinding  | Description                       |
| :------------ | :---------- | :-------------------------------- |
| Normal/Visual | `<C-space>` | Initialize incremental selection  |
| Normal/Visual | `<C-space>` | Increment selection (node)        |
| Normal/Visual | `<bs>`      | Decrement selection (node)        |
| Normal/Visual | `af`        | Select around function            |
| Normal/Visual | `if`        | Select inside function            |
| Normal/Visual | `ac`        | Select around class               |
| Normal/Visual | `ic`        | Select inside class               |
| Normal        | `]f`        | Go to next function start         |
| Normal        | `]c`        | Go to next class start            |
| Normal        | `[f`        | Go to previous function start     |
| Normal        | `[c`        | Go to previous class start        |

### LSP

| Mode | Keybinding  | Description                         |
| :--- | :---------- | :---------------------------------- |
| n    | `gr`        | Show LSP references                 |
| n    | `gD`        | Go to declaration                   |
| n    | `gd`        | Show LSP definitions                |
| n    | `gi`        | Show LSP implementations            |
| n    | `gt`        | Show LSP type definitions           |
| n, v | `<leader>la`| See available code actions          |
| n    | `<leader>lr`| Smart rename                        |
| n    | `<leader>ld`| Show buffer diagnostics             |
| n    | `<leader>lD`| Show line diagnostics               |
| n    | `[d`        | Go to previous diagnostic           |
| n    | `]d`        | Go to next diagnostic               |
| n    | `K`         | Show documentation for what is under cursor |
| n    | `<leader>ls`| Restart LSP                         |
| n    | `<leader>lf`| Format file                         |

---

## tmux Keybindings

**Prefix Key:** The tmux prefix key is set to `C-a`.

| Keybinding  | Description                   |
| :---------- | :---------------------------- |
| `C-a + R`   | Reload tmux configuration     |
| `C-a + |`   | Split window horizontally     |
| `C-a + -`   | Split window vertically       |
| `C-a + h/j/k/l` | Select left/down/up/right pane (vim-like) |
| `C-a + H/J/K/L` | Resize left/down/up/right pane |
| `C-a + m`   | Maximize pane (toggle zoom)   |
| `C-a + p`   | Previous window               |
| `C-a + n`   | Next window                   |
| `C-a + [`   | Enter copy mode               |
| `v`         | Begin selection (in copy mode)|
| `C-v`       | Rectangle selection (in copy mode) |
| `y`         | Copy selection and exit (in copy mode) |
| `C-a + I`   | Install plugins (TPM)         |
| `C-a + U`   | Update plugins (TPM)          |
| `C-a + C-s` | Save session (resurrect)      |
| `C-a + C-r` | Restore session (resurrect)   |
