# Keybindings Documentation

This document outlines the keybindings configured for Neovim, Emacs, and tmux.

---

## Neovim Keybindings

**Leader Key:** The Neovim leader key is set to `<Space>`.

### General

| Mode | Keybinding   | Description               |
| :--- | :----------- | :------------------------ |
| n    | `<Esc>`      | Clear search highlights   |
| n    | `<leader>bn` | Next buffer               |
| n    | `<leader>bp` | Previous buffer           |
| n    | `<leader>bd` | Delete buffer (Snacks)    |
| v    | `<`          | Indent left               |
| v    | `>`          | Indent right              |
| v    | `J`          | Move text down            |
| v    | `K`          | Move text up              |
| n    | `<C-d>`      | Scroll down and center    |
| n    | `<C-u>`      | Scroll up and center      |
| n    | `n`          | Next search result        |
| n    | `N`          | Previous search result    |
| x    | `<leader>p`  | Paste without yanking     |
| n    | `<C-s>`      | Save file                 |
| n    | `<leader>q`  | Quit                      |
| n    | `<leader>sv` | Split window vertically   |
| n    | `<leader>sh` | Split window horizontally |
| n    | `<leader>se` | Make splits equal size    |
| n    | `<leader>sx` | Close current split       |

### Smart Splits (Neovim + tmux Navigation & Resizing)

| Mode | Keybinding          | Description                       |
| :--- | :------------------ | :-------------------------------- |
| n    | `<C-h>`             | Move to left split/pane           |
| n    | `<C-j>`             | Move to below split/pane          |
| n    | `<C-k>`             | Move to above split/pane          |
| n    | `<C-l>`             | Move to right split/pane          |
| n    | `<A-h>`             | Resize split/pane left            |
| n    | `<A-j>`             | Resize split/pane down            |
| n    | `<A-k>`             | Resize split/pane up              |
| n    | `<A-l>`             | Resize split/pane right           |
| n    | `<leader><leader>h` | Swap buffer left                  |
| n    | `<leader><leader>j` | Swap buffer down                  |
| n    | `<leader><leader>k` | Swap buffer up                    |
| n    | `<leader><leader>l` | Swap buffer right                 |

These keybindings work seamlessly across both Neovim splits and tmux panes.

### Completion (nvim-cmp & luasnip)

| Mode | Keybinding  | Description                                  |
| :--- | :---------- | :------------------------------------------- |
| i    | `<C-k>`     | Select previous completion item              |
| i    | `<C-j>`     | Select next completion item                  |
| i    | `<C-b>`     | Scroll completion docs up                    |
| i    | `<C-f>`     | Scroll completion docs down                  |
| i    | `<C-Space>` | Trigger completion                           |
| i    | `<C-e>`     | Abort completion                             |
| i    | `<CR>`      | Confirm completion                           |
| i, s | `<Tab>`     | Select next item or expand/jump snippet      |
| i, s | `<S-Tab>`   | Select previous item or jump back in snippet |

### Editor (Comment.nvim, nvim-autopairs, vim-illuminate, undotree, refactoring.nvim)

| Mode | Keybinding   | Plugin          | Description                    |
| :--- | :----------- | :-------------- | :----------------------------- |
| n    | `gcc`        | Comment.nvim    | Toggle comment on current line |
| v    | `gbc`        | Comment.nvim    | Toggle block comment           |
| n    | `gc`         | Comment.nvim    | Comment line                   |
| v    | `gb`         | Comment.nvim    | Block comment                  |
| n    | `gcO`        | Comment.nvim    | Comment above                  |
| n    | `gco`        | Comment.nvim    | Comment below                  |
| n    | `gcA`        | Comment.nvim    | Comment to end of line         |
| i    | `<M-e>`      | nvim-autopairs  | Fast wrap                      |
| n    | `]]`         | vim-illuminate  | Next reference                 |
| n    | `[[`         | vim-illuminate  | Prev reference                 |
| n    | `<leader>u`  | undotree        | Toggle Undotree                |
| v    | `<leader>re` | refactoring.nvim | Extract Function              |
| v    | `<leader>rf` | refactoring.nvim | Extract Function To File      |
| v    | `<leader>rv` | refactoring.nvim | Extract Variable              |
| n, v | `<leader>ri` | refactoring.nvim | Inline Variable               |

### Git (gitsigns.nvim & lazygit.nvim)

| Mode | Keybinding   | Plugin       | Description       |
| :--- | :----------- | :----------- | :---------------- |
| n    | `]h`         | gitsigns     | Next git hunk     |
| n    | `[h`         | gitsigns     | Previous git hunk |
| n    | `<leader>gs` | gitsigns     | Stage hunk        |
| n    | `<leader>gr` | gitsigns     | Reset hunk        |
| n    | `<leader>gS` | gitsigns     | Stage buffer      |
| n    | `<leader>gu` | gitsigns     | Undo stage hunk   |
| n    | `<leader>gR` | gitsigns     | Reset buffer      |
| n    | `<leader>gp` | gitsigns     | Preview hunk      |
| n    | `<leader>gD` | gitsigns     | Diff this         |
| n    | `<leader>gg` | lazygit.nvim | Open LazyGit      |

### Git Advanced (diffview.nvim & git-conflict.nvim)

| Mode | Keybinding    | Plugin           | Description            |
| :--- | :------------ | :--------------- | :--------------------- |
| n    | `<leader>gdo` | diffview.nvim    | Open Diffview          |
| n    | `<leader>gdc` | diffview.nvim    | Close Diffview         |
| n    | `<leader>gdh` | diffview.nvim    | File History (current) |
| n    | `<leader>gdH` | diffview.nvim    | File History (all)     |
| n    | `<leader>gdf` | diffview.nvim    | Toggle Files Panel     |
| n    | `co`          | git-conflict.nvim | Choose ours           |
| n    | `ct`          | git-conflict.nvim | Choose theirs         |
| n    | `cb`          | git-conflict.nvim | Choose both           |
| n    | `c0`          | git-conflict.nvim | Choose none           |
| n    | `]x`          | git-conflict.nvim | Next conflict         |
| n    | `[x`          | git-conflict.nvim | Prev conflict         |

### NvimTree & Oil

| Mode | Keybinding   | Description                 |
| :--- | :----------- | :-------------------------- |
| n    | `<leader>ee` | Toggle file explorer        |
| n    | `<leader>ef` | Find file in explorer       |
| n    | `<leader>ec` | Collapse file explorer      |
| n    | `<leader>er` | Refresh file explorer       |
| n    | `-`          | Open parent directory (Oil) |
| n    | `<leader>eo` | Open Oil file explorer      |

### Telescope

| Mode | Keybinding   | Description                                            |
| :--- | :----------- | :----------------------------------------------------- |
| i    | `<C-k>`      | Move selection previous (in Telescope)                 |
| i    | `<C-j>`      | Move selection next (in Telescope)                     |
| i    | `<C-q>`      | Send selected to quickfix list and open (in Telescope) |
| n    | `<leader>ff` | Find files by frecency (Telescope)                     |
| n    | `<leader>fF` | Find all files (Telescope)                             |
| n    | `<leader>fr` | Find recent files (Telescope)                          |
| n    | `<leader>fg` | Live grep (Telescope)                                  |
| n    | `<leader>fc` | Find string under cursor (Telescope)                   |
| n    | `<leader>fb` | Find buffers (Telescope)                               |
| n    | `<leader>fh` | Find help (Telescope)                                  |
| n    | `<leader>fk` | Find keymaps (Telescope)                               |
| n    | `<leader>ft` | Find colorschemes (Telescope)                          |
| n    | `<leader>fu` | Undo tree browser (Telescope)                          |

**Extensions:** `telescope-ui-select` replaces all `vim.ui.select` prompts (code actions, etc.) with Telescope dropdown UI.

### Harpoon (Quick File Navigation)

| Mode | Keybinding   | Description            |
| :--- | :----------- | :--------------------- |
| n    | `<leader>ha` | Add file to Harpoon    |
| n    | `<leader>hh` | Open Harpoon menu      |
| n    | `<leader>1`  | Jump to Harpoon file 1 |
| n    | `<leader>2`  | Jump to Harpoon file 2 |
| n    | `<leader>3`  | Jump to Harpoon file 3 |
| n    | `<leader>4`  | Jump to Harpoon file 4 |
| n    | `<leader>5`  | Jump to Harpoon file 5 |
| n    | `<leader>hp` | Previous Harpoon file  |
| n    | `<leader>hn` | Next Harpoon file      |

### Flash (Navigation)

| Mode    | Keybinding | Description         |
| :------ | :--------- | :------------------ |
| n, x, o | `s`        | Flash jump          |
| n, x, o | `S`        | Flash Treesitter    |
| o       | `r`        | Remote Flash        |
| o, x    | `R`        | Treesitter Search   |
| c       | `<C-s>`    | Toggle Flash Search |

### Marks (Visual Marks in Gutter)

| Mode | Keybinding | Description                       |
| :--- | :--------- | :-------------------------------- |
| n    | `m{a-z}`   | Set mark (standard Vim)           |
| n    | `m,`       | Set next available lowercase mark |
| n    | `m;`       | Toggle next available mark        |
| n    | `m:`       | Preview mark                      |
| n    | `m]`       | Jump to next mark                 |
| n    | `m[`       | Jump to previous mark             |
| n    | `dm{a-z}`  | Delete a specific mark            |
| n    | `dm-`      | Delete marks on current line      |
| n    | `dm<space>`| Delete all marks in buffer        |

Marks appear as icons in the sign column (gutter) for visual feedback.

### Treesitter

| Mode          | Keybinding  | Description                      |
| :------------ | :---------- | :------------------------------- |
| Normal/Visual | `<C-space>` | Initialize incremental selection |
| Normal/Visual | `<C-space>` | Increment selection (node)       |
| Normal/Visual | `<bs>`      | Decrement selection (node)       |
| Normal/Visual | `af`        | Select around function           |
| Normal/Visual | `if`        | Select inside function           |
| Normal/Visual | `ac`        | Select around class              |
| Normal/Visual | `ic`        | Select inside class              |
| Normal        | `]f`        | Go to next function start        |
| Normal        | `]c`        | Go to next class start           |
| Normal        | `[f`        | Go to previous function start    |
| Normal        | `[c`        | Go to previous class start       |

### Mini.ai (Enhanced Text Objects)

| Mode | Text Object | Description                 |
| :--- | :---------- | :-------------------------- |
| o, v | `af/if`     | Around/inside function      |
| o, v | `ac/ic`     | Around/inside class         |
| o, v | `ao/io`     | Around/inside block/loop    |
| o, v | `aq/iq`     | Around/inside quotes        |
| o, v | `ab/ib`     | Around/inside brackets      |
| o, v | `at/it`     | Around/inside HTML tags     |
| o, v | `ad/id`     | Around/inside digits        |
| o, v | `au/iu`     | Around/inside function call |

### LSP

| Mode | Keybinding   | Description                                 |
| :--- | :----------- | :------------------------------------------ |
| n    | `gr`         | Show LSP references                         |
| n    | `gD`         | Go to declaration                           |
| n    | `gd`         | Show LSP definitions                        |
| n    | `gi`         | Show LSP implementations                    |
| n    | `gt`         | Show LSP type definitions                   |
| n, v | `<leader>la` | See available code actions                  |
| n    | `<leader>lr` | Smart rename                                |
| n    | `<leader>ld` | Show buffer diagnostics                     |
| n    | `<leader>lD` | Show line diagnostics                       |
| n    | `[d`         | Go to previous diagnostic                   |
| n    | `]d`         | Go to next diagnostic                       |
| n    | `K`          | Show documentation for what is under cursor |
| n    | `<leader>ls` | Restart LSP                                 |

### Formatting & Linting

| Mode | Keybinding   | Description              |
| :--- | :----------- | :----------------------- |
| n, v | `<leader>mp` | Format file or selection |
| n    | `<leader>ll` | Trigger linting          |

### Trouble (Diagnostics)

| Mode | Keybinding   | Description                          |
| :--- | :----------- | :----------------------------------- |
| n    | `<leader>xx` | Toggle Trouble (diagnostics list)    |
| n    | `<leader>xX` | Buffer diagnostics (Trouble)         |
| n    | `<leader>cs` | Symbols (Trouble)                    |
| n    | `<leader>cl` | LSP references/definitions (Trouble) |
| n    | `<leader>xL` | Location List (Trouble)              |
| n    | `<leader>xQ` | Quickfix List (Trouble)              |

### Search & Replace (Spectre)

| Mode | Keybinding   | Description                     |
| :--- | :----------- | :------------------------------ |
| n    | `<leader>sr` | Open Spectre (search & replace) |
| n    | `<leader>sw` | Search current word             |
| v    | `<leader>sw` | Search selection                |
| n    | `<leader>sp` | Search in current file          |

### Session Management

| Mode | Keybinding   | Description                   |
| :--- | :----------- | :---------------------------- |
| n    | `<leader>qs` | Restore session (current dir) |
| n    | `<leader>ql` | Restore last session          |
| n    | `<leader>qd` | Don't save current session    |

### Debugging (DAP)

| Mode | Keybinding   | Description             |
| :--- | :----------- | :---------------------- |
| n    | `<leader>db` | Toggle breakpoint       |
| n    | `<leader>dB` | Conditional breakpoint  |
| n    | `<leader>dc` | Continue                |
| n    | `<leader>dC` | Run to cursor           |
| n    | `<leader>di` | Step into               |
| n    | `<leader>do` | Step out                |
| n    | `<leader>dO` | Step over               |
| n    | `<leader>dp` | Pause                   |
| n    | `<leader>dt` | Terminate               |
| n    | `<leader>dr` | Toggle REPL             |
| n    | `<leader>dl` | Run last                |
| n    | `<leader>du` | Toggle DAP UI           |
| n, v | `<leader>de` | Eval expression         |
| n    | `<leader>dw` | Widgets                 |
| n    | `<leader>dj` | Down (stack)            |
| n    | `<leader>dk` | Up (stack)              |
| n    | `<leader>dg` | Go to line (no execute) |
| n    | `<leader>ds` | Session                 |

### Testing (neotest)

| Mode | Keybinding   | Description          |
| :--- | :----------- | :------------------- |
| n    | `<leader>tt` | Run Nearest Test     |
| n    | `<leader>tf` | Run File Tests       |
| n    | `<leader>ts` | Toggle Test Summary  |
| n    | `<leader>to` | Show Test Output     |
| n    | `<leader>tp` | Toggle Output Panel  |
| n    | `<leader>td` | Debug Nearest Test   |
| n    | `<leader>tS` | Stop Test            |

### Zen Mode

| Mode | Keybinding  | Description      |
| :--- | :---------- | :--------------- |
| n    | `<leader>z` | Toggle Zen Mode  |

### Markdown Preview

| Command                  | Description                  |
| :----------------------- | :--------------------------- |
| `:MarkdownPreview`       | Open preview in browser      |
| `:MarkdownPreviewStop`   | Stop preview                 |
| `:MarkdownPreviewToggle` | Toggle preview               |

### Snacks (Utilities)

| Mode | Keybinding   | Description                  |
| :--- | :----------- | :--------------------------- |
| n    | `<leader>.`  | Toggle scratch buffer        |
| n    | `<leader>S`  | Select scratch buffer        |
| n    | `<leader>n`  | Notification history         |
| n    | `<leader>bd` | Delete buffer                |
| n    | `<leader>gb` | Git blame line               |
| n    | `<leader>gB` | Git browse (open in browser) |
| n    | `<leader>un` | Dismiss all notifications    |
| n    | `<C-/>`      | Toggle terminal              |

---

## Emacs Keybindings

Emacs uses **Evil mode** for Vim-like keybindings, plus Emacs-style bindings for IDE features.

### General

| Keybinding | Description                   |
| :--------- | :---------------------------- |
| `C-` `     | Toggle vterm terminal         |
| `ESC`      | Keyboard escape/quit          |
| `M-o`      | Switch to other window        |
| `C-c w`    | Delete window                 |
| `C-c r`    | Replace string                |
| `C-.`      | Embark actions menu           |
| `M-.`      | Embark dwim (do what I mean)  |
| `C-x C-s`  | Save file                     |
| `C-x C-f`  | Find file                     |
| `C-x b`    | Switch buffer (with consult)  |
| `C-g`      | Cancel/quit current operation |

### File Management & Navigation

| Keybinding | Description                     |
| :--------- | :------------------------------ |
| `M-0`      | Select Treemacs window          |
| `C-x t t`  | Toggle Treemacs file explorer   |
| `M-s`      | Consult line search             |
| `M-y`      | Consult yank from kill ring     |
| `C-c j`    | Avy jump to character (2 chars) |

### Projectile (Project Management)

**Prefix**: `C-c p` opens the projectile command map

| Keybinding | Description              |
| :--------- | :----------------------- |
| `C-c p f`  | Find file in project     |
| `C-c p p`  | Switch project           |
| `C-c p s`  | Search in project (grep) |
| `C-c p c`  | Compile project          |
| `C-c p k`  | Kill all project buffers |
| `C-c p h`  | Show projectile help     |

### LSP (Eglot)

Active when editing a file with LSP support.

| Keybinding | Description                      |
| :--------- | :------------------------------- |
| `C-c l a`  | Code actions                     |
| `C-c l f`  | Format buffer                    |
| `C-c l r`  | Rename symbol                    |
| `C-c l d`  | Find declaration                 |
| `C-c l i`  | Find implementation              |
| `C-c l t`  | Find type definition             |
| `C-c l s`  | Search symbols (consult-eglot)   |
| `M-.`      | Go to definition (default Emacs) |
| `M-,`      | Pop back from definition         |

### Git (Magit)

| Keybinding | Description       |
| :--------- | :---------------- |
| `C-x g`    | Open Magit status |

**In Magit**: Press `?` to see all commands (staging, committing, pushing, pulling, branching, etc.)

### Debugging (DAP Mode)

| Keybinding | Description      |
| :--------- | :--------------- |
| `C-c d b`  | Add breakpoint   |
| `C-c d d`  | Start debugging  |
| `C-c d n`  | Next (step over) |
| `C-c d c`  | Continue         |

### Completion (Vertico & Corfu)

**Vertico** (minibuffer completion):

- `C-n` or `Down` - Next completion
- `C-p` or `Up` - Previous completion
- `RET` - Select completion
- `C-j` - Select without exiting

**Corfu** (in-buffer completion):

- Triggers automatically after 2 characters
- `C-n` / `C-p` - Navigate completions
- `RET` - Accept completion
- `C-g` - Cancel

### Org Mode

| Keybinding | Description |
| :--------- | :---------- |
| `C-c a`    | Org agenda  |

### Which-Key

- Press any prefix key (like `C-c`, `C-x`, or `C-c p`) and wait 0.3 seconds
- A popup will show all available completions for that prefix

---

## tmux Keybindings

**Prefix Key:** The tmux prefix key is set to `C-a`.

| Keybinding       | Description                                    |
| :--------------- | :--------------------------------------------- |
| `C-a + R`        | Reload tmux configuration                      |
| `C-a + \|`       | Split window horizontally                      |
| `C-a + -`        | Split window vertically                        |
| `C-a + h/j/k/l`  | Select left/down/up/right pane (vim-like)      |
| `C-a + H/J/K/L`  | Resize left/down/up/right pane                 |
| `C-a + m`        | Maximize pane (toggle zoom)                    |
| `C-a + p`        | Previous window                                |
| `C-a + n`        | Next window                                    |
| `C-a + T`        | Open sesh session picker (fuzzy)               |
| `C-h/j/k/l`     | Seamless Neovim/tmux pane navigation           |
| `A-h/j/k/l`     | Seamless Neovim/tmux pane resizing             |
| `C-a + [`        | Enter copy mode                                |
| `v`              | Begin selection (in copy mode)                 |
| `C-v`            | Rectangle selection (in copy mode)             |
| `y`              | Copy selection and exit (in copy mode)         |
| `C-a + I`        | Install plugins (TPM)                          |
| `C-a + U`        | Update plugins (TPM)                           |
| `C-a + C-s`      | Save session (resurrect)                       |
| `C-a + C-r`      | Restore session (resurrect)                    |

### Sesh Session Picker (`C-a + T`)

| Keybinding | Description                    |
| :--------- | :----------------------------- |
| `C-a`      | Show all sessions              |
| `C-t`      | Filter to tmux sessions        |
| `C-g`      | Filter to config sessions      |
| `C-x`      | Filter to zoxide directories   |
| `C-f`      | Find directories               |
| `C-d`      | Kill selected tmux session     |
