local keymap = vim.keymap

-- General keymaps

-- Clear search highlights
keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>", { desc = "Clear search highlights" })

-- Window navigation is provided by vim-tmux-navigator (plugins/navigation.lua),
-- which moves between Neovim windows and tmux panes with the same keys.

-- Window resize
keymap.set("n", "<C-Up>", "<cmd>resize +2<CR>", { desc = "Increase window height" })
keymap.set("n", "<C-Down>", "<cmd>resize -2<CR>", { desc = "Decrease window height" })
keymap.set("n", "<C-Left>", "<cmd>vertical resize -2<CR>", { desc = "Decrease window width" })
keymap.set("n", "<C-Right>", "<cmd>vertical resize +2<CR>", { desc = "Increase window width" })

-- Buffer navigation (]b / [b are builtin since 0.11)
keymap.set("n", "<leader>bn", "<cmd>bnext<CR>", { desc = "Next buffer" })
keymap.set("n", "<leader>bp", "<cmd>bprevious<CR>", { desc = "Previous buffer" })
-- Snacks.bufdelete keeps the window layout; :bdelete closed the split too.
keymap.set("n", "<leader>bd", function()
  Snacks.bufdelete()
end, { desc = "Delete buffer" })

-- Better indenting
keymap.set("v", "<", "<gv", { desc = "Indent left" })
keymap.set("v", ">", ">gv", { desc = "Indent right" })

-- Move text up and down
keymap.set("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move text down" })
keymap.set("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move text up" })

-- Keep cursor centered when scrolling
keymap.set("n", "<C-d>", "<C-d>zz", { desc = "Scroll down and center" })
keymap.set("n", "<C-u>", "<C-u>zz", { desc = "Scroll up and center" })

-- Keep cursor centered when searching
keymap.set("n", "n", "nzzzv", { desc = "Next search result" })
keymap.set("n", "N", "Nzzzv", { desc = "Previous search result" })

-- Better paste (don't yank replaced text)
keymap.set("x", "<leader>p", '"_dP', { desc = "Paste without yanking" })

-- Save file
keymap.set("n", "<C-s>", "<cmd>w<CR>", { desc = "Save file" })

-- Quit
keymap.set("n", "<leader>q", "<cmd>q<CR>", { desc = "Quit" })

-- Split windows
keymap.set("n", "<leader>sv", "<C-w>v", { desc = "Split window vertically" })
keymap.set("n", "<leader>sh", "<C-w>s", { desc = "Split window horizontally" })
keymap.set("n", "<leader>se", "<C-w>=", { desc = "Make splits equal size" })
keymap.set("n", "<leader>sx", "<cmd>close<CR>", { desc = "Close current split" })

-- Tabs as workspaces (one per project), mirroring Emacs tab-bar keys
keymap.set("n", "<leader><tab>n", "<cmd>tabnew<CR>", { desc = "New tab" })
keymap.set("n", "<leader><tab>x", "<cmd>tabclose<CR>", { desc = "Close tab" })
keymap.set("n", "<leader><tab>]", "<cmd>tabnext<CR>", { desc = "Next tab" })
keymap.set("n", "<leader><tab>[", "<cmd>tabprevious<CR>", { desc = "Previous tab" })
keymap.set("n", "<leader><tab>l", "g<Tab>", { desc = "Last tab" })
keymap.set("n", "<leader><tab>r", ":tabmove ", { desc = "Move tab" })

-- Sessions: one per working directory under stdpath("state")/sessions, with
-- the tab/window layout above ('sessionoptions' in options.lua). Plain
-- :mksession/:source -- no plugin.
local function session_file()
  local dir = vim.fn.stdpath("state") .. "/sessions"
  vim.fn.mkdir(dir, "p")
  return dir .. "/" .. (vim.fn.getcwd():gsub("[/:]", "%%")) .. ".vim"
end

keymap.set("n", "<leader><tab>s", function()
  vim.cmd.mksession({ vim.fn.fnameescape(session_file()), bang = true })
  vim.notify("Session saved for " .. vim.fn.getcwd(), vim.log.levels.INFO)
end, { desc = "Save session (cwd)" })

keymap.set("n", "<leader><tab>o", function()
  local file = session_file()
  if vim.fn.filereadable(file) == 0 then
    vim.notify("No saved session for " .. vim.fn.getcwd(), vim.log.levels.WARN)
    return
  end
  vim.cmd.source(vim.fn.fnameescape(file))
end, { desc = "Open session (cwd)" })

-- REPL (core/tasks.lua): filetype -> utop/ghci/racket/sml/ipython in a snacks
-- terminal, mirroring Emacs fp-repl. <localleader> is ","; group in plugins/ui.lua.
keymap.set("n", "<localleader>r", function()
  require("naamanu.core.tasks").repl()
end, { desc = "REPL toggle" })
keymap.set("n", "<localleader>e", function()
  require("naamanu.core.tasks").repl_send_line()
end, { desc = "REPL send line" })
keymap.set("x", "<localleader>e", function()
  require("naamanu.core.tasks").repl_send_selection()
end, { desc = "REPL send selection" })
keymap.set("n", "<localleader>b", function()
  require("naamanu.core.tasks").repl_send_buffer()
end, { desc = "REPL send buffer" })
