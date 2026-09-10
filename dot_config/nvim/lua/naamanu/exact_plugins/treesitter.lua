-- Parsers this config expects; anything missing is installed on startup.
local ensure_installed = {
  "asm",
  "bash",
  "bibtex",
  "c",
  "cmake",
  "cpp",
  "css",
  "cuda",
  "diff",
  "dockerfile",
  "fish",
  "git_rebase",
  "gitcommit",
  "go",
  "haskell",
  "html",
  "javascript",
  "json",
  "latex",
  "lua",
  "make",
  "markdown",
  "markdown_inline",
  "ocaml",
  "ocaml_interface",
  "proto",
  "python",
  "query",
  "racket",
  "regex",
  "rust",
  "scheme",
  "sql",
  "toml",
  "tsx",
  "typescript",
  "typst",
  "vim",
  "vimdoc",
  "yaml",
}

-- Treesitter indentexpr only where the indent queries beat the runtime indent
-- scripts. Python and Markdown are worse under treesitter; YAML has no queries.
local no_ts_indent = { python = true, markdown = true, yaml = true }

return {
  -- Treesitter.  The `main` branch is the maintained rewrite: setup() only
  -- takes install_dir, parsers are installed explicitly, and highlighting
  -- is started per buffer via vim.treesitter.start.  The old master-branch
  -- module options (highlight/indent/incremental_selection) do not exist.
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    -- `:TSUpdate` is asynchronous on the main branch, so a headless
    -- `Lazy! sync` quits before any parser finishes compiling and they
    -- silently fall behind the plugin's grammar revisions. Block on it.
    build = function()
      require("nvim-treesitter").update(nil, { summary = true }):wait(300000)
    end,
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local ts = require("nvim-treesitter")
      ts.setup({})

      local installed = require("nvim-treesitter.config").get_installed("parsers")
      local missing = vim.tbl_filter(function(lang)
        return not vim.list_contains(installed, lang)
      end, ensure_installed)
      if #missing > 0 then
        ts.install(missing)
      end

      vim.api.nvim_create_autocmd("FileType", {
        group = vim.api.nvim_create_augroup("treesitter-start", { clear = true }),
        callback = function(ev)
          if pcall(vim.treesitter.start, ev.buf) and not no_ts_indent[ev.match] then
            vim.bo[ev.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
          end
        end,
      })

      -- The main branch dropped incremental_selection; flash's
      -- treesitter mode is the label-based replacement.
      vim.keymap.set({ "n", "x" }, "<C-space>", function()
        require("flash").treesitter()
      end, { desc = "Treesitter selection" })
    end,
  },

  -- Treesitter text objects (new API)
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    branch = "main",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      require("nvim-treesitter-textobjects").setup({
        select = { lookahead = true },
        move = { set_jumps = true },
      })

      -- select
      vim.keymap.set({ "x", "o" }, "af", function()
        require("nvim-treesitter-textobjects.select").select_textobject(
          "@function.outer",
          "textobjects"
        )
      end)
      vim.keymap.set({ "x", "o" }, "if", function()
        require("nvim-treesitter-textobjects.select").select_textobject(
          "@function.inner",
          "textobjects"
        )
      end)
      vim.keymap.set({ "x", "o" }, "ac", function()
        require("nvim-treesitter-textobjects.select").select_textobject(
          "@class.outer",
          "textobjects"
        )
      end)
      vim.keymap.set({ "x", "o" }, "ic", function()
        require("nvim-treesitter-textobjects.select").select_textobject(
          "@class.inner",
          "textobjects"
        )
      end)

      -- move
      vim.keymap.set({ "n", "x", "o" }, "]f", function()
        require("nvim-treesitter-textobjects.move").goto_next_start(
          "@function.outer",
          "textobjects"
        )
      end)
      vim.keymap.set({ "n", "x", "o" }, "[f", function()
        require("nvim-treesitter-textobjects.move").goto_previous_start(
          "@function.outer",
          "textobjects"
        )
      end)

      -- ]c / [c are Vim's own next/previous-change motions in diff mode
      -- (gitsigns diffthis, Diffview). Keep the class motion everywhere
      -- else, but hand the key back to the builtin in a diff window.
      local function class_motion(direction)
        return function()
          if vim.wo.diff then
            vim.cmd("normal! " .. vim.v.count1 .. direction .. "c")
            return
          end
          local move = require("nvim-treesitter-textobjects.move")
          if direction == "]" then
            move.goto_next_start("@class.outer", "textobjects")
          else
            move.goto_previous_start("@class.outer", "textobjects")
          end
        end
      end
      vim.keymap.set(
        { "n", "x", "o" },
        "]c",
        class_motion("]"),
        { desc = "Next class (diff: next change)" }
      )
      vim.keymap.set(
        { "n", "x", "o" },
        "[c",
        class_motion("["),
        { desc = "Previous class (diff: previous change)" }
      )
    end,
  },

  -- Auto-close/rename HTML and JSX tags
  {
    "windwp/nvim-ts-autotag",
    event = "InsertEnter",
    opts = {},
  },

  -- Treesitter context (sticky headers)
  {
    "nvim-treesitter/nvim-treesitter-context",
    event = { "BufReadPre", "BufNewFile" },
    opts = {
      max_lines = 3,
    },
  },
}
