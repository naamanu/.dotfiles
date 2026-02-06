return {
  {
    "rebelot/heirline.nvim",
    event = "UiEnter",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      local conditions = require("heirline.conditions")
      local utils = require("heirline.utils")

      -- Pull colors from current colorscheme (adapts to catppuccin/kanagawa/etc)
      local function setup_colors()
        return {
          bg = utils.get_highlight("StatusLine").bg,
          fg = utils.get_highlight("StatusLine").fg,
          bright_bg = utils.get_highlight("Folded").bg,
          bright_fg = utils.get_highlight("Folded").fg,
          red = utils.get_highlight("DiagnosticError").fg,
          yellow = utils.get_highlight("DiagnosticWarn").fg,
          blue = utils.get_highlight("Function").fg,
          green = utils.get_highlight("String").fg,
          mauve = utils.get_highlight("Keyword").fg,
          orange = utils.get_highlight("Constant").fg,
          teal = utils.get_highlight("Special").fg,
          subtle = utils.get_highlight("Comment").fg,
          surface = utils.get_highlight("CursorLine").bg,
          diag_error = utils.get_highlight("DiagnosticError").fg,
          diag_warn = utils.get_highlight("DiagnosticWarn").fg,
          diag_hint = utils.get_highlight("DiagnosticHint").fg,
          diag_info = utils.get_highlight("DiagnosticInfo").fg,
          git_add = utils.get_highlight("diffAdded").fg or utils.get_highlight("String").fg,
          git_change = utils.get_highlight("diffChanged").fg or utils.get_highlight("DiagnosticWarn").fg,
          git_del = utils.get_highlight("diffRemoved").fg or utils.get_highlight("DiagnosticError").fg,
        }
      end

      -- Recompute on colorscheme change
      vim.api.nvim_create_autocmd("ColorScheme", {
        callback = function()
          utils.on_colorscheme(setup_colors)
        end,
      })

      local Align = { provider = "%=" }
      local Space = { provider = " " }

      -- Mode
      local ViMode = {
        init = function(self)
          self.mode = vim.fn.mode(1)
        end,
        static = {
          mode_names = {
            n = "NORMAL", no = "N-PENDING", nov = "N-PENDING", noV = "N-PENDING",
            i = "INSERT", ic = "INSERT", ix = "INSERT",
            v = "VISUAL", vs = "VISUAL", V = "V-LINE", Vs = "V-LINE",
            ["\22"] = "V-BLOCK", ["\22s"] = "V-BLOCK",
            s = "SELECT", S = "S-LINE", ["\19"] = "S-BLOCK",
            R = "REPLACE", Rc = "REPLACE", Rx = "REPLACE", Rv = "V-REPLACE",
            c = "COMMAND", cv = "EX", ce = "EX",
            r = "PROMPT", rm = "MORE", ["r?"] = "CONFIRM",
            ["!"] = "SHELL", t = "TERMINAL",
          },
          mode_colors = {
            n = "blue", i = "green", v = "mauve", V = "mauve",
            ["\22"] = "mauve", c = "orange", s = "teal", S = "teal",
            ["\19"] = "teal", R = "red", r = "red", ["!"] = "teal", t = "teal",
          },
        },
        provider = function(self)
          return " %2(" .. (self.mode_names[self.mode] or self.mode) .. "%) "
        end,
        hl = function(self)
          local m = self.mode:sub(1, 1)
          return { fg = "bg", bg = self.mode_colors[m] or "blue", bold = true }
        end,
        update = { "ModeChanged", pattern = "*:*" },
      }

      local ModeSepRight = {
        provider = "",
        hl = function()
          local m = vim.fn.mode(1):sub(1, 1)
          local colors_map = {
            n = "blue", i = "green", v = "mauve", V = "mauve",
            ["\22"] = "mauve", c = "orange", s = "teal", S = "teal",
            ["\19"] = "teal", R = "red", r = "red", ["!"] = "teal", t = "teal",
          }
          return { fg = colors_map[m] or "blue", bg = "bg" }
        end,
        update = { "ModeChanged", pattern = "*:*" },
      }

      -- Git
      local Git = {
        condition = conditions.is_git_repo,
        init = function(self)
          self.status_dict = vim.b.gitsigns_status_dict
        end,
        { provider = "  " },
        {
          provider = function(self)
            return " " .. (self.status_dict.head or "")
          end,
          hl = { fg = "mauve", bold = true },
        },
        {
          provider = function(self)
            local count = self.status_dict.added or 0
            return count > 0 and (" +" .. count) or ""
          end,
          hl = { fg = "git_add" },
        },
        {
          provider = function(self)
            local count = self.status_dict.changed or 0
            return count > 0 and (" ~" .. count) or ""
          end,
          hl = { fg = "git_change" },
        },
        {
          provider = function(self)
            local count = self.status_dict.removed or 0
            return count > 0 and (" -" .. count) or ""
          end,
          hl = { fg = "git_del" },
        },
      }

      -- File
      local FileIcon = {
        init = function(self)
          local filename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(0), ":t")
          local extension = vim.fn.fnamemodify(filename, ":e")
          self.icon, self.icon_color = require("nvim-web-devicons").get_icon_color(filename, extension, { default = true })
        end,
        provider = function(self)
          return self.icon and (self.icon .. " ")
        end,
        hl = function(self)
          return { fg = self.icon_color }
        end,
      }

      local FileName = {
        provider = function()
          local filename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(0), ":t")
          return filename == "" and "[No Name]" or filename
        end,
        hl = { fg = "fg", bold = true },
      }

      local FileFlags = {
        {
          condition = function()
            return vim.bo.modified
          end,
          provider = " [+]",
          hl = { fg = "green" },
        },
        {
          condition = function()
            return not vim.bo.modifiable or vim.bo.readonly
          end,
          provider = " ",
          hl = { fg = "red" },
        },
      }

      local FileBlock = {
        { provider = "  " },
        FileIcon,
        FileName,
        FileFlags,
        { provider = " " },
      }

      -- Diagnostics
      local Diagnostics = {
        condition = conditions.has_diagnostics,
        init = function(self)
          self.errors = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })
          self.warnings = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })
          self.hints = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.HINT })
          self.info = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.INFO })
        end,
        update = { "DiagnosticChanged", "BufEnter" },
        {
          provider = function(self)
            return self.errors > 0 and (" " .. self.errors .. " ")
          end,
          hl = { fg = "diag_error" },
        },
        {
          provider = function(self)
            return self.warnings > 0 and (" " .. self.warnings .. " ")
          end,
          hl = { fg = "diag_warn" },
        },
        {
          provider = function(self)
            return self.hints > 0 and ("󰠠 " .. self.hints .. " ")
          end,
          hl = { fg = "diag_hint" },
        },
        {
          provider = function(self)
            return self.info > 0 and (" " .. self.info .. " ")
          end,
          hl = { fg = "diag_info" },
        },
      }

      -- LSP
      local LSP = {
        condition = conditions.lsp_attached,
        update = { "LspAttach", "LspDetach" },
        provider = function()
          local names = {}
          for _, server in pairs(vim.lsp.get_clients({ bufnr = 0 })) do
            table.insert(names, server.name)
          end
          return "  " .. table.concat(names, ", ") .. " "
        end,
        hl = { fg = "subtle" },
      }

      -- Filetype
      local FileType = {
        provider = function()
          return " " .. vim.bo.filetype .. " "
        end,
        hl = { fg = "blue" },
      }

      -- Ruler
      local Ruler = {
        provider = " %l:%c ",
        hl = { fg = "fg" },
      }

      -- Right bookend pill
      local RightPill = {
        { provider = "", hl = { fg = "surface", bg = "bg" } },
        {
          hl = { bg = "surface" },
          Diagnostics,
          LSP,
          FileType,
          Ruler,
        },
      }

      -- Inactive statusline
      local InactiveStatusline = {
        condition = conditions.is_not_active,
        hl = { fg = "subtle", bg = "bg", force = true },
        { provider = "  " },
        {
          provider = function()
            local filename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(0), ":t")
            return filename == "" and "[No Name]" or filename
          end,
        },
        Align,
      }

      -- Active statusline
      local ActiveStatusline = {
        hl = { bg = "bg" },
        ViMode,
        ModeSepRight,
        Git,
        FileBlock,
        Align,
        RightPill,
      }

      -- Final statusline with fallthrough
      local StatusLines = {
        fallthrough = false,
        InactiveStatusline,
        ActiveStatusline,
      }

      require("heirline").setup({
        statusline = StatusLines,
        opts = {
          colors = setup_colors(),
        },
      })
    end,
  },
  {
    "akinsho/bufferline.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    version = "*",
    config = function()
      require("bufferline").setup({
        options = {
          mode = "buffers",
          separator_style = "thin",
          always_show_bufferline = false,
          show_buffer_close_icons = false,
          show_close_icon = false,
          color_icons = true,
        },
      })
    end,
  },

  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
    end,
    config = function()
      require("which-key").setup({
        win = {
          border = "rounded",
        },
      })
    end,
  },
}
