local js_like_filetypes = {
	javascript = true,
	javascriptreact = true,
	typescript = true,
	typescriptreact = true,
	vue = true,
}

return {
	"stevearc/conform.nvim",
	event = { "BufWritePre" },
	cmd = { "ConformInfo" },
	keys = {
		{
			"<leader>cf",
			function()
				require("conform").format({ async = true, lsp_format = "never" })
			end,
			mode = { "n", "v" },
			desc = "Format buffer",
		},
	},
	opts = {
		formatters_by_ft = {
			c = { "clang-format" },
			cpp = { "clang-format" },
			go = { "goimports", "gofmt" },
			rust = { "rustfmt" },
			ocaml = { "ocamlformat" },
			-- Sorting imports and formatting are idempotent; ruff_fix is not --
			-- on save it deleted an import you had just typed but not yet used.
			-- Fixes stay behind <leader>cF (ruff LSP source.fixAll).
			python = { "ruff_organize_imports", "ruff_format" },
			haskell = { "ormolu" },
			lua = { "stylua" },
			sh = { "shfmt" },
			bash = { "shfmt" },
			sql = { "sql_formatter" },
			json = { "prettier" },
			jsonc = { "prettier" },
			yaml = { "prettier" },
			markdown = { "prettier" },
			typescript = { "prettier" },
			typescriptreact = { "prettier" },
			javascript = { "prettier" },
			javascriptreact = { "prettier" },
			vue = { "prettier" },
			html = { "prettier" },
			css = { "prettier" },
			scss = { "prettier" },
		},
		format_on_save = function(bufnr)
			return {
				timeout_ms = 3000,
				lsp_format = js_like_filetypes[vim.bo[bufnr].filetype] and "never" or "fallback",
			}
		end,
		formatters = {
			prettier = {
				command = function(_, ctx)
					local tasks = require("naamanu.core.tasks")
					return tasks.local_package_binary("prettier", ctx.filename) or "prettier"
				end,
			},
			ocamlformat = {
				prepend_args = function(_, ctx)
					if ctx.filename and ctx.filename:match("%.mli$") then
						return { "--intf" }
					end
					return { "--impl" }
				end,
			},
		},
	},
}
