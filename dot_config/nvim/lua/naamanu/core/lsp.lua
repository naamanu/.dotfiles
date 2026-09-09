-- Native LSP configuration via vim.lsp.config/enable (no nvim-lspconfig).
-- Required from core/init.lua after lazy.nvim is set up, so requiring
-- lazy-loaded plugin modules (schemastore) works here.
--
-- NOTE: this is the native API — nvim-lspconfig concepts like
-- `on_new_config` and `single_file_support` do not exist and are silently
-- ignored; the native equivalent of single-file support is the default
-- `workspace_required = false`.

local lsp = vim.lsp
local function shared_cmd(binary, extra_args)
	local executable = vim.fn.exepath(binary)
	if executable == "" then
		return nil
	end
	return vim.list_extend({ executable }, extra_args or {})
end

local function apply_code_action(kind)
	vim.lsp.buf.code_action({
		apply = true,
		context = {
			only = { kind },
			diagnostics = {},
		},
	})
end

-- Diagnostic config
vim.diagnostic.config({
	virtual_text = { spacing = 4, prefix = "●" },
	signs = true,
	underline = true,
	update_in_insert = false,
	severity_sort = true,
	float = { border = "rounded" },
})

-- Shared PATH language servers
lsp.config("clangd", {
	cmd = shared_cmd("clangd", { "--background-index", "--clang-tidy" }),
	filetypes = { "c", "cpp", "objc", "objcpp", "cuda", "proto" },
	root_markers = {
		"compile_commands.json",
		"compile_flags.txt",
		"CMakeLists.txt",
		"Makefile",
		"meson.build",
		".git",
	},
})

lsp.config("rust_analyzer", {
	cmd = shared_cmd("rust-analyzer"),
	filetypes = { "rust" },
	settings = {
		["rust-analyzer"] = {
			checkOnSave = true,
			check = { command = "clippy" },
			cargo = { allFeatures = true },
			inlayHints = {
				bindingModeHints = { enable = true },
				closureReturnTypeHints = { enable = "always" },
				lifetimeElisionHints = { enable = "always" },
			},
		},
	},
})

lsp.config("lua_ls", {
	cmd = shared_cmd("lua-language-server"),
	filetypes = { "lua" },
	settings = {
		Lua = {
			runtime = { version = "LuaJIT" },
			workspace = { checkThirdParty = false },
			telemetry = { enable = false },
			hint = { enable = true },
		},
	},
})

-- Python interpreter resolution.
--
-- basedpyright only auto-detects a `.venv` relative to its own process cwd,
-- and Neovim launches servers with `cmd_cwd` defaulting to Neovim's cwd --
-- "not related to root_dir" (:h vim.lsp.ClientConfig).  Start Neovim anywhere
-- but the project root and every third-party import (fastapi, django, ...)
-- goes unresolved.  Resolve the interpreter here and hand it to the server as
-- `python.pythonPath` so the result no longer depends on where Neovim started.
local VENV_DIRS = { ".venv", "venv", ".env" }

local function venv_python(dir)
	for _, name in ipairs(VENV_DIRS) do
		local candidate = dir .. "/" .. name .. "/bin/python"
		if vim.fn.executable(candidate) == 1 then
			return candidate
		end
	end
	return nil
end

-- Search the workspace root and its ancestors, stopping at $HOME: uv
-- workspaces and monorepos keep a single venv above the directory that owns
-- pyproject.toml, so the nearest one upward is the right one.
local function python_path(root)
	local home = vim.env.HOME
	local dir = root
	while dir and dir ~= "" and dir ~= home and dir ~= "/" do
		local found = venv_python(dir)
		if found then
			return found
		end
		local parent = vim.fs.dirname(dir)
		if parent == dir then
			break
		end
		dir = parent
	end

	-- A venv activated in the shell before Neovim started.
	local active = vim.env.VIRTUAL_ENV
	if active and vim.fn.executable(active .. "/bin/python") == 1 then
		return active .. "/bin/python"
	end

	local fallback = vim.fn.exepath("python3")
	return fallback ~= "" and fallback or nil
end

lsp.config("ruff", {
	cmd = shared_cmd("ruff", { "server" }),
	filetypes = { "python" },
	root_markers = {
		{ "pyproject.toml", "ruff.toml", ".ruff.toml", "uv.lock", "requirements.txt", "manage.py" },
		".git",
	},
})

-- `:PythonEnv` answers "why is this import unresolved?" in one place: an
-- interpreter outside the project (or a `.venv` that was never populated) is
-- the usual cause.
vim.api.nvim_create_user_command("PythonEnv", function()
	local clients = vim.lsp.get_clients({ bufnr = 0, name = "basedpyright" })
	local client = clients[1]
	local root = client and client.root_dir or vim.fn.getcwd()
	local interpreter = client and vim.tbl_get(client.settings or {}, "python", "pythonPath") or python_path(root)

	local lines = {
		"root_dir    : " .. tostring(root),
		"interpreter : " .. tostring(interpreter),
		"attached    : " .. (client and "basedpyright" or "no basedpyright client"),
	}

	if interpreter and vim.fn.executable(interpreter) == 1 then
		local version = vim.system({ interpreter, "--version" }, { text = true }):wait()
		lines[#lines + 1] = "version     : " .. vim.trim(version.stdout .. version.stderr)
		local inside = vim.startswith(interpreter, root .. "/")
		lines[#lines + 1] = "in project  : " .. (inside and "yes" or "NO -- project deps will not resolve")
	else
		lines[#lines + 1] = "version     : interpreter not executable"
	end

	vim.notify(table.concat(lines, "\n"), vim.log.levels.INFO, { title = "Python environment" })
end, { desc = "Report the Python interpreter the LSP is using" })

-- basedpyright matches the Emacs setup, so both editors report the same
-- Python diagnostics.  ruff owns imports and formatting.
lsp.config("basedpyright", {
	cmd = shared_cmd("basedpyright-langserver", { "--stdio" }),
	filetypes = { "python" },
	-- Nested = equal priority, so the *nearest* Python marker wins rather than
	-- a distant pyproject.toml outranking the manage.py next to the buffer.
	root_markers = {
		{
			"pyproject.toml",
			"setup.py",
			"setup.cfg",
			"requirements.txt",
			"uv.lock",
			"Pipfile",
			"manage.py",
		},
		".git",
	},
	on_init = function(client)
		local interpreter = python_path(client.root_dir)
		if not interpreter then
			return
		end
		-- Deep-copy first: `vim.lsp.config` hands the same resolved `settings`
		-- table to every client, and two Python projects open at once each
		-- need their own interpreter.
		client.settings = vim.tbl_deep_extend("force", vim.deepcopy(client.settings or {}), {
			python = { pythonPath = interpreter },
		})
		-- Nvim's automatic didChangeConfiguration fires just before on_init,
		-- so the interpreter has to be pushed again here.  basedpyright also
		-- pulls it back via workspace/configuration, which reads the same
		-- per-client table.
		client:notify("workspace/didChangeConfiguration", { settings = client.settings })
	end,
	settings = {
		basedpyright = {
			disableOrganizeImports = true,
			analysis = {
				autoImportCompletions = true,
				diagnosticMode = "openFilesOnly",
			},
		},
	},
})

lsp.config("ocamllsp", {
	cmd = shared_cmd("ocamllsp"),
	filetypes = {
		"ocaml",
		"ocaml.interface",
		"ocaml.menhir",
		"ocaml.cram",
		"ocaml.ocamllex",
		"ocaml.ocamlyacc",
		"reason",
	},
	root_markers = { "dune-project", "dune-workspace", "dune", ".git" },
})

lsp.config("gopls", {
	cmd = shared_cmd("gopls"),
	filetypes = { "go", "gomod", "gowork", "gotmpl" },
	root_markers = { "go.work", "go.mod", ".git" },
	settings = { gopls = { gofumpt = true, staticcheck = true, usePlaceholders = true } },
})

lsp.config("bashls", {
	cmd = shared_cmd("bash-language-server", { "start" }),
	filetypes = { "bash", "sh" },
	root_markers = { ".git" },
})

lsp.config("dockerls", {
	cmd = shared_cmd("docker-langserver", { "--stdio" }),
	filetypes = { "dockerfile" },
	root_markers = { "Dockerfile", ".git" },
})

lsp.config("hls", {
	cmd = shared_cmd("haskell-language-server-wrapper"),
	filetypes = { "haskell", "lhaskell", "cabal" },
	root_markers = { "hie.yaml", "stack.yaml", "cabal.project", "package.yaml", ".git" },
	settings = {
		haskell = {
			formattingProvider = "ormolu",
		},
	},
})

lsp.config("vtsls", {
	cmd = shared_cmd("vtsls", { "--stdio" }),
	filetypes = {
		"javascript",
		"javascriptreact",
		"javascript.jsx",
		"typescript",
		"typescriptreact",
		"typescript.tsx",
	},
	root_markers = { "tsconfig.json", "jsconfig.json", "package.json", ".git" },
	settings = {
		vtsls = {
			-- Use the project's own typescript from node_modules when present,
			-- so editor diagnostics agree with `tsc`.
			autoUseWorkspaceTsdk = true,
			experimental = {
				completion = { enableServerSideFuzzyMatch = true },
			},
			tsserver = { globalPlugins = {} },
		},
		typescript = {
			updateImportsOnFileMove = { enabled = "always" },
			inlayHints = {
				parameterNames = { enabled = "all", suppressWhenArgumentMatchesName = false },
				parameterTypes = { enabled = true },
				variableTypes = { enabled = true },
				propertyDeclarationTypes = { enabled = true },
				functionLikeReturnTypes = { enabled = true },
				enumMemberValues = { enabled = true },
			},
		},
		javascript = {
			updateImportsOnFileMove = { enabled = "always" },
			inlayHints = {
				parameterNames = { enabled = "all", suppressWhenArgumentMatchesName = false },
				parameterTypes = { enabled = true },
				variableTypes = { enabled = true },
				propertyDeclarationTypes = { enabled = true },
				functionLikeReturnTypes = { enabled = true },
				enumMemberValues = { enabled = true },
			},
		},
	},
})

lsp.config("eslint", {
	cmd = shared_cmd("vscode-eslint-language-server", { "--stdio" }),
	filetypes = {
		"javascript",
		"javascriptreact",
		"javascript.jsx",
		"typescript",
		"typescriptreact",
		"typescript.tsx",
		"vue",
	},
	root_markers = {
		"eslint.config.js",
		"eslint.config.cjs",
		"eslint.config.mjs",
		"eslint.config.ts",
		".eslintrc",
		".eslintrc.js",
		".eslintrc.cjs",
		".eslintrc.json",
	},
	settings = {
		run = "onSave",
		quiet = true,
		format = false,
		codeActionOnSave = {
			enable = false,
			mode = "all",
		},
		workingDirectory = { mode = "auto" },
	},
})

lsp.config("tailwindcss", {
	cmd = shared_cmd("tailwindcss-language-server", { "--stdio" }),
	filetypes = {
		"html",
		"css",
		"scss",
		"javascript",
		"javascriptreact",
		"typescript",
		"typescriptreact",
		"vue",
	},
})

lsp.config("cssls", {
	cmd = shared_cmd("vscode-css-language-server", { "--stdio" }),
	filetypes = { "css", "scss", "less" },
	init_options = { provideFormatter = false },
})

lsp.config("jsonls", {
	cmd = shared_cmd("vscode-json-language-server", { "--stdio" }),
	filetypes = { "json", "jsonc" },
	init_options = { provideFormatter = false },
	settings = {
		json = {
			schemas = require("schemastore").json.schemas(),
			validate = { enable = true },
		},
	},
})

lsp.config("yamlls", {
	cmd = shared_cmd("yaml-language-server", { "--stdio" }),
	filetypes = { "yaml", "yaml.docker-compose", "yaml.gitlab" },
	settings = {
		yaml = {
			schemaStore = { enable = false, url = "" },
			schemas = require("schemastore").yaml.schemas(),
			validate = { enable = true },
		},
	},
})

-- Standard ML (millet) and Racket (racket-langserver, installed with
-- `raco pkg install racket-langserver`; Emacs uses racket-mode's own back end).
lsp.config("millet", {
	cmd = shared_cmd("millet-ls"),
	filetypes = { "sml" },
	root_markers = { "millet.toml", ".git" },
})

lsp.config("racket_langserver", {
	cmd = shared_cmd("racket", { "--lib", "racket-langserver" }),
	filetypes = { "racket", "scheme" },
	root_markers = { "info.rkt", ".git" },
})

-- Enable servers whose binary is present on the shared PATH, so a
-- missing tool degrades silently instead of erroring.
local shared_servers = {
	{ name = "clangd", binary = "clangd" },
	{ name = "rust_analyzer", binary = "rust-analyzer" },
	{ name = "gopls", binary = "gopls" },
	{ name = "bashls", binary = "bash-language-server" },
	{ name = "dockerls", binary = "docker-langserver" },
	{ name = "lua_ls", binary = "lua-language-server" },
	{ name = "ruff", binary = "ruff" },
	{ name = "basedpyright", binary = "basedpyright-langserver" },
	{ name = "ocamllsp", binary = "ocamllsp" },
	{ name = "hls", binary = "haskell-language-server-wrapper" },
	{ name = "vtsls", binary = "vtsls" },
	{ name = "eslint", binary = "vscode-eslint-language-server" },
	{ name = "tailwindcss", binary = "tailwindcss-language-server" },
	{ name = "cssls", binary = "vscode-css-language-server" },
	{ name = "jsonls", binary = "vscode-json-language-server" },
	{ name = "yamlls", binary = "yaml-language-server" },
	{ name = "millet", binary = "millet-ls" },
	{ name = "racket_langserver", binary = "racket" },
}

for _, server in ipairs(shared_servers) do
	if vim.fn.executable(server.binary) == 1 then
		lsp.enable(server.name)
	end
end

-- LspAttach keybindings
vim.api.nvim_create_autocmd("LspAttach", {
	group = vim.api.nvim_create_augroup("lsp-attach", { clear = true }),
	callback = function(event)
		local map = function(keys, func, desc)
			vim.keymap.set("n", keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
		end

		map("gd", vim.lsp.buf.definition, "Go to definition")
		map("gD", vim.lsp.buf.declaration, "Go to declaration")
		map("gr", vim.lsp.buf.references, "Go to references")
		map("gi", vim.lsp.buf.implementation, "Go to implementation")
		map("gt", vim.lsp.buf.type_definition, "Go to type definition")
		map("K", vim.lsp.buf.hover, "Hover documentation")
		map("<leader>la", vim.lsp.buf.code_action, "Code action")
		vim.keymap.set("n", "<leader>lr", function()
			return ":IncRename " .. vim.fn.expand("<cword>")
		end, { buffer = event.buf, expr = true, desc = "LSP: Rename (live preview)" })
		map("<leader>ld", vim.diagnostic.open_float, "Line diagnostics")
		map("<leader>ls", vim.lsp.buf.signature_help, "Signature help")
		map("<leader>ci", function()
			apply_code_action("source.organizeImports")
		end, "Organize imports")
		map("<leader>cI", function()
			apply_code_action("source.addMissingImports")
		end, "Add missing imports")
		map("<leader>cu", function()
			apply_code_action("source.removeUnused")
		end, "Remove unused imports")
		map("<leader>cF", function()
			apply_code_action("source.fixAll")
		end, "Fix all auto-fixable issues")
		map("<leader>cE", function()
			apply_code_action("source.fixAll.eslint")
		end, "Fix ESLint issues")
		map("<leader>lR", function()
			local clients = vim.lsp.get_clients({ bufnr = event.buf })
			for _, attached_client in ipairs(clients) do
				attached_client:stop(true)
			end
			vim.defer_fn(function()
				vim.cmd("edit")
			end, 100)
		end, "Restart LSP clients")

		-- Inlay hints (Neovim 0.10+)
		local client = vim.lsp.get_client_by_id(event.data.client_id)
		if client and client:supports_method("textDocument/inlayHint", event.buf) then
			vim.lsp.inlay_hint.enable(true, { bufnr = event.buf })
			map("<leader>lh", function()
				vim.lsp.inlay_hint.enable(
					not vim.lsp.inlay_hint.is_enabled({ bufnr = event.buf }),
					{ bufnr = event.buf }
				)
			end, "Toggle inlay hints")
		end

		-- Code lens
		if client and client:supports_method("textDocument/codeLens", event.buf) then
			map("<leader>lc", vim.lsp.codelens.run, "Run code lens")
			map("<leader>lC", function()
				vim.lsp.codelens.enable(true, { bufnr = event.buf })
			end, "Enable code lens")
			vim.lsp.codelens.enable(true, { bufnr = event.buf })
		end

		if client and client.name == "ruff" then
			client.server_capabilities.hoverProvider = false
		end
	end,
})
