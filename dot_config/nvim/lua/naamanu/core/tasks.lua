local M = {}

local function start_dir(startpath)
  local path = startpath
  if not path or path == "" then
    path = vim.api.nvim_buf_get_name(0)
  end
  if not path or path == "" then
    path = vim.fn.getcwd()
  end

  if vim.fn.isdirectory(path) == 1 then
    return path
  end

  return vim.fs.dirname(path)
end

local function find_root(markers, startpath)
  local found = vim.fs.find(markers, {
    upward = true,
    path = start_dir(startpath),
    stop = vim.uv.os_homedir(),
  })[1]

  return found and vim.fs.dirname(found) or nil
end

local function find_root_with_glob(markers, globs, startpath)
  local root = find_root(markers, startpath)
  if root then
    return root
  end

  local dir = start_dir(startpath)
  local home = vim.uv.os_homedir()

  while dir and dir ~= "" do
    for _, glob in ipairs(globs) do
      if #vim.fn.globpath(dir, glob, false, true) > 0 then
        return dir
      end
    end

    if dir == home then
      break
    end

    local parent = vim.fs.dirname(dir)
    if not parent or parent == dir then
      break
    end
    dir = parent
  end

  return nil
end

local function exists(path)
  return vim.fn.filereadable(path) == 1 or vim.fn.isdirectory(path) == 1
end

local function read_json(path)
  if vim.fn.filereadable(path) == 0 then
    return nil
  end

  local ok, decoded = pcall(vim.json.decode, table.concat(vim.fn.readfile(path), "\n"))
  if not ok or type(decoded) ~= "table" then
    return nil
  end

  return decoded
end

local function shell_command(command)
  return { "sh", "-lc", command }
end

-- Build/test/watch commands run in a terminal at the bottom of the screen
-- (snacks.terminal), one per command and directory, toggled by re-running.
-- The output stays after the command exits; `q` closes it.
local function run(spec)
  Snacks.terminal.open(spec.cmd, {
    cwd = spec.cwd,
    auto_close = false,
    win = { position = "bottom", height = 0.3, title = spec.name },
  })
end

local function package_root(startpath)
  return find_root({ "package.json" }, startpath)
end

local function python_root(startpath)
  return find_root(
    { "pyproject.toml", "uv.lock", "requirements.txt", "setup.py", "setup.cfg", ".python-version" },
    startpath
  )
end

local function package_json(root)
  return root and read_json(root .. "/package.json") or nil
end

local function package_scripts(root)
  local json = package_json(root)
  return json and type(json.scripts) == "table" and json.scripts or {}
end

local function has_script(root, name)
  return type(package_scripts(root)[name]) == "string"
end

local function package_manager(root)
  if exists(root .. "/bun.lockb") or exists(root .. "/bun.lock") then
    return "bun"
  end
  if exists(root .. "/pnpm-lock.yaml") then
    return "pnpm"
  end
  if exists(root .. "/yarn.lock") then
    return "yarn"
  end

  return "npm"
end

local function package_command(manager, script)
  if manager == "yarn" then
    return { "yarn", script }
  end
  if manager == "bun" then
    return { "bun", "run", script }
  end

  return { manager, "run", script }
end

local function local_bin(binary, startpath)
  local dir = package_root(startpath) or start_dir(startpath)
  local home = vim.uv.os_homedir()

  while dir and dir ~= "" do
    local candidate = dir .. "/node_modules/.bin/" .. binary
    if vim.fn.executable(candidate) == 1 then
      return candidate
    end

    if dir == home then
      break
    end

    local parent = vim.fs.dirname(dir)
    if not parent or parent == dir then
      break
    end
    dir = parent
  end

  return nil
end

local function configured_node_tool(tool, startpath)
  local root = package_root(startpath)
  if not root then
    return nil
  end

  local json = package_json(root)
  local dependencies = {}
  if json then
    for _, section in ipairs({
      "dependencies",
      "devDependencies",
      "peerDependencies",
      "optionalDependencies",
    }) do
      vim.list_extend(dependencies, vim.tbl_keys(json[section] or {}))
    end
  end

  if vim.tbl_contains(dependencies, tool) or local_bin(tool, root) then
    return root
  end

  return nil
end

local function project_kind(startpath)
  local root = find_root({ "Cargo.toml" }, startpath)
  if root then
    return "rust", root
  end

  root = find_root({ "mvnw", "gradlew", "pom.xml", "build.gradle", "build.gradle.kts" }, startpath)
  if root then
    return "java", root
  end

  root = python_root(startpath)
  if root then
    return "python", root
  end

  root = find_root({ "dune-project", "dune-workspace", "dune" }, startpath)
  if root then
    return "ocaml", root
  end

  root = find_root_with_glob(
    { "stack.yaml", "cabal.project", "package.yaml" },
    { "*.cabal" },
    startpath
  )
  if root then
    return "haskell", root
  end

  root =
    find_root({ "CMakeLists.txt", "meson.build", "Makefile", "compile_commands.json" }, startpath)
  if root then
    return "cpp", root
  end

  root = package_root(startpath)
  if root then
    return "node", root
  end

  root = find_root_with_glob({ "info.rkt" }, { "*.rkt" }, startpath)
  if root then
    return "racket", root
  end

  return nil, nil
end

local function c_family_compiler(file)
  local ft = vim.filetype.match({ filename = file }) or vim.bo.filetype
  if ft == "c" then
    return vim.fn.exepath("clang") ~= "" and "clang" or "cc"
  end

  return vim.fn.exepath("clang++") ~= "" and "clang++" or "c++"
end

function M.find_package_root(startpath)
  return package_root(startpath)
end

function M.find_cpp_root(startpath)
  return find_root(
    { "CMakeLists.txt", "meson.build", "Makefile", "compile_commands.json" },
    startpath
  )
end

function M.find_ocaml_root(startpath)
  return find_root({ "dune-project", "dune-workspace", "dune" }, startpath)
end

function M.find_haskell_root(startpath)
  return find_root_with_glob(
    { "stack.yaml", "cabal.project", "package.yaml" },
    { "*.cabal" },
    startpath
  )
end

function M.find_rust_root(startpath)
  return find_root({ "Cargo.toml" }, startpath)
end

function M.find_python_root(startpath)
  return python_root(startpath)
end

function M.detect_package_manager(root)
  return root and package_manager(root) or "npm"
end

function M.local_package_binary(binary, startpath)
  return local_bin(binary, startpath), package_root(startpath)
end

function M.prettier_config_root(startpath)
  local root = find_root({
    ".prettierrc",
    ".prettierrc.json",
    ".prettierrc.json5",
    ".prettierrc.yml",
    ".prettierrc.yaml",
    ".prettierrc.js",
    ".prettierrc.cjs",
    ".prettierrc.mjs",
    ".prettierrc.toml",
    "prettier.config.js",
    "prettier.config.cjs",
    "prettier.config.mjs",
    "prettier.config.ts",
  }, startpath)

  if root then
    return root
  end

  local pkg_root = package_root(startpath)
  local json = package_json(pkg_root)
  return json and json.prettier and pkg_root or nil
end

function M.eslint_config_root(startpath)
  local root = find_root({
    "eslint.config.js",
    "eslint.config.cjs",
    "eslint.config.mjs",
    "eslint.config.ts",
    ".eslintrc",
    ".eslintrc.js",
    ".eslintrc.cjs",
    ".eslintrc.json",
  }, startpath)

  return root or configured_node_tool("eslint", startpath)
end

function M.package_script_command(script, startpath)
  local root = package_root(startpath)
  if not root or not has_script(root, script) then
    return nil, root
  end

  return package_command(package_manager(root), script), root
end

function M.has_package_script(script, startpath)
  local root = package_root(startpath)
  return root and has_script(root, script) or false
end

function M.python_executable(startpath)
  local root = python_root(startpath) or start_dir(startpath)
  for _, path in ipairs({
    root .. "/.venv/bin/python",
    root .. "/venv/bin/python",
  }) do
    if vim.fn.executable(path) == 1 then
      return path
    end
  end

  if vim.fn.exepath("python3") ~= "" then
    return vim.fn.exepath("python3")
  end
  if vim.fn.exepath("python") ~= "" then
    return vim.fn.exepath("python")
  end

  return "python3"
end

function M.python_command(args, startpath)
  local root = python_root(startpath)
  if
    root
    and vim.fn.executable("uv") == 1
    and (exists(root .. "/uv.lock") or exists(root .. "/pyproject.toml"))
  then
    return vim.list_extend({ "uv", "run", "python" }, args), root
  end

  return vim.list_extend({ M.python_executable(startpath) }, args), root
end

function M.python_runner_command(startpath)
  local cmd = M.python_command({}, startpath)
  return cmd
end

function M.get_package_scripts(startpath)
  local root = package_root(startpath)
  if not root then
    return nil, nil, {}
  end

  local scripts = {}
  for name, command in pairs(package_scripts(root)) do
    scripts[#scripts + 1] = { name = name, command = command }
  end
  table.sort(scripts, function(a, b)
    return a.name < b.name
  end)

  return root, package_manager(root), scripts
end

function M.run_package_script()
  local root, manager, scripts = M.get_package_scripts(vim.api.nvim_buf_get_name(0))
  if not root then
    vim.notify("No package.json found", vim.log.levels.WARN)
    return
  end
  if vim.tbl_isempty(scripts) then
    vim.notify("No package scripts found", vim.log.levels.WARN)
    return
  end

  vim.ui.select(scripts, {
    prompt = "Run package script",
    format_item = function(item)
      return string.format("%s: %s", item.name, item.command)
    end,
  }, function(choice)
    if not choice then
      return
    end

    run({
      name = string.format("%s: %s", manager, choice.name),
      cmd = package_command(manager, choice.name),
      cwd = root,
    })
  end)
end

function M.run_named_package_script(script)
  local cmd, root = M.package_script_command(script, vim.api.nvim_buf_get_name(0))
  if not cmd then
    vim.notify(string.format("No package script named '%s'", script), vim.log.levels.WARN)
    return
  end

  run({
    name = "package: " .. script,
    cmd = cmd,
    cwd = root,
  })
end

function M.run_python_file(startpath)
  local file = startpath or vim.api.nvim_buf_get_name(0)
  if file == "" then
    vim.notify("Current buffer has no file", vim.log.levels.WARN)
    return
  end

  local cmd, root = M.python_command({ file }, file)
  run({ name = "python: run file", cmd = cmd, cwd = root or start_dir(file) })
end

function M.run_python_tests(startpath)
  local file = startpath or vim.api.nvim_buf_get_name(0)
  local root = python_root(file) or start_dir(file)
  local args = { "-m", "pytest" }

  if
    vim.fn.filereadable(root .. "/pyproject.toml") == 0
    and vim.fn.isdirectory(root .. "/tests") == 0
    and file ~= ""
  then
    args[#args + 1] = file
  end

  local cmd = M.python_command(args, root)
  run({ name = "python: pytest", cmd = cmd, cwd = root })
end

local function java_command(root, testing)
  if exists(root .. "/pom.xml") or exists(root .. "/mvnw") then
    return { exists(root .. "/mvnw") and "./mvnw" or "mvn", testing and "test" or "package" }
  end
  return { exists(root .. "/gradlew") and "./gradlew" or "gradle", testing and "test" or "build" }
end

function M.run_project_build(startpath)
  local kind, root = project_kind(startpath or vim.api.nvim_buf_get_name(0))
  if not kind then
    vim.notify("No supported project root found", vim.log.levels.WARN)
    return
  end

  local commands = {
    java = java_command(root, false),
    rust = { "cargo", "build" },
    python = exists(root .. "/pyproject.toml") and M.python_command({ "-m", "build" }, root) or nil,
    ocaml = { "dune", "build" },
    racket = { "raco", "make", "-v", "." },
    haskell = exists(root .. "/stack.yaml") and { "stack", "build" } or { "cabal", "build", "all" },
    node = has_script(root, "build") and package_command(package_manager(root), "build") or nil,
    cpp = exists(root .. "/CMakeLists.txt") and shell_command(
      "cmake -S . -B build && cmake --build build"
    ) or exists(root .. "/meson.build") and shell_command(
      "meson setup build --reconfigure && meson compile -C build"
    ) or exists(root .. "/Makefile") and { "make" } or nil,
  }

  local cmd = commands[kind]
  if not cmd then
    vim.notify("No build command found for " .. kind, vim.log.levels.WARN)
    return
  end

  run({ name = kind .. ": build", cmd = cmd, cwd = root })
end

function M.run_project_tests(startpath)
  local kind, root = project_kind(startpath or vim.api.nvim_buf_get_name(0))
  if not kind then
    vim.notify("No supported project root found", vim.log.levels.WARN)
    return
  end

  local commands = {
    java = java_command(root, true),
    rust = { "cargo", "test" },
    python = M.python_command({ "-m", "pytest" }, root),
    ocaml = { "dune", "runtest" },
    racket = { "raco", "test", "." },
    haskell = exists(root .. "/stack.yaml") and { "stack", "test" } or { "cabal", "test", "all" },
    node = has_script(root, "test") and package_command(package_manager(root), "test") or nil,
    cpp = exists(root .. "/Makefile") and { "make", "test" } or nil,
  }

  local cmd = commands[kind]
  if not cmd then
    vim.notify("No test command found for " .. kind, vim.log.levels.WARN)
    return
  end

  run({ name = kind .. ": test", cmd = cmd, cwd = root })
end

function M.compile_c_family_file(startpath)
  local file = startpath or vim.api.nvim_buf_get_name(0)
  if file == "" then
    vim.notify("Current buffer has no file", vim.log.levels.WARN)
    return
  end

  local root = M.find_cpp_root(file) or start_dir(file)
  local output = vim.fn.fnamemodify(file, ":r")
  local cmd = { c_family_compiler(file), "-Wall", "-Wextra", "-g", file, "-o", output }
  if (vim.filetype.match({ filename = file }) or vim.bo.filetype) ~= "c" then
    table.insert(cmd, 2, "-std=c++20")
  end

  run({ name = "c/c++: compile current file", cmd = cmd, cwd = root })
end

-- REPL (Emacs: fp-repl for OCaml/Haskell/Racket/SML, plus ipython for Python).
-- One snacks terminal per filetype and project root, toggled with
-- <localleader>r; <localleader>e sends the line or selection and <localleader>b
-- the buffer through nvim_chan_send. No plugin; keys are in core/keymaps.lua.
local function repl_command(ft, startpath)
  if ft == "java" then
    return "jshell", start_dir(startpath)
  elseif ft == "ocaml" then
    local root = find_root({ "dune-project", "dune-workspace" }, startpath)
    if root then
      return "dune utop", root
    end
    return "utop", start_dir(startpath)
  elseif ft == "haskell" or ft == "lhaskell" then
    local root = find_root_with_glob(
      { "stack.yaml", "cabal.project", "package.yaml" },
      { "*.cabal" },
      startpath
    )
    if root and #vim.fn.globpath(root, "*.cabal", false, true) > 0 then
      return "cabal repl", root
    elseif root and exists(root .. "/stack.yaml") then
      return "stack ghci", root
    end
    return "ghci", root or start_dir(startpath)
  elseif ft == "racket" or ft == "scheme" then
    return "racket -il readline",
      find_root({ "info.rkt", ".git" }, startpath) or start_dir(startpath)
  elseif ft == "sml" then
    return "sml", find_root({ "millet.toml", ".git" }, startpath) or start_dir(startpath)
  elseif ft == "python" then
    local root = python_root(startpath)
    if root and exists(root .. "/pyproject.toml") then
      return "uv run ipython", root
    end
    return "ipython", root or start_dir(startpath)
  end
  return nil
end

local function repl_spec()
  local ft = vim.bo.filetype
  local cmd, cwd = repl_command(ft, vim.api.nvim_buf_get_name(0))
  if not cmd then
    vim.notify("No REPL configured for filetype '" .. ft .. "'", vim.log.levels.WARN)
    return nil
  end
  return {
    cmd = cmd,
    ft = ft,
    opts = { cwd = cwd, win = { position = "right", width = 0.4 } },
  }
end

--- Toggle the REPL for the current buffer's filetype.
function M.repl()
  local spec = repl_spec()
  if spec then
    Snacks.terminal.toggle(spec.cmd, spec.opts)
  end
end

local function repl_send(lines)
  local spec = repl_spec()
  if not spec then
    return
  end
  local current = vim.api.nvim_get_current_win()
  local term = Snacks.terminal.get(spec.cmd, spec.opts)
  if not term then
    return
  end
  if not term:valid() then
    term:show()
  end
  local text = table.concat(lines, "\n")
  if spec.ft == "python" then
    -- Bracketed paste keeps an indented block together in IPython.
    text = "\27[200~" .. text .. "\27[201~"
  elseif (spec.ft == "haskell" or spec.ft == "lhaskell") and #lines > 1 then
    text = ":{\n" .. text .. "\n:}"
  elseif spec.ft == "ocaml" and not vim.trim(text):match(";;$") then
    -- utop evaluates a phrase only at `;;`; source files rarely carry them.
    text = text .. " ;;"
  end
  vim.api.nvim_chan_send(vim.bo[term.buf].channel, text .. "\n")
  if vim.api.nvim_win_is_valid(current) then
    vim.api.nvim_set_current_win(current)
  end
end

--- Send the current line.
function M.repl_send_line()
  repl_send({ vim.api.nvim_get_current_line() })
end

--- Send the visual selection (whole lines), then leave visual mode.
function M.repl_send_selection()
  local first = vim.fn.line("v")
  local last = vim.fn.line(".")
  if first > last then
    first, last = last, first
  end
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "n", false)
  repl_send(vim.api.nvim_buf_get_lines(0, first - 1, last, false))
end

--- Send the whole buffer.
function M.repl_send_buffer()
  repl_send(vim.api.nvim_buf_get_lines(0, 0, -1, false))
end

--- Send the top-level form under the cursor: the outermost treesitter node
--- below the root (an OCaml `let`, a Haskell equation or signature, a Racket
--- s-expression, an SML declaration). Falls back to the paragraph.
function M.repl_send_form()
  local lines
  local ok, node = pcall(vim.treesitter.get_node, { ignore_injections = true })
  if ok and node then
    while node:parent() and node:parent():parent() do
      node = node:parent()
    end
    local first, _, last, last_col = node:range()
    -- A node ending at column 0 stops before its last line.
    if last_col == 0 and last > first then
      last = last - 1
    end
    lines = vim.api.nvim_buf_get_lines(0, first, last + 1, false)
  else
    lines = vim.api.nvim_buf_get_lines(0, vim.fn.line("'{") - 1, vim.fn.line("'}"), false)
  end
  -- Paragraph bounds, and some grammars, include the surrounding blank lines.
  while #lines > 1 and vim.trim(lines[#lines]) == "" do
    table.remove(lines)
  end
  while #lines > 1 and vim.trim(lines[1]) == "" do
    table.remove(lines, 1)
  end
  repl_send(lines)
end

--- Rebuild (or re-test) on every save, in a terminal: dune's own
--- watcher for OCaml, ghcid for Haskell, watchexec for everything else.
function M.run_watch(tests)
  local kind, root = project_kind(vim.api.nvim_buf_get_name(0))
  if not kind then
    vim.notify("No supported project root found", vim.log.levels.WARN)
    return
  end

  local function watchexec(exts, cmd)
    if vim.fn.executable("watchexec") == 0 then
      return nil
    end
    return vim.list_extend({ "watchexec", "--clear", "--exts", exts, "--" }, cmd)
  end

  local cmd
  if kind == "ocaml" then
    cmd = tests and { "dune", "build", "@runtest", "--watch" } or { "dune", "build", "--watch" }
  elseif kind == "haskell" then
    if vim.fn.executable("ghcid") == 1 then
      cmd = { "ghcid", "--command", "cabal repl", tests and "--test=:main" or nil }
    else
      cmd =
        watchexec("hs,cabal", tests and { "cabal", "test", "all" } or { "cabal", "build", "all" })
    end
  elseif kind == "rust" then
    cmd = watchexec("rs,toml", tests and { "cargo", "test" } or { "cargo", "check" })
  elseif kind == "racket" then
    cmd = watchexec("rkt", { "raco", tests and "test" or "make", "." })
  elseif kind == "python" then
    cmd = watchexec("py", M.python_command({ "-m", "pytest", "-x", "-q" }, root))
  end

  if not cmd then
    vim.notify("No watch command for " .. kind .. " (install watchexec)", vim.log.levels.WARN)
    return
  end
  run({ name = kind .. (tests and ": watch tests" or ": watch"), cmd = cmd, cwd = root })
end

return M
