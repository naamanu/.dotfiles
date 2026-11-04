-- Debugging through nvim-dap (Neovim has no built-in DAP client). Go talks
-- to Delve's own DAP server (`dlv dap`), so no nvim-dap-go. Emacs's dape uses
-- the same adapter with the same launch configurations.
--
--   F5 / <leader>dc  continue (starts a session: pick a configuration)
--   F10 F11 F12      step over / into / out
--   <leader>db       toggle breakpoint       <leader>dB  conditional
--   <leader>dt       debug the Go test under the cursor
--   <leader>dl       re-run last             <leader>dq  terminate
--   <leader>dr       REPL                    <leader>ds  scopes   <leader>dh hover

local dap = require("dap")
local widgets = require("dap.ui.widgets")

vim.fn.sign_define("DapBreakpoint", { text = "●", texthl = "DiagnosticError" })
vim.fn.sign_define("DapBreakpointCondition", { text = "◆", texthl = "DiagnosticWarn" })
vim.fn.sign_define("DapLogPoint", { text = "◉", texthl = "DiagnosticInfo" })
vim.fn.sign_define("DapStopped", { text = "▶", texthl = "DiagnosticOk", linehl = "Visual" })

dap.adapters.go = {
  type = "server",
  port = "${port}",
  executable = { command = "dlv", args = { "dap", "-l", "127.0.0.1:${port}" } },
  -- Delve compiles the program before it answers `initialize`.
  options = { initialize_timeout_sec = 30 },
}

-- The nearest enclosing `func TestX` / BenchmarkX / FuzzX above the cursor.
local function nearest_go_test()
  local pattern = [[^func \(Test\|Benchmark\|Fuzz\)\w*(]]
  local lnum = vim.fn.search(pattern, "bcnW")
  if lnum == 0 then
    return nil
  end
  return vim.fn.getline(lnum):match("^func ([%w_]+)%(")
end

local function test_args()
  local name = nearest_go_test()
  if not name then
    vim.notify("No Go test above the cursor; running the whole package", vim.log.levels.WARN)
    return {}
  end
  return { "-test.run", "^" .. name .. "$" }
end

dap.configurations.go = {
  { type = "go", name = "Debug package", request = "launch", program = "${fileDirname}" },
  { type = "go", name = "Debug file", request = "launch", program = "${file}" },
  {
    type = "go",
    name = "Debug package (with args)",
    request = "launch",
    program = "${fileDirname}",
    args = function()
      return vim.split(vim.fn.input("Args: "), " ", { trimempty = true })
    end,
  },
  {
    type = "go",
    name = "Debug test at cursor",
    request = "launch",
    mode = "test",
    program = "${fileDirname}",
    args = test_args,
  },
  { type = "go", name = "Debug package tests", request = "launch", mode = "test", program = "${fileDirname}" },
  {
    type = "go",
    name = "Attach to process",
    request = "attach",
    mode = "local",
    processId = require("dap.utils").pick_process,
  },
}

local map = function(lhs, rhs, desc)
  vim.keymap.set("n", lhs, rhs, { desc = "Debug: " .. desc })
end

map("<F5>", dap.continue, "Continue / start")
map("<F10>", dap.step_over, "Step over")
map("<F11>", dap.step_into, "Step into")
map("<F12>", dap.step_out, "Step out")
map("<leader>dc", dap.continue, "Continue / start")
map("<leader>dn", dap.step_over, "Step over")
map("<leader>di", dap.step_into, "Step into")
map("<leader>do", dap.step_out, "Step out")
map("<leader>db", dap.toggle_breakpoint, "Toggle breakpoint")
map("<leader>dB", function()
  dap.set_breakpoint(vim.fn.input("Condition: "))
end, "Conditional breakpoint")
map("<leader>dC", dap.run_to_cursor, "Run to cursor")
map("<leader>dl", dap.run_last, "Run last")
map("<leader>dq", dap.terminate, "Terminate")
map("<leader>dr", dap.repl.toggle, "Toggle REPL")
map("<leader>dh", widgets.hover, "Hover value")
map("<leader>ds", function()
  widgets.sidebar(widgets.scopes).toggle()
end, "Scopes")
map("<leader>dt", function()
  for _, config in ipairs(dap.configurations[vim.bo.filetype] or {}) do
    if config.name == "Debug test at cursor" then
      return dap.run(config)
    end
  end
  vim.notify("No test configuration for " .. vim.bo.filetype, vim.log.levels.WARN)
end, "Debug test at cursor")

return {}
