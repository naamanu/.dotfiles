-- Eclipse JDT LS from the system (Homebrew's `jdtls`, setup.sh), not mason:
-- current releases need a newer JDK to *run* (25+) than the JDK 21 projects
-- build with. The server runs on Homebrew's newest `openjdk`; code is checked
-- against the shell's $JAVA_HOME (JDK 21) as the default runtime.
local server_jdk = "/opt/homebrew/opt/openjdk/libexec/openjdk.jdk/Contents/Home"
local project_jdk = vim.env.JAVA_HOME

local config = {}
if vim.uv.fs_stat(server_jdk) then
  config.cmd_env = { JAVA_HOME = server_jdk }
end
if project_jdk and project_jdk ~= "" then
  config.settings = {
    java = {
      configuration = {
        runtimes = { { name = "JavaSE-21", path = project_jdk, default = true } },
      },
    },
  }
end
return config
