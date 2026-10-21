# Java 21 is the default; direnv/project toolchains can override JAVA_HOME.
if not set -q JAVA_HOME
    for jdk in /opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home /usr/local/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home
        if test -d "$jdk"
            set -gx JAVA_HOME "$jdk"
            break
        end
    end
end
if set -q JAVA_HOME
    fish_add_path -g "$JAVA_HOME/bin"
end
