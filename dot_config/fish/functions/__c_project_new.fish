function __c_project_new --description "Create a C or C++ CMake starter project" --argument-names language helper
    set -l args $argv[3..-1]
    if test (count $args) -ne 1
        printf 'Usage: %s <project-name>\n' $helper >&2
        return 1
    end
    set -l name $args[1]
    if not string match -qr '^[A-Za-z][A-Za-z0-9_-]*$' -- "$name"
        echo "Project name must start with a letter and contain only letters, digits, underscores, or hyphens." >&2
        return 1
    end
    if test -e "$name"; or test -L "$name"
        printf 'Path already exists: %s\n' "$name" >&2
        return 1
    end

    set -l extension c
    set -l standard 17
    if test "$language" = CXX
        set extension cpp
        set standard 20
    end

    mkdir -- "$name"; or return 1
    mkdir -- "$name/src"; or return 1
    printf '%s\n' \
        'cmake_minimum_required(VERSION 3.16)' \
        "project($name LANGUAGES $language)" \
        '' \
        "set(CMAKE_"$language"_STANDARD $standard)" \
        "set(CMAKE_"$language"_STANDARD_REQUIRED ON)" \
        "set(CMAKE_"$language"_EXTENSIONS OFF)" \
        'set(CMAKE_EXPORT_COMPILE_COMMANDS ON)' \
        '' \
        "add_executable($name src/main.$extension)" >"$name/CMakeLists.txt"; or return 1
    if test "$language" = CXX
        # A CMake run target resolves the executable for every build configuration.
        printf '%s\n' \
            '' \
            "add_custom_target($name-run" \
            "    COMMAND \$<TARGET_FILE:$name>" \
            "    DEPENDS $name" \
            '    USES_TERMINAL' \
            '    VERBATIM)' >>"$name/CMakeLists.txt"; or return 1
        printf '%s\n' \
            '# Plain make builds and runs the app; make build only compiles it.' \
            '.PHONY: run build configure' \
            '' \
            'run: configure' \
            "	+cmake --build build --config Debug --target $name-run" \
            '' \
            'build: configure' \
            '	+cmake --build build --config Debug' \
            '' \
            'configure:' \
            '	cmake -S . -B build' >"$name/Makefile"; or return 1
        printf '%s\n' '#include <iostream>' '' 'int main() {' '    std::cout << "Hello, world!\n";' '    return 0;' '}' >"$name/src/main.cpp"; or return 1
    else
        printf '%s\n' '#include <stdio.h>' '' 'int main(void) {' '    puts("Hello, world!");' '    return 0;' '}' >"$name/src/main.c"; or return 1
    end
    printf '%s\n' /build/ >"$name/.gitignore"; or return 1
    printf '%s\n' 'CompileFlags:' '  CompilationDatabase: build' >"$name/.clangd"; or return 1
    cd -- "$name"; or return 1
    printf "Project '%s' ready!\n\n" "$name"
    if test "$language" = CXX
        printf '%s\n' '  make        # Build and run' '  make build  # Build only'
    end
    printf '%s\n' '  c-run' '  c-run arg1 arg2  # Pass arguments to your app'
end
