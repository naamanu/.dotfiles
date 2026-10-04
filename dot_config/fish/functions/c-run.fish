function c-run --description "Build and run a c-new or cpp-new CMake project"
    if not test -f CMakeLists.txt
        echo "Run c-run from the project folder containing CMakeLists.txt." >&2
        return 1
    end
    command cmake -S . -B build; or return $status
    command cmake --build build --config Debug; or return $status

    # The starter's executable has the CMake project name, even after a folder rename.
    set -l name (string match -r -- '^CMAKE_PROJECT_NAME:STATIC=(.+)$' < build/CMakeCache.txt)[2]
    if test -z "$name"
        echo "Could not determine the CMake project name." >&2
        return 1
    end
    set -l executable "./build/$name"
    if not test -x "$executable"
        set executable "./build/Debug/$name"
    end
    if not test -f "$executable"; or not test -x "$executable"
        printf 'No executable named %s found. c-run expects the layout created by c-new or cpp-new.\n' "$name" >&2
        return 1
    end
    "$executable" $argv
end
