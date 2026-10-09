function c-new-make --description "Bootstrap a new multi-file C project with Make"
    set -l name $argv[1]

    if test -z "$name"
        echo "Usage: c-new-make <project-name>"
        return 1
    end

    if not string match -qr '^[A-Za-z][A-Za-z0-9_-]*$' -- "$name"
        echo "Project name must start with a letter and contain only letters, numbers, '-' or '_'."
        return 1
    end

    if test -e "$name"
        echo "Cannot create '$name': the path already exists."
        return 1
    end

    set -l module_name (string replace -a '-' '_' -- "$name")
    set -l symbol (string join "" "$module_name" "_message")
    set -l header_guard (string upper "$module_name"_H)

    command mkdir -p "$name/src" "$name/include" "$name/lib"
    or return 1

    printf '%s\n' \
        '#include <stdio.h>' \
        "#include \"$module_name.h\"" \
        '' \
        'int main(void)' \
        '{' \
        "    printf(\"%s\\n\", $symbol());" \
        '    return 0;' \
        '}' > "$name/src/main.c"

    printf '%s\n' \
        "#ifndef $header_guard" \
        "#define $header_guard" \
        '' \
        "const char *$symbol(void);" \
        '' \
        "#endif /* $header_guard */" > "$name/include/$module_name.h"

    printf '%s\n' \
        "#include \"$module_name.h\"" \
        '' \
        "const char *$symbol(void)" \
        '{' \
        "    return \"Hello from $name!\";" \
        '}' > "$name/lib/$module_name.c"

    printf '%s\n' \
        'CC       ?= cc' \
        'AR       ?= ar' \
        'CFLAGS   ?= -std=c17 -Wall -Wextra -Wpedantic -Werror -O2' \
        'CPPFLAGS ?= -Iinclude' \
        'LDFLAGS  ?=' \
        'LDLIBS   ?=' \
        '' \
        "TARGET    := bin/$name" \
        "LIBRARY   := build/lib$module_name.a" \
        'APP_SRC   := $(wildcard src/*.c)' \
        'LIB_SRC   := $(wildcard lib/*.c)' \
        'APP_OBJ   := $(patsubst src/%.c,build/src/%.o,$(APP_SRC))' \
        'LIB_OBJ   := $(patsubst lib/%.c,build/lib/%.o,$(LIB_SRC))' \
        'DEPS      := $(APP_OBJ:.o=.d) $(LIB_OBJ:.o=.d)' \
        '' \
        '.PHONY: all run debug clean' \
        '' \
        'all: $(TARGET)' \
        '' \
        '$(TARGET): $(APP_OBJ) $(LIBRARY) | bin' \
        '	$(CC) $(LDFLAGS) $(APP_OBJ) $(LIBRARY) $(LDLIBS) -o $@' \
        '' \
        '$(LIBRARY): $(LIB_OBJ) | build' \
        '	$(AR) rcs $@ $^' \
        '' \
        'build/src/%.o: src/%.c' \
        '	@mkdir -p $(@D)' \
        '	$(CC) $(CPPFLAGS) $(CFLAGS) -MMD -MP -c $< -o $@' \
        '' \
        'build/lib/%.o: lib/%.c' \
        '	@mkdir -p $(@D)' \
        '	$(CC) $(CPPFLAGS) $(CFLAGS) -MMD -MP -c $< -o $@' \
        '' \
        'bin build:' \
        '	mkdir -p $@' \
        '' \
        'run: $(TARGET)' \
        '	./$(TARGET)' \
        '' \
        'debug: CFLAGS := -std=c17 -Wall -Wextra -Wpedantic -Werror -O0 -g3' \
        'debug: clean all' \
        '' \
        'clean:' \
        '	rm -rf build bin' \
        '' \
        '-include $(DEPS)' > "$name/Makefile"

    printf '%s\n' \
        'build/' \
        'bin/' \
        '*.o' \
        '*.a' > "$name/.gitignore"

    cd "$name"
    and echo "C project '$name' ready! Run 'make run' to build and start it."
end
