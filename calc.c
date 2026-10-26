
/*
 * Name: AISHA SAH
 * Student ID: 260AIB131
 * Repository: https://github.com/Aishasah12345/calc-lab-home
 * Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
 * Resources used: OpenAI ChatGPT (used for guidance on understanding
 * and implementing the arithmetic parser assignment).
 *  * Conversation history: https://chatgpt.com/share/6adf06f4-d5a4-83eb-aa3e-4dcd8ef023a6
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>

/*
 * This parser uses source offsets rather than pointers alone so that
 * errors can be reported using 1-based positions in the original file.
 * Recursive descent separates precedence levels; power() parses its
 * exponent recursively, making ** right-associative.
 */

typedef struct {
    double value;
    long long integer;
    int is_integer;
} Value;

static const char *source;
static size_t pos;
static size_t line_end;
static size_t error_pos;
static int failed;

static void skip_spaces(void)
{
    while (pos < line_end &&
           (source[pos] == ' ' || source[pos] == '\t' ||
            source[pos] == '\r'))
        pos++;
}

static void set_error(size_t where)
{
    if (!failed) {
        failed = 1;
        error_pos = where;
    }
}

static Value parse_expr(void);

static Value make_integer(long long n)
{
    Value v = { (double)n, n, 1 };
    return v;
}

static Value make_float(double n)
{
    Value v = { n, 0, 0 };
    return v;
}

static Value parse_primary(void)
{
    Value v;
    skip_spaces();

    if (pos >= line_end) {
        set_error(pos);
        return make_integer(0);
    }

    if (source[pos] == '(') {
        pos++;
        v = parse_expr();
        skip_spaces();

        if (!failed) {
            if (pos >= line_end)
                set_error(pos);
            else if (source[pos] != ')')
                set_error(pos);
            else
                pos++;
        }
        return v;
    }

    if (isdigit((unsigned char)source[pos])) {
        size_t start = pos;
        int is_float = 0;

        while (pos < line_end &&
               isdigit((unsigned char)source[pos]))
            pos++;

        if (pos < line_end && source[pos] == '.') {
            is_float = 1;
            pos++;
            if (pos >= line_end ||
                !isdigit((unsigned char)source[pos])) {
                set_error(pos - 1);
                return make_integer(0);
            }
            while (pos < line_end &&
                   isdigit((unsigned char)source[pos]))
                pos++;
        }

        {
            size_t len = pos - start;
            char *number = malloc(len + 1);
            char *endptr;

            if (number == NULL) {
                set_error(start);
                return make_integer(0);
            }

            memcpy(number, source + start, len);
            number[len] = '\0';
            errno = 0;

            if (is_float) {
                double d = strtod(number, &endptr);
                if (errno == ERANGE || !isfinite(d))
                    set_error(start);
                v = make_float(d);
            } else {
                long long n = strtoll(number, &endptr, 10);
                v = make_integer(n);
            }

            free(number);
            return v;
        }
    }

    set_error(pos);
    return make_integer(0);
}

static Value parse_power(void)
{
    Value left = parse_primary();
    size_t operator_pos, exponent_pos;

    skip_spaces();

    if (failed || pos + 1 >= line_end ||
        source[pos] != '*' || source[pos + 1] != '*')
        return left;

    operator_pos = pos;
 pos += 2;
exponent_pos = pos;
skip_spaces();

    if (pos >= line_end) {
        set_error(pos);
        return make_integer(0);
    }

    /* Recursion on the right groups 2**3**2 as 2**(3**2). */
    {
        Value right = parse_power();
        double result;

        if (failed)
            return make_integer(0);

        if (left.is_integer && right.is_integer) {
            long long base = left.integer;
            long long exp = right.integer;
            long long result_int = 1;

            if (exp < 0) {
                set_error(exponent_pos);
                return make_integer(0);
            }

            while (exp > 0) {
                if (exp & 1)
                    result_int *= base;
                exp >>= 1;
                if (exp > 0)
                    base *= base;
            }
            return make_integer(result_int);
        }

        result = pow(left.value, right.value);
        if (!isfinite(result)) {
            set_error(operator_pos);
            return make_float(0.0);
        }
        return make_float(result);
    }
}

static Value parse_term(void)
{
    Value left = parse_power();

    for (;;) {
        char op;
        size_t operator_pos, right_pos;
        Value right;

        skip_spaces();
        if (failed || pos >= line_end ||
            (source[pos] != '*' && source[pos] != '/'))
            return left;

        if (source[pos] == '*' && pos + 1 < line_end &&
            source[pos + 1] == '*')
            return left;

        operator_pos = pos;
        op = source[pos++];
        skip_spaces();
        right_pos = pos;
        right = parse_power();

        if (failed)
            return left;

        if (op == '/' && right.value == 0.0) {
            set_error(right_pos);
            return left;
        }

        if (left.is_integer && right.is_integer) {
            if (op == '*')
                left = make_integer(left.integer * right.integer);
            else
                left = make_integer(left.integer / right.integer);
        } else {
            double result = op == '*'
                ? left.value * right.value
                : left.value / right.value;

            if (!isfinite(result)) {
                set_error(operator_pos);
                return left;
            }
            left = make_float(result);
        }
    }
}

static Value parse_expr(void)
{
    Value left = parse_term();

    for (;;) {
        char op;
        Value right;

        skip_spaces();
        if (failed || pos >= line_end ||
            (source[pos] != '+' && source[pos] != '-'))
            return left;

        op = source[pos++];
        right = parse_term();

        if (failed)
            return left;

        if (left.is_integer && right.is_integer) {
            if (op == '+')
                left = make_integer(left.integer + right.integer);
            else
                left = make_integer(left.integer - right.integer);
        } else {
            double result = op == '+'
                ? left.value + right.value
                : left.value - right.value;

            if (!isfinite(result)) {
                set_error(pos - 1);
                return left;
            }
            left = make_float(result);
        }
    }
}

static char *read_file(const char *filename, size_t *length)
{
    FILE *f = fopen(filename, "rb");
    char *data;
    size_t used = 0, capacity = 8192;
    int ch;

    if (f == NULL)
        return NULL;

    data = malloc(capacity);
    if (data == NULL) {
        fclose(f);
        return NULL;
    }

    while ((ch = fgetc(f)) != EOF) {
        if (used == capacity) {
            size_t new_capacity = capacity * 2;
            char *tmp = realloc(data, new_capacity);

            if (tmp == NULL) {
                free(data);
                fclose(f);
                return NULL;
            }
            data = tmp;
            capacity = new_capacity;
        }
        data[used++] = (char)ch;
    }

    if (ferror(f)) {
        free(data);
        fclose(f);
        return NULL;
    }

    fclose(f);
    *length = used;
    return data;
}

static int evaluate_file(const char *input, const char *output)
{
    size_t length, start = 0;
    char *data = read_file(input, &length);
    FILE *out;
    char *results = NULL;
    size_t used = 0, capacity = 0;

    if (data == NULL) {
        fprintf(stderr, "Cannot read input file: %s\n", input);
        return 1;
    }

    while (start < length) {
        size_t end = start;
        size_t first;
        Value result;

        while (end < length && data[end] != '\n')
            end++;

        source = data;
        pos = start;
        line_end = end;
        failed = 0;
        error_pos = 0;

        first = start;
        while (first < end &&
               (data[first] == ' ' || data[first] == '\t' ||
                data[first] == '\r'))
            first++;

        if (first < end && data[first] != '#') {
            pos = start;
            result = parse_expr();
            skip_spaces();

            if (!failed && pos < line_end) {
                set_error(pos);
            }

            if (failed) {
                char message[64];
                int n = snprintf(message, sizeof message,
                                 "ERROR:%zu\n", error_pos + 1);
                free(results);
                results = malloc((size_t)n + 1);
                if (results == NULL) {
                    free(data);
                    return 1;
                }
                memcpy(results, message, (size_t)n);
                used = (size_t)n;
                break;
            }

            {
                char message[128];
                int n;

                if (result.is_integer)
                    n = snprintf(message, sizeof message,
                                 "%lld\n", result.integer);
                else
                    n = snprintf(message, sizeof message,
                                 "%.15g\n", result.value);

                if (used + (size_t)n + 1 > capacity) {
                    size_t new_capacity = capacity ? capacity * 2 : 256;
                    char *tmp;

                    while (new_capacity < used + (size_t)n + 1)
                        new_capacity *= 2;

                    tmp = realloc(results, new_capacity);
                    if (tmp == NULL) {
                        free(results);
                        free(data);
                        return 1;
                    }
                    results = tmp;
                    capacity = new_capacity;
                }
                memcpy(results + used, message, (size_t)n);
                used += (size_t)n;
            }
        }

        start = end < length ? end + 1 : length;
    }

    out = fopen(output, "w");
    if (out == NULL) {
        fprintf(stderr, "Cannot write output file: %s\n", output);
        free(results);
        free(data);
        return 1;
    }

    if (used > 0)
        fwrite(results, 1, used, out);

    free(results);
    free(data);

    if (fclose(out) != 0) {
        fprintf(stderr, "Error closing output file: %s\n", output);
        return 1;
    }
    return 0;
}

static int make_output_path(char *dest, size_t size,
                            const char *dir, const char *input)
{
    const char *slash = strrchr(input, '/');
    const char *name = slash ? slash + 1 : input;
    char base[256];
    char *dot;

    if (strlen(name) >= sizeof base)
        return 0;

    strcpy(base, name);
    dot = strrchr(base, '.');
    if (dot != NULL && strcmp(dot, ".txt") == 0)
        *dot = '\0';

    return snprintf(dest, size, "%s/%s_aisha_sah_260AIB131.txt",
                    dir, base) < (int)size;
}

int main(int argc, char **argv)
{
    const char *input = NULL, *outdir = NULL, *dir = NULL;
    int i;
    struct stat st;

    for (i = 1; i < argc; i++) {
        if ((!strcmp(argv[i], "-o") ||
             !strcmp(argv[i], "--output-dir")) && i + 1 < argc) {
            outdir = argv[++i];
        } else if ((!strcmp(argv[i], "-d") ||
                    !strcmp(argv[i], "--dir")) && i + 1 < argc) {
            dir = argv[++i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            return 1;
        } else if (input == NULL) {
            input = argv[i];
        } else {
            fprintf(stderr, "Too many input arguments\n");
            return 1;
        }
    }

    if ((dir == NULL && input == NULL) ||
        (dir != NULL && input != NULL)) {
        fprintf(stderr, "Usage: calc [-d DIR] [-o OUTDIR] [input.txt]\n");
        return 1;
    }

    if (outdir == NULL) {
        const char *base = dir != NULL ? dir : input;
        const char *slash = strrchr(base, '/');
        base = slash ? slash + 1 : base;

        {
            char folder[512];
            if (snprintf(folder, sizeof folder,
                         "%s_aisha_260AIB131", base) >= (int)sizeof folder) {
                fprintf(stderr, "Output directory name too long\n");
                return 1;
            }
            if (mkdir(folder, 0777) != 0 && stat(folder, &st) != 0) {
                fprintf(stderr, "Cannot create output directory\n");
                return 1;
            }
            char *saved_dir = malloc(strlen(folder) + 1);
if (saved_dir == NULL) {
    fprintf(stderr, "Memory allocation failed\n");
    return 1;
}
strcpy(saved_dir, folder);
outdir = saved_dir;
        }
    } else {
        if (mkdir(outdir, 0777) != 0 && stat(outdir, &st) != 0) {
            fprintf(stderr, "Cannot create output directory: %s\n", outdir);
            return 1;
        }
    }

    if (dir == NULL) {
        char output[1024];

        if (!make_output_path(output, sizeof output, outdir, input)) {
            fprintf(stderr, "Output path too long\n");
            return 1;
        }
        return evaluate_file(input, output);
    }

    {
        DIR *folder = opendir(dir);
        struct dirent *entry;
        int status = 0;

        if (folder == NULL) {
            fprintf(stderr, "Cannot open directory: %s\n", dir);
            return 1;
        }

        while ((entry = readdir(folder)) != NULL) {
            size_t n = strlen(entry->d_name);
            char input_path[1024], output_path[1024];
            struct stat info;

            if (n < 4 || strcmp(entry->d_name + n - 4, ".txt") != 0)
                continue;

            if (snprintf(input_path, sizeof input_path, "%s/%s",
                         dir, entry->d_name) >= (int)sizeof input_path)
                continue;

            if (stat(input_path, &info) != 0 || !S_ISREG(info.st_mode))
                continue;

            if (!make_output_path(output_path, sizeof output_path,
                                  outdir, entry->d_name)) {
                status = 1;
                continue;
            }

            if (evaluate_file(input_path, output_path) != 0)
                status = 1;
        }

        closedir(folder);
        return status;
    }
}
