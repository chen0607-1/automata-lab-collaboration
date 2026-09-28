#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <locale.h>

#define MAX_NT 32
#define MAX_P 512
#define MAX_RHS 32
#define MAX_STATE 5
#define MAX_STACK 5
// Add input validation for automata transformation
typedef struct {
    char lhs;
    char rhs[MAX_RHS];   /* empty string is saved as "" */
} Production;

typedef struct {
    char start;
    char nt[MAX_NT];
    int nt_count;
    Production p[MAX_P];
    int p_count;
} Grammar;

void init_grammar(Grammar *g, char start) {
    g->start = start;
    g->nt_count = 0;
    g->p_count = 0;
}

int nt_index(const Grammar *g, char c) {
    int i;
    for (i = 0; i < g->nt_count; i++) {
        if (g->nt[i] == c) return i;
    }
    return -1;
}

int is_nt(const Grammar *g, char c) {
    return nt_index(g, c) >= 0;
}

void add_nt(Grammar *g, char c) {
    if (nt_index(g, c) < 0 && g->nt_count < MAX_NT) {
        g->nt[g->nt_count++] = c;
    }
}

void add_prod(Grammar *g, char lhs, const char *rhs) {
    int i;
    add_nt(g, lhs);
    for (i = 0; rhs[i]; i++) {
        if (isupper((unsigned char)rhs[i])) add_nt(g, rhs[i]);
    }
    for (i = 0; i < g->p_count; i++) {
        if (g->p[i].lhs == lhs && strcmp(g->p[i].rhs, rhs) == 0) return;
    }
    if (g->p_count < MAX_P) {
        g->p[g->p_count].lhs = lhs;
        strcpy(g->p[g->p_count].rhs, rhs);
        g->p_count++;
    }
}

void trim(char *s) {
    int i, j = 0;
    char t[256];
    for (i = 0; s[i]; i++) {
        if (!isspace((unsigned char)s[i])) t[j++] = s[i];
    }
    t[j] = '\0';
    strcpy(s, t);
}

int is_epsilon(const char *s) {
    return strcmp(s, "") == 0 || strcmp(s, "#") == 0 ||
           strcmp(s, "ε") == 0 || strcmp(s, "eps") == 0 ||
           strcmp(s, "EPS") == 0 || strcmp(s, "?") == 0;
}

void parse_line(Grammar *g, char *line) {
    char *arrow, *rhs, *alt;
    char lhs;
    trim(line);
    if (line[0] == '\0') return;
    arrow = strstr(line, "->");
    if (arrow == NULL) arrow = strstr(line, "→");
    if (arrow == NULL) return;
    lhs = line[0];
    if (g->p_count == 0) g->start = lhs;
    add_nt(g, lhs);
    rhs = arrow + (arrow[0] == '-' ? 2 : (int)strlen("→"));
    alt = strtok(rhs, "|");
    while (alt != NULL) {
        add_prod(g, lhs, is_epsilon(alt) ? "" : alt);
        alt = strtok(NULL, "|");
    }
}

void print_grammar(const Grammar *g, const char *title) {
    int i, j, first;
    printf("\n%s\n", title);
    for (i = 0; i < g->nt_count; i++) {
        first = 1;
        for (j = 0; j < g->p_count; j++) {
            if (g->p[j].lhs == g->nt[i]) {
                if (first) {
                    printf("%c->", g->nt[i]);
                    first = 0;
                } else {
                    printf("|");
                }
                printf("%s", g->p[j].rhs[0] ? g->p[j].rhs : "#");
            }
        }
        if (!first) printf("\n");
    }
}

int rhs_all_nullable(const Grammar *g, const char *rhs, int nullable[]) {
    int i, id;
    if (rhs[0] == '\0') return 1;
    for (i = 0; rhs[i]; i++) {
        id = nt_index(g, rhs[i]);
        if (id < 0 || !nullable[id]) return 0;
    }
    return 1;
}

void compute_nullable(const Grammar *g, int nullable[]) {
    int i, id, changed = 1;
    for (i = 0; i < MAX_NT; i++) nullable[i] = 0;
    while (changed) {
        changed = 0;
        for (i = 0; i < g->p_count; i++) {
            id = nt_index(g, g->p[i].lhs);
            if (id >= 0 && !nullable[id] &&
                rhs_all_nullable(g, g->p[i].rhs, nullable)) {
                nullable[id] = 1;
                changed = 1;
            }
        }
    }
}

char fresh_start_symbol(const Grammar *g) {
    char c;
    for (c = 'Z'; c >= 'A'; c--) {
        if (c != g->start && nt_index(g, c) < 0) return c;
    }
    return '\0';
}

void gen_without_nullable(Grammar *out, const Grammar *g, char lhs,
                          const char *rhs, int pos, char *buf, int len,
                          int nullable[]) {
    int id;
    if (rhs[pos] == '\0') {
        buf[len] = '\0';
        if (len > 0 || lhs == g->start) add_prod(out, lhs, buf);
        return;
    }
    id = nt_index(g, rhs[pos]);
    if (id >= 0 && nullable[id]) {
        gen_without_nullable(out, g, lhs, rhs, pos + 1, buf, len, nullable);
    }
    buf[len] = rhs[pos];
    gen_without_nullable(out, g, lhs, rhs, pos + 1, buf, len + 1, nullable);
}

void remove_epsilon(Grammar *g) {
    int nullable[MAX_NT] = {0};
    int i, id;
    Grammar base, out;
    char buf[MAX_RHS];
    char new_start, old_start_rhs[2];

    compute_nullable(g, nullable);
    id = nt_index(g, g->start);
    if (id >= 0 && nullable[id]) {
        new_start = fresh_start_symbol(g);
        if (new_start != '\0') {
            init_grammar(&base, new_start);
            add_nt(&base, new_start);
            for (i = 0; i < g->nt_count; i++) add_nt(&base, g->nt[i]);
            old_start_rhs[0] = g->start;
            old_start_rhs[1] = '\0';
            add_prod(&base, new_start, old_start_rhs);
            add_prod(&base, new_start, "");
            for (i = 0; i < g->p_count; i++) {
                add_prod(&base, g->p[i].lhs, g->p[i].rhs);
            }
            *g = base;
            compute_nullable(g, nullable);
        }
    }

    init_grammar(&out, g->start);
    for (i = 0; i < g->nt_count; i++) add_nt(&out, g->nt[i]);
    for (i = 0; i < g->p_count; i++) {
        if (g->p[i].rhs[0] != '\0') {
            gen_without_nullable(&out, g, g->p[i].lhs, g->p[i].rhs,
                                 0, buf, 0, nullable);
        } else if (g->p[i].lhs == g->start) {
            add_prod(&out, g->p[i].lhs, "");
        }
    }
    *g = out;
}

int is_unit_prod(const Grammar *g, const Production *p) {
    return strlen(p->rhs) == 1 && is_nt(g, p->rhs[0]);
}

void remove_unit(Grammar *g) {
    int unit[MAX_NT][MAX_NT] = {{0}};
    int i, j, k, a, b, changed = 1;
    Grammar out;

    for (i = 0; i < g->nt_count; i++) unit[i][i] = 1;
    for (i = 0; i < g->p_count; i++) {
        if (is_unit_prod(g, &g->p[i])) {
            a = nt_index(g, g->p[i].lhs);
            b = nt_index(g, g->p[i].rhs[0]);
            unit[a][b] = 1;
        }
    }
    while (changed) {
        changed = 0;
        for (i = 0; i < g->nt_count; i++) {
            for (j = 0; j < g->nt_count; j++) {
                if (unit[i][j]) {
                    for (k = 0; k < g->nt_count; k++) {
                        if (unit[j][k] && !unit[i][k]) {
                            unit[i][k] = 1;
                            changed = 1;
                        }
                    }
                }
            }
        }
    }

    init_grammar(&out, g->start);
    for (i = 0; i < g->nt_count; i++) add_nt(&out, g->nt[i]);
    for (i = 0; i < g->nt_count; i++) {
        for (j = 0; j < g->nt_count; j++) {
            if (unit[i][j]) {
                for (k = 0; k < g->p_count; k++) {
                    if (g->p[k].lhs == g->nt[j] && !is_unit_prod(g, &g->p[k])) {
                        add_prod(&out, g->nt[i], g->p[k].rhs);
                    }
                }
            }
        }
    }
    *g = out;
}

int rhs_generating(const Grammar *g, const char *rhs, int gen[]) {
    int i, id;
    for (i = 0; rhs[i]; i++) {
        id = nt_index(g, rhs[i]);
        if (id >= 0 && !gen[id]) return 0;
    }
    return 1;
}

void remove_useless(Grammar *g) {
    int gen[MAX_NT] = {0}, reach[MAX_NT] = {0};
    int changed = 1, i, j, id, lhs_id, ok;
    Grammar temp, out;

    while (changed) {
        changed = 0;
        for (i = 0; i < g->p_count; i++) {
            lhs_id = nt_index(g, g->p[i].lhs);
            if (!gen[lhs_id] && rhs_generating(g, g->p[i].rhs, gen)) {
                gen[lhs_id] = 1;
                changed = 1;
            }
        }
    }

    init_grammar(&temp, g->start);
    for (i = 0; i < g->nt_count; i++) {
        if (gen[i]) add_nt(&temp, g->nt[i]);
    }
    for (i = 0; i < g->p_count; i++) {
        lhs_id = nt_index(g, g->p[i].lhs);
        if (lhs_id < 0 || !gen[lhs_id]) continue;
        ok = 1;
        for (j = 0; g->p[i].rhs[j]; j++) {
            id = nt_index(g, g->p[i].rhs[j]);
            if (id >= 0 && !gen[id]) ok = 0;
        }
        if (ok) add_prod(&temp, g->p[i].lhs, g->p[i].rhs);
    }

    id = nt_index(&temp, temp.start);
    if (id >= 0) reach[id] = 1;
    changed = 1;
    while (changed) {
        changed = 0;
        for (i = 0; i < temp.p_count; i++) {
            lhs_id = nt_index(&temp, temp.p[i].lhs);
            if (lhs_id >= 0 && reach[lhs_id]) {
                for (j = 0; temp.p[i].rhs[j]; j++) {
                    id = nt_index(&temp, temp.p[i].rhs[j]);
                    if (id >= 0 && !reach[id]) {
                        reach[id] = 1;
                        changed = 1;
                    }
                }
            }
        }
    }

    init_grammar(&out, temp.start);
    for (i = 0; i < temp.nt_count; i++) {
        if (reach[i]) add_nt(&out, temp.nt[i]);
    }
    for (i = 0; i < temp.p_count; i++) {
        lhs_id = nt_index(&temp, temp.p[i].lhs);
        if (lhs_id < 0 || !reach[lhs_id]) continue;
        ok = 1;
        for (j = 0; temp.p[i].rhs[j]; j++) {
            id = nt_index(&temp, temp.p[i].rhs[j]);
            if (id >= 0 && !reach[id]) ok = 0;
        }
        if (ok) add_prod(&out, temp.p[i].lhs, temp.p[i].rhs);
    }
    *g = out;
}

void simplify(Grammar *g) {
    remove_epsilon(g);
    print_grammar(g, "After removing epsilon-productions:");
    remove_unit(g);
    print_grammar(g, "After removing unit-productions:");
    remove_useless(g);
    print_grammar(g, "After removing useless symbols:");
}

void read_cfg(Grammar *g) {
    int n, i;
    char line[256];
    printf("Input number of production lines: ");
    scanf("%d", &n);
    getchar();
    init_grammar(g, 'S');
    printf("Input productions line by line, for example: S->a|bA|#\n");
    printf("Use # for epsilon. The program can also recognize epsilon if your terminal supports it.\n");
    for (i = 0; i < n; i++) {
        fgets(line, sizeof(line), stdin);
        line[strcspn(line, "\r\n")] = '\0';
        parse_line(g, line);
    }
}

void sample_cfg(Grammar *g) {
    char lines[][64] = {
        "S->a|bA|B|ccD",
        "A->abB|#",
        "B->aA",
        "C->ddC",
        "D->ddd"
    };
    int i;
    init_grammar(g, 'S');
    for (i = 0; i < 5; i++) parse_line(g, lines[i]);
}

char var_name(int p, int stack, int q) {
    return (char)('A' + p * 4 + stack * 2 + q);
}

void add_push2(Grammar *g, int p, char in, int stack, int r, int y, int z) {
    int q, s;
    char rhs[MAX_RHS];
    for (q = 0; q < 2; q++) {
        for (s = 0; s < 2; s++) {
            rhs[0] = in;
            rhs[1] = var_name(r, y, s);
            rhs[2] = var_name(s, z, q);
            rhs[3] = '\0';
            add_prod(g, var_name(p, stack, q), rhs);
        }
    }
}

void add_pop(Grammar *g, int p, const char *in, int stack, int r) {
    add_prod(g, var_name(p, stack, r), in);
}

void pda_cfg(Grammar *g) {
    int p, st, q;
    init_grammar(g, 'S');
    add_nt(g, 'S');
    for (p = 0; p < 2; p++) {
        for (st = 0; st < 2; st++) {
            for (q = 0; q < 2; q++) add_nt(g, var_name(p, st, q));
        }
    }

    add_prod(g, 'S', "C"); /* C=[q0,z0,q0] */
    add_prod(g, 'S', "D"); /* D=[q0,z0,q1] */

    /* stack: 0 表示 B，1 表示 z0；state: 0 表示 q0，1 表示 q1 */
    add_push2(g, 0, 'b', 1, 0, 0, 1); /* δ(q0,b,z0)=(q0,Bz0) */
    add_push2(g, 0, 'b', 0, 0, 0, 0); /* δ(q0,b,B) =(q0,BB)  */
    add_pop(g, 0, "a", 0, 1);         /* δ(q0,a,B) =(q1,ε)   */
    add_pop(g, 1, "a", 0, 1);         /* δ(q1,a,B) =(q1,ε)   */
    add_pop(g, 1, "", 0, 1);          /* δ(q1,ε,B) =(q1,ε)   */
    add_pop(g, 1, "", 1, 1);          /* δ(q1,ε,z0)=(q1,ε)   */
}

void print_pda_vars(void) {
    printf("\nVariable meanings in PDA-to-CFG construction:\n");
    printf("A=[q0,B,q0]  B=[q0,B,q1]  C=[q0,z0,q0]  D=[q0,z0,q1]\n");
    printf("E=[q1,B,q0]  F=[q1,B,q1]  G=[q1,z0,q0]  H=[q1,z0,q1]\n");
}

char custom_var_name(int p, int stack, int q, int state_count) {
    const char pool[] = "ABCDEFGHIJKLMNOPQRTUVWXYZ"; /* S is reserved */
    int idx = stack * state_count * state_count + p * state_count + q;
    if (idx < 0 || idx >= (int)strlen(pool)) return '?';
    return pool[idx];
}

int stack_index(char stack_symbols[], int stack_count, char c) {
    int i;
    for (i = 0; i < stack_count; i++) {
        if (stack_symbols[i] == c) return i;
    }
    return -1;
}

void print_custom_pda_vars(int state_count, int stack_count, char stack_symbols[]) {
    int p, st, q;
    printf("\nVariable meanings:\n");
    for (st = 0; st < stack_count; st++) {
        for (p = 0; p < state_count; p++) {
            for (q = 0; q < state_count; q++) {
                printf("%c=[q%d,%c,q%d]  ",
                       custom_var_name(p, st, q, state_count),
                       p, stack_symbols[st], q);
            }
            printf("\n");
        }
    }
}

void add_custom_push_rhs(Grammar *g, int state_count, int lhs_p, int lhs_stack,
                         int first_state, int final_state, char input,
                         int push_ids[], int push_len, int depth,
                         int mid_states[]) {
    int s, i, from, to, len = 0;
    char rhs[MAX_RHS];

    if (depth < push_len - 1) {
        for (s = 0; s < state_count; s++) {
            mid_states[depth] = s;
            add_custom_push_rhs(g, state_count, lhs_p, lhs_stack, first_state,
                                final_state, input, push_ids, push_len,
                                depth + 1, mid_states);
        }
        return;
    }

    if (input != '\0') rhs[len++] = input;
    for (i = 0; i < push_len; i++) {
        from = (i == 0) ? first_state : mid_states[i - 1];
        to = (i == push_len - 1) ? final_state : mid_states[i];
        rhs[len++] = custom_var_name(from, push_ids[i], to, state_count);
    }
    rhs[len] = '\0';
    add_prod(g, custom_var_name(lhs_p, lhs_stack, final_state, state_count), rhs);
}

void add_custom_transition(Grammar *g, int state_count, int p, char input,
                           int stack_top, int r, int push_ids[], int push_len) {
    int q;
    int mid_states[MAX_RHS];
    char rhs[MAX_RHS];

    if (push_len == 0) {
        if (input == '\0') rhs[0] = '\0';
        else {
            rhs[0] = input;
            rhs[1] = '\0';
        }
        add_prod(g, custom_var_name(p, stack_top, r, state_count), rhs);
        return;
    }

    for (q = 0; q < state_count; q++) {
        add_custom_push_rhs(g, state_count, p, stack_top, r, q, input,
                            push_ids, push_len, 0, mid_states);
    }
}

void read_custom_pda_cfg(Grammar *g) {
    int state_count, stack_count, start_state, start_stack, trans_count;
    int i, j, p, r, push_len, push_ids[MAX_RHS];
    char stack_symbols[MAX_STACK + 1], input_str[32], stack_str[32], push_str[32];
    char input;

    printf("This PDA is accepted by empty stack.\n");
    printf("States are numbered from 0 to n-1.\n");
    printf("Input state count and stack-symbol count: ");
    scanf("%d%d", &state_count, &stack_count);
    if (state_count <= 0 || state_count > MAX_STATE ||
        stack_count <= 0 || stack_count > MAX_STACK ||
        state_count * state_count * stack_count > 25) {
        printf("Too many states or stack symbols. Need states^2 * stack_symbols <= 25.\n");
        init_grammar(g, 'S');
        return;
    }

    printf("Input stack symbols as one word, e.g. BZ means B and Z: ");
    scanf("%s", stack_symbols);
    printf("Input start state number and start stack symbol: ");
    scanf("%d%s", &start_state, stack_str);
    start_stack = stack_index(stack_symbols, stack_count, stack_str[0]);

    init_grammar(g, 'S');
    add_nt(g, 'S');
    for (i = 0; i < stack_count; i++) {
        for (p = 0; p < state_count; p++) {
            for (r = 0; r < state_count; r++) {
                add_nt(g, custom_var_name(p, i, r, state_count));
            }
        }
    }
    for (r = 0; r < state_count; r++) {
        char s[2];
        s[0] = custom_var_name(start_state, start_stack, r, state_count);
        s[1] = '\0';
        add_prod(g, 'S', s);
    }

    printf("Input transition count: ");
    scanf("%d", &trans_count);
    printf("Transition format: p input stackTop r pushString\n");
    printf("Use # for epsilon. Example: 0 b Z 0 BZ\n");

    for (i = 0; i < trans_count; i++) {
        scanf("%d%s%s%d%s", &p, input_str, stack_str, &r, push_str);
        input = is_epsilon(input_str) ? '\0' : input_str[0];
        start_stack = stack_index(stack_symbols, stack_count, stack_str[0]);
        if (start_stack < 0) {
            printf("Skip transition %d: unknown stack symbol.\n", i + 1);
            continue;
        }

        push_len = 0;
        if (!is_epsilon(push_str)) {
            for (j = 0; push_str[j]; j++) {
                push_ids[push_len] = stack_index(stack_symbols, stack_count, push_str[j]);
                if (push_ids[push_len] < 0) {
                    printf("Skip transition %d: unknown symbol in push string.\n", i + 1);
                    push_len = -1;
                    break;
                }
                push_len++;
            }
        }
        if (push_len < 0) continue;
        add_custom_transition(g, state_count, p, input, start_stack, r, push_ids, push_len);
    }

    print_custom_pda_vars(state_count, stack_count, stack_symbols);
}

int main(void) {
    int choice;
    Grammar g;
    setlocale(LC_ALL, "");

    printf("CFG Transformation Experiment\n");
    printf("1. Input CFG and simplify\n");
    printf("2. Use the given CFG sample\n");
    printf("3. Convert the given PDA to CFG and simplify\n");
    printf("4. Input PDA, convert to CFG and simplify\n");
    printf("Choose: ");
    if (scanf("%d", &choice) != 1) return 0;
    getchar();

    if (choice == 1) {
        read_cfg(&g);
    } else if (choice == 2) {
        sample_cfg(&g);
    } else if (choice == 3) {
        pda_cfg(&g);
        print_pda_vars();
    } else if (choice == 4) {
        read_custom_pda_cfg(&g);
    } else {
        printf("Invalid choice.\n");
        return 0;
    }

    print_grammar(&g, "Original grammar:");
    simplify(&g);
    return 0;
}
