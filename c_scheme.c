#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#define MAX_TOK_LEN 32
#define MAX_LINE_LEN 1024

// ..................................Types....................................
typedef enum Boolean {FALSE, TRUE} Boolean;

typedef enum ObjectType {INT, CHAR, FUNCTION, STRING, SYMBOL, PAIR, NILL} ObjectType;

typedef struct Object {
    ObjectType type;
    union {
        long integer;
        char character;
        char *string;
        char *symbol;
        struct pair {
            struct Object *car;
            struct Object *cdr;
        } pair;
        struct Object* (*function)(struct Object*);
    } value;
} Object;

Object *nill;
Object *primitive_procedure_tag;
Object *compound_procedure_tag;

// keywords
Object lambda_sym = { .type=SYMBOL, .value.symbol="lambda"};
Object if_sym = { .type=SYMBOL, .value.symbol="if"};
Object define_sym = { .type=SYMBOL, .value.symbol="define"};
Object quote_sym = { .type=SYMBOL, .value.symbol="quote"};

// true / false
Object true_sym = { .type=SYMBOL, .value.symbol="#t"};
Object false_sym = { .type=SYMBOL, .value.symbol="#f"};


Object *car(Object *obj) 
{
    if (obj->type == PAIR) {
        return obj->value.pair.car;
    }
    else {
        printf("Error: Called car on non-pair. Exiting.\n");
        exit(EXIT_FAILURE);
    }
}

Object *cdr(Object *obj)
{
    if (obj->type == PAIR) {
        return obj->value.pair.cdr;
    }
    else {
        printf("Error: Called cdr on non-pair. Exiting.\n");
        exit(EXIT_FAILURE);
    }
}

Object *cadr(Object *pair)
{
    return car(cdr(pair));
}

Object *caadr(Object *pair)
{
    return car(car(cdr(pair)));
}

Object *caddr(Object *pair) 
{
    return car(cdr(cdr(pair)));
}

Object *cadddr(Object *pair) 
{
    return car(cdr(cdr(cdr(pair))));
}

Object *cdadr(Object *pair) 
{
    return cdr(car(cdr(pair)));
}

Object *cddr(Object *pair) 
{
    return cdr(cdr(pair));
}

void set_car(Object *obj, Object *val)
{
    obj->value.pair.car = val;
}

void set_cdr(Object *obj, Object *val)
{
    obj->value.pair.cdr = val;
}

Object *alloc_object(ObjectType type)
{
    Object *new_obj = (Object*)malloc(sizeof(Object));
    new_obj->type = type;
    return new_obj;
}

Object *new_int(long i)
{
    Object *new_obj = alloc_object(INT);
    new_obj->value.integer = i;
    return new_obj;
}

Object *new_char(char c)
{
    Object *new_obj = alloc_object(CHAR);
    new_obj->value.character = c;
    return new_obj;
}

Object *new_function(Object* (*fun)(Object*))
{
    Object *new_obj = alloc_object(FUNCTION);
    new_obj->value.function = fun;
    return new_obj;
}

Object *new_string(char *str)
{
    Object *new_obj = alloc_object(STRING);
    new_obj->value.string = malloc(strlen(str) + 1);
    strcpy(new_obj->value.string, str);
    return new_obj;
}

Object *new_symbol(char *sym)
{
    Object *new_obj = alloc_object(SYMBOL);
    new_obj->value.string = malloc(strlen(sym) + 1);
    strcpy(new_obj->value.string, sym);
    return new_obj;
}

Object *cons(Object *head, Object *tail) 
{
    Object *new_obj = alloc_object(PAIR);
    new_obj->value.pair.car = head;
    new_obj->value.pair.cdr = tail;
    return new_obj;
}

Object *list(int argc, Object *argv[]) 
{
    Object *res_list = nill;
    for(int i=argc-1; i >= 0; i--) {
        res_list = cons(argv[i], res_list);
    }
    return res_list;
}

char is_pair(Object *obj) 
{
    return obj->type == PAIR;
}

char is_atom(Object *obj) 
{
    return obj->type != PAIR;
}

char is_integer(Object *obj)
{
    return obj->type == INT;
}

char is_number(Object *obj)
{
    return is_integer(obj);
}

char is_string(Object *obj)
{
    return obj->type == STRING;
}

char is_symbol(Object *obj)
{
    return obj->type == SYMBOL;
}

char is_nill(Object *obj)
{
    return obj == nill;
}

char is_list(Object *obj)
{
    if (is_nill(obj)) {
        return 1;
    }
    else {
        return (is_pair(obj) && is_list(cdr(obj)));
    }
}

Object *map(Object* (*fun)(Object*), Object *list)
{
    if (is_nill(list)) {
        return nill;
    }
    else {
        return cons(fun(car(list)), map(fun, cdr(list)));
    }
}

Object *map_in_env(Object* (*fun)(Object*, Object*), Object *list, Object *env)
{
    if (is_nill(list)) 
    {
        return nill;
    }
    else {
        return cons(fun(car(list), env), map_in_env(fun, cdr(list), env));
    }
}

char eq(Object *obj_a, Object *obj_b)
{
    if (obj_a->type != obj_b->type)
        return 0;
    switch (obj_a->type) {
        case NILL:
            return 1;
        case INT:
            return (obj_a->value.integer == obj_b->value.integer);
        case CHAR:
            return (obj_a->value.character == obj_b->value.character);
        case FUNCTION:
            return (obj_a->value.function == obj_b->value.function);
        case STRING:
            return strcmp(obj_a->value.string, obj_b->value.string) == 0;
        case SYMBOL:
            return strcmp(obj_a->value.symbol, obj_b->value.symbol) == 0;
        case PAIR:
            return eq(car(obj_a), car(obj_b)) && eq(cdr(obj_a), cdr(obj_b));
    }
}

Object *wrapped_eq(Object *pair)
{
    Object *obj_a = car(pair);
    Object *obj_b = cadr(pair);
    return eq(obj_a, obj_b) ? &true_sym : &false_sym;
}

Object *numerical_eq(Object *pair)
{
    Object *num_a = car(pair);
    Object *num_b = cadr(pair);
    if (!(is_integer(num_a) && is_integer(num_b))) {
        printf("ERROR: numerical_eq applied to non-number.");
        return &false_sym;
    }
    else
        return num_a->value.integer == num_b->value.integer ? &true_sym : &false_sym;
}

Object *numerical_lt(Object *pair)
{
    Object *num_a = car(pair);
    Object *num_b = cadr(pair);
    if (!(is_integer(num_a) && is_integer(num_b))) {
        printf("ERROR: numerical_lt applied to non-number.");
        return &false_sym;
    }
    else
        return num_a->value.integer < num_b->value.integer ? &true_sym : &false_sym;
}

Object *numerical_gt(Object *pair)
{
    Object *num_a = car(pair);
    Object *num_b = cadr(pair);
    if (!(is_integer(num_a) && is_integer(num_b))) {
        printf("ERROR: numerical_gt applied to non-number.");
        return &false_sym;
    }
    else
        return num_a->value.integer > num_b->value.integer ? &true_sym : &false_sym;
}

char is_tagged_list(Object *tag, Object *obj) 
{
    return (is_pair(obj) && eq(car(obj), tag));
}

char is_primitive_procedure(Object *list)
{
    return is_tagged_list(primitive_procedure_tag, list);
}

Object *primitive_procedure(Object *list)
{
    return cadr(list);
}

char is_compound_procedure(Object *list)
{
    return is_tagged_list(compound_procedure_tag, list);
}

Object *procedure_params(Object *compound_proc)
{
    return cadr(compound_proc);
}

Object *procedure_body(Object *compound_proc)
{
    return caddr(compound_proc);
}

Object *procedure_environment(Object *compound_proc)
{
    return cadddr(compound_proc);
}

Object *make_compound_procedure(Object *params, Object *body, Object *environment)
{
    Object *argv[] = {compound_procedure_tag, params, body, environment};
    return list(4, argv);
}

// .................................Reader....................................
char delim(char c) 
{
    return (c == '(') || (c == ')') || (c == ' ') || (c == '\'');
}

void tokenize_string(char *s, char toks[][MAX_TOK_LEN])
{
    if (!*s) {
        printf("Exiting.\n");
        exit(0);
    }
    char c;
    char *t;
    char *tok_start;
    size_t n = strlen(s);
    size_t i = 0;
    size_t j = 0;
    while (*s) {
        while (*s == ' ')
            ++s;
        if (*s == '\'') {
            strcpy(*toks++, "'");
            ++s;
        }
        if (*s == '(') {
            strcpy(*toks++, "(");
            ++s;
        }
        else if (*s == ')') {
            strcpy(*toks++, ")");
            ++s;
        }
        else if (*s == '"') {
            tok_start = *toks++;
            *tok_start++ = '"';
            ++s;
            while (*s && (*s != '"')) {
                *tok_start++ = *s++;
            }
            if (*s != '"') {
                printf("ERROR: missing closing \"\n");
                exit(1);
            }
            ++s;
            *tok_start++ = '"';
            *tok_start = '\0';
        }
        else {
            tok_start = *toks++;
            while (*s && !delim(*s)) {
                *tok_start++ = *s++;
            }
            *tok_start = '\0';
        }
    }
}

Object *read_atom(char toks[][MAX_TOK_LEN], size_t *curr_index)
{
    char *current_tok = toks[*curr_index];
    char first_char = *current_tok;
    *curr_index = *curr_index + 1;
    if (((first_char=='-') 
                && (strlen(current_tok)>0) 
                && isdigit(*(current_tok+1)) )
            || isdigit(first_char)) {
        return new_int(atoi(current_tok));
    }
    else if (first_char=='"') {
        return new_string(current_tok);
    }
    else  {
        return new_symbol(current_tok);
    }
}

Object *read(char toks[][MAX_TOK_LEN], size_t *curr_index);
Object *read_pair(char toks[][MAX_TOK_LEN], size_t *curr_index)
{
    if (strcmp(toks[*curr_index], ")") == 0) {
        *curr_index = *curr_index + 1;
        return nill;
    }
    else {
        Object *head = read(toks, curr_index);
        Object *tail = read_pair(toks, curr_index);
        return cons(head, tail);
    }
}

Object *read(char toks[][MAX_TOK_LEN], size_t *curr_index)
{
    if (strcmp(toks[*curr_index], "'") == 0) {
        *curr_index = *curr_index + 1;
        Object *expr = read(toks, curr_index);
        return cons(&quote_sym, cons(expr, nill));
    }
    if (strcmp(toks[*curr_index], "(") == 0) {
        *curr_index = *curr_index + 1;
        return read_pair(toks, curr_index);
    }
    else
        return read_atom(toks, curr_index);
}
// ...................Interpreter Data Structures..............................
// Association list - pair of list or list of pairs?
// Environment list of assoc lists: (most-recent, parent, ..., global-env, empty)
Object *the_empty_environment;
Object *the_global_environment;

Object *zip(Object *list_a, Object *list_b) {
    if (is_nill(list_a) || is_nill(list_b)) {
        return nill;
    } else {
        return cons(cons(car(list_a), car(list_b)),
                zip(cdr(list_a), cdr(list_b)));
    }
}

Object *(*new_environment)(Object*, Object*) = cons;
Object *extend_environment(Object *params, Object *vals, Object *parent_env)
{
    Object *bindings = zip(params, vals);
    return new_environment(bindings, parent_env);
}

Object* (*first_frame)(Object*) = car;
Object* (*parent_env)(Object*) = cdr;

Object* lookup_variable(Object *name, Object *environment)
{
    if (eq(environment, the_empty_environment)) {
        printf("ERROR: %s not defined.", name->value.symbol);
        return nill;
    }
    Object *frame = first_frame(environment);
    Object *this_binding;
    Object *val;
    while (!is_nill(frame)) {
        this_binding = car(frame);
        if (eq(car(this_binding), name)) {
            val = cdr(this_binding);
            return val;
        }
        else {
            frame = cdr(frame);
        }
    }
    return lookup_variable(name, parent_env(environment));
}

void define_variable(Object *variable, Object *value, Object *environment) 
{
    Object *frame = first_frame(environment);
    Object *this_binding;
    while (!is_nill(frame)) {
        this_binding = car(frame);
        if (eq(car(this_binding), variable)) {
            set_cdr(this_binding, value);
            return;
        }
        else {
            frame = cdr(frame);
        }
    }
    Object *new_binding = cons(variable, value);
    set_car(environment, cons(new_binding, first_frame(environment)));
}

// ..............................Builtins......................................
Object *add(Object *arg_list)
{
    if (is_integer(arg_list)) {
        return arg_list;
    }
    long acc = 0;
    while (!eq(arg_list, nill)) {
        acc += car(arg_list)->value.integer;
        arg_list = cdr(arg_list);
    }
    return new_int(acc);
}

Object *mul(Object *arg_list)
{
    if (is_integer(arg_list)) {
        return arg_list;
    }
    long acc = 1;
    while (!eq(arg_list, nill)) {
        acc *= car(arg_list)->value.integer;
        arg_list = cdr(arg_list);
    }
    return new_int(acc);
}


Object *sub(Object *arg_list)
{
    if (is_integer(arg_list)) {
        return new_int(-1 * arg_list->value.integer);
    }
    long acc = car(arg_list)->value.integer;
    arg_list = cdr(arg_list);
    while (!eq(arg_list, nill)) {
        acc -= car(arg_list)->value.integer;
        arg_list = cdr(arg_list);
    }
    return new_int(acc);
}

Object *cons_on_list(Object  *arg_list)
{
    Object *head = car(arg_list);
    Object *tail = cadr(arg_list);
    return cons(head, tail);
}

Object *make_primitive_procedure(Object *proc)
{
    Object *argv[] = {primitive_procedure_tag, proc};
    return list(2, argv);
}

Object *load_builtins(void)
{
    Object *bindings[] = {
            cons(new_symbol("+"), make_primitive_procedure(new_function(add))),
            cons(new_symbol("*"), make_primitive_procedure(new_function(mul))),
            cons(new_symbol("-"), make_primitive_procedure(new_function(sub))),
            cons(new_symbol("="), make_primitive_procedure(new_function(numerical_eq))),
            cons(new_symbol(">"), make_primitive_procedure(new_function(numerical_gt))),
            cons(new_symbol("<"), make_primitive_procedure(new_function(numerical_lt))),
            cons(new_symbol("eq"), make_primitive_procedure(new_function(wrapped_eq))),
            cons(new_symbol("cons"), make_primitive_procedure(new_function(cons_on_list))),
            cons(new_symbol("car"), make_primitive_procedure(new_function(car))),
            cons(new_symbol("cdr"), make_primitive_procedure(new_function(cdr)))};

    Object *binding_list = list(sizeof(bindings)/sizeof(bindings[0]), bindings);
    return new_environment(binding_list, the_empty_environment);
}

// .......................Syntax manipulation..................................
char is_lambda(Object *expr) 
{
    return is_tagged_list(&lambda_sym, expr);
}

Object *lambda_params(Object *lambda_definition)
{
    return cadr(lambda_definition);
}

Object *lambda_body(Object *lambda_definition)
{
    return cddr(lambda_definition);
}

Object *make_lambda(Object *lambda_params, Object *lambda_body)
{
    return cons(&lambda_sym, cons(lambda_params, lambda_body));
}

char is_definition(Object *expr) 
{
    return is_tagged_list(&define_sym, expr);
}

Object *definition_variable(Object *expr)
{
    if (is_symbol(cadr(expr))) {
        return cadr(expr);
    }
    else {
        return caadr(expr);
    }
}

Object *definition_value(Object *expr)
{
    if (is_symbol(cadr(expr))) {
        return caddr(expr);
    }
    else {
        return make_lambda(cdadr(expr), cddr(expr));
    }
}

char is_self_evaluating(Object *expr) 
{
    return is_number(expr) || is_string(expr) || is_nill(expr);
}

char is_application(Object *expr) 
{
    return is_pair(expr);
}

char is_last_exp(Object *expr_seq) 
{
    return is_pair(expr_seq) && is_nill(cdr(expr_seq));
}

char is_if(Object *expr)
{
    return is_tagged_list(&if_sym, expr);
}

Object *if_test(Object *expr) 
{
    return cadr(expr);
}

Object *if_consequent(Object *expr) 
{
    return caddr(expr);
}

Object *if_subsequent(Object *expr) 
{
    if (!is_nill(cadddr(expr)))
        return cadddr(expr);
    else
        return &false_sym;
}

char is_quoted(Object *expr)
{
    return is_tagged_list(&quote_sym, expr);
}

Object *quotation_text(Object *expr)
{
    return cadr(expr);
}

// ....................................EVAL....................................
Object *eval(Object *expr, Object *env);
Object *eval_definition(Object *expr, Object *env);
Object *eval_if(Object *expr, Object *env);
Object *apply(Object *function, Object *arg_list);

Object *eval_sequence(Object *expr_seq, Object *env)
{
    if (is_last_exp(expr_seq)) {
        return eval(car(expr_seq), env);
    }
    else {
        eval(car(expr_seq), env); // for side-effects
        return eval_sequence(cdr(expr_seq), env);
    }
}

Object *eval(Object *expr, Object *env) 
{
    if (is_self_evaluating(expr))
        return expr;
    if (is_quoted(expr))
        return quotation_text(expr);
    else if (is_symbol(expr))
        return lookup_variable(expr, env);
    else if (is_if(expr))
        return eval_if(expr, env);
    else if (is_lambda(expr)) {
        Object *params = lambda_params(expr);
        Object *body = lambda_body(expr);
        return make_compound_procedure(params, body, env);
    }
    else if (is_definition(expr)) {
        return eval_definition(expr, env);
    }
    else if (is_application(expr)) {
        Object *evalled_pair = map_in_env(eval, expr, env);
        Object *fun = car(evalled_pair);
        Object *arg_list = cdr(evalled_pair);
        return apply(fun, arg_list);
    }
    else {
        printf("I don't know how to evaluate this expr");
        return nill;
    }
}

Object *eval_definition(Object *expr, Object *env)
{
    Object *var = definition_variable(expr);
    Object *val = eval(definition_value(expr), env);
    define_variable(var, val, env);
    return nill;
}

Object *eval_if(Object *expr, Object *env)
{
    Object *test = if_test(expr);
    Object *consq = if_consequent(expr);
    Object *subsq = if_subsequent(expr);
    if (eq(eval(if_test(expr), env), &true_sym))
        return eval(consq, env);
    else
        return eval(subsq, env);
}


Object *apply(Object *function, Object *arg_list)
{
    if (is_primitive_procedure(function)) {
        return primitive_procedure(function)->value.function(arg_list);
    }
    else if (is_compound_procedure(function)) {
        Object *proc_env = procedure_environment(function);
        Object *params = procedure_params(function);
        Object *new_env = extend_environment(params, arg_list, proc_env);
        Object *body = procedure_body(function);
        return eval_sequence(body, new_env);
    }
    else {
        printf("ERROR: First element is not a procedure.\n");
        return nill;
    }
    
}

// ....................................PRINT...................................
void display(Object *expr);

void display_pair(Object *expr) {
    Object *head = car(expr);
    Object *tail = cdr(expr);
    printf("%c", '(');
    while ((!is_atom(tail)) && !is_nill(tail)) {
        display(head);
        printf("%c", ' ');
        head = car(tail);
        tail = cdr(tail);
    }
    if (is_nill(tail)) {
        display(head);
        printf("%c", ')');
    }
    else {
        display(head);
        printf("%s", " . ");
        display(tail);
        printf("%c", ')');
    }
}

void display(Object *expr) {
    if (is_integer(expr)) {
        printf("%ld", expr->value.integer);
    }
    else if (is_string(expr)) {
        printf("%s", expr->value.string);
    }
    else if (is_symbol(expr)) {
        printf("%s", expr->value.symbol);
    }
    else if (is_pair(expr)) {
        display_pair(expr);
    }
    else if (is_nill(expr)) {
        printf("()");
    }
    else {
        printf("I don't know how to display this yet :(");
    }
}

// ....................................LOOP....................................

void init(void) {
    nill = alloc_object(NILL);
    the_empty_environment = nill;
    primitive_procedure_tag = new_symbol("primitive_procedure");
    compound_procedure_tag = new_symbol("compound_procedure");
    the_global_environment = load_builtins();
}

int main(void) {
    printf("Mini-scheme interpreter in C.\n");
    printf("Ctrl-c to exit.\n");
    char token_array[MAX_LINE_LEN][MAX_TOK_LEN];
    char *line = NULL;
    size_t max_len = 0;
    size_t line_length;
    size_t token_index = 0;

    init();
    int counter = 0;
    Object *expr;
    Object *value;
    while (1) {
        token_index = 0;
        printf("[In  %d]: ", counter);
        line = NULL;
        max_len = 0;
        memset(token_array, 0, sizeof(token_array));
        line_length = getline(&line, &max_len, stdin);
        line[line_length-1] = '\0';
        tokenize_string(line, token_array);
        expr = read(token_array, &token_index);
        value = eval(expr, the_global_environment);
        printf("[Out %d]: ", counter);
        display(value);
        printf("\n");
        ++counter;
        clearerr(stdin);
    }
    return 0;
}
