//Junxian Liu
//115991378
#include "hw7.h"



bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if(root == NULL){
        bst_sf *new_node = malloc(sizeof(bst_sf));
        new_node->left_child = NULL;
        new_node->right_child = NULL;
        new_node->mat = mat;
        return new_node;
    }else{
        if(mat->name < root->mat->name){
            root->left_child = insert_bst_sf(mat, root->left_child);
        }else if (mat->name > root->mat->name){
            root->right_child = insert_bst_sf(mat, root->right_child);
        }else{
            mat->name = mat->name - 1;
            root = insert_bst_sf(mat, root);
        }
        return root;
    }
    return NULL;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if(root == NULL){
        return NULL;
    }else{
        if(name < root->mat->name){
            return find_bst_sf(name, root->left_child);
        }else if(name > root->mat->name){
            return find_bst_sf(name, root->right_child);
        }else{
            return root->mat;
        }
    }
    return NULL;
}

void free_bst_sf(bst_sf *root) {
    if(root == NULL){
        return;
    }
    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);
    if (root->mat != NULL){
        free(root->mat);
    }
    free(root);
    return;
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    unsigned cols = mat1->num_cols;
    unsigned rows = mat1->num_rows;
    unsigned size = cols * rows;
    matrix_sf *result = (matrix_sf *)(malloc(sizeof(matrix_sf) + size * sizeof(int)));
    static char temp_name_1 = 'a' - 1;
    temp_name_1++;

    result->name = temp_name_1;
    result->num_cols = cols;
    result->num_rows = rows;
    for(int i = 0; i < size; i++){
        result->values[i] = mat1->values[i] + mat2->values[i];
    }
    return result;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    unsigned rows = mat1->num_rows;
    unsigned shared = mat1->num_cols;
    unsigned cols = mat2->num_cols;
    unsigned size = rows * cols;
    matrix_sf *result = (matrix_sf *)(malloc(sizeof(matrix_sf) + size * sizeof(int)));
    static char temp_name_2 = 'a' - 1;
    temp_name_2++;

    result->name = temp_name_2;
    result->num_cols = cols;
    result->num_rows = rows;
    int sum;
    for (int i = 0; i < rows; i++){
        for (int j = 0; j < cols; j++){
            sum = 0;
            for (int k = 0; k < shared; k++){
                sum += mat1->values[i * shared + k] * mat2->values[k * cols + j];
            }
            result->values[i * cols + j] = sum;
        }
    }
   return result;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    unsigned cols = mat->num_cols;
    unsigned rows = mat->num_rows;
    unsigned size = cols * rows;
    matrix_sf *result = (matrix_sf *)(malloc(sizeof(matrix_sf) + size * sizeof(int)));
    static char temp_name_3 = 'a' - 1;
    temp_name_3++;

    result->name = temp_name_3;
    result->num_cols = rows;
    result->num_rows = cols;
    for (unsigned int i = 0; i < rows; i++){
        for (unsigned int j = 0; j < cols; j++){
            result->values[j * rows + i] = mat->values[i * cols + j];
        }
    }
    return result;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    if(expr == NULL){
        return NULL;
    }
    char *temp = (char *)expr;
    int rows = strtol(temp, &temp, 10);
    int cols = strtol(temp, &temp, 10);
    long size = rows * cols;
    matrix_sf *result = (matrix_sf *)(malloc(sizeof(matrix_sf) + size * sizeof(int)));

    result->name = name;
    result->num_cols = cols;
    result->num_rows = rows;
    int count = 0;
    while (*temp != '\0' && count < size){
        while ((*temp < '0' || *temp > '9') && *temp != '-' && *temp != '\0'){
            temp++;
        }
        result->values[count] = strtol(temp, &temp, 10);
        count++;
    }
    return result;
}

char* infix2postfix_sf(char *infix) {
    if (infix == NULL){
        return NULL;
    }
    int len = strlen(infix) + 10;
    char *result = (char *)malloc(len * sizeof(char));
    char *stack = (char *)malloc(len * sizeof(char));
    char *index = infix, *postfix = result, *stack_p = stack;
    *postfix = '\0';
    int count = 0;
    while (*index != '\0' && *index != '\n'){
        char c = *index;
        if(c == ' '){
            index++;
            continue;
        }
        if((c >= 'A' && c <= 'Z') || c == '\''){
            *postfix++ = c;
        }else if(c == '('){
            stack++;
            count++;
            *stack = '(';
        }else if(c == ')'){
            while (count > 0 && *stack != '(') {
                *postfix++ = *stack--;
                count--;
            }
            if (count > 0 && *stack == '(') {
                stack--;
                count--;
            }
        }else if(c == '+' || c == '-' || c == '*' || c == '/'){
            int cur_prec;
            if(c == '*' || c == '/'){
                cur_prec = 2;
            }else{
                cur_prec = 1;
            }
            while (count > 0 && *stack != '(') {
                char t = *stack;
                int top_prec;
                if(t == '*' || t == '/'){
                    top_prec = 2;
                }else{
                    if(t == '+' || t == '-'){
                        top_prec = 1;
                    }else{
                        top_prec = 0;
                    }
                }
                if(top_prec < cur_prec) {
                    break;
                }
                *postfix++ = *stack--;
                count--;
            }
            stack++;
            count++;
            *stack = c;
        }
        index++;
    }
    while (count > 0)
    {
        *postfix = *stack;
        postfix++;
        stack--;
        count--;
    }
    *postfix = '\0';
    free(stack_p);
    return result;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    if (expr == NULL || root == NULL){
        return NULL;
    }
    char *postfix_buf = infix2postfix_sf(expr);
    if (postfix_buf == NULL) {
        return NULL;
    }
    size_t cap = strlen(postfix_buf) + 2;
    matrix_sf **val_stack = (matrix_sf **)malloc(cap * sizeof(matrix_sf *));
    if (val_stack == NULL) {
        free(postfix_buf);
        return NULL;
    }
    int top = -1;
    bst_sf *temp_root = NULL;
    for(char *p = postfix_buf; *p != '\0'; ++p){
        if(*p >= 'A' && *p <= 'Z'){
            matrix_sf *mat = find_bst_sf(*p, root);
            if(mat == NULL){
                free(postfix_buf);
                free(val_stack);
                free_bst_sf(temp_root);
                return NULL;
            }
            val_stack[++top] = mat;
        }else{
            if(*p == '\''){
                if(top < 0){
                    free(postfix_buf);
                    free(val_stack);
                    free_bst_sf(temp_root);
                    return NULL;
                }
                matrix_sf *temp_mat = transpose_mat_sf(val_stack[top]);
                temp_root = insert_bst_sf(temp_mat, temp_root);
                val_stack[top] = temp_mat;
            }else if(*p == '+'){
                if (top < 1) {
                    free(postfix_buf);
                    free(val_stack);
                    free_bst_sf(temp_root);
                    return NULL;
                }
                matrix_sf *rhs = val_stack[top--];
                matrix_sf *lhs = val_stack[top];
                matrix_sf *sum = add_mats_sf(lhs, rhs);
                temp_root = insert_bst_sf(sum, temp_root);
                val_stack[top] = sum;
            }else if(*p == '*'){
                if (top < 1) {
                    free(postfix_buf);
                    free(val_stack);
                    free_bst_sf(temp_root);
                    return NULL;
                }
                matrix_sf *rhs = val_stack[top--];
                matrix_sf *lhs = val_stack[top];
                matrix_sf *prod = mult_mats_sf(lhs, rhs);
                temp_root = insert_bst_sf(prod, temp_root);
                val_stack[top] = prod;
            }else{
                continue;
            }
        }
    }
    if(top != 0 || val_stack[0] == NULL){
        free(postfix_buf);
        free(val_stack);
        free_bst_sf(temp_root);
        return NULL;
    }
    matrix_sf *final = val_stack[0];
    int rows = final->num_rows;
    int cols = final->num_cols;
    int size = rows * cols;
    matrix_sf *result = (matrix_sf *)malloc(sizeof(matrix_sf) + (size_t)size * sizeof(int));
    if (result == NULL) {
        free(postfix_buf);
        free(val_stack);
        free_bst_sf(temp_root);
        return NULL;
    }

    result->name = name;
    result->num_rows = rows;
    result->num_cols = cols;
    for (int i = 0; i < size; i++) {
        result->values[i] = final->values[i];
    }
    free(postfix_buf);
    free(val_stack);
    free_bst_sf(temp_root);
    return result;
}

matrix_sf *execute_script_sf(char *filename) {
    if(filename == NULL){
        return NULL;
    }
    FILE *fp = fopen(filename, "r");
    if(fp == NULL){
        return NULL;
    }
    size_t len = MAX_LINE_LEN + 10;
    char *s = NULL;
    bst_sf *root = NULL;
    size_t read;
    matrix_sf *new = NULL;

    while((read = getline(&s, &len, fp)) != -1){
        char *s_p = s;
        while(*s_p < 'A' || *s_p > 'Z'){
            s_p++;
        }
        char name = *s_p;
        while(*s_p != '=' && *s_p != '\0'){
            s_p++;
        }
        if(*s_p == '=') {
            s_p++;
        }
        while(isspace(*s_p)){
            s_p++;
        }
        if(*s_p >= '0' && *s_p <= '9'){
            new = create_matrix_sf(name, s_p);
            root = insert_bst_sf(new, root);
        }else{
            new = evaluate_expr_sf(name, s_p, root);
            root = insert_bst_sf(new, root);
        }
    }
    fclose(fp);
    int rows = new->num_rows;
    int cols = new->num_cols;
    int size = rows * cols;
    matrix_sf *result = (matrix_sf *)malloc(sizeof(matrix_sf) + size * sizeof(int));
    result->name = new->name;
    result->num_cols = cols;
    result->num_rows = rows;
    for(int i = 0; i < size; i++){
        result->values[i] = new->values[i];
    }
    free_bst_sf(root);
    free(s);
    return result;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
