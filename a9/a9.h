void (*pc)();

void *vor__m__tor__m__eval, *vor__m__envr__m__cps, *vor__m__k, *akr__m__k, *akr__m__v, *aer__m__env, *aer__m__y, *aer__m__kr__ex__, *acr__m__cr__m__cps, *acr__m__a, *acr__m__kr__ex__;

struct expr;
typedef struct expr expr;
struct expr {
  enum {
    _const_expr,
    _var_expr,
    _if_expr,
    _mult_expr,
    _subr1_expr,
    _zero_expr,
    _letcc_expr,
    _throw_expr,
    _let_expr,
    _lambda_expr,
    _app_expr
  } tag;
  union {
    struct { void *_cexp; } _const;
    struct { void *_n; } _var;
    struct { void *_test; void *_conseq; void *_alt; } _if;
    struct { void *_nexpr1; void *_nexpr2; } _mult;
    struct { void *_nexp; } _subr1;
    struct { void *_nexp; } _zero;
    struct { void *_body; } _letcc;
    struct { void *_kexp; void *_vexp; } _throw;
    struct { void *_exp; void *_body; } _let;
    struct { void *_body; } _lambda;
    struct { void *_rator; void *_rand; } _app;
  } u;
};

void *exprr_const(void *cexp);
void *exprr_var(void *n);
void *exprr_if(void *test, void *conseq, void *alt);
void *exprr_mult(void *nexpr1, void *nexpr2);
void *exprr_subr1(void *nexp);
void *exprr_zero(void *nexp);
void *exprr_letcc(void *body);
void *exprr_throw(void *kexp, void *vexp);
void *exprr_let(void *exp, void *body);
void *exprr_lambda(void *body);
void *exprr_app(void *rator, void *rand);

struct clos;
typedef struct clos clos;
struct clos {
  enum {
    _closure_clos
  } tag;
  union {
    struct { void *_body; void *_envr__m__cps; } _closure;
  } u;
};

void *closr_closure(void *body, void *envr__m__cps);

struct envr;
typedef struct envr envr;
struct envr {
  enum {
    _emptyr__m__env_envr,
    _extendr__m__env_envr
  } tag;
  union {
    struct { char dummy; } _emptyr__m__env;
    struct { void *_valuer__ex__; void *_envr__m__cpsr__ex__; } _extendr__m__env;
  } u;
};

void *envrr_emptyr__m__env();
void *envrr_extendr__m__env(void *valuer__ex__, void *envr__m__cpsr__ex__);

struct kt;
typedef struct kt kt;
struct kt {
  enum {
    _kr__m__init_kt,
    _kr__m__multr__m__nr2_kt,
    _kr__m__multr__m__nr1_kt,
    _kr__m__subr1_kt,
    _kr__m__zeror__q___kt,
    _kr__m__if_kt,
    _kr__m__throw_kt,
    _kr__m__let_kt,
    _kr__m__operand_kt,
    _kr__m__rator_kt
  } tag;
  union {
    struct { void *_jumpout; } _kr__m__init;
    struct { void *_nr1r__ex__; void *_kr__ex__; } _kr__m__multr__m__nr2;
    struct { void *_xr2r__ex__; void *_envr__m__cpsr__ex__; void *_kr__ex__; } _kr__m__multr__m__nr1;
    struct { void *_kr__ex__; } _kr__m__subr1;
    struct { void *_kr__ex__; } _kr__m__zeror__q__;
    struct { void *_conseqr__ex__; void *_altr__ex__; void *_envr__m__cpsr__ex__; void *_kr__ex__; } _kr__m__if;
    struct { void *_vr__m__expr__ex__; void *_envr__m__cpsr__ex__; } _kr__m__throw;
    struct { void *_bodyr__ex__; void *_envr__m__cpsr__ex__; void *_kr__ex__; } _kr__m__let;
    struct { void *_cr__m__cpsr__ex__; void *_kr__ex__; } _kr__m__operand;
    struct { void *_randr__ex__; void *_envr__m__cpsr__ex__; void *_kr__ex__; } _kr__m__rator;
  } u;
};

void *ktr_kr__m__init(void *jumpout);
void *ktr_kr__m__multr__m__nr2(void *nr1r__ex__, void *kr__ex__);
void *ktr_kr__m__multr__m__nr1(void *xr2r__ex__, void *envr__m__cpsr__ex__, void *kr__ex__);
void *ktr_kr__m__subr1(void *kr__ex__);
void *ktr_kr__m__zeror__q__(void *kr__ex__);
void *ktr_kr__m__if(void *conseqr__ex__, void *altr__ex__, void *envr__m__cpsr__ex__, void *kr__ex__);
void *ktr_kr__m__throw(void *vr__m__expr__ex__, void *envr__m__cpsr__ex__);
void *ktr_kr__m__let(void *bodyr__ex__, void *envr__m__cpsr__ex__, void *kr__ex__);
void *ktr_kr__m__operand(void *cr__m__cpsr__ex__, void *kr__ex__);
void *ktr_kr__m__rator(void *randr__ex__, void *envr__m__cpsr__ex__, void *kr__ex__);

void valuer__m__ofr__m__cps();
void applyr__m__k();
void applyr__m__env();
void applyr__m__closure();
void emptyr__m__env();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

