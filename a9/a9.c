#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "a9.h"

void *ktr_kr__m__init(void *jumpout) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _kr__m__init_kt;
  _data->u._kr__m__init._jumpout = jumpout;
  return (void *)_data;
}

void *ktr_kr__m__multr__m__nr2(void *nr1r__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _kr__m__multr__m__nr2_kt;
  _data->u._kr__m__multr__m__nr2._nr1r__ex__ = nr1r__ex__;
  _data->u._kr__m__multr__m__nr2._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_kr__m__multr__m__nr1(void *xr2r__ex__, void *envr__m__cpsr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _kr__m__multr__m__nr1_kt;
  _data->u._kr__m__multr__m__nr1._xr2r__ex__ = xr2r__ex__;
  _data->u._kr__m__multr__m__nr1._envr__m__cpsr__ex__ = envr__m__cpsr__ex__;
  _data->u._kr__m__multr__m__nr1._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_kr__m__subr1(void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _kr__m__subr1_kt;
  _data->u._kr__m__subr1._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_kr__m__zeror__q__(void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _kr__m__zeror__q___kt;
  _data->u._kr__m__zeror__q__._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_kr__m__if(void *conseqr__ex__, void *altr__ex__, void *envr__m__cpsr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _kr__m__if_kt;
  _data->u._kr__m__if._conseqr__ex__ = conseqr__ex__;
  _data->u._kr__m__if._altr__ex__ = altr__ex__;
  _data->u._kr__m__if._envr__m__cpsr__ex__ = envr__m__cpsr__ex__;
  _data->u._kr__m__if._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_kr__m__throw(void *vr__m__expr__ex__, void *envr__m__cpsr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _kr__m__throw_kt;
  _data->u._kr__m__throw._vr__m__expr__ex__ = vr__m__expr__ex__;
  _data->u._kr__m__throw._envr__m__cpsr__ex__ = envr__m__cpsr__ex__;
  return (void *)_data;
}

void *ktr_kr__m__let(void *bodyr__ex__, void *envr__m__cpsr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _kr__m__let_kt;
  _data->u._kr__m__let._bodyr__ex__ = bodyr__ex__;
  _data->u._kr__m__let._envr__m__cpsr__ex__ = envr__m__cpsr__ex__;
  _data->u._kr__m__let._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_kr__m__operand(void *cr__m__cpsr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _kr__m__operand_kt;
  _data->u._kr__m__operand._cr__m__cpsr__ex__ = cr__m__cpsr__ex__;
  _data->u._kr__m__operand._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_kr__m__rator(void *randr__ex__, void *envr__m__cpsr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _kr__m__rator_kt;
  _data->u._kr__m__rator._randr__ex__ = randr__ex__;
  _data->u._kr__m__rator._envr__m__cpsr__ex__ = envr__m__cpsr__ex__;
  _data->u._kr__m__rator._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *envrr_emptyr__m__env() {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__env_envr;
  return (void *)_data;
}

void *envrr_extendr__m__env(void *valuer__ex__, void *envr__m__cpsr__ex__) {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _extendr__m__env_envr;
  _data->u._extendr__m__env._valuer__ex__ = valuer__ex__;
  _data->u._extendr__m__env._envr__m__cpsr__ex__ = envr__m__cpsr__ex__;
  return (void *)_data;
}

void *closr_closure(void *body, void *envr__m__cps) {
clos* _data = (clos*)malloc(sizeof(clos));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _closure_clos;
  _data->u._closure._body = body;
  _data->u._closure._envr__m__cps = envr__m__cps;
  return (void *)_data;
}

void *exprr_const(void *cexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _const_expr;
  _data->u._const._cexp = cexp;
  return (void *)_data;
}

void *exprr_var(void *n) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _var_expr;
  _data->u._var._n = n;
  return (void *)_data;
}

void *exprr_if(void *test, void *conseq, void *alt) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _if_expr;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *)_data;
}

void *exprr_mult(void *nexpr1, void *nexpr2) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mult_expr;
  _data->u._mult._nexpr1 = nexpr1;
  _data->u._mult._nexpr2 = nexpr2;
  return (void *)_data;
}

void *exprr_subr1(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_expr;
  _data->u._subr1._nexp = nexp;
  return (void *)_data;
}

void *exprr_zero(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zero_expr;
  _data->u._zero._nexp = nexp;
  return (void *)_data;
}

void *exprr_letcc(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letcc_expr;
  _data->u._letcc._body = body;
  return (void *)_data;
}

void *exprr_throw(void *kexp, void *vexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throw_expr;
  _data->u._throw._kexp = kexp;
  _data->u._throw._vexp = vexp;
  return (void *)_data;
}

void *exprr_let(void *exp, void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _let_expr;
  _data->u._let._exp = exp;
  _data->u._let._body = body;
  return (void *)_data;
}

void *exprr_lambda(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _lambda_expr;
  _data->u._lambda._body = body;
  return (void *)_data;
}

void *exprr_app(void *rator, void *rand) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _app_expr;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *)_data;
}

int main()
{
vor__m__envr__m__cps = (void *)envrr_emptyr__m__env();
vor__m__tor__m__eval = (void *)exprr_let(exprr_lambda(exprr_lambda(exprr_if(exprr_zero(exprr_var((void *)0)),exprr_const((void *)1),exprr_mult(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_subr1(exprr_var((void *)0))))))),exprr_mult(exprr_letcc(exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_throw(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_const((void *)4))))),exprr_const((void *)5)));
pc = &valuer__m__ofr__m__cps;
mount_tram();
printf("Fact 5: %d\n", (int)akr__m__v);}

void emptyr__m__env()
{
return(envrr_emptyr__m__env());
}

void applyr__m__closure()
{
clos* _c = (clos*)acr__m__cr__m__cps;
switch (_c->tag) {
case _closure_clos: {
void *body = _c->u._closure._body;
void *envr__m__cps = _c->u._closure._envr__m__cps;
vor__m__k = (void *)acr__m__kr__ex__;
vor__m__tor__m__eval = (void *)body;
vor__m__envr__m__cps = (void *)envrr_extendr__m__env(acr__m__a,envr__m__cps);
pc = &valuer__m__ofr__m__cps;
break; }
}
}

void applyr__m__env()
{
envr* _c = (envr*)aer__m__env;
switch (_c->tag) {
case _extendr__m__env_envr: {
void *valuer__ex__ = _c->u._extendr__m__env._valuer__ex__;
void *envr__m__cpsr__ex__ = _c->u._extendr__m__env._envr__m__cpsr__ex__;
if((aer__m__y == 0)) {
  akr__m__k = (void *)aer__m__kr__ex__;
akr__m__v = (void *)valuer__ex__;
pc = &applyr__m__k;

} else {
  aer__m__env = (void *)envr__m__cpsr__ex__;
aer__m__y = (void *)(void *)((int)aer__m__y - 1);
pc = &applyr__m__env;

}
break; }
case _emptyr__m__env_envr: {
fprintf(stderr, "unbound identifier");
 exit(1);
break; }
}
}

void applyr__m__k()
{
kt* _c = (kt*)akr__m__k;
switch (_c->tag) {
case _kr__m__init_kt: {
void *jumpout = _c->u._kr__m__init._jumpout;
_trstr *trstr = (_trstr *)jumpout;
longjmp(*trstr->jmpbuf, 1);
break; }
case _kr__m__multr__m__nr2_kt: {
void *nr1r__ex__ = _c->u._kr__m__multr__m__nr2._nr1r__ex__;
void *kr__ex__ = _c->u._kr__m__multr__m__nr2._kr__ex__;
akr__m__k = (void *)kr__ex__;
akr__m__v = (void *)(void *)((int)nr1r__ex__ * (int)akr__m__v);
pc = &applyr__m__k;
break; }
case _kr__m__multr__m__nr1_kt: {
void *xr2r__ex__ = _c->u._kr__m__multr__m__nr1._xr2r__ex__;
void *envr__m__cpsr__ex__ = _c->u._kr__m__multr__m__nr1._envr__m__cpsr__ex__;
void *kr__ex__ = _c->u._kr__m__multr__m__nr1._kr__ex__;
vor__m__k = (void *)ktr_kr__m__multr__m__nr2(akr__m__v,kr__ex__);
vor__m__tor__m__eval = (void *)xr2r__ex__;
vor__m__envr__m__cps = (void *)envr__m__cpsr__ex__;
pc = &valuer__m__ofr__m__cps;
break; }
case _kr__m__subr1_kt: {
void *kr__ex__ = _c->u._kr__m__subr1._kr__ex__;
akr__m__k = (void *)kr__ex__;
akr__m__v = (void *)(void *)((int)akr__m__v - 1);
pc = &applyr__m__k;
break; }
case _kr__m__zeror__q___kt: {
void *kr__ex__ = _c->u._kr__m__zeror__q__._kr__ex__;
akr__m__k = (void *)kr__ex__;
akr__m__v = (void *)(akr__m__v == 0);
pc = &applyr__m__k;
break; }
case _kr__m__if_kt: {
void *conseqr__ex__ = _c->u._kr__m__if._conseqr__ex__;
void *altr__ex__ = _c->u._kr__m__if._altr__ex__;
void *envr__m__cpsr__ex__ = _c->u._kr__m__if._envr__m__cpsr__ex__;
void *kr__ex__ = _c->u._kr__m__if._kr__ex__;
if(akr__m__v) {
  vor__m__k = (void *)kr__ex__;
vor__m__tor__m__eval = (void *)conseqr__ex__;
vor__m__envr__m__cps = (void *)envr__m__cpsr__ex__;
pc = &valuer__m__ofr__m__cps;

} else {
  vor__m__k = (void *)kr__ex__;
vor__m__tor__m__eval = (void *)altr__ex__;
vor__m__envr__m__cps = (void *)envr__m__cpsr__ex__;
pc = &valuer__m__ofr__m__cps;

}
break; }
case _kr__m__throw_kt: {
void *vr__m__expr__ex__ = _c->u._kr__m__throw._vr__m__expr__ex__;
void *envr__m__cpsr__ex__ = _c->u._kr__m__throw._envr__m__cpsr__ex__;
vor__m__k = (void *)akr__m__v;
vor__m__tor__m__eval = (void *)vr__m__expr__ex__;
vor__m__envr__m__cps = (void *)envr__m__cpsr__ex__;
pc = &valuer__m__ofr__m__cps;
break; }
case _kr__m__let_kt: {
void *bodyr__ex__ = _c->u._kr__m__let._bodyr__ex__;
void *envr__m__cpsr__ex__ = _c->u._kr__m__let._envr__m__cpsr__ex__;
void *kr__ex__ = _c->u._kr__m__let._kr__ex__;
vor__m__k = (void *)kr__ex__;
vor__m__tor__m__eval = (void *)bodyr__ex__;
vor__m__envr__m__cps = (void *)envrr_extendr__m__env(akr__m__v,envr__m__cpsr__ex__);
pc = &valuer__m__ofr__m__cps;
break; }
case _kr__m__operand_kt: {
void *cr__m__cpsr__ex__ = _c->u._kr__m__operand._cr__m__cpsr__ex__;
void *kr__ex__ = _c->u._kr__m__operand._kr__ex__;
acr__m__kr__ex__ = (void *)kr__ex__;
acr__m__cr__m__cps = (void *)cr__m__cpsr__ex__;
acr__m__a = (void *)akr__m__v;
pc = &applyr__m__closure;
break; }
case _kr__m__rator_kt: {
void *randr__ex__ = _c->u._kr__m__rator._randr__ex__;
void *envr__m__cpsr__ex__ = _c->u._kr__m__rator._envr__m__cpsr__ex__;
void *kr__ex__ = _c->u._kr__m__rator._kr__ex__;
vor__m__k = (void *)ktr_kr__m__operand(akr__m__v,kr__ex__);
vor__m__tor__m__eval = (void *)randr__ex__;
vor__m__envr__m__cps = (void *)envr__m__cpsr__ex__;
pc = &valuer__m__ofr__m__cps;
break; }
}
}

void valuer__m__ofr__m__cps()
{
expr* _c = (expr*)vor__m__tor__m__eval;
switch (_c->tag) {
case _const_expr: {
void *constr__m__expr = _c->u._const._cexp;
akr__m__k = (void *)vor__m__k;
akr__m__v = (void *)constr__m__expr;
pc = &applyr__m__k;
break; }
case _mult_expr: {
void *xr1 = _c->u._mult._nexpr1;
void *xr2 = _c->u._mult._nexpr2;
vor__m__k = (void *)ktr_kr__m__multr__m__nr1(xr2,vor__m__envr__m__cps,vor__m__k);
vor__m__tor__m__eval = (void *)xr1;
pc = &valuer__m__ofr__m__cps;
break; }
case _subr1_expr: {
void *x = _c->u._subr1._nexp;
vor__m__k = (void *)ktr_kr__m__subr1(vor__m__k);
vor__m__tor__m__eval = (void *)x;
pc = &valuer__m__ofr__m__cps;
break; }
case _zero_expr: {
void *x = _c->u._zero._nexp;
vor__m__k = (void *)ktr_kr__m__zeror__q__(vor__m__k);
vor__m__tor__m__eval = (void *)x;
pc = &valuer__m__ofr__m__cps;
break; }
case _if_expr: {
void *test = _c->u._if._test;
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
vor__m__k = (void *)ktr_kr__m__if(conseq,alt,vor__m__envr__m__cps,vor__m__k);
vor__m__tor__m__eval = (void *)test;
pc = &valuer__m__ofr__m__cps;
break; }
case _letcc_expr: {
void *body = _c->u._letcc._body;
vor__m__tor__m__eval = (void *)body;
vor__m__envr__m__cps = (void *)envrr_extendr__m__env(vor__m__k,vor__m__envr__m__cps);
pc = &valuer__m__ofr__m__cps;
break; }
case _throw_expr: {
void *kr__m__exp = _c->u._throw._kexp;
void *vr__m__exp = _c->u._throw._vexp;
vor__m__k = (void *)ktr_kr__m__throw(vr__m__exp,vor__m__envr__m__cps);
vor__m__tor__m__eval = (void *)kr__m__exp;
pc = &valuer__m__ofr__m__cps;
break; }
case _let_expr: {
void *e = _c->u._let._exp;
void *body = _c->u._let._body;
vor__m__k = (void *)ktr_kr__m__let(body,vor__m__envr__m__cps,vor__m__k);
vor__m__tor__m__eval = (void *)e;
pc = &valuer__m__ofr__m__cps;
break; }
case _var_expr: {
void *y = _c->u._var._n;
aer__m__kr__ex__ = (void *)vor__m__k;
aer__m__env = (void *)vor__m__envr__m__cps;
aer__m__y = (void *)y;
pc = &applyr__m__env;
break; }
case _lambda_expr: {
void *body = _c->u._lambda._body;
akr__m__k = (void *)vor__m__k;
akr__m__v = (void *)closr_closure(body,vor__m__envr__m__cps);
pc = &applyr__m__k;
break; }
case _app_expr: {
void *rator = _c->u._app._rator;
void *rand = _c->u._app._rand;
vor__m__k = (void *)ktr_kr__m__rator(rand,vor__m__envr__m__cps,vor__m__k);
vor__m__tor__m__eval = (void *)rator;
pc = &valuer__m__ofr__m__cps;
break; }
}
}

int mount_tram ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
vor__m__k= (void *)ktr_kr__m__init(dismount);
for(;;) {
pc();
}
}
return 0;
}
