#include <assert.h>
#include <ctype.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pthread_barrier.h"

#define PRINT 0 /* enable/disable prints. */

/* the funny do-while next clearly performs one iteration of the loop.
 * if you are really curious about why there is a loop, please check
 * the course book about the C preprocessor where it is explained. it
 * is to avoid bugs and/or syntax errors in case you use the pr in an
 * if-statement without { }.
 *
 */

#if PRINT
#define pr(...)                   \
  do {                            \
    fprintf(stderr, __VA_ARGS__); \
  } while (0)
#else
#define pr(...) /* no effect at all */
#endif

#define MIN(a, b) (((a) <= (b)) ? (a) : (b))

typedef struct graph_t graph_t;
typedef struct node_t node_t;
typedef struct edge_t edge_t;
typedef struct list_t list_t;
typedef struct work_args work_args;
typedef struct action_t action_t;

struct action_t {
  node_t* node;   /* node to act on */
  int relabel;    /* relabel or push */
  int flo;        /* if push, flow amount */
  int dir;        /* if push, direction */
  edge_t* edge;   /* if push, edge */
  action_t* next; /* next in list */
};

struct work_args {
  graph_t* g;
  pthread_barrier_t* bar1;
  pthread_barrier_t* bar2;
  int i;
};

struct list_t {
  edge_t* edge;
  list_t* next;
};

struct node_t {
  int h;        /* height.			*/
  int e;        /* excess flow.			*/
  list_t* edge; /* adjacency list.		*/
  node_t* next; /* with excess preflow.		*/
};

struct edge_t {
  node_t* u; /* one of the two nodes.	*/
  node_t* v; /* the other. 			*/
  int f;     /* flow > 0 if from u to v.	*/
  int c;     /* capacity.			*/
};

struct graph_t {
  int thr;
  int fin;
  int n;     /* nodes.			*/
  int m;     /* edges.			*/
  node_t* v; /* array of n nodes.		*/
  edge_t* e; /* array of m edges.		*/
  node_t* s; /* source.			*/
  node_t* t; /* sink.			*/
  node_t** active;
  action_t** action;
};

static char* progname;

#if PRINT

static int id(graph_t* g, node_t* v) { return v - g->v; }
#endif

void error(const char* fmt, ...) {
  va_list ap;
  char buf[BUFSIZ];

  va_start(ap, fmt);
  vsprintf(buf, fmt, ap);

  if (progname != NULL) fprintf(stderr, "%s: ", progname);

  fprintf(stderr, "error: %s\n", buf);
  exit(1);
}

static int next_int() {
  int x;
  int c;
  x = 0;
  while (isdigit(c = getchar())) x = 10 * x + c - '0';

  return x;
}

static void* xmalloc(size_t s) {
  void* p;
  p = malloc(s);

  if (p == NULL) error("out of memory: malloc(%zu) failed", s);

  return p;
}

static void* xcalloc(size_t n, size_t s) {
  void* p;
  p = xmalloc(n * s);
  memset(p, 0, n * s);
  return p;
}

static void add_edge(node_t* u, edge_t* e) {
  list_t* p;
  p = xmalloc(sizeof(list_t));
  p->edge = e;
  p->next = u->edge;
  u->edge = p;
}

static void connect(node_t* u, node_t* v, int c, edge_t* e) {
  /* connect two nodes by putting a shared (same object)
   * in their adjacency lists.
   *
   */

  e->u = u;
  e->v = v;
  e->c = c;

  add_edge(u, e);
  add_edge(v, e);
}

static graph_t* new_graph(FILE* in, int n, int m, int nthreads) {
  graph_t* g;
  node_t* u;
  node_t* v;
  int i;
  int a;
  int b;
  int c;

  g = xmalloc(sizeof(graph_t));

  g->n = n;
  g->m = m;
  g->thr = nthreads;
  g->fin = 0;

  g->v = xcalloc(n, sizeof(node_t));
  g->e = xcalloc(m, sizeof(edge_t));

  g->s = &g->v[0];
  g->t = &g->v[n - 1];
  g->action = xcalloc(nthreads, sizeof(action_t*));
  g->active = xcalloc(nthreads, sizeof(node_t*));

  for (i = 0; i < m; i += 1) {
    a = next_int();
    b = next_int();
    c = next_int();
    u = &g->v[a];
    v = &g->v[b];
    connect(u, v, c, g->e + i);
  }

  return g;
}

static node_t* other(node_t* u, edge_t* e) {
  if (u == e->u)
    return e->v;
  else
    return e->u;
}

static int direction(node_t* u, edge_t* e) { return (u == e->u) ? 1 : -1; }

static int available(edge_t* e, int dir) { return e->c - dir * e->f; }

static void add_active(graph_t* g, node_t* node, int thr) {
  if (node != g->t && node != g->s) {
    node->next = g->active[thr];
    g->active[thr] = node;
  }
}

static node_t* pop_active(graph_t* g, int thr) {
  node_t* a;
  a = g->active[thr];
  if (a != NULL) g->active[thr] = a->next;
  return a;
}

static void queue_action(graph_t* g, action_t* action, int thr) {
  action->next = g->action[thr];
  g->action[thr] = action;
}

static action_t* pop_action(graph_t* g, int thr) {
  action_t* a;
  a = g->action[thr];
  if (a != NULL) g->action[thr] = a->next;
  return a;
}

static void* work(void* arg) {
  node_t* nei;
  edge_t* edg;
  list_t* adj;
  action_t* action;
  node_t* active;

  int dir;
  int ava;
  int flo;

  work_args* args = (work_args*)arg;
  graph_t* g = args->g;

  while (!g->fin) {
    active = pop_active(g, args->i);

    while (active != NULL) {
      adj = active->edge;

      while (adj != NULL && active->e > 0) {
        edg = adj->edge;
        adj = adj->next;

        // Get direction and other node
        nei = other(active, edg);
        dir = direction(active, edg);
        ava = available(edg, dir);

        // Can push to neighbour, queue a push
        if (active->h > nei->h && ava > 0) {
          flo = MIN(active->e, ava);
          active->e -= flo;

          // Allocate and init a new action
          action = xmalloc(sizeof(action_t));
          action->dir = dir;
          action->flo = flo;
          action->edge = edg;
          action->node = nei;
          action->relabel = 0;
          queue_action(g, action, args->i);
        }
      }

      // All edges checked, relabel if excess > 0
      if (active->e > 0) {
        action = xmalloc(sizeof(action_t));
        action->relabel = 1;
        action->node = active;
        queue_action(g, action, args->i);
      }

      active = pop_active(g, args->i);
    }

    pthread_barrier_wait(args->bar1);
    pthread_barrier_wait(args->bar2);
  }

  return 0;
}

static int preflow(graph_t* g) {
  node_t* src;
  node_t* nei;
  edge_t* edg;
  list_t* adj;
  int dir;
  int i = 0;
  int d = 0;

  src = g->s;
  src->h = g->n;

  adj = src->edge;

  // Initial push from source
  while (adj != NULL) {
    edg = adj->edge;
    adj = adj->next;
    nei = other(src, edg);
    dir = direction(src, edg);
    edg->f += dir * edg->c;
    nei->e += edg->c;

    add_active(g, nei, i);

    if (i == g->thr - 1)
      i = 0;
    else
      i += 1;
  }

  pthread_barrier_t barr[2];
  pthread_barrier_init(&barr[0], NULL, g->thr + 1);
  pthread_barrier_init(&barr[1], NULL, g->thr + 1);

  pthread_t thread[g->thr];
  work_args args[g->thr];

  for (i = 0; i < g->thr; i += 1) {
    args[i].bar1 = &barr[0];
    args[i].bar2 = &barr[1];
    args[i].g = g;
    args[i].i = i;

    if (pthread_create(&thread[i], NULL, work, &args[i]) != 0)
      error("pthread_create failed");
  }

  while (!g->fin) {
    d = 0;
    pthread_barrier_wait(&barr[0]);

    for (i = 0; i < g->thr; i += 1) {
      action_t* action = pop_action(g, i);

      while (action != NULL) {
        if (action->relabel) {
          action->node->h += 1;
          add_active(g, action->node, i);
        } else {
          action->node->e += action->flo;
          action->edge->f += action->flo * action->dir;

          if (action->node->e == action->flo) {
            add_active(g, action->node, i);
          }
        }

        free(action);
        action = pop_action(g, i);
      }

      if (g->active[i] == NULL) d += 1;
    }

    if (d == g->thr) g->fin = 1;

    pthread_barrier_wait(&barr[1]);
  }

  for (i = 0; i < g->thr; i += 1)
    if (pthread_join(thread[i], NULL) != 0) error("pthread_join failed");

  return g->t->e;
}

static void free_graph(graph_t* g) {
  int i;
  list_t* p;
  list_t* q;

  for (i = 0; i < g->n; i += 1) {
    p = g->v[i].edge;
    while (p != NULL) {
      q = p->next;
      free(p);
      p = q;
    }
  }

  free(g->v);
  free(g->e);
  free(g);
}

int main(int argc, char* argv[]) {
  FILE* in;   /* input file set to stdin	*/
  graph_t* g; /* undirected graph. 		*/
  int f;      /* output from preflow.		*/
  int n;      /* number of nodes.		*/
  int m;      /* number of edges.		*/

  progname = argv[0]; /* name is a string in argv[0]. */

  in = stdin; /* same as System.in in Java.	*/

  n = next_int();
  m = next_int();

  /* skip C and P from the 6railwayplanning lab in EDAF05 */
  next_int();
  next_int();

  g = new_graph(in, n, m, 2);

  fclose(in);

  f = preflow(g);

  printf("f = %d\n", f);

  free_graph(g);

  return 0;
}
