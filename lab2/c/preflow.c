#include <assert.h>
#include <ctype.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

struct work_args {
  graph_t* g;
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
  pthread_mutex_t mutex;
};

struct edge_t {
  node_t* u; /* one of the two nodes.	*/
  node_t* v; /* the other. 			*/
  int f;     /* flow > 0 if from u to v.	*/
  int c;     /* capacity.			*/
};

struct graph_t {
  int n;          /* nodes.			*/
  int m;          /* edges.			*/
  node_t* v;      /* array of n nodes.		*/
  edge_t* e;      /* array of m edges.		*/
  node_t* s;      /* source.			*/
  node_t* t;      /* sink.			*/
  node_t* excess; /* nodes with e > 0 except s,t.	*/
  pthread_mutex_t mutex;
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

static graph_t* new_graph(FILE* in, int n, int m) {
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

  g->v = xcalloc(n, sizeof(node_t));
  g->e = xcalloc(m, sizeof(edge_t));

  g->s = &g->v[0];
  g->t = &g->v[n - 1];
  g->excess = NULL;

  for (i = 0; i < m; i += 1) {
    a = next_int();
    b = next_int();
    c = next_int();
    u = &g->v[a];
    v = &g->v[b];
    connect(u, v, c, g->e + i);
  }

  // pthread_mutexattr_t attr;
  // pthread_mutexattr_init(&attr);
  // pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);

  pthread_mutex_init(&g->mutex, NULL);

  for (i = 0; i < n; i += 1) {
    pthread_mutex_init(&g->v[i].mutex, NULL);
  }

  return g;
}

static void enter_excess(graph_t* g, node_t* v) {
  pthread_mutex_lock(&g->mutex);
  if (v != g->t && v != g->s) {  // && v != g->s
    v->next = g->excess;
    g->excess = v;
  }
  pthread_mutex_unlock(&g->mutex);
}

static node_t* leave_excess(graph_t* g) {
  node_t* v;
  pthread_mutex_lock(&g->mutex);
  v = g->excess;
  if (v != NULL) g->excess = v->next;
  pthread_mutex_unlock(&g->mutex);
  return v;
}

static node_t* other(node_t* u, edge_t* e) {
  if (u == e->u)
    return e->v;
  else
    return e->u;
}

void lock_in_order(node_t* u, node_t* v) {
  if (u < v) {
    pthread_mutex_lock(&u->mutex);
    pthread_mutex_lock(&v->mutex);
  } else {
    pthread_mutex_lock(&v->mutex);
    pthread_mutex_lock(&u->mutex);
  }
}

static int direction(node_t* u, edge_t* e) { return (u == e->u) ? 1 : -1; }

static int available(edge_t* e, int dir) { return e->c - dir * e->f; }

static void* work(void* arg) {
  pr("<--- thread started --->\n");

  node_t* nei;
  edge_t* edg;
  list_t* adj;
  int dir;
  int ava;
  int flo;

  graph_t* g = ((work_args*)arg)->g;

  node_t* excess = leave_excess(g);
  while (excess != NULL) {
    adj = excess->edge;

    while (adj != NULL) {
      edg = adj->edge;
      adj = adj->next;

      // Get direction and other node
      nei = other(excess, edg);
      dir = direction(excess, edg);

      lock_in_order(excess, nei);

      ava = available(edg, dir);

      if (excess->h > nei->h && ava > 0) {
        break;
      } else {
        pthread_mutex_unlock(&excess->mutex);
        pthread_mutex_unlock(&nei->mutex);
        nei = NULL;
      }
    }

    // Push or relabel
    if (nei != NULL) {
      flo = MIN(excess->e, ava);
      excess->e -= flo;
      nei->e += flo;
      edg->f += dir * flo;

      if (nei->e == flo) {
        enter_excess(g, nei);
      }

      pthread_mutex_unlock(&nei->mutex);
    } else {
      pthread_mutex_lock(&excess->mutex);
      excess->h += 1;
    }

    if (excess->e == 0) {
      pthread_mutex_unlock(&excess->mutex);
      excess = leave_excess(g);
    } else {
      pthread_mutex_unlock(&excess->mutex);
    }
  }

  pr("<--- thread done --->\n");

  return 0;
}

static int preflow(graph_t* g, int nthread) {
  node_t* src;
  node_t* nei;
  edge_t* edg;
  list_t* adj;
  int dir;
  int i;

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
    enter_excess(g, nei);
  }

  pthread_t thread[nthread];
  work_args arg = {g};

  for (i = 0; i < nthread; i += 1)
    if (pthread_create(&thread[i], NULL, work, &arg) != 0)
      error("pthread_create failed");

  for (i = 0; i < nthread; i += 1)
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

    pthread_mutex_destroy(&g->v[i].mutex);
  }

  pthread_mutex_destroy(&g->mutex);
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

  g = new_graph(in, n, m);

  fclose(in);

  f = preflow(g, 4);

  printf("f = %d\n", f);

  free_graph(g);

  return 0;
}
