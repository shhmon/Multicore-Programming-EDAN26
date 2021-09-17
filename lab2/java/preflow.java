import java.util.Scanner;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.LinkedList;
import java.util.concurrent.locks.ReentrantLock;

import java.io.*;

class Node {
	int	h;
	int	e;
	int	i;
	Node	next;
	LinkedList<Edge>	adj;
	ReentrantLock mutex;

	Node(int i)
	{
		this.i = i;
		adj = new LinkedList<Edge>();
		mutex = new ReentrantLock();
	}

	public void relabel() {
		this.h += 1;
	}
}

class Edge {
	Node	u;
	Node	v;
	int	f;
	int	c;

	Edge(Node u, Node v, int c)
	{
		this.u = u;
		this.v = v;
		this.c = c;
	}

	public int direction(Node n) {
		return n == u ? 1 : -1;
	}

	public int available(int dir) {
		return c - dir * f;
	}

	public Node other(Node n) {
		if (u == n)	
			return v;
		else
			return u;
	}
}

class Work extends Thread {
	Node excess;
	Node s;
	Node t;
	Graph g;

	Work(Node s, Node t, Graph g) {
		this.s = s;
		this.t = t;
		this.g = g;

		excess = g.leave_excess();
	}

	void lock_in_order(Node u, Node v) {
		if (u.i < v.i) {
			u.mutex.lock();
			v.mutex.lock();
		} else {
			v.mutex.lock();
			u.mutex.lock();
		}
	}

	public void run() {
		ListIterator<Edge> iter;
		Edge edg = null;
		Node nei = null;

		int  dir = 1;
		int  flo = 0;
		int  ava = 0;

		// Excess is a node with excess preflow
		while (excess != null) {
			iter = excess.adj.listIterator();

			// For every adjecent edge
			while (iter.hasNext()) {
				// Non-changing properties
				edg = iter.next();
				nei = edg.other(excess);
				dir = edg.direction(excess);

				// Lock both nodes in right order
				lock_in_order(excess, nei);
				
				ava = edg.available(dir);
				flo = Math.min(excess.e, ava);

				if (excess.h > nei.h && ava > 0) {
					break;
				} else {
					excess.mutex.unlock();
					nei.mutex.unlock();
					nei = null;
				}
			}

			// if nei != null, we broke the while loop
			// and still have both locks => push => unlock
			
			// push or relabel
			if (nei != null) {
				excess.e -= flo;
				nei.e += flo;
				edg.f += dir * flo;

				if (nei.e == flo) {
					g.enter_excess(nei);
				}
				
				nei.mutex.unlock();
			} else {
				excess.mutex.lock();
				excess.h += 1;
			}

			// either case above, we have excess lock
			if (excess.e == 0) {
				excess.mutex.unlock();
				excess = g.leave_excess();
			} else {
				excess.mutex.unlock();
			}

		}

		System.out.println(getName() + " done!");
	}
}

class Graph {

	int	s;
	int	t;
	int	n;
	int	m;
	Node	excess;		// list of nodes with excess preflow
	Node	node[];
	Edge	edge[];

	Graph(Node node[], Edge edge[])
	{
		this.node	= node;
		this.n		= node.length;
		this.edge	= edge;
		this.m		= edge.length;
	}

	synchronized void enter_excess(Node u) {
		if (u != node[s] && u != node[t]) {
			u.next = excess;
			excess = u;
		}
	}

	synchronized Node leave_excess() {
		Node t = excess;
		if (t != null) excess = t.next;
		return t;
	}

	int preflow(int s, int t, int nthread) {
		ListIterator<Edge>	iter;
		Edge				edg;
		Node				nei;
		int					dir;

		this.s = s;
		this.t = t;
		node[s].h = n;

		// Initial pushes to neighbours
		iter = node[s].adj.listIterator();
		while (iter.hasNext()) {
			edg = iter.next();
			nei = edg.other(node[s]);
			dir = edg.direction(node[s]);
			nei.e += edg.c;
			edg.f += dir * edg.c;
			enter_excess(nei);
		}

		Work[] work = new Work[nthread];
		
		for (int i = 0; i < nthread; ++i)
			work[i] = new Work(node[s], node[t], this);
		for (int i = 0; i < nthread; ++i)
			work[i].start();
		for (int i = 0; i < nthread; ++i) {
			try {
				work[i].join();
			} catch (Exception e) {
				System.out.println("" + e);
			}
		}

		return node[t].e;
	}
}

class Preflow {
	public static void main(String args[])
	{
		double	begin = System.currentTimeMillis();
		Scanner s = new Scanner(System.in);
		int	n;
		int	m;
		int	i;
		int	u;
		int	v;
		int	c;
		int	f;
		Graph	g;

		n = s.nextInt();
		m = s.nextInt();
		s.nextInt();
		s.nextInt();
		Node[] node = new Node[n];
		Edge[] edge = new Edge[m];

		for (i = 0; i < n; i += 1)
			node[i] = new Node(i);

		for (i = 0; i < m; i += 1) {
			u = s.nextInt();
			v = s.nextInt();
			c = s.nextInt(); 
			edge[i] = new Edge(node[u], node[v], c);
			node[u].adj.addLast(edge[i]);
			node[v].adj.addLast(edge[i]);
		}

		g = new Graph(node, edge);
		f = g.preflow(0, n-1, 2);
		double	end = System.currentTimeMillis();
		System.out.println("t = " + (end - begin) / 1000.0 + " s");
		System.out.println("f = " + f);
	}
}
