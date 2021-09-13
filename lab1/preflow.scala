import scala.util._
import java.util.Scanner
import java.io._
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.{Await,ExecutionContext,Future,Promise}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.io._

case class Flow(f: Int)
case class Debug(debug: Boolean)
case class Control(control:ActorRef)
case class Source(n: Int)
case class Push(f: Int, h: Int, edge: Edge)
case class Accept(amt: Int)
case class Decline(amt: Int)

case object Print
case object Start
case object Excess
case object Maxflow
case object Sink

class Edge(var u: ActorRef, var v: ActorRef, var c: Int) {
	var	f = 0

	def direction(r: ActorRef): Int = if (r == u) 1 else -1

	def available(dir: Int): Int = c - dir * f
}

class Node(val index: Int) extends Actor {
	var	e = 0;						/* excess preflow. */
	var	h = 0;						/* height. */
	var	control:ActorRef = null		/* controller to report to when e is zero. */
	var	source:Boolean	= false		/* true if we are the source. */
	var	sink:Boolean	= false		/* true if we are the sink. */
	var	edges: List[Edge] = Nil		/* adjacency list with edge objects shared with other nodes. */
	var	debug = false				/* to enable printing. */

	var req = 0
	var res = 0
	
	def min(a:Int, b:Int) : Int = { if (a < b) a else b }

	def id: String = "@" + index;

	def other(a:Edge, u:ActorRef) : ActorRef = {
		if (u == a.u) a.v
		else a.u
	}

	def status: Unit = { if (debug) println(id + " e = " + e + ", h = " + h) }

	def enter(func: String): Unit = {
		if (debug) {
			println(id + " enters " + func);
			status
		} 
	}

	def exit(func: String): Unit = {
		if (debug) {
			println(id + " exits " + func);
			status
		}
	}

	def relabel : Unit = {
		h += 1
	}

	def discharge: Unit = {
		var edg: List[Edge] = edges.splitAt(req)._2
		var cur: Edge = null
		var nei: ActorRef = null

		var dir = 0
		var ava = 0
		var flo = 0

		while (edg != Nil) {
			cur = edg.head
			edg = edg.tail
			nei = other(cur, self)

			// Available flow in direction
			dir = cur.direction(self)
			ava = cur.available(dir)
			flo = min(ava, e)

			if (flo > 0) {
				e -= flo
				nei ! Push(dir * flo, h, cur)
				req += 1
			}
			
			if (ava == 0) {
				req += 1
				res += 1
			}
		}
	}

	def next: Unit = {
		// All requests answered
		if (req == res) {
			if (e > 0) {
				// All edges checked
				if (req == edges.length) {
					req = 0
					res = 0
					relabel
				}

				discharge
			
			// No excess but last response
			} else {
				req = 0
				res = 0
			}
		}
	}

	def receive = {

	case Push(f:Int, h:Int, edge:Edge) => {
		if (h > this.h) {
			e += f.abs
			edge.f += f
			sender ! Accept(f.abs)

			if (sink || source) control ! Flow(e)

			if (!sink) next

		} else {
			sender ! Decline(f.abs)
		}
	}

	case Accept(amt: Int) => {
		res += 1
		next
	}

	case Decline(amt: Int) => {
		e += amt
		res += 1
		next
	}

	case Start => {
		edges.foreach(e => other(e, self) ! Push(e.c, h, e))
		res = -edges.length
	}

	case Sink	=> { sink = true }

	case Source(n:Int)	=> {
		h = n;
		source = true
		e = -edges.map(e => e.c).sum
		sender ! Flow(e)
	}

	case Excess => { sender ! Flow(e) /* send our current excess preflow to actor that asked for it. */ }

	case edge:Edge => { this.edges = (edge :: this.edges).sortBy(e => -e.direction(self)) /* put this edge first in the adjacency-list. */ }

	case Control(control:ActorRef)	=> this.control = control
	
	case Debug(debug: Boolean)	=> this.debug = debug

	case Print => status

	case _		=> {
		println("" + index + " received an unknown message" + _) }
		assert(false)
	}

}


class Preflow extends Actor {
	var	s	= 0;						/* index of source node. */
	var	t	= 0;						/* index of sink node. */
	var	n	= 0;						/* number of vertices in the graph. */
	var	edges:Array[Edge]	 = null		/* edges in the graph. */
	var	nodes:Array[ActorRef] = null	/* vertices in the graph. */
	var	ret:ActorRef 		 = null		/* Actor to send result to. */
	var se	= 0;
	var te = 0;

	def receive = {

	case nodes:Array[ActorRef]	=> {
		this.nodes = nodes
		n = nodes.size
		t = n-1

		for (node <- nodes) {
			node ! Control(self)
			if (node == nodes(s)) node ! Source(n)
			if (node == nodes(t)) node ! Sink
		}
	}

	case edges:Array[Edge] => this.edges = edges

	case Flow(f:Int) => {
		if (sender == nodes(s)) se = f
		if (sender == nodes(t)) te = f
		if (se.abs == te.abs) ret ! te
	}


	case Maxflow => {
		ret = sender
		nodes(s) ! Start
	}
	}
}

object main extends App {
	implicit val t = Timeout(600 seconds);

	val	begin = System.currentTimeMillis()
	val system = ActorSystem("Main")
	val control = system.actorOf(Props[Preflow], name = "control")

	var	n = 0;
	var	m = 0;
	var edges: Array[Edge] = null
	var	nodes: Array[ActorRef] = null

	val	s = new Scanner(System.in);

	n = s.nextInt
	m = s.nextInt

	/* next ignore c and p from 6railwayplanning */
	s.nextInt
	s.nextInt

	nodes = new Array[ActorRef](n)

	for (i <- 0 to n-1)
		nodes(i) = system.actorOf(Props(new Node(i)), name = "v" + i)

	edges = new Array[Edge](m)

	for (i <- 0 to m-1) {
		val fr = s.nextInt
		val to = s.nextInt
		val ca = s.nextInt

		edges(i) = new Edge(nodes(fr), nodes(to), ca)

		nodes(fr) ! edges(i)
		nodes(to) ! edges(i)
	}

	control ! nodes
	control ! edges

	val flow = control ? Maxflow
	val f = Await.result(flow, t.duration)

	println("f = " + f)

	system.stop(control);
	system.terminate()

	val	end = System.currentTimeMillis()

	println("t = " + (end - begin) / 1000.0 + " s")
}
