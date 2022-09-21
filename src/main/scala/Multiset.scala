
class Multiset[K](val counts: Map[K, Int])(implicit ord: Ordering[K]) {

  require(!counts.exists { case (_, n) => n <= 0 }, "Counts must be strictly positive")

  def apply(k: K): Int = counts.getOrElse(k, 0)

  def keySet: Set[K] = counts.keySet

  def isEmpty: Boolean = counts.isEmpty

  def toSeq: Seq[K] = counts.flatMap { case (k, n) => Seq.fill(n)(k) }.toSeq

  def --(other: Multiset[K]): Multiset[K] = {
    val ks = (counts.keySet ++ other.counts.keySet).toSeq
    def ns(s: Multiset[K]): Seq[Int] = ks.map(s.counts.get(_).getOrElse(0))
    Multiset(ks.zip((ns(this), ns(other)).zipped.map { _ - _ }).filter { case (k, n) => n > 0 })
  }

  def ++(other: Multiset[K]): Multiset[K] = {
    val ks = (counts.keySet ++ other.counts.keySet).toSeq
    def ns(s: Multiset[K]): Seq[Int] = ks.map(s.counts.get(_).getOrElse(0))
    Multiset(ks.zip((ns(this), ns(other)).zipped.map { _ + _ }))
  }

  // Subtracting a key is defined to mean dropping the key from the multiset, regardless of its
  // count.
  def -(k: K): Multiset[K] = Multiset(counts - k)

  // Subtracting a set of keys is defined to mean dropping from the multiset all keys that in that
  // set, regardless of their counts.
  def --(other: Set[K]): Multiset[K] =
    Multiset(counts.filterKeys(!other.contains(_)))

  // Adding a set of keys is defined to mean ensuring that every key in that set is either already
  // in the multiset or is added with a count of 1.
  def ++(other: Set[K]): Multiset[K] =
    this ++ Multiset(other.filterNot(this.keySet.contains).map((_, 1)))

  def filter(p: ((K, Int)) => Boolean): Multiset[K] =
    Multiset(counts.filter(p))

  def filterKeys(p: K => Boolean): Multiset[K] =
    Multiset(counts.filterKeys(p))

  override def equals(other: Any): Boolean = {
    if (other.isInstanceOf[Multiset[K]]) {
      counts == other.asInstanceOf[Multiset[K]].counts
    } else if (other.isInstanceOf[Seq[K]]) {
      this == Multiset(other.asInstanceOf[Seq[K]].groupBy(identity).mapValues(_.length))
    } else {
      false
    }
  }

  override def toString: String =
    f"Multiset(${counts.toSeq.map { case (k, n) => f"${k} -> ${n}" }.mkString(", ")})"
}


object Multiset {
  // Multisets should have strictly positive counts at all times (keys with a count of zero should
  // be dropped).

  def apply[T](counts: Map[T, Int])(implicit ord: Ordering[T]): Multiset[T] =
    new Multiset(counts.filter { case (_, n) => n > 0 })

  def apply[T](entries: (T, Int)*)(implicit ord: Ordering[T]): Multiset[T] =
    Multiset(Map(entries: _*))

  def apply[T](entries: Iterable[(T, Int)])(implicit ord: Ordering[T]): Multiset[T] =
    Multiset(entries.toMap)

  // TODO: If these constructors are permitted, need to distinguish types from iterables and varargs of (T, Int).
  // Constructing from a set of keys means creating a Multiset with a count of 1 for each key.
  // def apply[T](keys: Set[T])(implicit ord: Ordering[T]): Multiset[T] =
  //   Multiset(keys.map((_, 1)))

  def fromKeySet[T](keys: Set[T])(implicit ord: Ordering[T]): Multiset[T] =
    Multiset(keys.groupBy(identity).mapValues(_.size))

  // TODO: Allow constructing from a Seq or vararg of keys?
}

class MultisetTest {
  test("Cannot construct with non-positive counts") {
    intercept[IllegalArgumentException] {
      new Multiset(Map(1 -> 0))
    }
    assert(Multiset(Map(1 -> 0)).isEmpty)
    assert(Multiset(1 -> 0).isEmpty)
    assert(Multiset(Seq(1 -> 0)).isEmpty)
  }

  test("++ is correct and commutative") {
    val empty = Map[Int, Int]()
    for ((m1, m2, sum) <- Seq(
        (empty, empty, empty),
        (empty, Map(0 -> 1), Map(0 -> 1)),
        (empty, Map(0 -> 2), Map(0 -> 2)),
        (Map(0 -> 1), Map(0 -> 2), Map(0 -> 3)),
        (Map(0 -> 1, 1 -> 1), Map(0 -> 1, 1 -> 1, 2 -> 1), Map(0 -> 2, 1 -> 2, 2 -> 1)),
        (Map(0 -> 1, 1 -> 0), Map(0 -> 0, 1 -> 1, 2 -> 0), Map(0 -> 1, 1 -> 1, 2 -> 0)),
      )) {
      assert(Multiset(m1) ++ Multiset(m2) == Multiset(sum))
      assert(Multiset(m2) ++ Multiset(m1) == Multiset(sum))
    }
  }

  test("set subtraction") {
    val empty = Map[Int, Int]()
    val emptySet = Set[Int]()
    for ((m, s, res) <- Seq(
        (empty, emptySet, empty),
        (Map(0 -> 1), emptySet, Map(0 -> 1)),
        (Map(0 -> 2), emptySet, Map(0 -> 2)),
        (Map(0 -> 1), Set(0), empty),
        (Map(0 -> 1, 1 -> 1), Set(0), Map(1 -> 1)),
        (Map(0 -> 1, 1 -> 0, 2 -> 1), Set(0, 2), Map(1 -> 0)),
      )) {
      assert(Multiset(m) -- s == Multiset(res))
    }
  }

  test("fromKeySet") {
    assert(Multiset.fromKeySet(Set[Int]()) == Multiset[Int]())
    assert(Multiset.fromKeySet(Set(1)) == Multiset(1 -> 1))
    assert(Multiset.fromKeySet(Set('a', 'b')) == Multiset('a' -> 1, 'b' -> 1))
  }
}
