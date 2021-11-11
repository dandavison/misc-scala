/*
https://leetcode.com/problems/lru-cache/

Design a data structure that follows the constraints of a Least Recently Used
(LRU) cache.

Implement the LRUCache class:

LRUCache(int capacity) Initialize the LRU cache with positive size capacity.

- int get(int key) Return the value of the key if the key exists, otherwise
  return -1.

- void put(int key, int value) Update the value of the key if the key exists.
  Otherwise, add the key-value pair to the cache. If the number of keys exceeds
  the capacity from this operation, evict the least recently used key.

The functions get and put must each run in O(1) average time complexity.
 */
import scala.collection.mutable;

class LRUCache(val capacity: Int) {
  private val firstSentinel = new ListNode(key = -1, value = -1)
  private val lastSentinel = new ListNode(key = -1, value = -1)
  private val nodes = mutable.Map[Int, ListNode]()

  firstSentinel.next = Some(lastSentinel)
  lastSentinel.prev = Some(firstSentinel)

  def get(key: Int): Int = {
    nodes.get(key) match {
      case Some(node) => {
        moveToLast(node)
        node.value
      }
      case None => -1
    }
  }

  def put(key: Int, value: Int): Unit = {
    val node = nodes.get(key) match {
      case Some(node) => {
        // Update
        node.value = value
        node
      }
      case None => {
        // Insert
        if (nodes.size == capacity) {
          expireLRU()
        }
        val node =
          new ListNode(key = key, value = value)
        nodes(key) = node
        node
      }
    }
    moveToLast(node)
  }

  private def expireLRU() {
    val lru = firstSentinel.next.get
    unlink(lru)
    nodes -= lru.key
  }

  private def moveToLast(node: ListNode): Unit = {
    unlink(node)
    link(lastSentinel.prev.get, node, lastSentinel)
  }

  private def unlink(node: ListNode): Unit = {
    val prev = node.prev
    val next = node.next

    if (!prev.isEmpty) {
      prev.get.next = next
    }
    if (!next.isEmpty) {
      next.get.prev = prev
    }
  }

  private def link(
      prev: ListNode,
      node: ListNode,
      next: ListNode
  ): Unit = {
    node.prev = Some(prev)
    prev.next = Some(node)

    node.next = Some(next)
    next.prev = Some(node)
  }

}

class ListNode(
    var prev: Option[ListNode] = None,
    val key: Int,
    var value: Int,
    var next: Option[ListNode] = None
)
