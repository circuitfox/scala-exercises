// Copyright (c) 2014 Chris Stankus under the MIT license
package io.github.circuitfox.scala.exercises

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Solutions for 'Scala exercises for Beginners'.
 *
 * The following functions form List are prohibited:
 * length
 * map
 * filter
 * ::: (and variations such as ++)
 * flatten
 * flatMap
 * reverse (and variations i.e. reverseMap, reverse_:::)
 * This also includes for-comprehensions
 *
 * @author Chris Stankus
 * @version 0.0.1
 */
object BeginnerExercises extends App {
  def succ(n: Int): Int = n + 1

  def pred(n: Int): Int = n - 1

  // `x + y' is not allowed, only succ/pred
  @tailrec
  def add(x: Int, y: Int): Int = if (y == 0) x else add(succ(x), pred(y))

  def sum(ls: List[Int]): Int = (ls foldLeft 0)(add)

  def length[A](ls: List[A]): Int =
    (ls foldLeft 0)((x, _) => succ(x))

  def map[A, B](ls: List[A], f: A => B): List[B] =
    (ls foldRight List.empty[B])((x, xs) => {f(x) :: xs})

  def filter[A](ls: List[A], f: A => Boolean): List[A] =
    (ls foldRight List.empty[A])((x, xs) => {if (f(x)) x :: xs else xs})

  def append[A](xs: List[A], ys: List[A]): List[A] = (xs foldRight ys)(_ :: _)

  def concat[A](ls: List[List[A]]): List[A] = ls reduceLeft append

  def concatMap[A, B](ls: List[A], f: A => List[B]): List[B] =
    concat(map(ls, f))

  def maximum(ls: List[Int]): Int = (ls foldLeft 0)(math.max)

  def reverse[A](ls: List[A]): List[A] =
    (ls foldRight List.empty[A])((x, xs) => {xs :+ x})

  // these weren't in the exercise; I may rewrite them for scalatest...

  lazy val tList = List.range(1, 101)
  lazy val tList1 = List.range(101, 201)
  lazy val tList2 = List(tList, tList1)

  def isEven(n: Int): Boolean = n % 2 == 0

  def double(n: Int): Int = n * 2

  def doubleMap(ls: List[Int]): List[Int] = map(ls, double)

  val testMap = Map(
    "add" -> (add(1, 2) == 3),
    "sum" -> (sum(tList) == tList.sum),
    "length" -> (length(tList) == tList.length),
    "map" -> (map(tList, double) == tList.map(double)),
    "filter" -> (filter(tList, isEven) == tList.filter(isEven)),
    "append" -> (append(tList, tList1) == tList ++ tList1),
    "concat" -> (concat(tList2) == tList2.flatten),
    "concatMap" ->
      (concatMap(tList2, doubleMap) == tList2.flatMap(a => doubleMap(a))),
    "maximum" -> (maximum(tList) == tList.max),
    "reverse" -> (reverse(tList) == tList.reverse)
  )

  println(testMap forall { case (_, _) => true})
}
