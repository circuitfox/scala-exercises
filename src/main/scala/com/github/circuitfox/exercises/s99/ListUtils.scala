/*
 * Copyright (c) 2014 Chris Stankus
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package com.github.circuitfox.exercises.s99

import annotation.tailrec

object ListUtils {
  // P01 - Find the last element of a list.
  def last[A](ls: List[A]): A = ls.last

  // P02 - Find the second-to-last element of a list.
  def penultimate[A](ls: List[A]): A =
    if (ls.isEmpty) throw new NoSuchElementException("empty list")
    else ls.init.last

  // P03 - Find the nth element of a list.
  def nth[A](n: Int, ls: List[A]): A =
    if (n < 0 || n >= ls.size) throw new NoSuchElementException("invalid index")
    else ls(n)

  // P04 - Find the number of elements in a list.
  def length[A](ls: List[A]): Int = ls.length

  // P05 - Reverse a list
  def reverse[A](ls: List[A]): List[A] = ls.reverse

  // P06 - Find out if a list is a palindrome
  def isPalindrome[A](ls: List[A]): Boolean = ls == ls.reverse

  // P07 - Flatten a nested list
  def flatten[A](ls: List[List[A]]): List[A] = ls.flatten

  // P08 - Eliminate consecutive duplicates in a list
  def compress[A](ls: List[A]): List[A] = ls.foldRight(List[A]()) { (h, t) =>
    if (t.isEmpty || t.head != h) h :: t else t
  }

  // P09 - Pack consecutive duplicates into sublists
  def pack[A](ls: List[A]): List[List[A]] = {
    @tailrec
    def pack1(xs: List[A], ys: List[List[A]]): List[List[A]] =
      if (xs.isEmpty) ys
      else {
        val (p, n) = xs.span(_ == xs.head)
        if (n == Nil) p :: ys
        else pack1(n, p :: ys)
      }
    pack1(ls, List.empty[List[A]]).reverse
  }

  // P10 - Run-length encoding of a list
  def encode[A](ls: List[A]): List[(Int, A)] = pack(ls) map { lls =>
    (lls.length, lls.head)
  }

  // P11 - Run-length encoding of a list; single-element tuples are compressed
  def encodeCompress[A](ls: List[A]): List[Either[A, (Int, A)]] =
    encode(ls) map { t => if (t._1 == 1) Left(t._2) else Right(t)}

  // P12 - Decode a run-length encoded list
  def decode[A](ls: List[(Int, A)]): List[A] = ls flatMap { case (l, e) =>
    List.fill(l)(e)
  }

  // P13 - Direct run-length encoding of a list
  def encodeDirect[A](ls: List[A]): List[(Int, A)] = {
    @tailrec
    def encodeD(xs: List[A], ys: List[(Int, A)]): List[(Int, A)] =
      if (xs.isEmpty) ys
      else {
        val (p, n) = xs.span(_ == xs.head)
        if (n == Nil) (p.length, p.head) :: ys
        else encodeD(n, (p.length, p.head) :: ys)
      }
    encodeD(ls, List.empty[(Int, A)]).reverse
  }

  // P14 - Duplicate the elements of a list
  def duplicate[A](ls: List[A]): List[A] = ls flatMap { e => List(e, e) }

  // P15 - Duplicate the elements of a list n times
  def duplicateN[A](n: Int, ls: List[A]): List[A] = ls flatMap { e =>
    List.fill(n)(e)
  }

  // P16 - Drop every nth element from a list
  def drop[A](n: Int, ls: List[A]): List[A] = ls.zipWithIndex.filter {
    case (e, i) => (i+1) % n != 0
  }.map(_._1)

  // P17 - Split a list at the given index
  def split[A](n: Int, ls: List[A]): (List[A], List[A]) = ls splitAt n

  // P18 - Extract a slice from the given list
  def slice[A](from: Int, to: Int, ls: List[A]): List[A] = ls.slice(from, to)

  // P19 - Rotate a list n places to the left
  @tailrec
  def rotate[A](n: Int, ls: List[A]): List[A] = {
    val ln = if (ls.isEmpty) 0 else n % ls.length
    if (n < 0 ) rotate(ln + ls.length, ls)
    else ls.drop(ln) ::: ls.take(ln)
  }

  // P20 - Remove the kth element of a list
  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (h, e :: t) => (h ::: t, e)
    case (h, Nil) => throw new NoSuchElementException
  }
}
