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
        if (n == Nil) List(p)
        else pack1(n, p :: ys)
      }
    pack1(ls, List(List()))
  }
}
