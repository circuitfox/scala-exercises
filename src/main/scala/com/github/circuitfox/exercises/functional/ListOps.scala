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

package com.github.circuitfox.exercises.functional

import annotation.tailrec

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
object ListOps {
  def succ(n: Int): Int = n + 1

  def pred(n: Int): Int = n - 1

  @tailrec
  def add(x: Int, y: Int): Int = if (y == 0) x else add(succ(x), pred(y))

  def sum(ls: List[Int]): Int = ls.foldLeft(0)(add)

  def length[A](ls: List[A]): Int =
    ls.foldLeft(0)((x, _) => succ(x))

  def map[A, B](ls: List[A], f: A => B): List[B] =
    ls.foldRight(List.empty[B])((x, xs) => {f(x) :: xs})

  def filter[A](ls: List[A], f: A => Boolean): List[A] =
    ls.foldRight(List.empty[A])((x, xs) => {if (f(x)) x :: xs else xs})

  def append[A](xs: List[A], ys: List[A]): List[A] = xs.foldRight(ys)(_ :: _)

  def concat[A](ls: List[List[A]]): List[A] = ls.reduceLeft(append)

  def concatMap[A, B](ls: List[A], f: A => List[B]): List[B] =
    concat(map(ls, f))

  def maximum(ls: List[Int]): Int = ls.foldLeft(0)(math.max)

  def reverse[A](ls: List[A]): List[A] =
    ls.foldLeft(List.empty[A])((x, xs) => xs :: x )
}
