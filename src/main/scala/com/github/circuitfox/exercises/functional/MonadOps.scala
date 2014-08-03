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

// TODO
object MonadOps {
  def sequence[M[_], A](as: List[M[A]], m: Monad[M]): M[List[A]] = ???

  def fmap[M[_], A, B](a: M[A], f: A => B, m: Monad[M]): M[B] = ???

  def flatten[M[_], A](a: M[M[A]], m: Monad[M]): M[A] = ???

  def apply[M[_], A, B](f: M[A => B], a: M[A], m: Monad[M]): M[B] = ???

  def filterM[M[_], A](f: A => M[Boolean], as: List[A],
                       m: Monad[M]): M[List[A]] = ???

  def replicateM[M[_], A](n: Int, a: M[A], m: Monad[M]): M[List[A]] = ???

  def lift2[M[_], A, B, C](f: (A, B) => C ,a: M[A], b: M[B],
                           m: Monad[M]): M[C] = ???
}
