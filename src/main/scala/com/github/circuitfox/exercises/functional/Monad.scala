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

import scala.util.Either

trait Monad[M[_]] extends Functor[M] {
  def flatMap[A, B](ma: M[A], f: A => M[B]): M[B]

  def unit[A](a: A): M[A]

  def map[A, B](ma: M[A], f: A => B): M[B] = flatMap(ma, f andThen unit)
}

object Monad {
  implicit def ListMonad: Monad[List] = new Monad[List] {
    def flatMap[A, B](ma: List[A], f: A => List[B]): List[B] = ma flatMap f

    def unit[A](a: A): List[A] = List(a)
  }

  implicit def OptionMonad: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](ma: Option[A], f: A => Option[B]): Option[B] =
      ma flatMap f

    def unit[A](a: A): Option[A] = Option(a)
  }

  implicit def StreamMonad: Monad[Stream] = new Monad[Stream] {
    def flatMap[A, B](ma: Stream[A], f: A => Stream[B]): Stream[B] =
      ma flatMap f

    def unit[A](a: A): Stream[A] = Stream(a)
  }

  implicit def Function1Monad[X]: Monad[({type λ[α] = X => α})#λ] =
    new Monad[({type λ[α] = X => α})#λ] {
      def flatMap[A, B](ma: X => A, f: A => X => B): X => B =
        x => (ma andThen f)(x)(x)

      def unit[A](a: A): X => A = Function.const(a)
    }

  implicit def EitherLeftMonad[X]:
    Monad[({type λ[α] = Either.LeftProjection[α, X]})#λ] =
    new Monad[({type λ[α] = Either.LeftProjection[α, X]})#λ] {
      def flatMap[A, B](ma: Either.LeftProjection[A, X],
                        f: A => Either.LeftProjection[B, X]):
        Either.LeftProjection[B, X] = ma.flatMap(f(_).e).left

      def unit[A](a: A): Either.LeftProjection[A, X] =
        Either.LeftProjection(Left(a))
    }

  implicit def EitherRightMonad[X]:
    Monad[({type λ[α] = Either.RightProjection[X, α]})#λ] =
    new Monad[({type λ[α] = Either.RightProjection[X, α]})#λ] {
      def flatMap[A, B](ma: Either.RightProjection[X, A],
                        f: (A) => Either.RightProjection[X, B]):
        Either.RightProjection[X, B] = ma.flatMap(f(_).e).right

      def unit[A](a: A): Either.RightProjection[X, A] =
        Either.RightProjection(Right(a))
    }
}
