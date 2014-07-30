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

trait Functor[F[_]] {
  def map[A, B](fa: F[A], f: A => B): F[B]
}

object Functor {
  implicit def ListFunctor: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A], f: A => B): List[B] = fa map f
  }

  implicit def OptionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A], f: A => B): Option[B] = fa map f
  }

  implicit def StreamFunctor: Functor[Stream] = new Functor[Stream] {
    def map[A, B](fa: Stream[A], f: A => B): Stream[B] = fa map f
  }

  implicit def Function1Functor[X]: Functor[({type λ[α] = X => α})#λ] =
    new Functor[({type λ[α] = X => α})#λ] {
      def map[A, B](fa: X => A, f: A => B): X => B = fa andThen f
    }

  implicit def EitherLeftFunctor[X]:
  Functor[({type λ[α] = Either.LeftProjection[α, X]})#λ] =
    new Functor[({type λ[α] = Either.LeftProjection[α, X]})#λ] {
      def map[A, B](fa: Either.LeftProjection[A, X], f: A => B) = fa.map(f).left
    }

  implicit def EitherRightFunctor[X]:
  Functor[({type λ[α] = Either.RightProjection[X, α]})#λ] =
    new Functor[({type λ[α] = Either.RightProjection[X, α]})#λ] {
      def map[A, B](fa: Either.RightProjection[X, A], f: A => B) =
        fa.map(f).right
    }
}
