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

import util.Random

import org.scalatest.FlatSpec

object RandomListFactory {
  def apply(f: Int, t: Int): List[Int] = {
    val rnd = new Random(System.currentTimeMillis())
    rnd.shuffle(List.range(f, t))
  }

  def apply(n: Int): List[Int] = {
    val rnd = new Random(System.currentTimeMillis())
    Stream.continually(rnd.nextInt()).take(n).toList
  }
}

class ListOpsSpec extends FlatSpec {
  import ListOps._

  def isEven(n: Int): Boolean = n % 2 == 0

  def double(n: Int): Int = n * 2

  def doubleMap(ls: List[Int]): List[Int] = map(ls, double)

  "ListOps.add" should "add any two integers together" in {
    val rnd = new Random(System.currentTimeMillis())
    val rand1 = rnd.nextInt()
    val rand2 = rnd.nextInt()
    assert(add(1, 2) === 3)
    assert(add(58, 33) === 91)
    assert(add(rand1, rand2) == rand1 + rand2)
  }

  "ListOps.sum" should "sum the values of any given list" in {
    val tList = RandomListFactory(1, 101)
    val tList1 = RandomListFactory(101, 201)
    val tList2 = Random.shuffle(append(tList, tList1))
    assert(sum(tList) === (1 to 100).sum)
    assert(sum(tList1) === (101 to 200).sum)
    assert(sum(tList2) === (1 to 200).sum)
  }

  "ListOps.length" should "return the length of a list" in {
    val tList = RandomListFactory(1, 101)
    val tList1 = RandomListFactory(101, 201)
    val tList2 = Random.shuffle(append(tList, tList1))
    assert(length(tList) === 100)
    assert(length(tList1) === 100)
    assert(length(tList2) === 200)
  }

  "ListOps.map" should "apply a function over all elements of a list" in {
    val tList = RandomListFactory(1, 101)
    val tList1 = RandomListFactory(101, 201)
    val tList2 = Random.shuffle(append(tList, tList1))
    assert(map(tList, double) === tList.map(double))
    assert(map(tList1, double) === tList1.map(double))
    assert(map(tList2, double) === tList2.map(double))
  }

  "ListOps.filter" should
    "return the elements of a list that match a given predicate" in {
    val tList = RandomListFactory(1, 101)
    val tList1 = RandomListFactory(101, 201)
    val tList2 = Random.shuffle(append(tList, tList1))
    assert(filter(tList, isEven) === tList.filter(isEven))
    assert(filter(tList1, isEven) === tList1.filter(isEven))
    assert(filter(tList2, isEven) === tList2.filter(isEven))
  }

  "ListOps.append" should "combine two lists into one" in {
    val tList = RandomListFactory(1, 101)
    val tList1 = RandomListFactory(101, 201)
    val tList2 = RandomListFactory(1, 201)
    assert(append(tList, tList1) === tList ++ tList1)
    assert(append(tList, tList2) === tList ++ tList2)
    assert(append(tList1, tList) === tList1 ++ tList)
    assert(append(tList1, tList2) === tList1 ++ tList2)
    assert(append(tList2, tList) === tList2 ++ tList)
    assert(append(tList2, tList1) === tList2 ++ tList1)
  }

  "ListOps.concat" should "flatten a list of lists into one single list" in {
    val tList = List(RandomListFactory(1, 101), RandomListFactory(101, 201))
    val tList1 = tList.transpose
    assert(concat(tList) === tList.flatten)
    assert(concat(tList1) === tList1.flatten)
  }

  "ListOps.concatMap" should
    "map a function over a list and concatenate the results" in {
    val tList = RandomListFactory(1, 101)
    val tList1 = RandomListFactory(101, 201)
    val tList2 = List(tList, tList1)
    assert(concatMap(tList2, doubleMap) === tList2.flatMap(a => doubleMap(a)))
  }

  "ListOps.maximum" should "get the maximum value of a list of integers" in {
    val tList = RandomListFactory(1, 101)
    val tList1 = RandomListFactory(101, 201)
    val tList2 = RandomListFactory(100)
    assert(maximum(tList) === 100)
    assert(maximum(tList1) === 200)
    assert(maximum(tList2) === tList2.max)
  }

  "ListOps.reverse" should "reverse the contents of a list" in {
    val tList = RandomListFactory(1, 101)
    val tList1 = RandomListFactory(101, 201)
    val tList2 = RandomListFactory(100)
    assert(reverse(tList) === tList.reverse)
    assert(reverse(tList1) === tList1.reverse)
    assert(reverse(tList2) === tList2.reverse)
  }
}
