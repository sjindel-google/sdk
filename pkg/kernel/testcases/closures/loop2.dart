/*
 * Copyright (c) 2017, the Dart project authors.  Please see the AUTHORS file
 * for details. All rights reserved. Use of this source code is governed by a
 * BSD-style license that can be found in the LICENSE file.
 */
void doit(int x) {
  final int max = 10;
  final double expectedSum = ((max - 1) * max) / 2;

  int counter = 0;
  var calls = [];
  while (counter < max) {
    int pos = counter;
    calls.add(() => pos + x);
    counter++;
  }

  double sum = 0.0;
  for (var c in calls) sum += c();
  if (sum != expectedSum)
    throw new Exception("Unexpected sum = $sum != $expectedSum");
}

void main() {
  doit(0);
}
