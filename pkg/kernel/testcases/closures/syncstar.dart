/*
 * Copyright (c) 2017, the Dart project authors.  Please see the AUTHORS file
 * for details. All rights reserved. Use of this source code is governed by a
 * BSD-style license that can be found in the LICENSE file.
 */
range(int high) {
  iter(int low) sync* {
    while (high-- > low) yield high;
  }

  return iter;
}

main() {
  var sum = 0;
  for (var x in range(10)(2)) sum += x;

  if (sum != 44) {
    throw new Exception("Incorrect output.");
  }
}
