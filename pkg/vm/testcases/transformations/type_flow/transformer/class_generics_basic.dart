// Copyright (c) 2018, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

class C<T> {
  foo() => D<T>();
  dynamic id1(T x) => x;
  dynamic id2(T x) => x;
}

class D<T> {}

class E<S, T> extends C<T> {
  foo() => super.foo();
  bar() => D<S>();
  baz() => D<T>();
}

class X {}

class Y extends X {}

class Z extends X {}

main() {
  // Test that type arguments are instantiated correctly on concrete types.
  dynamic used;
  used = C<int>().foo();
  used = E<int, String>().foo();
  used = E<int, String>().bar();
  used = E<int, String>().baz();

  // Test that narrow against type-parameters works.
  C<X> c = new C<Y>();
  c.id1(Y());
  c.id2(Z());

  return used;
}
