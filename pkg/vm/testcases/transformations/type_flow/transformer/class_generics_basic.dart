class C<T> {
  foo() => D<T>();
  dynamic id(dynamic x) => x as T;
  dynamic id2(T x) => x;
}

class D<T> {}

class E<S, T> extends C<T> {
  foo() => super.foo();
  bar() => D<S>();
  baz() => D<T>();
}

class X {
  shouldBeDead() {}
}

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
  c.id(Z()).shouldBeDead();
  c.id2(Z()).shouldBeDead();

  return used;
}
