class C<T> {
  foo() => D<T>();
}

class D<T> {}

class E<S, T> extends C<T> {
  foo() => super.foo();
  bar() => D<S>();
  baz() => D<T>();
}

main() {
  dynamic used;
  used = C<int>().foo();
  used = E<int, String>().foo();
  used = E<int, String>().bar();
  used = E<int, String>().baz();
  return used;
}
