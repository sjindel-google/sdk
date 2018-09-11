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
  C<int>().foo();
  E<int, String>().foo();
}
