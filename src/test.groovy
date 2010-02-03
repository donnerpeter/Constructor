class Foo {
  def foo(msg) {
  }
}

def z = new Foo() {

  def foo(Object msg) {
    return super.foo(msg);
  }

}

z.foo("42")