class Foo {
  def Foo(String... s) { }
}
class ImplOneParameter extends Foo {
  def ImplOneParameter(String s) {
    super(s)
  }
}
class ImplArray extends Foo {
  def ImplArray(String[] s) {
    super(s)
  }
}

new ImplArray("String")
new ImplOneParameter("String")