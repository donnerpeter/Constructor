package constructor

/**
 * @author peter
 */
class ConstructionBuilder {
  def name
  private def famous = false
  private def tracked = false
  private Set<String> akas = []
  private Map expectations = [:]

  def ConstructionBuilder(String name) {
    this.name = name;
  }

  ConstructionBuilder aka(String... akas) {
    this.akas.addAll(Arrays.asList(akas))
    return this
  }

  ConstructionBuilder track() {
    tracked = true
    this
  }
  ConstructionBuilder famous() {
    famous = true
    this
  }

  ConstructionBuilder expect(List pattern, action) {
    expectations[pattern] = action
    return this
  }

  def build(args) {
    def c = new Construction(name, args)
    c.famous = famous
    c.tracked = tracked
    c.aka(akas.toArray())
    expectations.each { k, v -> c.expect(k, v) }
    return c
  }

}
