package constructor

/**
 * @author peter
 */
class ConstructionBuilder {
  def name
  private def famous = false
  private def tracked = false
  private Set<String> akas = []
  private Map<?, ConstructionBuilder> expectations = [:]
  private Set<Integer> consumedArgs = [] as Set
  private Set<Integer> demotedArgs = [] as Set

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

  ConstructionBuilder expect(Map<?, Integer> pattern, ConstructionBuilder action) {
    expectations[pattern] = action
    return this
  }

  Construction build(List<Construction> args) {
    def c = new Construction(name, args)
    c.famous = famous
    c.tracked = tracked
    c.aka(akas.toArray())
    expectations.each { k, v -> c.expect(k, v) }
    c.consumes(consumedArgs as int[])
    c.demotes(demotedArgs as int[])
    return c
  }

  ConstructionBuilder consumes(int... argIndices) {
    consumedArgs.addAll(argIndices as List)
    return this
  }
  ConstructionBuilder demotes(int... argIndices) {
    consumedArgs.addAll(argIndices as List)
    return this
  }


}
