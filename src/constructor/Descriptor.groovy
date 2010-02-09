package constructor

/**
 * @author peter
 */
class Descriptor {
  def name
  private def famous = false
  private def tracked = false
  private Set<String> akas = []
  private Map<?, Descriptor> expectations = [:]
  private Set<Integer> consumedArgs = [] as Set
  private Set<Integer> demotedArgs = [] as Set

  def Descriptor(String name) {
    this.name = name;
  }

  Descriptor aka(String... akas) {
    this.akas.addAll(Arrays.asList(akas))
    return this
  }

  Descriptor track() {
    tracked = true
    this
  }
  Descriptor famous() {
    famous = true
    this
  }

  Descriptor expect(List pattern, action) {
    expectations[pattern] = action
    return this
  }

  Descriptor expect(Map<?, Integer> pattern, Descriptor action) {
    expectations[pattern] = action
    return this
  }

  Construction build(List<Construction> args) {
    def c = new Construction(this, args)
    c.famous = famous
    c.tracked = tracked
    c.aka(akas.toArray())
    expectations.each { k, v -> c.expect(k, v) }
    c.consumes(consumedArgs as int[])
    c.demotes(demotedArgs as int[])
    return c
  }

  Descriptor consumes(int... argIndices) {
    consumedArgs.addAll(argIndices as List)
    return this
  }
  Descriptor demotes(int... argIndices) {
    demotedArgs.addAll(argIndices as List)
    return this
  }


}
