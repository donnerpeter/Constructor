package constructor

/**
 * @author peter
 */
class Descriptor {
  def name
  private def famous = false
  private def tracked = false
  private Set<String> pings = [] as Set
  private Map<?, Descriptor> expectations = [:]
  Set<Integer> consumedArgs = [] as Set
  Set<Integer> demotedArgs = [] as Set

  def Descriptor(String name) {
    this.name = name
    pings << name
  }

  Descriptor aka(String... akas) {
    this.pings.addAll(Arrays.asList(akas))
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
    expectations.each { k, v -> c.expect(k, v) }
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

  def ping(Construction c, message) {
    pings.contains(message)
  }

}
