package constructor

/**
 * @author peter
 */
class PhraseConstruction extends Construction {
  static def HEAD = new Descriptor('$$$head')
  static def PHRASE = '$$$phrase'

  Set<String> possibleHeads = [] as Set

  def PhraseConstruction(Descriptor descr) {
    super(descr, []);
  }

  Construction head(Cloud cloud) {
    def head = cloud.strongUsages(this, HEAD.name)
    return head ? head[0].args[1] : null
  }

  static Construction project(Construction c, Cloud ctx) {
    def heads = ctx.strongUsages(c, HEAD.name)
    if (heads) {
      return heads[0].args[0]
    }
    return c
  }

  protected boolean oneWord(Cloud cloud) {
    def h = head(cloud)
    if (h) {
      return h.oneWord(cloud)
    }
    return super.oneWord(cloud);
  }


  protected String _prettyPrint(VarNameGenerator varName, String indent, Cloud cloud) {
    def h = head(cloud)
    if (h) {
      return h.prettyPrint(varName, indent, cloud)
    }

    return descr.name;
  }

  def String toString() {
    return super.toString() + ": " + possibleHeads;
  }


}
