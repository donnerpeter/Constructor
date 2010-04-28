package constructor

/**
 * @author peter
 */
class Colored extends Construction {
  int color

  def Colored(int color) {
    super(new Descriptor("Color" + color).semantics {
      def result = new LinkedHashSet()
      def allChildren = [] as Set
      for (i in 0..it.size() - 1) {
        def arg = it[i]
        if (arg) {
          result << arg
        }
        if (arg instanceof Frame) {
          allChildren += arg.allChildren()
        }
      }
      return (result-allChildren) as List
    }, []);
    this.color = color;
  }

  List<Construction> children(Cloud cloud) {
    cloud.reduce().findAll { cloud.colors[it] == color && it != this } as List
  }

  def String prettyPrint(VarNameGenerator varName, String indent, Cloud cloud) {
    return "Color{$color}\n" + cloud.prettyPrint(color, indent + TAB, varName)
  }

  protected boolean oneWord(Cloud cloud) {
    return false
  }


}
