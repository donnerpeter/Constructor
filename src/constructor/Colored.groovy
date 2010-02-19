package constructor

/**
 * @author peter
 */
class Colored extends Construction {
  int color
  Cloud cloud

  def Colored(int color, Cloud cloud) {
    super(new Descriptor("Color" + color).semantics { cloud.semantics(color).frames.iterator().next() }, []);
    this.color = color;
  }

  def String prettyPrint(VarNameGenerator varName, String indent, Cloud cloud) {
    return "Color{$color}\n" + cloud.prettyPrint(color, indent + TAB, varName)
  }


}
