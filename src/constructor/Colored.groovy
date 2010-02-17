package constructor

/**
 * @author peter
 */
class Colored extends Construction {
  int color

  def Colored(color) {
    super(new Descriptor("Color" + color), []);
    this.color = color;
  }

  def String prettyPrint(VarNameGenerator varName, String indent, Cloud cloud) {
    return "Color{$color}\n" + cloud.prettyPrint(color, indent + TAB, varName)
  }


}
