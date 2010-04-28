package constructor

/**
 * @author peter
 */
class PhraseDescriptor extends Descriptor {

  def PhraseDescriptor(String name) {
    super(name)
    aka(PhraseConstruction.PHRASE)
    famous()
  }

  def Construction build(List<Construction> args, Cloud cloud) {
    assert args.isEmpty()
    return new PhraseConstruction(this);
  }

  def boolean activate(Construction c, ParsingContext ctx) {
    def head = ((PhraseConstruction) c).head(ctx.cloud)
    if (head) {
      head.descr.activate(head, ctx)
    }

    return super.activate(c, ctx);
  }

}
