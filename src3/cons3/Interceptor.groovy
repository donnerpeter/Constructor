package cons3

/**
 * @author peter
 */
public class Interceptor {

  ParsingState intercept(List<Mite> contribution, ParsingState state, Function2<List<Mite>, ParsingState, ParsingState> base) {
    base(contribution, state)
  }

}