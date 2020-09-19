package criterium.measured;

import clojure.lang.IFn;
import org.openjdk.jmh.infra.Blackhole;

public class Helpers {
  public static long loop(Blackhole blackhole,
                          long evalCount,
                          IFn f,
                          Object state
                          ) {
    long start = System.nanoTime();
    for (;evalCount > 0; evalCount--) {
      blackhole.consume(f.invoke(state));
    }
    return System.nanoTime() - start;
  }

  public static long loop(Blackhole blackhole,
                          long evalCount,
                          IFn f
                          ) {
    long start = System.nanoTime();
    for (;evalCount > 0; evalCount--) {
      blackhole.consume(f.invoke());
    }
    return System.nanoTime() - start;
  }
}
