package crt;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class CrtTest {
  @Test
  public void threeNoopsInWindow() throws Exception {
    Crt crt = new Crt(1);
    crt.doCycles(3,"noop\nnoop\nnoop");
    assertEquals("###", crt.getPixels());
  }

  @Test
  public void threeNoopsNotInWindow() throws Exception {
    Crt crt = new Crt(10);
    crt.doCycles(3,"noop\nnoop\nnoop");
    assertEquals("...", crt.getPixels());
  }

  @Test
  public void addxPositionsXAfterExecution() throws Exception {
    Crt crt = new Crt(1);
    crt.doCycles(5, "addx 4\nnoop\nnoop\nnoop");
    assertEquals("##..#", crt.getPixels());
    assertEquals(5, crt.getX());
  }

  @Test
  public void fullScreen() throws Exception {
    Crt crt = new Crt(1);
    crt.doCycles(240, "noop\n".repeat(240));
    assertEquals(
      "###.....................................".repeat(6),
      crt.getPixels());
  }
}
