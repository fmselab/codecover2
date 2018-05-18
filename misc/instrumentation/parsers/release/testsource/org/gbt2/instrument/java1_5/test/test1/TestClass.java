package org.gbt2.instrument.java1_5.test.test1;

public class TestClass
{
  public TestClass()
  {    
    System.out.println("Juhuu");

    doSomething();
  }

  private void doSomething()
  {
    int a = 45;

    int b = a * a;

    double c = Math.pow(a, b);

    --c;
    
    b = b * a;
    
    String s = new String("Ökologische Äcker sind übermütig und weiß.").trim();
  }
}
