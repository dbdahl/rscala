package org.ddahl.rscala;

/** An interface to an R interpreter.
*
* An object <tt>R</tt> is the instance of this class available in a Scala interpreter created by calling the function
* <tt>scala</tt> from the package <a href="http://cran.r-project.org/package=rscala">rscala</a>.  It is through this instance <tt>R</tt> that
* callbacks to the original <a href="http://www.r-project.org">R</a> interpreter are possible.
* 
* In a JVM-based application, an instance of this class is created using its companion object.  See below.  The paths of the
* rscala's JARs (for all supported versions of Scala) are available from <a href="http://www.r-project.org">R</a> using <tt>rscala::rscalaJar()</tt>.
* To get just the JAR for Scala 2.11, for example, use <tt>rscala::rscalaJar("2.11")</tt>.
* 
* <pre><span class="inner-pre" style="font-size:-2px">
* {@code 
* import org.ddahl.rscala.RClient4Java;
* 
* public class Example {
* 
*   public static void main(String[] args) {
* 
*     RClient4Java R = new RClient4Java();
*  
*     double a = R.evalD0("rnorm(8)");
*     double[] b = R.evalD1("rnorm(8)");
*     double[][] c = R.evalD2("matrix(rnorm(8),nrow=4)");
* 
*     R.eval("v <- rbinom(8,size=10,prob=0.4)\n"+
*            "m <- matrix(v,nrow=4)",true);
* 
*     Object[] v1 = R.get("v");
*     int[] v2 = (int[]) R.get("v")[0];   // This works, but is not very convenient
*     int v3 = R.getI0("v");      // Get the first element of R's "v" as a Int
*     int[] v4 = R.getI1("v");    // Get R's "v" as an int[]
*     int[][] v5 = R.getI2("m");  // Get R's "m" as an int[][]
* 
*     int[] aa = {5,6,4};
*     R.set("a",aa);
* 
*     R.eval("b <- matrix(NA,nrow=3,ncol=2)");
*     for ( int i=0; i<3; i++ ) {
*       int[] bb = {2*i,2*i+1};
*       R.set("b",bb,""+(i+1)+",",true);
*     }
* 
*     R.eval("myList <- list()");
*     String[] cc = {"David","Grace","Susan"};
*     R.set("myList",cc,"'names'",false);
*     int[] dd = {5,4,5};
*     R.set("myList",dd,"'counts'",false);
*     R.eval("print(myList)");
* 
*   }
* 
* }
*
* }
*
* </span></pre>
* @author David B. Dahl
*/
public class RClient4Java {

  private org.ddahl.rscala.RClient c;

  public RClient4Java() { this(null); }

  public RClient4Java(String rCmd) { this(rCmd,false); }

  public RClient4Java(String rCmd, boolean serializeOutput) { this(rCmd,serializeOutput,true); }

  public RClient4Java(String rCmd, boolean serializeOutput, boolean rowMajor) { this(rCmd,serializeOutput,rowMajor,0); }

  public RClient4Java(String rCmd, boolean serializeOutput, boolean rowMajor, int port) { this(rCmd,serializeOutput,rowMajor,port,false); }

  public RClient4Java(String rCmd, boolean serializeOutput, boolean rowMajor, int port, boolean debug) { this(rCmd,serializeOutput,rowMajor,port,debug,60); }

  public RClient4Java(String rCmd, boolean serializeOutput, boolean rowMajor, int port, boolean debug, int timeout) { c = org.ddahl.rscala.RClient.apply(rCmd,serializeOutput,rowMajor,port,debug,timeout); }

  /** Closes the interface to the R interpreter.
  * 
  * Subsequent calls to the other methods will fail.
  */
  public void exit() { c.exit(); }

  /** Calls and returns <b><tt>eval(snippet,true)</tt></b>.
  *   @param snippet  The snippet to be evaluated.
  *   @return         The evaluated Object.
  */
  public Object eval(String snippet) { return eval(snippet,true); }

  /** Evaluates <tt>snippet</tt> in the R interpreter.
  *
  * Returns <tt>null</tt> if <tt>evalOnly</tt>.  If <tt>!evalOnly</tt>, the last result of the R expression is converted if possible.
  * Conversion to integers, doubles, booleans, and strings are supported, as are vectors (i.e. arrays) and matrices
  * (i.e. retangular arrays of arrays) of these types.  The static type of the result, however, is <tt>Any</tt> so using the
  * method <tt>evalXY</tt> (where <tt>X</tt> is <tt>I</tt>, <tt>D</tt>, <tt>B</tt>, or <tt>S</tt> and <tt>Y</tt> is <tt>0</tt>, <tt>1</tt>, or <tt>2</tt>) may be more convenient (e.g.
  * {@link #evalD0(String) evalD0}).
  * @param snippet  The snippet to be evaluated.
  * @param evalOnly An indicator of whether the snippet should be evaluated only, or if otehr steps should be performed.
  * @return   The evaluated Object.
  */
  public Object eval(String snippet, boolean evalOnly) { return c.eval(snippet,evalOnly); }

  /** Calls <b><tt>eval(snippet,true)</tt></b> and returns the result using {@link #getI0(String) getI0}.  
  * @param snippet  The snippet to be evaluated.
  * @return   The evaluated int.
  */
  public int evalI0(String snippet) { return c.evalI0(snippet); }

  /** Calls <b><tt>eval(snippet,true)</tt></b> and returns the result using {@link #getD0(String) getD0}. 
  * @param snippet  The snippet to be evaluated.
  * @return   The evaluated double.
  */
  public double evalD0(String snippet) { return c.evalD0(snippet); }

  /** Calls <b><tt>eval(snippet,true)</tt></b> and returns the result using {@link #getB0(String) getB0}.  
  * @param snippet  The snippet to be evaluated.
  * @return   The evaluated boolean.
  */
  public boolean evalB0(String snippet) { return c.evalB0(snippet); }

  /** Calls <b><tt>eval(snippet,true)</tt></b> and returns the result using {@link #getS0(String) getS0}.  
  * @param snippet  The snippet to be evaluated.
  * @return   The evaluated string.
  */
  public String evalS0(String snippet) { return c.evalS0(snippet); }

  /** Calls <b><tt>eval(snippet,true)</tt></b> and returns the result using {@link #getR0(String) getR0}.  
  * @param snippet  The snippet to be evaluated.
  * @return   The evaluated byte.
  */
  public byte evalR0(String snippet) { return c.evalR0(snippet); }

  /** Calls <b><tt>eval(snippet,true)</tt></b> and returns the result using {@link #getI1(String) getI1}.  
  * @param snippet  The snippet to be evaluated.
  * @return   The evaluated int array.
  */
  public int[] evalI1(String snippet) { return c.evalI1(snippet); }

  /** Calls <b><tt>eval(snippet,true)</tt></b> and returns the result using {@link #getD1(String) getD1}. 
  * @param snippet  The snippet to be evaluated.
  * @return   The evaluated double array.
  */
  public double[] evalD1(String snippet) { return c.evalD1(snippet); }

  /** Calls <b><tt>eval(snippet,true)</tt></b> and returns the result using {@link #getB1(String) getB1}.  
  * @param snippet  The snippet to be evaluated.
  * @return   The evaluated boolean array.
  */
  public boolean[] evalB1(String snippet) { return c.evalB1(snippet); }

  /** Calls <b><tt>eval(snippet,true)</tt></b> and returns the result using {@link #getS1(String) getS1}. 
  * @param snippet  The snippet to be evaluated.
  * @return   The evaluated string array.
  */
  public String[] evalS1(String snippet) { return c.evalS1(snippet); }

  /** Calls <b><tt>eval(snippet,true)</tt></b> and returns the result using {@link #getR1(String) getR1}.  
  * @param snippet  The snippet to be evaluated.
  * @return   The evaluated byte array.
  */
  public byte[] evalR1(String snippet) { return c.evalR1(snippet); }

  /** Calls <b><tt>eval(snippet,true)</tt></b> and returns the result using {@link #getI2(String) getI2}.  
  * @param snippet  The snippet to be evaluated.
  * @return   The evaluated int matrix.
  */
  public int[][] evalI2(String snippet) { return c.evalI2(snippet); }

  /** Calls <b><tt>eval(snippet,true)</tt></b> and returns the result using {@link #getD2(String) getD2}.  
  * @param snippet  The snippet to be evaluated.
  * @return   The evaluated double matrix.
  */
  public double[][] evalD2(String snippet) { return c.evalD2(snippet); }

  /** Calls <b><tt>eval(snippet,true)</tt></b> and returns the result using {@link #getB2(String) getB2}.  
  * @param snippet  The snippet to be evaluated.
  * @return   The evaluated boolean matrix.
  */
  public boolean[][] evalB2(String snippet) { return c.evalB2(snippet); }

  /** Calls <b><tt>eval(snippet,true)</tt></b> and returns the result using {@link #getS2(String) getS2}.  
  * @param snippet  The snippet to be evaluated.
  * @return   The evaluated String matrix.
  */
  public String[][] evalS2(String snippet) { return c.evalS2(snippet); }

  /** Calls <b><tt>eval(snippet,true)</tt></b> and returns the result using {@link #getR2(String) getR2}.  
  * @param snippet  The snippet to be evaluated.
  * @return   The evaluated byte matrix.
  */
  public byte[][] evalR2(String snippet) { return c.evalR2(snippet); }

  /** Equivalent to calling <b><tt>set(identifier, value, "", true)</tt></b>. 
  * @param identifier The string of input to be set.
  * @param value  The value of the string.
  */
  public void set(String identifier, Object value) { c.set(identifier,value,"",true); }

  /** Assigns <tt>value</tt> to a variable <tt>identifier</tt> in the R interpreter.
  * 
  * Integers, doubles, booleans, and strings are supported, as are vectors (i.e. arrays) and matrices
  * (i.e. retangular arrays of arrays) of these types.
  * 
  * If <tt>index != ""</tt>, assigned into elements of <tt>identifier</tt> are performed by using either single brackets
  * (<tt>singleBrackets=true</tt>) or double brackets (<tt>singleBrackets=false</tt>).
  *
  * @param identifier The string of input to be set.
  * @param value  The value of the string.
  * @param index  A string containing the index values.
  * @param singleBrackets A Boolean indicating whether or not to use single brackets.
  */
  public void set(String identifier, Object value, String index, boolean singleBrackets) { c.set(identifier,value,index,singleBrackets); }

  /** Returns the value of <tt>identifier</tt> in the R interpreter.  The runtime type is an array whose first element is the value for
  * the identifier and whose second element is <tt>String</tt> indicates the runtime type.
  *
  * Conversion to integers, doubles, booleans, and strings are supported, as are vectors (i.e.
  * arrays) and matrices (i.e. retangular arrays of arrays) of these types.    Using the method <tt>getXY</tt> (where <tt>X</tt> is
  * <tt>I</tt>, <tt>D</tt>, <tt>B</tt>, or <tt>S</tt> and <tt>Y</tt> is <tt>0</tt>, <tt>1</tt>, or <tt>2</tt>) may be more convenient (e.g.  {@link #getD0(String) getD0}).
  *
  * @param identifier The string of input to be set.
  * @return   The evaluated Object.
  */
  public Object[] get(String identifier) {
    scala.Tuple2<Object, String> t = c.get(identifier);
    Object[] r = { t._1, t._2 };
    return r;
  }

  /** Calls <b><tt>get(identifier,false)</tt></b> and converts the result to an <tt>int</tt>.
  *
  * Integers, doubles, booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  * @param identifier The string to be evaluated.
  * @return   The evaluated int.
  */
  public int getI0(String identifier) { return c.getI0(identifier); }

  /** Calls <b><tt>get(identifier,false)</tt></b> and converts the result to a <tt>double</tt>.
  *
  * Integers, doubles, booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  * @param identifier The string to be evaluated.
  * @return   The evaluated double.
  */
  public double getD0(String identifier) { return c.getD0(identifier); }

  /** Calls <b><tt>get(identifier,false)</tt></b> and converts the result to a <tt>boolean</tt>.
  *
  * Integers, doubles, booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  * @param identifier The string to be evaluated.
  * @return   The evaluated boolean.
  */
  public boolean getB0(String identifier) { return c.getB0(identifier); }

  /** Calls <b><tt>get(identifier,false)</tt></b> and converts the result to a <tt>String</tt>.
  *
  * Integers, doubles, booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  * @param identifier The string to be evaluated.
  * @return The evaluated String.
  */
  public String getS0(String identifier) { return c.getS0(identifier); }

  /** Calls <b><tt>get(identifier,false)</tt></b> and converts the result to a <tt>byte</tt>.
  *
  * Integers, doubles, booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  * @param identifier The string to be evaluated.
  * @return The evaluated byte.
  */
  public byte getR0(String identifier) { return c.getR0(identifier); }

  /** Calls <b><tt>get(identifier,false)</tt></b> and converts the result to an <tt>int[]</tt>.
  *
  * Integers, doubles, booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  * @param identifier The string to be evaluated.
  * @return   The evaluated int array.
  */
  public int[] getI1(String identifier) { return c.getI1(identifier); }

  /** Calls <b><tt>get(identifier,false)</tt></b> and converts the result to a <tt>double[]</tt>.
  *
  * Integers, doubles, booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  * @param identifier The string to be evaluated.
  * @return   The evaluated double array.
  */
  public double[] getD1(String identifier) { return c.getD1(identifier); }

  /** Calls <b><tt>get(identifier,false)</tt></b> and converts the result to a <tt>boolean[]</tt>.
  *
  * Integers, doubles, booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  * @param identifier The string to be evaluated.
  * @return   The evaluated boolean array.
  */
  public boolean[] getB1(String identifier) { return c.getB1(identifier); }

  /** Calls <b><tt>get(identifier,false)</tt></b> and converts the result to a <tt>String[]</tt>.
  *
  * Integers, doubles, booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  * @param identifier The string to be evaluated.
  * @return   The evaluated String array.
  */
  public String[] getS1(String identifier) { return c.getS1(identifier); }

  /** Calls <b><tt>get(identifier,false)</tt></b> and converts the result to a <tt>byte[]</tt>.
  *
  * Integers, doubles, booleans, and strings are supported.  Vectors (i.e. arrays) of these types are also supported by
  * converting the first element.  Matrices (i.e. rectangular arrays of arrays) are not supported.
  * @param identifier The string to be evaluated.
  * @return   The evaluated byte array.
  */
  public byte[] getR1(String identifier) { return c.getR1(identifier); }

  /** Calls <b><tt>get(identifier,false)</tt></b> and converts the result to an <tt>int[][]</tt>.
  *
  * Matrices (i.e. rectangular arrays of arrays) of integers, doubles, booleans, and strings are supported.  Integers, doubles,
  * booleans, and strings themselves are not supported.  Vectors (i.e. arrays) of these
  * types are also not supported.
  * @param identifier The string to be evaluated.
  * @return   The evaluated int matrix.
  */
  public int[][] getI2(String identifier) { return c.getI2(identifier); }

  /** Calls <b><tt>get(identifier,false)</tt></b> and converts the result to a <tt>double[][]</tt>.
  *
  * Matrices (i.e. rectangular arrays of arrays) of integers, doubles, booleans, and strings are supported.  Integers, doubles,
  * booleans, and strings themselves are not supported.  Vectors (i.e. arrays) of these
  * types are also not supported.
  * @param identifier The string to be evaluated.
  * @return   The evaluated double matrix.
  */
  public double[][] getD2(String identifier) { return c.getD2(identifier); }

  /** Calls <b><tt>get(identifier,false)</tt></b> and converts the result to a <tt>boolean[][]</tt>.
  *
  * Matrices (i.e. rectangular arrays of arrays) of integers, doubles, booleans, and strings are supported.  Integers, doubles,
  * booleans, and strings themselves are not supported.  Vectors (i.e. arrays) of these
  * types are also not supported.
  * @param identifier The string to be evaluated.
  * @return   The evaluated boolean matrix.
  */
  public boolean[][] getB2(String identifier) { return c.getB2(identifier); }

  /** Calls <b><tt>get(identifier,false)</tt></b> and converts the result to a <tt>String[][]</tt>.
  *
  * Matrices (i.e. rectangular arrays of arrays) of integers, doubles, booleans, and strings are supported.  Integers, doubles,
  * booleans, and strings themselves are not supported.  Vectors (i.e. arrays) of these
  * types are also not supported.
  * @param identifier The string to be evaluated.
  * @return   The evaluated string matrix.
  */
  public String[][] getS2(String identifier) { return c.getS2(identifier); }

  /** Calls <b><tt>getR2(identifier,false)</tt></b> and converts the result to an <tt>byte[][]</tt>.
  *
  * Matrices (i.e. rectangular arrays of arrays) of integers, doubles, booleans, and strings are supported.  Integers, doubles,
  * booleans, and strings themselves are not supported.  Vectors (i.e. arrays) of these
  * types are also not supported.
  * @param identifier The string to be evaluated.
  * @return   The evaluated int matrix.
  */
  public byte[][] getR2(String identifier) { return c.getR2(identifier); }

}

