/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This file may be used, modifies and redistributed     *
 * under the terms of either the Eclipse Public License v1.0 which            *
 * accompanies this distribution and is available at                          *
 * http://www.eclipse.org/legal/epl-v10.html or the MIT license, available at *
 * http://www.opensource.org/licenses/mit-license.php                         *
 ******************************************************************************/

package org.codecover.model.utils;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: StringUtil.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class StringUtil {

  /**
   * Parses a String, that was escaped using
   * <code>MeasurementConstants#escapeName(String)</code> or
   * <code>MeasurementConstants#escapeComment(String)</code> and surrounded by <code>""</code>.<br>
   * <code>"This is:\nfunny."</code> &rarr;
   * <code>This is:<br>
   * funny.</code>
   * 
   * @param stringLiteral
   *          The escaped String.
   * @return
   *          The parsed and unescaped String.
   */
  public static String parseStringLiteral(String stringLiteral) {
      if (stringLiteral.charAt(0) != '"' ||
        stringLiteral.charAt(stringLiteral.length() - 1) != '"') {
        throw new IllegalArgumentException(stringLiteral +
          " has a wrong format: not surrounded by \"\"");
      }

      return parseNonQuotedStringLiteral(stringLiteral.substring(1,
          stringLiteral.length() -1));
  }
  
  /**
   * Parses a String, that was escaped using
   * <code>MeasurementConstants#escapeName(String)</code> or
   * <code>MeasurementConstants#escapeComment(String)</code>.<br>
   * <code>This is:\nfunny.</code> &rarr;
   * <code>This is:<br>
   * funny.</code>
   * 
   * @param stringLiteral
   *          The escaped String.
   * @return
   *          The parsed and unescaped String.
   *          
   * @see #parseStringLiteral(String)
   */
  public static String parseNonQuotedStringLiteral(String stringLiteral) {
      if (stringLiteral.indexOf('\\') == -1) {
          // there was nothing escaped
          return stringLiteral;
      }
      boolean backslashBackuped = false;
      StringBuilder buffer = new StringBuilder(stringLiteral.length());
      for (int i = 0; i <= stringLiteral.length() - 1; i++) {
          char c = stringLiteral.charAt(i);
          if (backslashBackuped) {
              backslashBackuped = false;
              switch (c) {
              case 'n':
                  buffer.append('\n');
                  break;
              case 'r':
                  buffer.append('\r');
                  break;
              case 'f':
                  buffer.append('\f');
                  break;
              case 't':
                  buffer.append('\t');
                  break;
              case '"':
                  buffer.append('"');
                  break;
              case '\\':
                  buffer.append('\\');
                  break;
              default:
                  throw new IllegalArgumentException(stringLiteral +
                          " has a wrong format");
              }
          }
          else if (c == '\\') {
              backslashBackuped = true;
          } else {
              buffer.append(c);
          }
      }

      return buffer.toString();
  }
}
