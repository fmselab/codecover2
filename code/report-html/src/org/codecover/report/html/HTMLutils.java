/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.report.html;

/**
 * Offers utilities for working with HTML-Code. Mostly used for escaping.
 *
 * @author Michael Starzmann
 * @author Robert Hanussek
 * @version 1.0 ($Id: HTMLutils.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class HTMLutils {

    /**
     * The default space character is ' '.
     * @see #replaceWhiteSpacesByNonbreakingSpaces(String, int)
     */
    public static final char[] DEFAULT_SPACES = {' '};

    /**
     * The default tabulation character is '\t'.
     * @see #replaceWhiteSpacesByNonbreakingSpaces(String, int)
     */
    public static final char[] DEFAULT_TABS = {'\t'};

    /**
     * The default HTML code for a single non breaking whitespace is
     * &quot;&amp;nbsp;&quot;.
     * @see #replaceWhiteSpacesByNonbreakingSpaces(String, int)
     */
    public static final String DEFAULT_NBSP = "&nbsp;";


    private char[] spaces;

    private char[] tabs;

    private String nbsp;


    /**
     * Sets the characters which are recognized as spaces (not including
     * tabulation characters). If not set, it defaults to
     * {@link #DEFAULT_SPACES}.
     * <p>
     * Note: Avoid intermixing characters set by this method with characters set
     * by {@link #setTabChars(char[])} because this could raise unexpected
     * results!
     */
    public void setSpaceChars(char[] spaces) {
        this.spaces = spaces;
    }

    /**
     * Sets the characters which are recognized as tabulation characters. If not
     * set, it defaults to {@link #DEFAULT_TABS}.
     * <p>
     * Note: Avoid intermixing characters set by this method with characters set
     * by {@link #setSpaceChars(char[])} because this could raise unexpected
     * results!
     */
    public void setTabChars(char[] tabs) {
        this.tabs = tabs;
    }

    /**
     * Sets the <code>String</code> which is used as non breaking space. If not
     * set, it defaults to {@link #DEFAULT_NBSP}.
     *
     * @throws NullPointerException if given <code>String</code> is
     *                              <code>null</code>
     */
    public void setNBSP(String nbsp) {
        if(nbsp != null) {
            this.nbsp = nbsp;
        } else {
            throw new NullPointerException("Non breaking space string mustn't" +
                    " be null.");
        }
    }

    /**
     * Returns the characters which are recognized as spaces.
     */
    public char[] getSpaceChars() {
        return this.spaces;
    }

    /**
     * Returns the characters which are recognized as tabulation characters.
     */
    public char[] getTabChars() {
        return this.tabs;
    }

    /**
     * Returns the <code>String</code> which is used as non breaking space.
     */
    public String getNBSP() {
        return this.nbsp;
    }

    public HTMLutils() {
        this.spaces = HTMLutils.DEFAULT_SPACES;
        this.tabs = HTMLutils.DEFAULT_TABS;
        this.nbsp = HTMLutils.DEFAULT_NBSP;
    }

    public static String escape(char c) {
        return escape(new String() + c);
    }
    
    public static String escape(String text) {
        if (text == null) {
            return null;
        } else {
        char[] chars = text.toCharArray();
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < chars.length; i++) {
            switch (chars[i]) {
                case '<':
                    sb.append("&lt;");
                    break;
                case '>':
                    sb.append("&gt;");
                    break;
                case '&':
                    sb.append("&amp;");
                    break;
                case '"':
                    sb.append("&quot;");
                    break;
                default:
                    sb.append(chars[i]);
                    break;
            }
        }
        return sb.toString();
        }
    }

    /**
     * Escape a String for usage in an URI only / is kept as a special
     *  character (the separator for file-Path elements).
     * 
     * @param uriFragment
     * the string to escape
     * @return
     * the escaped string
     */
    public static String escapeURI(String uriFragment) {
        
        /* currently keeps a..z, A..z, '_', '-', '.', '/' and 0..9 as they are
         *  and escapes all  other characters using %-Codes.)
         */
        
        //XXX: write/find more efficient implementation
        char buffer[] = uriFragment.toCharArray();
        StringBuilder sb = new StringBuilder();
        
        /* iterate through characters of uri */
        for (int i = 0; i < buffer.length; i++) {
            
            /* keep only a..z, A..z, '_', '-', '.', '/' and 0..9 */
            if ((buffer[i] < 'a' || buffer[i] > 'z') &&
                    (buffer[i] < 'A' || buffer[i] > 'Z') &&
                    (buffer[i] < '-' || buffer[i] > '9') &&
                    buffer[i] != '_') {
                
                /* transform to hex with even length */
                String hex = Integer.toHexString(buffer[i]);
                if (hex.length() % 2 == 1) {
                    hex = hex + "0";
                }

                /* convert hex to {"%" . <DIGIT> . <DIGIT>} */
                for (int j = 0; j < hex.length(); j += 2) {
                    sb.append('%');
                    sb.append(hex.subSequence(j, j + 2));
                }
            } else {
                
                /* keep this non special character */
                sb.append(buffer[i]);
            }
        }
        
        return sb.toString();
    }
    
    /**
     * Replaces whitespace of given line with non breaking spaces. Tabulation
     * characters are expanded to resemble correct column widths.
     *
     * @param line      the line which whitespace is to be replaced
     * @param tabWidth  the width of tabulation characters (column width), if 0
     *                  tabulation characters are just deleted
     * @return          the <code>String</code> with whitespaces replaced by
     *                  non breaking spaces
     *
     * @see #setSpaceChars(char[])
     * @see #setTabChars(char[])
     * @see #setNBSP(String)
     */
    public String replaceWhiteSpacesByNonbreakingSpaces(String line,
                                                        int tabWidth) {
        return replaceWhiteSpacesByNonbreakingSpaces(line, tabWidth, false);
    }

    /**
     * Replaces whitespace of given line with non breaking spaces. Tabulation
     * characters are expanded to resemble correct column widths.
     *
     * @param line          the line which whitespace is to be replaced
     * @param tabWidth      the width of tabulation characters (column width),
     *                      if 0 tabulation characters are just deleted
     * @param ignoreSingle  single spaces are not replaced, only multiple which
     *                      are usually indented as indentation
     * 
     * @return  the <code>String</code> with whitespace replaced by non breaking
     *          spaces
     *
     * @see #setSpaceChars(char[])
     * @see #setTabChars(char[])
     * @see #setNBSP(String)
     */
    public String replaceWhiteSpacesByNonbreakingSpaces(String line,
                                                        int tabWidth,
                                                        boolean ignoreSingle) {
        // gatekeeper for null values
        if(line == null) {
            return null;
        }

        char[] in = line.toCharArray();
        StringBuilder out = new StringBuilder();
        int nbrToAppend;
        tabWidth = Math.abs(tabWidth);
        for(int inPos = 0, outPos = 0 ; inPos < in.length ; inPos++) {
            // if tab found replace it with non breaking spaces
            if(this.isTab(in[inPos])) {
                if(tabWidth != 0) {
                    // calculate number of spaces to insert
                    nbrToAppend = tabWidth - (outPos % tabWidth);
                    // append spaces instead of the current tab
                    for(int aPos = 0 ; aPos < nbrToAppend ; aPos++) {
                        out.append(this.nbsp);
                        outPos++;
                    }
                } // else if tabWidth == 0 tab character is deleted
            // if normal space found replace it with non breaking space
            } else if(this.isSpace(in[inPos])) {
                if(!ignoreSingle
                    || (inPos + 1 < in.length && !this.isSpace(in[inPos+1]))) {
                    out.append(this.nbsp);
                    outPos++;
                }
            // else append current character
            } else {
                out.append(in[inPos]);
                outPos++;
            }
        }
        return out.toString();
    }
    
    
    /**
     * Determines if the given character is a space character by comparing it to
     * the internal list of space characters which can be set by
     * {@link #setSpaceChars(char[])}.
     * @param c the character to check
     * @return  <code>true</code> if the character is a space character;
     *          <code>false</code> otherwise.
     */
    
    private boolean isSpace(char c) {
        for(char space : this.spaces) {
            if(c == space) {
                return true;
            }
        }
        return false;
    }

    /**
     * Determines if the given character is a tabulation character by comparing
     * it to the internal list of tabulation characters which can be set by
     * {@link #setTabChars(char[])}.
     * @param c the character to check
     * @return  <code>true</code> if the character is a tabulation character;
     *          <code>false</code> otherwise.
     */
    private boolean isTab(char c) {
        for(char tab : this.tabs) {
            if(c == tab) {
                return true;
            }
        }
        return false;
    }

    public String pathUp(int levels) {
        String path = "../";
        for (int i = 0; i < levels; i++) {
            path += "../";
        }
        return path;    
    }
    
}
