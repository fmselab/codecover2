package org.codecover.report.csv;

import java.io.*;
import java.util.*;

/**
 * A simple writer for CSV-files taking care of the escaping of characters in
 * the fields needed for the CSV-format. This class creates CSV-files following
 * the RFC 4180 (see e.g. at http://tools.ietf.org/html/rfc4180)
 * 
 * @author Michael Starzmann
 * @version 1.0 ($Id: CSVWriter.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CSVWriter {

    private final static String FIELD_SEPERATOR = ",";
    
    private final static String LINE_SEPERATOR = "\r\n";
    
    private final static Locale locale = Locale.US;
    
    private final PrintWriter printer;
    
    private List<String> line = new Vector<String>();
    
    /**
     * Creates a new CSVWriter writing the csv-Data to out. NB: the CSVWriter
     * has to be closed to have the data been written.
     * 
     * @param out   the OutputStream this writer should write to
     */
    public CSVWriter (OutputStream out) {
        try {
            // We always use UTF-8 here because we say so in the content-type
            printer = new PrintWriter(new OutputStreamWriter (out, "UTF-8"));
        } catch (UnsupportedEncodingException e) {
            // UTF-8 should always be supported.
            throw new RuntimeException(e);
        }
    }
    
    /**
     * Prints the (if necessary escaped) field to the csv-file 
     * 
     * @param field   a data that should be written in one cell of the csv-table
     */
    public void print(String field) {
        line.add(escape(field));
    }
    
    /**
     * Prints the (if necessary escaped) format string with the placeholders
     * following the syntax of String.format replaced by the args as a field to
     * the csv-file 
     * 
     * @param format   the data that should be (after replacement of the place-
     *                 holders with the corresponding args) written in one cell
     *                 of the csv-table
     * @param args     the args placeholders in format access
     */
    public void printf(String format, Object ... args) {
        line.add(escape(String.format(locale, format, args)));
    }
    
    /**
     * ends the line with LINE_SEPERATOR, puts it out and starts a new one.
     * NB: to have the data written, you have to close this writer.
     */
    public void newLine() {
        StringBuilder sb = new StringBuilder();
        if (line.size() > 0) {
            for (int i = 0; i < line.size() - 1; i++) {
               sb.append(line.get(i));
               sb.append(FIELD_SEPERATOR);
            }
            sb.append(line.get(line.size()-1));
        }
        sb.append(LINE_SEPERATOR);
        printer.print(sb.toString());
        line.clear();
    }
    
    /**
     * closes this writer which makes him write all its data.
     * 
     * @throws IOException   something with the actual output/closing went wrong
     */
    public void close() throws IOException {
        if (line.size() > 0) {
            newLine();
        }
        printer.flush();
        printer.close();
    }
    
    /**
     * if s contains ", they are escaped by doubling, if it contains other
     * special characters, s is enclosed in ""
     * 
     * @param s   the string to escape
     * @return    the escaped string s
     */
    private String escape(String s) {
        if (   s.contains("\n")            || s.contains("\r")
            || s.contains("\"")            || s.contains(",")
            || s.contains(FIELD_SEPERATOR) || s.contains(LINE_SEPERATOR)) {
            // quoting needed
            return "\"" + s.replace("\"", "\"\"") + "\"";
        } else {
            // no quoting needed
            return s;
        }
    }
}
