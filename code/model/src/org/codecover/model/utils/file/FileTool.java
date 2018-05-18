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

package org.codecover.model.utils.file;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.nio.charset.Charset;

import org.codecover.CodeCoverInfo;

/**
 * Some methods that are related to file handling and are needed for common
 * usage.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: FileTool.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class FileTool
{
    private static final int COPY_BUFFER_SIZE = 1024 * 32;

    /**
     * Reads out a file with <code>{@link CodeCoverInfo#CODE_FILE_CHARSET}</code>
     * and dumps it to a String.
     * 
     * @param file The file to read.
     *  
     * @return The content of the file as a String.
     * 
     * @throws IOException If there was an exception while reading.
     */
    public static String getContentFromFile(File file) throws IOException {
        return getContentFromFile(file, CodeCoverInfo.CODE_FILE_CHARSET);
    }

    /**
     * Reads out a file with a specified {@link Charset} and dumps it to a
     * String.
     * 
     * @param file The file to read.
     * @param charset The {@link Charset} to use.
     *  
     * @return The content of the file as a String.
     * 
     * @throws IOException If there was an exception while reading.
     */
    public static String getContentFromFile(File file, Charset charset)
        throws IOException {
        FileInputStream fileInputStream = new FileInputStream(file);
        InputStreamReader inputStreamReader = new InputStreamReader(
                fileInputStream, charset);

        int sourceFileLength = (int)file.length();
        char[] sourceFileArray = new char[sourceFileLength];
        try
        {
            sourceFileLength = Math.max(inputStreamReader.read(
                  sourceFileArray, 0, sourceFileLength), 0);
        } finally {
            inputStreamReader.close();
        }

        return new String(sourceFileArray, 0, sourceFileLength);
    }
    
    /**
     * Reads out a stream with <code>{@link CodeCoverInfo#CODE_FILE_CHARSET}</code>
     * and dumps it to a String.
     * 
     * @param stream The stream to use.
     *  
     * @return The content of the file as a String.
     * 
     * @throws IOException If there was an exception while reading.
     */
    public static String getContentFromStream(InputStream stream)
        throws IOException {
        return getContentFromStream(stream, CodeCoverInfo.CODE_FILE_CHARSET);
    }

    /**
     * Reads out a stream with a specified {@link Charset} and dumps it to a
     * String.
     * 
     * @param stream The stream to use.
     * @param charset The {@link Charset} to use.
     *  
     * @return The content of the file as a String.
     * 
     * @throws IOException If there was an exception while reading.
     */
    public static String getContentFromStream(InputStream stream, Charset charset)
        throws IOException {
        InputStreamReader reader = new InputStreamReader(stream, charset);
        return getContentFromReader(reader);
    }

    /**
     * Reads out a reader and dumps it to a String.
     * 
     * @param reader The {@link Reader} to use.
     *  
     * @return The content of the file as a String.
     * 
     * @throws IOException If there was an exception while reading.
     */
    public static String getContentFromReader(Reader reader)
    throws IOException {
      char[] buffer = new char[COPY_BUFFER_SIZE];
      StringBuilder builder = new StringBuilder();
      try {
        while (true) {
          int iReadSize = reader.read(buffer);
          if (iReadSize > 0) {
            builder.append(buffer, 0, iReadSize);
          } else {
            break;
          }
        }
      } finally {
        reader.close();
      }
      
      return builder.toString();
    }

    /**
     * Gets the extension of a file.<br>
     * <br>
     * An extension is definded as the String after the last dot. The extension
     * is always returned in lower case. If there is no dot, the extensions is
     * defined as "".
     * 
     * <pre>
     *      getExtension(Main.java) == &quot;java&quot;
     *      getExtension(Main.java.in) == &quot;in&quot;
     *      getExtension(Main) == &quot;&quot;
     * </pre>
     * 
     * @param file
     *            The given file.
     * @return The extension of the file.
     * 
     * @see #getExtension(String)
     */
    public static String getExtension(File file) {
        return getExtension(file.getName());
    }

    /**
     * Gets the extension of a file.<br>
     * <br>
     * An extension is definded as the String after the last dot. The extension
     * is always returned in lower case. If there is no dot, the extensions is
     * defined as "".
     * 
     * <pre>
     *      getExtension(Main.java) == &quot;java&quot;
     *      getExtension(Main.java.in) == &quot;in&quot;
     *      getExtension(Main) == &quot;&quot;
     * </pre>
     * 
     * @param fileName
     *            The name of the file.
     * @return The extension of the file.
     */
    public static String getExtension(String fileName) {
        String extensions = "";
        int i = fileName.lastIndexOf('.');
        if (i >= 0 && i < fileName.length() - 1) {
            extensions = fileName.substring(i + 1).toLowerCase();
        }
        return extensions;
    }

    /**
     * Copies a file <code>source</code> to the file <code>target</code>.<br>
     * <br>
     * If the <code>target</code> exists, it is overwritten, else it is created.
     * All parent folders of <code>target</code> are created before.  
     *  
     * @param source The source file to copy.
     * @param target The destination, where to copy source.
     * @throws IOException If there occur read / write exceptions.
     */
    public static void copy(File source, File target) throws IOException {
        File absoluteSource = source.getAbsoluteFile();
        File absoluteTarget = target.getAbsoluteFile();

        absoluteTarget.getParentFile().mkdirs();

        FileInputStream fileInputStream = new FileInputStream(absoluteSource);
        FileOutputStream fileOutputStream = new FileOutputStream(absoluteTarget);

        try
        {
          byte[] buffer = new byte[Math.min((int)absoluteSource.length(), COPY_BUFFER_SIZE)];
          while (true) {
              int iReadSize = fileInputStream.read(buffer);
              if (iReadSize > 0) {
                  fileOutputStream.write(buffer, 0, iReadSize);
              } else {
                  break;
              }
          }
          fileOutputStream.flush();
        } finally {
          fileInputStream.close();
          fileOutputStream.close();
        }
    }

    /**
     * Copies a file <code>source</code> to the file <code>target</code> by
     * honoring charsets.<br>
     * <br>
     * If the <code>target</code> exists, it is overwritten, else it is created.
     * All parent folders of <code>target</code> are created before.  
     *  
     * @param source The source file to copy.
     * @param sourceCharset The charset of the source file.
     * @param target The destination, where to copy source.
     * @param targetCharset The charset of the target file.
     * @throws IOException If there occur read / write exceptions.
     */
    public static void copy(File source, Charset sourceCharset,
                            File target, Charset targetCharset) throws IOException {
      File absoluteSource = source.getAbsoluteFile();
      File absoluteTarget = target.getAbsoluteFile();

      absoluteTarget.getParentFile().mkdirs();

      FileInputStream fileInputStream = new FileInputStream(absoluteSource);
      InputStreamReader reader = new InputStreamReader(fileInputStream, sourceCharset);
      FileOutputStream fileOutputStream = new FileOutputStream(absoluteTarget);
      OutputStreamWriter writer = new OutputStreamWriter(fileOutputStream, targetCharset);

      try
      {
        char[] buffer = new char[COPY_BUFFER_SIZE];
        while (true) {
          int iReadSize = reader.read(buffer);
          if (iReadSize > 0) {
            writer.write(buffer, 0, iReadSize);
          } else {
            break;
          }
        }
        writer.flush();
      } finally {
        reader.close();
        writer.close();
      }
    }
}
