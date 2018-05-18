package org.codecover.instrumentation.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.Charset;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: FileTestSource.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class FileTestSource implements JavaTestSource {

    private final File file;

    private final Charset charset;

    public FileTestSource(String fileName, Charset charset) throws IOException {
        this.file = new File(fileName).getCanonicalFile();
        this.charset = charset;
    }

    public FileTestSource(String fileName) throws IOException {
        this(fileName, Charset.defaultCharset());
    }

    public String getFullName() {
        return this.file.getPath();
    }

    public String getName() {
        return this.file.getName();
    }

    public long getSize() {
        return this.file.length();
    }
    
    public Charset getCharset() {
        return this.charset;
    }

    public Reader getReader() throws IOException {
        FileInputStream inputStream = new FileInputStream(this.file);
        InputStreamReader inputStreamReader = new InputStreamReader(inputStream, this.charset);
        BufferedReader bufferedReader = new BufferedReader(inputStreamReader);
        return bufferedReader;
    }
}
