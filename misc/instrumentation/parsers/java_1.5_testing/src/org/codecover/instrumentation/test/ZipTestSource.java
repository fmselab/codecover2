package org.codecover.instrumentation.test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.Reader;
import java.nio.charset.Charset;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: ZipTestSource.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class ZipTestSource implements JavaTestSource {

    private final ZipEntry entry;

    private final ZipFile file;

    private final Charset charset;

    public ZipTestSource(ZipFile file, ZipEntry entry, Charset charset) {
        this.file = file;
        this.entry = entry;
        this.charset = charset;
    }

    public ZipTestSource(ZipFile file, ZipEntry entry) {
        this(file, entry, Charset.defaultCharset());
    }

    public String getFullName() {
        return this.file.getName() + "!" + getName();
    }

    public String getName() {
        return this.entry.getName();
    }

    public long getSize() {
        return this.entry.getSize();
    }
    
    public Charset getCharset() {
        return this.charset;
    }

    public Reader getReader() throws IOException {
        InputStream inputStream = this.file.getInputStream(this.entry);
        InputStreamReader inputStreamReader = new InputStreamReader(inputStream, this.charset);
        BufferedReader bufferedReader = new BufferedReader(inputStreamReader);
        return bufferedReader;
    }
}
