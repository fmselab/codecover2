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

package org.codecover.report;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.regex.MatchResult;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;

import org.codecover.model.utils.Logger;
import org.codecover.report.exceptions.TemplateIncompatibleException;
import org.codecover.report.exceptions.TemplateParseException;
import org.codecover.report.exceptions.TemplateReadException;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * Represents a report template. Reads a template from a (XML) file or DOM tree
 * and provides its content via convenient getter methods.
 *
 * @author Robert Hanussek
 * @version 1.0 ($Id: Template.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
// XXX: use an XML schema, instead of the manual validation via isParentA
public class Template {

    ////////////////////////////////////////////////////////////
    // fields

    private File file;

    private Document document;

    private Node template;

    private String reportGeneratorName;

    private String pluginName;

    private String documentVersion;

    private String templateVersion;

    private Map<String,String> names;

    private Map<String,String> descriptions;

    /**
     * The logger, mustn't be <code>null</code>, set to <code>Logger.NULL</code>
     * if you don't need a logger (this is what {@link #setLogger(Logger)}
     * does, when called with the argument being <code>null</code>).
     */
    private Logger logger;

    ////////////////////////////////////////////////////////////
    // constants

    /**
     * The version of the report template the Template is able to read.
     */
    public static final String READER_VERSION = "1.0";

    /*
     * XML-elements used in the templates (only public to allow JUnit tests)
     */
    public static final String REPORT_NAMESPACE
            = "http://www.codecover.org/xml/report-template";
    public static final String XML_NAMESPACE
            = "http://www.w3.org/XML/1998/namespace";
    public static final String REPORT_ELEMENT = "report";
    public static final String REPORT_VERSION_ATTR = "version";
    public static final String GENERATOR_ELEMENT = "generator";
    public static final String PLUGIN_ELEMENT = "plugin";
    public static final String NAME_ELEMENT = "name";
    public static final String NAME_LANG_ATTR = "lang";
    public static final String DESC_ELEMENT = "description";
    public static final String DESC_LANG_ATTR = "lang";
    public static final String TEMPLATE_ELEMENT = "template";
    public static final String TEMPLATE_VERSION_ATTR = "version";

    private static final String LANG_CODE_SEPARATOR = "-";

    ////////////////////////////////////////////////////////////
    // constructor, initializer

    /**
     * Constructs an object of class <code>Template</code>.
     * 
     * @param logger    the logger, mustn't be <code>null</code>, set to
     *                  <code>Logger.NULL</code> if you don't need logging
     * 
     * @throws NullPointerException if the given logger is <code>null</code>
     */
    public Template(Logger logger) throws NullPointerException {
        this.setLogger(logger);
        this.init();
    }

    private void init() {
        this.reportGeneratorName = null;
        this.pluginName = null;
        this.documentVersion = null;
        this.templateVersion = null;
        this.names = new HashMap<String,String>();
        this.descriptions = new HashMap<String,String>();
        this.file = null;
        this.document = null;
        this.template = null;
    }

    ////////////////////////////////////////////////////////////
    // setters

    /**
     * Sets the template file.
     *
     * @param file  the template file
     */
    public void setFile(File file) {
        this.file = file;
    }

    /**
     * Sets the template which is represented by a DOM tree.
     *
     * @param document
     * the template represented by the root node of a DOM tree
     */
    public void setDocument(Document document) {
        this.document = document;
    }

    /**
     * Sets the logger, mustn't be <code>null</code>, set to
     * <code>Logger.NULL</code> if you don't need a logger.
     * 
     * @param logger    the logger
     * 
     * @throws NullPointerException if the given logger is <code>null</code>
     */
    public void setLogger(Logger logger) throws NullPointerException {
        if(logger == null) {
            throw new NullPointerException("logger mustn't be null");
        }
        this.logger = logger;
    }

    ////////////////////////////////////////////////////////////
    // getters

    /**
     * Returns the whole template document as an
     * <code>org.w3c.dom.Document</code> (DOM tree).
     * 
     * @return  the whole template document as an
     *          <code>org.w3c.dom.Document</code> (DOM tree).
     */
    public Document getDocument() {
        return this.document;
    }

    /**
     * Returns the node of the DOM tree of the template document which
     * represents the root of the template specific part (in other words: the
     * node which represents the &lt;template&nbsp;...&gt;-tag in the XML
     * document).
     * 
     * @return  the node of the DOM tree of the template document which
     *          represents the root of the template specific part
     */
    public Node getTemplate() {
        return this.template;
    }

    /**
     * Returns the name of the extension of the report generator.
     * 
     * @return  the name of the extension of the report generator
     */
    public String getReportGeneratorName() {
        return this.reportGeneratorName;
    }

    /**
     * Returns the name of the plugin which handles the report generator
     * extensions.
     * 
     * @return  the name of the plugin which handles the report generator
     *          extensions
     */
    public String getPluginName() {
        return this.pluginName;
    }

    /**
     * Returns the version of the document structure of the report template.
     * This version is contained in the &lt;report&nbsp;...&gt;-tag of the
     * template.
     * 
     * @return  the version of the document structure of the report template
     */
    public String getDocumentVersion() {
        return this.documentVersion;
    }

    /**
     * Returns the version of the template specific part. This version is
     * contained in the &lt;template&nbsp;...&gt;-tag of the template.
     * 
     * @return  the version of the template specific part
     */
    public String getTemplateVersion() {
        return this.templateVersion;
    }

    /**
     * Returns the names of this template as a <code>Map</code> with the
     * language tags as the keys and the names as the values.
     * 
     * @return  the names of this template as a <code>Map</code> with the
     *          language tags as the keys and the names as the values
     */
    public Map<String, String> getNames() {
        return this.names;
    }

    /**
     * Returns the name of this template which is marked with the given
     * language tag, which must be RFC4646-compliant. If no name with the
     * given language tag exists for this Template, the method can fall back on
     * a name which language tag begins with the same subtags as the
     * given language tag.
     * <p>
     * For example, if the given language tag is 'de' but there only exist
     * names with language tags 'de-DE' or 'de-AT', one of these
     * names is returned. Or vice versa, if the given language tag is
     * 'de-DE' but there only exists a name with the language tag 'de',
     * this name is returned.
     * <p>
     * This simplified matching was implemented because the method should run
     * rather fast and considering that in most cases the language subtag is
     * sufficient this is not too much of a trade-off. But notice that this
     * behaviour doesn't always return the closest matching language. For
     * example in the given language tag 'zh-Hans-CN' the script subtag is used.
     * Let's assume there are two names with language tags 'zh-Hans-TW' and
     * 'zh-CN'. Since the tag 'zh-Hans-TW' matches 'zh-Hans-CN' in the first two
     * subtags, the name with the tag 'zh-Hans-TW' is returned, and <em>not</em>
     * the name with the tag 'zh-CN' were only the first subtag matches.
     *
     * @param lang
     * the RFC4646-compliant language tag by which a name is searched for
     * @param fallback
     * if <code>true</code> and no name with the given language tag exists, a
     * name, which language tag begins with the same subtags as the given
     * language tag, is returned. If <code>false</code> only names with
     * the exact given language tag are returned, if no name with a
     * matching language tag exists <code>null</code> is returned
     *
     * @return
     * if <code>fallback</code> is <code>true</code> and no name with the
     * given language tag exists, a name, which language tag begins with
     * the same subtags as the given language tag, is returned. If
     * <code>fallback</code> is <code>false</code>, only names with the
     * exact given language tag are returned. If no name with a matching
     * language tag could be found, <code>null</code> is returned.
     */
    public String getName(String lang, boolean fallback) {
        return Template.findStringByLang(lang, this.names, fallback);
    }

    /**
     * Returns the English name of this template.
     *
     * @return
     * the English name of this template or <code>null</code> if there is no
     * English name for this template. More precisely this method returns a name
     * which is marked with the language tag 'en'. If such a name does not exist
     * a name with language tag 'en-*' (for example 'en-US') is returned.
     */
    public String getEnglishName() {
        return this.getName("en", true);
    }

    /**
     * Returns the descriptions of this template as a <code>Map</code> with the
     * language tags as the keys and the descriptions as the values.
     * 
     * @return  the descriptions of this template as a <code>Map</code> with the
     *          language tags as the keys and the descriptions as the values
     */
    public Map<String, String> getDescriptions() {
        return this.descriptions;
    }

    /**
     * Returns the description of this template which is marked with the given
     * language tag, which must be RFC4646-compliant. If no description with the
     * given language tag exists for this Template, the method can fall back on
     * a description which language tag begins with the same subtags as the
     * given language tag.
     * <p>
     * For example, if the given language tag is 'de' but there only exist
     * descriptions with language tags 'de-DE' or 'de-AT', one of these
     * descriptions is returned. Or vice versa, if the given language tag is
     * 'de-DE' but there only exists a description with the language tag 'de',
     * this description is returned. This simplified matching was implemented
     * because the method should run rather fast and considering that in most
     * cases the language subtag is sufficient this is not too much of a
     * trade-off. But notice that this behavior doesn't always return the
     * closest matching language. For example in the given language tag
     * 'zh-Hans-CN' the script subtag is used. Let's assume there are two
     * descriptions with language tags 'zh-Hans-TW' and 'zh-CN'. Since the tag
     * 'zh-Hans-TW' matches 'zh-Hans-CN' in the first two subtags, the
     * description with the tag 'zh-Hans-TW' is returned, and <em>not</em> the
     * description with the tag 'zh-CN' were only the first subtag matches.
     *
     * @param lang
     * the RFC4646-compliant language tag by which a description is searched for
     * @param fallback
     * if <code>true</code> and no description with the given language tag
     * exists, a description, which language tag begins with the same subtags as
     * the given language tag, is returned. If <code>false</code> only
     * descriptions with the exact given language tag are returned, if no
     * description with a matching language tag exists <code>null</code> is
     * returned
     *
     * @return
     * if <code>fallback</code> is <code>true</code> and no description with the
     * given language tag exists, a description, which language tag begins with
     * the same subtags as the given language tag, is returned. If
     * <code>fallback</code> is <code>false</code>, only descriptions with the
     * exact given language tag are returned. If no description with a matching
     * language tag exists, <code>null</code> is returned.
     */
    public String getDescription(String lang, boolean fallback) {
        return Template.findStringByLang(lang, this.descriptions, fallback);
    }

    /**
     * Returns the English description of this template.
     *
     * @return
     * the English description of this template or <code>null</code> if there is
     * no English description for this template. More precisely this method
     * returns a description which is marked with language tag 'en'. If such a
     * description does not exist a description with language tag 'en-*' (for
     * example 'en-US') is returned.
     */
    public String getEnglishDescription() {
        return this.getDescription("en", true);
    }

    ////////////////////////////////////////////////////////////
    // language tag handling methods

    private static String findStringByLang( String lang,
                                            Map<String,String> strings,
                                            boolean fallback)
    {
        String str;
        String closestLang;
        if((str = strings.get(lang)) != null) {
            return str;
        } else if(fallback) {
            if((closestLang =
                Template.findClosestLangTag(strings.keySet(), lang))
                    != null) {
                return strings.get(closestLang);
            } else {
                return null;
            }
        } else {
            return null;
        }
    }

    private static String findClosestLangTag(Set<String> langTags, String lang){
        String oldPrefix = "";
        String curPrefix = Template.headOfLangTag(lang, 0); //current prefix
        //this.logger.debug("curPrefix: "+curPrefix);
        Set<String> oldExtractedKeySet = new HashSet<String>();
        Set<String> curExtractedKeySet
                = findLangTagsByPrefix(langTags, curPrefix);
        for(    int limit = 1 ;
                !curExtractedKeySet.isEmpty() && oldPrefix != curPrefix ;
                limit++ ){
            oldPrefix = curPrefix;
            curPrefix = Template.headOfLangTag(lang, limit);
            oldExtractedKeySet = curExtractedKeySet;
            curExtractedKeySet = findLangTagsByPrefix(langTags,curPrefix);
        }
        /* if oldExtractedKeySet is not empty, that is if we found one or
         * more language tags which first n subtags match the first n
         * subtags of the requested language tag, return one of these
         * language tags
         */
        if(oldExtractedKeySet.iterator().hasNext()) {
            return oldExtractedKeySet.iterator().next();
        } else {
            return null;
        }
    }

    private static Set<String> findLangTagsByPrefix(Set<String> keySet,
            String prefix) {
        Set<String> subSet = new HashSet<String>();
        String curKey; // current key
        for(Iterator<String> i = keySet.iterator() ; i.hasNext() ; ) {
            curKey = i.next();
            if(curKey.equals(prefix) || curKey.startsWith(prefix
                    +Template.LANG_CODE_SEPARATOR)) {
                subSet.add(curKey);
            }
        }
        return subSet;
    }

    private static String headOfLangTag(String lang, int limit) {
        int indexOfDelimitingSep =
            findIndexOfNthOccurrence(lang, Template.LANG_CODE_SEPARATOR, limit);
        if(indexOfDelimitingSep >= 0) {
            return lang.substring(0, indexOfDelimitingSep);
        } else {
            return lang;
        }
    }

    private static int findIndexOfNthOccurrence(String str, String find, int n){
        int curPos = -1;
        int occurrence = -1;
        do {
            curPos = str.indexOf(find, curPos+1);
            occurrence++;
        }while(occurrence < n && curPos != -1);
        return curPos;
    }

    ////////////////////////////////////////////////////////////
    // read methods

    /**
     * Reads the settings (e.g. path to the class of the required report
     * generator) from a template file or <code>org.w3c.dom.Document</code> into
     * this object, depending on what has been set via
     * {@link #setFile(File)} or {@link #setDocument(Document)}. If an
     * error occurs while reading the template file and an exception is thrown,
     * all fields of this object are set to their initial values to avoid
     * undefined states.
     * <p>
     * For details, like exceptions, see {@link #readFile()} and
     * {@link #readDocument()}. This method only dispatches processing to one of
     * these methods.
     */
    public void read() throws   NullPointerException,
                                TemplateParseException,
                                TemplateReadException,
                                TemplateIncompatibleException
    {
        if(this.file != null) {
            this.readFile();
        } else {
            this.readDocument();
        }
    }

    /**
     * Reads the settings (e.g. path to the class of the required report
     * generator) from a template file into this object. If an error occurs
     * while reading the template file and an exception is thrown, all fields
     * of this object are set to their initial values to avoid undefined states.
     *
     * @throws  NullPointerException
     * if the {@linkplain #setFile(File) file} is not set
     * @throws  TemplateReadException
     * <ul>
     * <li>if the runtime environment can't provide a
     * <code>DocumentBuilderFactory</code> which meets the requirements
     * (encapsulates a <code>FactoryConfigurationError</code>)</li>
     * <li>if the runtime environment can't provide a parser which meets the
     * requirements (encapsulates a
     * <code>ParserConfigurationException</code>)</li>
     * <li>if an error occurs while reading the template file (encapsulates a
     * <code>IOException</code>)</li>
     * <li>if an error occurs while parsing the template file (encapsulates a
     * <code>SAXException</code>)</li>
     * </ul>
     * @throws  TemplateParseException
     * <ul>
     * <li>if an XML element has an invalid position in the template file </li>
     * <li>if an XML element doesn't have a required attribute </li>
     * <li>if the version (in the report element) of the template has an invalid
     * format </li>
     * <li>if a required value couldn't be read from the template </li>
     * </ul>
     * (Be sure to check <code>getCause()</code> to return <code>null</code>
     * if you want to catch only this type of exception because other exceptions
     * are encapsulated in this one.)
     * @throws  TemplateIncompatibleException
     * if the version (in the report element) of the template is not compatible
     * with Template, that is if the major version numbers don't match
     */
    public void readFile() throws   NullPointerException,
                                    TemplateReadException,
                                    TemplateParseException,
                                    TemplateIncompatibleException
    {
        DocumentBuilder builder = null;
        if(this.file == null) {
            throw new NullPointerException("filepath must be set");
        }
        /*
         * Create the parser (builder) which is used to convert the XML into
         * a DOM tree.
         */
        try {
            DocumentBuilderFactory factory
                    = DocumentBuilderFactory.newInstance();
            factory.setValidating(false);
            factory.setNamespaceAware(true);
            factory.setExpandEntityReferences(true);

            builder = factory.newDocumentBuilder();
            /* removed custom error handling because it didn't allow throwing
             * exceptions encapsulated in TemplateParseException
             * builder.setErrorHandler(this);
             */
        } catch(FactoryConfigurationError e) {
            throw new TemplateReadException(e);
        } catch(ParserConfigurationException e) {
            throw new TemplateReadException(e);
        }
        // Open template file and create DOM tree based on its contents
        try {
            this.document = builder.parse(file);
        } catch(IOException e) {
            this.init();
            throw new TemplateReadException(e);
        } catch(SAXException e) {
            this.init();
            throw new TemplateParseException(e);
        }
        // travers DOM tree and read the settings of the template
        this.readDocument();
    }

    /**
     * Reads the settings (e.g. path to the class of the required report
     * generator) from a DOM tree into this object. If an error occurs
     * while reading the template file and an exception is thrown, all fields
     * of this object are set to their initial values to avoid undefined states.
     *
     * @throws NullPointerException
     * if the {@linkplain #setDocument(Document) document} is not set
     * @throws TemplateParseException
     * if an XML element has an invalid position in the template file <br>
     * if an XML element doesn't have a required attribute <br>
     * if the version (in the report element) of the template has an invalid
     * format <br>
     * if a required value couldn't be read from the template
     * @throws TemplateIncompatibleException
     * if the version (in the report element) of the template is not compatible
     * with Template, that is if the major version numbers don't match
     */
    public void readDocument() throws   NullPointerException,
                                        TemplateParseException,
                                        TemplateIncompatibleException
    {
        if(this.document == null)
            throw new NullPointerException("Document must be set" +
                    " (use method setDocument()).");
        // travers DOM tree and read the settings of the template
        try {
            this.processNode(this.document, null);
        } catch(TemplateParseException e) {
            this.init();
            throw e;
        } catch(TemplateIncompatibleException e) {
            this.init();
            throw e;
        }
        // check if all required values were read from the template file
        try {
            this.verifyReadValues();
        } catch(TemplateParseException e) {
            this.init();
            throw e;
        }
        this.logger.debug("Finished reading report template." +
                        " (document-version: " + this.documentVersion +
                " template-version: " + this.templateVersion + ")");
    }

    /**
     * Checks if all required values were read from the template file. The
     * values are <em>not</em> validated! If a value is missing an exception
     * is thrown, which indicates the missing value in its message.
     * <p>
     * The following values must be read from the template file in order to be
     * verified as correct:
     * <ul>
     * <li>the version of the document structure</li>
     * <li>the path to the class of the report generator</li>
     * <li>a name of the template</li>
     * <li>a description of the template</li>
     * <li>the XML element (the &lt;template&nbsp;...&gt;-tag) which contains
     * the template specific part</li>
     * <li>the version of the template (specific part)</li>
     * </ul>
     *
     * @throws TemplateParseException
     * if a required value was not read from the template
     */
    private void verifyReadValues() throws TemplateParseException {
        if(this.documentVersion == null) {
            throw new TemplateParseException("The version of the document" +
                    " structure is missing" +
                    " (Attribute \""+REPORT_VERSION_ATTR+"\" of element" +
                    " \""+REPORT_ELEMENT+"\"" +
                    " with namespace \""+REPORT_NAMESPACE+"\").");
        }

        if(this.pluginName == null) {
            throw new TemplateParseException("The name of the" +
                    " report generator plugin is missing " +
                    " (Element \""+PLUGIN_ELEMENT+"\"" +
                    " with namespace \""+REPORT_NAMESPACE+"\").");
        }
        if(this.reportGeneratorName == null) {
            throw new TemplateParseException("The name of the" +
                    " report generator is missing " +
                    " (Element \""+GENERATOR_ELEMENT+"\"" +
                    " with namespace \""+REPORT_NAMESPACE+"\").");
        }
        if(this.names.isEmpty()) {
            throw new TemplateParseException("No name of the template" +
                    " found (Element \""+NAME_ELEMENT+"\"" +
                    " with namespace \""+REPORT_NAMESPACE+"\").");
        }
        if(this.descriptions.isEmpty()) {
            throw new TemplateParseException("No description of the template" +
                    " found (Element \""+DESC_ELEMENT+"\"" +
                    " with namespace \""+REPORT_NAMESPACE+"\").");

        }
        if(this.template == null){
            throw new TemplateParseException("The XML element (the" +
                    " <template ...>-tag) which contains the template" +
                    " specific part is missing" +
                    " (Element \""+TEMPLATE_ELEMENT+"\").");
        }
        if(this.templateVersion == null) {
            throw new TemplateParseException("The version of the template" +
                    "(specific part) is missing" +
                    " (Attribute \""+TEMPLATE_VERSION_ATTR+"\" of element" +
                    " \""+TEMPLATE_ELEMENT+"\"" +
                    " with namespace \""+REPORT_NAMESPACE+"\").");
        }
    }

    ////////////////////////////////////////////////////////////
    // processing methods

    private void processNode(Node node, Object parentObject)
    throws  TemplateParseException,
            TemplateIncompatibleException
    {
        short type = node.getNodeType();
        String name = node.getNodeName();
        String namespace = node.getNamespaceURI();
        switch(type) {
            case Node.ELEMENT_NODE:
                if(name.equals(REPORT_ELEMENT)
                        && namespace.equals(REPORT_NAMESPACE)) {
                    processReportElement(node);
                } else if(name.equals(GENERATOR_ELEMENT)
                        && namespace.equals(REPORT_NAMESPACE)) {
                    this.processChildren(node, null);
                } else if(name.equals(PLUGIN_ELEMENT)
                        && namespace.equals(REPORT_NAMESPACE)) {
                    this.processChildren(node, null);
                } else if(name.equals(NAME_ELEMENT)
                        && namespace.equals(REPORT_NAMESPACE)) {
                    processNameElement(node);
                } else if(name.equals(DESC_ELEMENT)
                        && namespace.equals(REPORT_NAMESPACE)) {
                    processDescriptionElement(node);
                } else if(name.equals(TEMPLATE_ELEMENT)) {
                    processTemplateElement(node);
                } else {
                    // unknown elements are ignored
                    this.logger.debug("Unknown element in report template" +
                                " encountered: '" + name + "'" +
                                " (ns: '"+namespace+"')");
                }
            break;
            case Node.TEXT_NODE:
                processText(node, parentObject);
            break;
            case Node.CDATA_SECTION_NODE:
                processText(node, parentObject);
            break;
            case Node.DOCUMENT_NODE:
                processChildren(node, null);
            break;
/*          case Node.ENTITY_NODE:
                this.logger.debug("Entity-Knoten gefunden: "
                        + node.getNodeName());
                processChildren(node);
            break; */
            default:
                this.logger.debug("Found unknown node (in report template): "
                        + node.getNodeName());
/*              // process children of unknown node
                processChildren(node); */
        }
    }

    private void processReportElement(Node node)
    throws  TemplateParseException,
            TemplateIncompatibleException
    {
        Node parentNode = node.getParentNode();
        if(parentNode == null) {
            throw new TemplateParseException(
                    "report-element mustn't be the root of the DOM");
        }

        short parentNodeType = parentNode.getNodeType();
        if(parentNodeType == Node.DOCUMENT_NODE) {
            // process attributes
            try {
                this.documentVersion = getAttributeAsString(
                        REPORT_VERSION_ATTR, null, node);
            } catch(IllegalArgumentException e) {
                throw new TemplateParseException("element " + REPORT_ELEMENT +
                        " requires attribute " + e.getMessage());
            }

            if(getPartOfVersionNumber(this.documentVersion, false) !=
                    getPartOfVersionNumber(READER_VERSION, false)) {
                throw new TemplateIncompatibleException(
                        "version of template: " + this.documentVersion +
                        ", version implementation can read: " + READER_VERSION,
                        TemplateIncompatibleException.VersionField.INNER);
            }

            // process children of report element
            this.processChildren(node, null);
        } else {
            throw new TemplateParseException(
                    "parent node of \"" + REPORT_ELEMENT+"\"-element" +
                    " must be a document node");
        }
    }

    private void processNameElement(Node node) throws  TemplateParseException {
        String lang;
        if(isParentA(REPORT_ELEMENT, node)) {
            try {
                lang = getAttributeAsString(
                        NAME_LANG_ATTR, XML_NAMESPACE, node);
            } catch(IllegalArgumentException e) {
                throw new TemplateParseException("element " + NAME_ELEMENT +
                        " requires attribute " + e.getMessage());
            }

            // names without a language code are ignored
            if(lang.length() > 0) {
                try {
                    this.processChildren(node, lang);
                } catch(TemplateIncompatibleException e) {
                    /*
                     * is catched and ignored because it can't be thrown on this
                     * level, since the report version attribute only appears in
                     * the report element, which is above the level of the name
                     * element
                     */
                }
            }
        }
        else {
            throw new TemplateParseException(
                    "the parent of a name-element must be a report-element");
        }
    }

    private void processDescriptionElement(Node node)
    throws  TemplateParseException {
        String lang;
        if(isParentA(REPORT_ELEMENT, node)) {
            try {
                lang = getAttributeAsString(
                        DESC_LANG_ATTR, XML_NAMESPACE, node);
            } catch(IllegalArgumentException e) {
                throw new TemplateParseException("element " + DESC_ELEMENT +
                        " requires attribute " + e.getMessage());
            }

            try {
                this.processChildren(node, lang);
            } catch(TemplateIncompatibleException e) {
                /*
                 * is catched and ignored because it can't be thrown on this
                 * level, since the report version attribute only appears in the
                 * report element, which is above the level of the description
                 * element
                 */
            }
        }
        else {
            throw new TemplateParseException(
                    "the parent of a description-element must be a" +
                    " report-element");
        }
    }

    private void processTemplateElement(Node node)
    throws TemplateParseException {
        if(isParentA(REPORT_ELEMENT, node)) {
            this.template = node;
            try {
                this.templateVersion = getAttributeAsString(
                        TEMPLATE_VERSION_ATTR, null, node);
            } catch(IllegalArgumentException e) {
                throw new TemplateParseException("element " +
                        TEMPLATE_ELEMENT + " requires attribute " +
                        e.getMessage());
            }
        }
        else {
            throw new TemplateParseException(
                    "the parent of a description-element must be a" +
                    " report-element");
        }
    }

    private void processText(Node node, Object parentObject) {
        if(Template.isParentA(GENERATOR_ELEMENT, REPORT_ELEMENT, node)) {
            this.reportGeneratorName =
                (this.reportGeneratorName == null) ? node.getNodeValue()
                        : (this.reportGeneratorName + node.getNodeValue());
        } else if(Template.isParentA(PLUGIN_ELEMENT, REPORT_ELEMENT, node)) {
            this.pluginName =
                (this.pluginName == null) ? node.getNodeValue()
                        : (this.pluginName + node.getNodeValue());
        } else if(Template.isParentA(NAME_ELEMENT, REPORT_ELEMENT, node)
                && parentObject instanceof String){
            String name = this.names.get(parentObject);
            this.names.put((String)parentObject,
                    ((name != null) ? name : "") + node.getNodeValue());
        } else if(Template.isParentA(DESC_ELEMENT, REPORT_ELEMENT, node)
                && parentObject instanceof String){
            String desc = this.descriptions.get(parentObject);
            this.descriptions.put((String)parentObject,
                    ((desc != null) ? desc : "") + node.getNodeValue());
        }
    }

    private void processChildren(Node node, Object parentObject)
    throws  TemplateParseException,
            TemplateIncompatibleException
    {
        NodeList l = node.getChildNodes();
        for(int i = 0 ; i < l.getLength() ; i++)
            this.processNode(l.item(i), parentObject);
    }

    ////////////////////////////////////////////////////////////
    // processing helper methods (all static)

    /**
     * Checks, if the parent of node <code>n</code> is a specific element by
     * checking the name of the parent node.
     *
     * @param parentName
     * the name, which is to be checked against the parent node of
     * <code>n</code> (e.g., <code>GENERATOR_ELEMENT</code>)
     * @param n
     * the node, which is to be checked
     *
     * @return
     * <code>true</code>, if the parent of <code>n</code> has the name
     * <code>parentName</code>; else <code>false</code>
     */
    private static boolean isParentA(   String parentName,
                                        Node n)
    {
        Node parentNode = n.getParentNode();
        if(parentNode == null)
            return false;
        short parentNodeType = parentNode.getNodeType();
        String parentNodeName = parentNode.getNodeName();
        return (parentNodeType == Node.ELEMENT_NODE
                && parentNodeName.equals(parentName));
    }

    /**
     * Checks, if the parent of node <code>n</code> has the name
     * <code>parentName</code> and if the grandparent has the name
     * <code>grandParentName</code>.
     *
     * @param parentName
     * the name, which is to be checked against the parent node of
     * <code>n</code> (e.g., <code>GENERATOR_ELEMENT</code>)
     * @param grandParentName
     * the name, which is to be checked against the grandparent node of
     * <code>n</code> (e.g., <code>GENERATOR_ELEMENT</code>)
     * @param n
     * the node, which is to be checked
     *
     * @return
     * <code>true</code>, if the parent of node <code>n</code> has the name
     * <code>parentName</code> and if the grandparent has the name
     * <code>grandParentName</code>; else <code>false</code>
     */
    private static boolean isParentA(   String parentName,
                                        String grandParentName,
                                        Node n)
    {
        Node parentNode = n.getParentNode();
        if(parentNode == null)
            return false;
        Node grandParentNode = parentNode.getParentNode();
        if(grandParentNode == null)
            return false;
        short parentNodeType = parentNode.getNodeType();
        String parentNodeName = parentNode.getNodeName();
        short grandParentNodeType = grandParentNode.getNodeType();
        String grandParentNodeName = grandParentNode.getNodeName();
        return (parentNodeType == Node.ELEMENT_NODE
                && parentNodeName.equals(parentName)
                && grandParentNodeType == Node.ELEMENT_NODE
                && grandParentName.equals(grandParentNodeName));
    }

    /**
     * Returns the value of the attribute with name <code>name</code> and
     * namespace <code>namespace</code> of node <code>node</code>. If the
     * namespace is set to null, it is ignored.
     *
     * @param name
     * the name of the attribute which value is to be returned
     * @param namespace
     * the namespace of the attribute which value is to be returned
     * @param node
     * the node which contains the attribute which value is to be read
     *
     * @return
     * the value of the attribute with name <code>attributeName</code> and
     * namespace <code>namespace</code> of node <code>node</code>
     *
     * @throws IllegalArgumentException
     * if node <code>node</code> doesn't have an attribute with the given
     * namespace and name <code>attributeName</code>, which are set as the
     * message of the exception
     */
    private static String getAttributeAsString( String name,
                                                String namespace,
                                                Node node)
    throws IllegalArgumentException {
        NamedNodeMap attrs = node.getAttributes();
        Node attr;
        if(attrs == null
                || (attr = attrs.getNamedItemNS(namespace,name)) == null)
            throw new IllegalArgumentException(
                    ((namespace != null) ? namespace : "") + "/" + name);
        return attr.getNodeValue();
    }

    /**
     * Returns either the major or the minor part of a version number
     * (MAJOR.MINOR e.g., 1.10).
     *
     * @param version
     * the version number which major or minor part is to be extracted
     * @param minor
     * if <code>true</code> the minor version number is returned, else the major
     * version number is returned
     *
     * @return
     * either the major or the minor part of a version number
     *
     * @throws TemplateParseException
     * if the version number doesn't match the format NUMBER.NUMBER
     */
    private static int getPartOfVersionNumber(String version, boolean minor)
    throws TemplateParseException {
        Scanner s = new Scanner(version);
        MatchResult result;
        s.findInLine("^\\s*(\\d+)\\.(\\d+)\\s*$");
        try {
            result = s.match();
        } catch(IllegalStateException ise) {
            throw new TemplateParseException(
                    "The template contains an invalid report version number.");
        }
        s.close();
        return (new Integer(result.group(minor ? 2 : 1))).intValue();
    }

}
