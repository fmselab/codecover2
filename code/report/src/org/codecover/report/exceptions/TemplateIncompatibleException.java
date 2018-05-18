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

package org.codecover.report.exceptions;

/**
 * An exception class used for signaling failure of reading a report template
 * because the implementation is incompatible to the version of the template.
 *
 * @author Robert Hanussek
 * @version 1.0 ($Id: TemplateIncompatibleException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class TemplateIncompatibleException extends TemplateException {

    /**
     * Version numbers fields of a template.
     */
    public enum VersionField {
        /**
         * the templates outer Version. In report unspecific Part. (report tag)
         */
        OUTER,
        
        /**
         * The template inner Version. In Report specific Part. (template tag)
         */
        INNER,
    }
    
    private static final long serialVersionUID = 0L;
    private final VersionField field; 
    
    /**
     * Constructs a TemplateIncompatibleException with a given message.
     *
     * @param message  the error message
     * @param reason
     * the field that triggered the Exception 
     */
    public TemplateIncompatibleException(String message, VersionField reason) {
        super(message);
        field = reason;
    }

    /**
     * Constructs a TemplateIncompatibleException with a given
     * <code>Throwable</code> that was its underlying cause.
     *
     * @param cause  the <code>Throwable</code> that caused this exception to
     *               occur
     * @param reason
     * the field that triggered the Exception 
     */
    public TemplateIncompatibleException(Throwable cause, VersionField reason) {
        super(cause);
        field = reason;
    }

    /**
     * Constructs a TemplateIncompatibleException with a given message and a
     * <code>Throwable</code> that was its underlying cause.
     *
     * @param message   the error message
     * @param cause     the <code>Throwable</code> that caused this exception to
     *                  occur
     * @param reason
     * the field that triggered the Exception 
     */
    public TemplateIncompatibleException(String message, Throwable cause,
            VersionField reason) {
        super(message, cause);
        field = reason;
    }
    
    /**
     * @return the field that triggered the Exception
     */
    public VersionField getField() {
        return field;
    }
    
    /**
     * @return true, iff the field that triggered the Exception is the
     * VersionField.OUTER
     */
    public boolean isOuter() {
       return field == VersionField.OUTER; 
    }
    
    /**
     * @return true, iff the field that triggered the Exception is the
     * VersionField.INNER
     */
    public boolean isInner() {
       return field == VersionField.INNER; 
    }
}
