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

import java.io.IOException;
import java.io.Writer;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;
import org.apache.velocity.exception.MethodInvocationException;
import org.apache.velocity.exception.ParseErrorException;
import org.apache.velocity.exception.ResourceNotFoundException;
import org.codecover.model.utils.Logger;

/**
 * Utility Class to hide init of Velocity. Offers some common methods to more
 *  easily use Velocity.
 * 
 * @author Johannes Langauf <j@j8l.de>
 */

public class VelocityHelper {
    
    /* always valid after constructor finshed */
    private VelocityEngine velocityEngine = null;
    
    private Logger logger = null;
    
    public VelocityHelper() {
        this ((Logger) null);
    }
            
    public VelocityHelper(Logger logger) {
        this.logger = logger;
        
        velocityEngine = new VelocityEngine();
        try {
            init(); //initialize velocityEngine
        } catch (Exception e) {
            /* should be unreachable */
            logFatal("Velocity is in a really bad mood today. Please"
                     + " check your hardware and your installation of"
                     + " Velocity (bundled with codecover). If they are"
                     + " fine this code is not. Please contact Johannes"
                     + " Langauf <software-stuproa@j8l.de> with the"
                     + " complete text of this message.", e);
        }
    }
    
    public VelocityHelper(String templatePath) throws Exception {
        velocityEngine = new VelocityEngine();
        
        /* set default Path for Templates */
        velocityEngine.setProperty("file.resource.loader.path", templatePath);
        init(); //init velocity engine
    }
    
    private void init() throws Exception {
    
        /* make Velocity quiet */
        
        velocityEngine.setProperty("runtime.log.logsystem.class",
            "org.apache.velocity.runtime.log.NullLogSystem");
        
        velocityEngine.init();
    }
    
    /**
     * Apply a template to the outputstream and if possible return the parsed
     *  and cached Template for later Reuse.
     * 
     * @param template
     *             The velocity template to evaluate as a String.
     * @param templateName
     *             A name for the template or null.
     * @param context
     *             The context to evaluate the template.
     * @param out
     *             The stream to write output the Result.
     * 
     * @return the parsed Template or null
     * 
     * @throws ParseErrorException
     *             thrown when the template contains syntax errors
     * @throws MethodInvocationException
     *             thrown when method calls inside the template return exceptions
     * @throws IOException
     *             thrown on write errors
     */
    public Template evaluate(VelocityContext context, Writer out,
                String templateName, String template, Template vtl)
                    throws ParseErrorException,
                        MethodInvocationException, IOException {
        
    /* use given chached template, if any */
        if (vtl != null) {
            try {
                vtl.merge(context, out);
            } catch (ResourceNotFoundException e) {
                
                /*mark cached template as broken to trigger it's regeneration*/
                vtl = null;
            } catch (Exception e) {
            
                /* something is really broken with Velocity, when this is
                 *  reached */
                
                /* mark cached template as broken */
                vtl = null;

                logWarn("The cached template is damaged in an unexpected way."
                        + " Please report this bug.", e);
            }
            return vtl;
        }
        
        if (vtl == null) {
            if (templateName == null) {
                templateName = "VelocityHelper_evaluate";
            }
        
            try {
                velocityEngine.evaluate(context, out, templateName, template);
            } catch (ResourceNotFoundException e) {
            
                /* Really don't expect this as we don't use the ResourceLoader
                 *  here. 
                 */
                logFatal("Unrequested resource not found", e);
            }
        }
        
        return null;
    }
    
    /**
     * ( see above, with templateName=null )
     */
    public Template evaluate(String template, VelocityContext context, Writer out) throws ParseErrorException, MethodInvocationException, IOException {

        return evaluate(context, out, template, null, null);
    }
    
    /*
     * get and set
     */
    public VelocityEngine getVelocityEngine() {
        return velocityEngine;
    }
    
    public void setLogger(Logger logger) {
        this.logger = logger;
    }

    /*
     * utilites
     */
    private void logFatal (String msg, Exception e) {
        if (logger != null) {
            logger.fatal(msg, e);
        } else {
            throw new RuntimeException(e);
        }
    }
    private void logWarn (String msg, Exception e) {
        if (logger != null) {
            logger.warning(msg, e);
        } else {
            throw new RuntimeException(e);
        }
    }
    
}
