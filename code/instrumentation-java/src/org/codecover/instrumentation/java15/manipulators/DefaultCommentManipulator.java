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

package org.codecover.instrumentation.java15.manipulators;

import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.codecover.instrumentation.java.measurement.Protocol;
import org.codecover.instrumentation.java15.syntaxtree.NodeToken;
/**
 * This {@link Manipulator} is used to transform comments into calls of
 * {@link Protocol}:
 * <ul>
 * <li><code>// startTestCase("Name", "Comment");</code> &rarr;
 * <code>org.codecover.instrumentation.java.measurement.Protocol.startTestCase("Name", "Comment");</code></li>
 * <li><code>// endTestCase("Name");</code> &rarr;
 * <code>org.codecover.instrumentation.java.measurement.Protocol.endTestCase("Name");</code></li>
 * </ul>
 * 
 * @see CommentManipulator
 * @see Protocol
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: DefaultCommentManipulator.java 22 2008-05-25 20:08:53Z ahija $)
 */
public class DefaultCommentManipulator extends AbstractDefaultManipulator
    implements CommentManipulator {

    private static final String NAME_REG_EXP = "(([^\"\\\\\n\r\t\b\f])|(\\\\[\\\\\'\"]))*";

    private static final String COMMENT_REG_EXP = "(([^\"\\\\\n\r\t\b\f])|(\\\\[ntbrf\\\\'\"]))*";

    private static final String START_REGEXP = "// (startTestCase\\(\""
        + NAME_REG_EXP + "\"(, \"" + COMMENT_REG_EXP + "\")?\\);)\\s*";

    /**
     * The regexp, for the {@link Protocol#startTestCase(String, String)}
     */
    public static final Pattern START_PATTERN = Pattern.compile(START_REGEXP);

    private static final String END_REGEXP = "// (endTestCase\\((\""
        + NAME_REG_EXP + "\"(, \"" + COMMENT_REG_EXP + "\")?)?\\);)\\s*";

    /**
     * The regexp, for the {@link Protocol#endTestCase(String, String)}
     */
    public static final Pattern END_PATTERN = Pattern.compile(END_REGEXP);

    private static final String FINISH_REGEXP = "// (finishTestSession\\(\\);)\\s*";

    /**
     * The regexp, for the {@link Protocol#finishTestSession()}
     */
    public static final Pattern FINISH_PATTERN = Pattern.compile(FINISH_REGEXP);

    private static final int PATTERN_MIN_LENGTH =
        Math.min("// startTestCase(\"\");".length(),
                 Math.min("// endTestCase();".length(),
                          "// finishTestSession();".length()));

    private static final String REPLACEMENT_PREFIX = Protocol.class.getName() + '.';

    public boolean manipulate(NodeToken token) throws IOException {
        String tokenImage = token.getParsedImage();
        if (tokenImage.length() >= PATTERN_MIN_LENGTH
            && tokenImage.charAt(0) == '/'
            && tokenImage.charAt(1) == '/'
            && tokenImage.charAt(2) == ' ') {

            // try to find the startTestCase();
            Matcher matcher = START_PATTERN.matcher(tokenImage);
            if (matcher.matches()) {
                super.getWriter().write(REPLACEMENT_PREFIX);
                super.getWriter().write(matcher.group(1));
                super.getWriter().write('\n');
                return true;
            }
            // start not found ->
            // try to find the endTestCase();
            matcher = END_PATTERN.matcher(tokenImage);
            if (matcher.matches()) {
                super.getWriter().write(REPLACEMENT_PREFIX);
                super.getWriter().write(matcher.group(1));
                super.getWriter().write('\n');
                return true;
            }
            // start and end not found ->
            // try to find the finishTestSession()
            matcher = FINISH_PATTERN.matcher(tokenImage);
            if (matcher.matches()) {
                super.getWriter().write(REPLACEMENT_PREFIX);
                super.getWriter().write(matcher.group(1));
                super.getWriter().write('\n');
                return true;
            }
        }
        return false;
    }

    public boolean requiresBlockExpansionsForBranches() {
        return false;
    }

    public boolean requiresBlockExpansionsForLoops() {
        return false;
    }

    public void writeDeclarations() throws IOException {
        // nothing to do
    }

    public void writeReset() throws IOException {
        // nothing to do
    }

    public void writeSerialzeAndReset() throws IOException {
        // nothing to do
    }
}
