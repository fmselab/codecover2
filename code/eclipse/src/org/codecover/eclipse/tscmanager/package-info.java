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

/**
 * Provides the data management of the Eclipse plugin of CodeCover, that is it
 * handles the test session containers and active test cases available in
 * Eclipse. This package provides the following functions available through
 * {@link org.codecover.eclipse.tscmanager.TSContainerManager}:
 * <ul>
 * <li>Providing methods to select the active test session container and the
 * active test cases</li>
 * <li>Providing access to the currently active test session container (to
 * add/delete/modify test elements)</li>
 * <li>Providing the currently active test cases</li>
 * <li>Providing a list of the currently known test session containers</li>
 * <li>Providing methods to add and delete test session containers</li>
 * <li>Notification of listeners (e.g., the views of the plugin) about
 * changes</li>
 * <li>Saving and loading the test session containers</li>
 * <li>Saving and loading which test cases are active</li>
 * </ul>
 * 
 * <h3>Terminology</h3>
 * 
 * The following terms are used throughout the documentation of this package.
 * <dl>
 * <dt>Test session container</dt>
 * <dd>An object of class <code>TestSessionContainer</code>. The Eclipse plugin
 * of CodeCover can handle multiple test session containers. Each test session
 * container is associated with a specific project in Eclipse.</dd>
 * <dt>Known test session container</dt>
 * <dd>A Test session container which is associated with a currently open
 * project is called a known test session container. The reason for this term is
 * that files and folders which reside in closed projects aren't accessible and
 * thus can't be &quot;known&quot; by the plugin.</dd>
 * <dt>CodeCover-folder</dt>
 * <dd>The files of the test session containers are stored in the
 * CodeCover-folder of the associated project.</dd>
 * <dt>Active test session container</dt>
 * <dd>The active test session container is the known test session container
 * which contains the active test cases which are currently visualized in the
 * plugin's views, e.g. the Coverage view and the Test Sessions view. The active
 * test cases can be selected in the Test Sessions view.</dd>
 * <dt>Test element</dt>
 * <dd>A test element is either a test session or a test case, that is an object
 * of class <code>TestSession</code> or class <code>TestCase</code>.</dd>
 * </dl>
 * 
 * <h3>Out of memory errors</h3>
 * 
 * The {@link org.codecover.eclipse.tscmanager.TSContainerManager} catches
 * <code>OutOfMemoryError</code>s when loading or saving test session
 * containers. This seems to be strange but is necessary, because if we don't
 * catch them Eclipse will. And if we don't catch and handle them, the plugin
 * can enter an inconsistent state (which wouldn't be a problem if Eclipse would
 * crash on <code>OutOfMemoryError</code>s, but it doesn't always).
 * 
 * @version $Id: package-info.java 1 2007-12-12 17:37:26Z t-scheller $
 */

package org.codecover.eclipse.tscmanager;