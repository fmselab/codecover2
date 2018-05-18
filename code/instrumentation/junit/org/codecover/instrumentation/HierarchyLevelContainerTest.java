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

package org.codecover.instrumentation;

import static org.codecover.UtilsForTestingInstr.newMASTBuilder;

import java.util.Collections;
import java.util.List;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingInstr;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.HierarchyLevelType;
import org.codecover.model.mast.StatementSequence;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: HierarchyLevelContainerTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class HierarchyLevelContainerTest extends TestCase {

    private MASTBuilder builder;

    private UtilsForTestingInstr.HLTProvider hltProvider;

    @Override
    protected void setUp() throws Exception {
        this.builder = newMASTBuilder();
        this.hltProvider = new UtilsForTestingInstr.HLTProvider(this.builder);
    }

    @Override
    protected void tearDown() throws Exception {
        this.builder = null;
        this.hltProvider = null;
    }

    /**
     * tests {@link HierarchyLevelContainer#HierarchyLevelContainer(String, HierarchyLevelType, HierarchyLevelType)}
     */
    public void testHierarchyLevelContainer1() {
        String name = "org";

        HierarchyLevelContainer hlc = new HierarchyLevelContainer(name,
                this.hltProvider.getPackageType(),
                this.hltProvider.getClassType());

        Assert.assertSame(name, hlc.getName());
        Assert.assertSame(this.hltProvider.getPackageType(), hlc.getType());
        Assert.assertSame(this.hltProvider.getClassType(), hlc.getSubtypes());
    }

    /**
     * tests
     * {@link HierarchyLevelContainer#addHierarchyLevelToRoot(org.codecover.model.mast.HierarchyLevel)}
     */
    public void testAddHierarchyLevelToRoot() {
        String packageName = "main package";
        String name = "class 1";

        HierarchyLevelContainer hlc = new HierarchyLevelContainer(packageName,
                this.hltProvider.getDefaultPackageType(),
                this.hltProvider.getPackageType());

        hlc.addHierarchyLevelToRoot(createHL(name));

        HierarchyLevel root = hlc.transformToHierarchyLevel(this.builder);
        Assert.assertEquals(packageName, root.getName());
        Assert.assertTrue(root.getSequences().isEmpty());
        Assert.assertEquals(1, root.getChildren().size());
        HierarchyLevel child = root.getChildren().get(0);
        
        Assert.assertEquals(name, child.getName());
    }

    public void testPackagePathToList1() {
        String packageName = "";
        List<String> packageList = HierarchyLevelContainer.packagePathToList(packageName, false);

        Assert.assertTrue(packageList.isEmpty());
    }

    public void testPackagePathToList2() {
        String packageName = "org";
        List<String> packageList = HierarchyLevelContainer.packagePathToList(packageName, false);

        Assert.assertEquals(1, packageList.size());
        Assert.assertEquals(packageName, packageList.get(0));
    }

    public void testPackagePathToList3() {
        String packageName = "org.codecover";
        List<String> packageList = HierarchyLevelContainer.packagePathToList(packageName, false);

        Assert.assertEquals(2, packageList.size());
        Assert.assertEquals("org", packageList.get(0));
        Assert.assertEquals("codecover", packageList.get(1));
    }

    public void testPackagePathToList4() {
        String packageName = ".org.codecover";

        try {
            HierarchyLevelContainer.packagePathToList(packageName, false);
            Assert.fail("IllegalArgumentException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
        }
    }

    public void testPackagePathToList5() {
        String packageName = "org.codecover.";

        try {
            HierarchyLevelContainer.packagePathToList(packageName, false);
            Assert.fail("IllegalArgumentException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
        }
    }

    public void testPackagePathToList6() {
        String packageName = ".org.codecover.";

        try {
            HierarchyLevelContainer.packagePathToList(packageName, false);
            Assert.fail("IllegalArgumentException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
        }
    }

    public void testPackagePathToListFull1() {
        String packageName = "";
        List<String> packageList = HierarchyLevelContainer.packagePathToList(packageName, true);

        Assert.assertTrue(packageList.isEmpty());
    }

    public void testPackagePathToListFull2() {
        String packageName = "org";
        List<String> packageList = HierarchyLevelContainer.packagePathToList(packageName, true);

        Assert.assertEquals(1, packageList.size());
        Assert.assertEquals(packageName, packageList.get(0));
    }

    public void testPackagePathToListFull3() {
        String packageName = "org.codecover";
        List<String> packageList = HierarchyLevelContainer.packagePathToList(packageName, true);

        Assert.assertEquals(2, packageList.size());
        Assert.assertEquals("org", packageList.get(0));
        Assert.assertEquals(packageName, packageList.get(1));
    }

    public void testPackagePathToListFull4() {
        String packageName = ".org.codecover";

        try {
            HierarchyLevelContainer.packagePathToList(packageName, true);
            Assert.fail("IllegalArgumentException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
        }
    }

    public void testPackagePathToListFull5() {
        String packageName = "org.codecover.";

        try {
            HierarchyLevelContainer.packagePathToList(packageName, true);
            Assert.fail("IllegalArgumentException expected");
        } catch (RuntimeException e) {
            Assert.assertTrue(e instanceof IllegalArgumentException);
        }
    }

    private HierarchyLevel createHL(String name) {
        return this.builder.createHierarchyLevel(this.builder.createEmptyLocationList(),
                name,
                this.builder.createEmptyLocationList(),
                this.hltProvider.getClassType(),
                Collections.<HierarchyLevel>emptyList(),
                Collections.<StatementSequence>emptyList());
    }
}
