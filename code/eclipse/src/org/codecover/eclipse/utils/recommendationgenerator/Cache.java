package org.codecover.eclipse.utils.recommendationgenerator;

import java.util.HashMap;
import java.util.List;

import org.codecover.model.TestCase;
import org.codecover.model.mast.HierarchyLevel;
import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;

public class Cache {

	public static HashMap<HierarchyLevel, IResource> hLevToFileCache = new HashMap<HierarchyLevel, IResource>();
	public static HashMap<HierarchyLevel, IJavaElement> hLevToPackageCache = new HashMap<HierarchyLevel, IJavaElement>();
	public static HashMap<HierarchyLevel, ICompilationUnit> hLevToCompUnitCache = new HashMap<HierarchyLevel, ICompilationUnit>();
	public static HashMap<IResource, List<TestCase>> fileToTestCaseMap = new HashMap<IResource, List<TestCase>>();

}
