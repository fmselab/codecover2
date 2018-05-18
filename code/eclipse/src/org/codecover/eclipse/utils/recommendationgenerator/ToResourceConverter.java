package org.codecover.eclipse.utils.recommendationgenerator;

import java.util.HashMap;
import java.util.Set;

import org.codecover.eclipse.utils.EclipseMASTLinkage;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jdt.core.ICompilationUnit;

public class ToResourceConverter {

	private static HashMap<String, IResource> pathToResourceMap = new HashMap<String, IResource>();
	
	private static HashMap<String, IResource> classToResourceMap = new HashMap<String, IResource>();
	
	
	/**
	 * Gibt ne IResource zurück. Path muss relativ zum WorkspaceRoot angegeben sein. Beispielsweise in der Form
	 * projektname/src/de/bundesregierung/Bundestrojaner.java
	 * @param path
	 * @return
	 */
	public static IResource getResourceFromPath(String path) {
		if (pathToResourceMap.containsKey(path) && pathToResourceMap.get(path) != null) {
			return pathToResourceMap.get(path);
		}
		IResource resource = ResourcesPlugin.getWorkspace().getRoot().findMember(path);
		pathToResourceMap.put(path, resource);
		return resource;
	}
	
	/**
	 * Gibt ne IResource zurück. Class ist ein fully-qualified class name.
	 * Beispielsweise de.bundesregierung.Bundestrojaner
	 * @param path
	 * @return
	 */
	public static IResource getResourceFromClass(String clazz) {
		if (classToResourceMap.containsKey(clazz) && classToResourceMap.get(clazz) != null) {
			return classToResourceMap.get(clazz);
		}
		Set<ICompilationUnit> cuSet = EclipseMASTLinkage.Eclipse.findCompilationUnit(clazz);
		IResource resource = null;
		// Scheinbar enthält das Set stets nur eine CompilationUnit...
		for (ICompilationUnit cu : cuSet) {
			resource = cu.getResource();
			break;
		}
		classToResourceMap.put(clazz, resource);
		return resource;
	}
}
