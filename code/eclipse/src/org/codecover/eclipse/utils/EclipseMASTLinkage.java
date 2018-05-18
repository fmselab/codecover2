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

package org.codecover.eclipse.utils;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.file.FileTool;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.core.search.SearchMatch;
import org.eclipse.jdt.core.search.SearchParticipant;
import org.eclipse.jdt.core.search.SearchPattern;
import org.eclipse.jdt.core.search.SearchRequestor;
import org.eclipse.jdt.internal.core.SourceType;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.texteditor.ITextEditor;

/**
 * Utilities to provide mappings between Eclipse Models and the MAST.
 * 
 * @author Johannes Langauf
 * @version 1.0 ($Id: EclipseMASTLinkage.java 75 2011-06-28 13:28:30Z dobrowolsk $)
 */
public abstract class EclipseMASTLinkage {
    
    /* Note: Mapping between SourceFile and IFile is unavailable, because:
     * 1. source files don't have paths relative to root (could be added)
     * 2. instrumentation root is unknown (could be added)
     * 3. no Mapping between instrumentation root and project root is possible
     *   (no way, if TSC has no added information from Eclipse)
     * 
     * Currently the best approximation is to search the workspace for
     * equivalent FileName and Content.
     */
    
    /**
     * Select and show <code>loc</code> in an editor with coverage
     * highlighting.
     * 
     * @param editor
     * the editor showing the unchanged <code>loc.getSourceFile()</code>
     * @param loc
     * the position to show
     * 
     * @return true
     */
    public static boolean showInEditor(ITextEditor editor, Location loc) {
        editor.selectAndReveal(loc.getStartOffset(), loc.getLength());
        
        return true;
    }
    
    /**
     * Open a java editor that contains the code of the header of hLev.
     * 
     * @param hLev
     *            the hLev that is a top level class or contained in one
     * @param tsc
     *            the Java test session container that contains
     *            <code>hLev</code>
     * @return the java editor
     */
    public static ITextEditor openClassInEditor(HierarchyLevel hLev,
            TestSessionContainer tsc) {
        ITextEditor result = null;
        
        hLev = MAST.getTopLevelClass(hLev, tsc);
        
        /* find corresponding compilation unit by class name */
        String fqn = MAST.getFQName(hLev, tsc);
        Set<ICompilationUnit> cuSet; 
        cuSet = EclipseMASTLinkage.Eclipse.findCompilationUnit(fqn);
        
        /* got matches by qualified class name */
        
        /* filter for matches that fit class */
        Location target = MAST.getHighlightLocation(hLev);
        SourceFile source = target.getFile();
        Set<ICompilationUnit> unchangedCu = null;
        unchangedCu = new HashSet<ICompilationUnit>(cuSet.size());
        for (ICompilationUnit cu: cuSet) {
            /* check identity */
            IResource resource = cu.getResource();
            if (resource != null) {
                IFile file = (IFile)resource;
                if (equals(file, source)) {
                    unchangedCu.add(cu);
                }
            }
        }
        
        int matchCount = unchangedCu.size();
        if (matchCount > 0) {
            if (matchCount > 1) {
                CodeCoverPlugin.getDefault().getLogger().warning("found too many matching cu (using first):" + matchCount); //$NON-NLS-1$
            }
            result = Eclipse.openMember(unchangedCu.iterator().next());
        } else {
            CodeCoverPlugin.getDefault().getLogger().debug("found no matching cu for: " + fqn); //$NON-NLS-1$
        }
        
        return result;
    }

    /**
     * Check if the given resouce and sourceFile are identical. i.e.
     * instrumenting the resource with the current settings would produce the
     * same SourceFile as instrumenting <code>sourceFile</code>.
     *
     * @param file
     * a synchronized Eclipse resource
     * @param sourceFile
     * the original source file to match
     * 
     * @return true, iff <code>resource</code> matches all attributes in
     * <code>sourceFile</code>
     */
    @SuppressWarnings("nls")
    public static boolean equals(IFile file, SourceFile sourceFile) {
        if (! file.exists()) {
            throw new IllegalArgumentException("file does not exist");
        }
        
        /* file name (without path) */
        if (!sourceFile.getFileName().equals(file.getName())) {
            return false;
        }
        
        /* extract content of file */
        String contentOfFile = null;
        try {
            contentOfFile = Eclipse.getContent(file);
        } catch (Exception e) {
            //TODO: is there a better way to handle files that are not synchronized (happened) or not local
            CodeCoverPlugin.getDefault().getLogger().warning("possible false"
                    + " negative comparing '" + file.toString()
                    + "' to match '" + sourceFile.toString()
                    + "', because content is unavailable:", e);
            return false;
        }
        String sourceFileContent = sourceFile.getContent();
        if (! sourceFileContent.equals(contentOfFile)) {
        	return false;
        }
        
        return true;
    }

    /**
     * Check if the given resource and sourceFile are identical. i.e.
     * instrumenting the resource with the current settings would produce the
     * same as instrumenting <code>sourceFile</code>.
     * 
     * @param resource
     *            the Eclipse resource
     * @param sourceFile
     *            the original source file to match
     * @return true, iff <code>resource</code> matches all attributes in
     *         <code>sourceFile</code>
     */
    public static boolean equals(IResource resource, SourceFile sourceFile) {
        IFile file;
        if (resource instanceof IFile) {
            /* shortcut */
            file = (IFile) resource;
        } else {
            file = resource.getAdapter(IFile.class);
            if (file == null) {
                throw new IllegalArgumentException("resource is no file"); //$NON-NLS-1$
            }
        }
        return equals(file, sourceFile);
    }
    
    /**
     * Create a Location that represents the current selection of an editor.
     * 
     * @param editor
     * @param file
     * the file that is opened in <code>editor</code>
     * @return
     * the current selection of <code>editor</code>
     */
    @SuppressWarnings("nls")
    public static Location getSelection(ITextEditor editor, SourceFile file) {
        MASTBuilder builder = new MASTBuilder(Logger.NULL);
        
        /* determine cursor position and selection */
        int offset = -1;
        int length = -1;        
        ISelection s = editor.getSelectionProvider().getSelection();
        ITextSelection selection;
        if (s instanceof ITextSelection) {
            selection = (ITextSelection) s;
            offset = selection.getOffset();
            length = selection.getLength();
        } else {
            throw new IllegalArgumentException("editor does not return a"
                    + " proper ITextSelection: " + s);
        }
        
        /* offset and length are set to the selection of editor */

        return builder.createLocation(file, offset, offset + length);        
    }
    

    /**
     * Find the corresponding CodeCover MAST HierarchyLevel to the given
     * Eclipse Java Element.
     * 
     * @param code
     *   root of code (MAST) to search
     * @param element
     *   search key
     * @return
     *   the HierarchyLevel of <i>element</i>, null if not found
     */
    public static HierarchyLevel findSource (HierarchyLevel code, IJavaElement element) {
        HierarchyLevel result = null; //null until element is found
        
        /* check input */
        if (code == null) {
            throw new IllegalArgumentException ("code is null"); //$NON-NLS-1$
        }
        if (element == null) {
            throw new IllegalArgumentException ("element is null"); //$NON-NLS-1$
        }
    
        /* get corresponding ICompilationUnit */
        ICompilationUnit compilationUnit;
        if (element.getElementType() == IJavaElement.COMPILATION_UNIT) {
            compilationUnit = (ICompilationUnit) element;
        } else {
            compilationUnit = (ICompilationUnit)
            element.getAncestor(IJavaElement.COMPILATION_UNIT);
        }
    
        if (compilationUnit != null) {
    
            /* Extract fully qualified class name with its package */
            String fileName = compilationUnit.getElementName();
            String className = fileName.split("\\.")[0]; //$NON-NLS-1$
            IPackageFragment pkgF = (IPackageFragment) compilationUnit.getAncestor(IJavaElement.PACKAGE_FRAGMENT);
            if (pkgF != null) {
                String path[];
                if (pkgF.getElementName().equals("")) { //$NON-NLS-1$
                    path = new String[] { className };
                } else {
                    className = pkgF.getElementName() + "." + className; //$NON-NLS-1$
                    path = className.split("\\."); //$NON-NLS-1$
                }
    
                /* find HierarchyLevel for the class by name */
                HierarchyLevel current = code;
                boolean found = true;
    
                for (int i = 0; i < path.length && found; ++i) {    
                    found = false;
                    
                    /* find next HierarchyLevel in path */
                    for (HierarchyLevel l: current.getChildren()) {
                        if (l.getName().equals(path[i])) {
                            current = l;
                            found = true;
                            break;
                        }
                    }
                }
                if (found) {
    
                    /* the whole path was successfully traversed */
                    result = current;
                }
            }
        }
    
        return result;
    }


    /**
     * Tools to work with MAST.
     * 
     * @version 1.0 ($Id: EclipseMASTLinkage.java 75 2011-06-28 13:28:30Z dobrowolsk $)
     * @author Johannes Langauf
     */
    public static class MAST {
        
        /**
         * Get a <code>Location</code> representing <code>hLev</code> in the
         * code. Does not have to contain all code of <code>hLev</code>.
         * 
         * @param hLev
         * a HierarchyLevel with a none empty header
         * 
         * @return a Location to represent <code>hLev</code>
         * @see HierarchyLevel#getHeader()
         */
        public static Location getHighlightLocation(HierarchyLevel hLev) {    
            List<Location> locList = hLev.getHeader().getLocations();
            Location target;
            if (locList.size() > 0) {
                target = locList.get(0);
            } else {
                throw new IllegalArgumentException("hLev has no header Location."); //$NON-NLS-1$
            }

            return target; 
        }
        
        /**
         * Get the top level class of a java hierarchy level as defined in the
         * JLS:
         * http://java.sun.com/docs/books/jls/third_edition/html/classes.html#246201
         * 
         * @param hLev
         * a top level class or its descendant
         * @param tsc
         * the java test session container that contains hLev
         * @return
         * the top level class that contains hLev or hLev, if it's the top level
         * class
         */
        public static HierarchyLevel getTopLevelClass(HierarchyLevel hLev,
                TestSessionContainer tsc) {
            HierarchyLevel parentPackage = hLev;
            HierarchyLevel topLevelClass = null;
            
            while (! parentPackage.getType().getInternalName().equals("package") //$NON-NLS-1$
                    && ! parentPackage.getType().getInternalName().equals("default package")) { //$NON-NLS-1$
                topLevelClass = parentPackage; 
                parentPackage = tsc.getParentOfHierarchyLevel(parentPackage);
            }
            
            return topLevelClass;
        }
        
        /**
         * Get the package of hLev.
         * 
         * @param hLev
         * a class or package
         * @param tsc
         * the java test session container that contains hLev
         * @return
         * the package of hLev, hLev itself if it's a package
         */
        public static HierarchyLevel getPackage(HierarchyLevel hLev,
                TestSessionContainer tsc) {
            //same as above, just return the parentPackage
            HierarchyLevel parentPackage = hLev;
            @SuppressWarnings("unused")
            HierarchyLevel topLevelClass = null;
            
            while (! parentPackage.getType().getInternalName().equals("package") //$NON-NLS-1$
                    && ! parentPackage.getType().getInternalName().equals("default package")) { //$NON-NLS-1$
                topLevelClass = parentPackage; 
                parentPackage = tsc.getParentOfHierarchyLevel(parentPackage);
            }
            
            return parentPackage;
        }
        
        /**
         * Return the fully qualified name of a compilation unit in a Java TSC.
         * Types of Hierarchy Levels can be found in
         * <code>HierarchyLevelTypeProvider</code>.
         * 
         * @param element
         * the compilation unit, valid types are class, interface, enum, &at;interface,  
         * @param tsc
         * the test session container containing <code>element</code>
         * 
         * @return the fully qualified name of <code>element</code>
         * @see org.codecover.instrumentation.java15.HierarchyLevelTypeProvider
         */
        @SuppressWarnings("nls")
        public static String getFQName(HierarchyLevel element,
                                       TestSessionContainer tsc) {
            if (element == null) {
                throw new IllegalArgumentException("HierarchyLevel is null");
            }
            String internalName =  element.getType().getInternalName();
            if (!internalName.equals("class")
                    && !internalName.equals("interface")
                    && !internalName.equals("enum")
                    && !internalName.equals("@interface")) {
                throw new IllegalArgumentException(
                        "HierarchyLevel is no Compilation unit. Type: "
                        + internalName);
            }
            if (tsc == null) {
                throw new IllegalArgumentException("tsc is null");
            }
            
            /* extract name of innermost level */
            String fqName = element.getName();
            element = tsc.getParentOfHierarchyLevel(element);
            
            /* add names of parent levels, not including the unnamed top level */
            while (!element.getType().getInternalName().equals("default package")) {
                fqName = element.getName() + "." + fqName;
                element = tsc.getParentOfHierarchyLevel(element);
            }
            return fqName;
        }
    }
    
    /**
     * Tools to work with Eclipse Java model.
     * 
     * @version 1.0 ($Id: EclipseMASTLinkage.java 75 2011-06-28 13:28:30Z dobrowolsk $)
     * @author Johanne Langauf
     */
    public static class Eclipse {
        
        /**
         * Read a whole file into a String.
         * 
         * @param file
         * the synchronized, existing, readable and local file
         * 
         * @return
         * the contents of the file
         * 
         * @see IResource#isSynchronized(int)
         * @see IResource#isLocal(int)
         */
        public static String getContent(IFile file) {
            String contentOfFile = null;
            try {
                InputStream inStream = file.getContents();
                Charset charsetOfFile = Charset.forName(file.getCharset());
                contentOfFile = FileTool.getContentFromStream(inStream, charsetOfFile);
            } catch (CoreException e) {
                boolean isSynchronized = false;
                boolean isLocal = false;
                try {
                    isSynchronized = file.isSynchronized(IResource.DEPTH_ZERO);
                    isLocal = file.isLocal(IResource.DEPTH_ZERO);
                } catch (Exception ex) {
                    CodeCoverPlugin.getDefault().getLogger().fatal(
                            "something is totally broken", ex); //$NON-NLS-1$
                }
                if (! isSynchronized) {
                    // There seems to be no way to handle this without
                    // bothering the user. Like the editor does. Asking
                    // the user if he want's to synchronize a file he hasn'
                    // t even seen like the editor does is stupid so we let
                    // callers take care of what they want.
                    throw new IllegalArgumentException("file is not in sync"); //$NON-NLS-1$
                }
                if (! isLocal) {
                    //TODO: Can this happen? - if so handle gracefully.
                    CodeCoverPlugin.getDefault().getLogger().error(
                            "Unexpected not local resource.  Please " //$NON-NLS-1$
                            + "file a bug with the whole message.", e); //$NON-NLS-1$
                    throw new IllegalArgumentException("file is not local"); //$NON-NLS-1$
                }

            } catch (IOException e) {
                CodeCoverPlugin.getDefault().getLogger().debug("Can't read: " //$NON-NLS-1$
                        + file.getFullPath().toOSString(), e);
                throw new IllegalArgumentException("File not readable."); //$NON-NLS-1$
            }
            return contentOfFile;
        }
        
        /**
         * Find a type by its name.
         * 
         * @param fQName
         * fully qualified name of the type
         * @return
         * list of matching compilation units
         */
        public static Set<ICompilationUnit> findCompilationUnit(String fQName) {
           final Set<ICompilationUnit> result = new HashSet<ICompilationUnit>(1);
           
           SearchPattern pattern = SearchPattern.createPattern(fQName,
                   IJavaSearchConstants.TYPE,
                   IJavaSearchConstants.DECLARATIONS,
                   SearchPattern.R_CASE_SENSITIVE
                   | SearchPattern.R_EXACT_MATCH);

           IJavaSearchScope scope = SearchEngine.createWorkspaceScope();
           //to narrow search, e.g. on one project you could use this:
//           IJavaProject p = null;
//           IJavaElement project[] = {p};
//           IJavaSearchScope scope;
//           scope = SearchEngine.createJavaSearchScope(project);

           SearchRequestor requestor = new SearchRequestor() {
               @Override
               public void acceptSearchMatch(SearchMatch match) throws CoreException {
                   if (match.getAccuracy() == SearchMatch.A_ACCURATE) {
                       addMatch(match.getResource());
                       addMatch(match.getElement());                   
                   }
               }
               private void addMatch(Object match) {
                   ICompilationUnit cu = null;
                   if (match instanceof SourceType) {
                       SourceType sf = 
                           (SourceType)match;
                       cu = sf.getCompilationUnit();
                   }
                   if (cu == null && match instanceof IJavaElement) {
                       
                       IJavaElement adaptable = (IJavaElement)match;
                       
                       cu = adaptable.getAdapter(ICompilationUnit.class);
                   }
                   if (cu != null) {
                       synchronized (result) {
                           result.add(cu);
                       }
                   }
               }
           };
           SearchEngine searchEngine = new SearchEngine();
           
           try {
               searchEngine.search(pattern,
                       new SearchParticipant[] {SearchEngine.getDefaultSearchParticipant()},
                       scope, requestor, null);
           } catch (CoreException e) {
               CodeCoverPlugin.getDefault().getLogger().warning(
                       "Ignoring failed search for: " + fQName, e); //$NON-NLS-1$
           }

           return result;
        }
        
        /**
         * Open a Java-Element in an <code>ITextEditor</code>. Don't focus it.
         * 
         * @param member
         * the element to open
         * 
         * @return the editor showing <code>member</code>, null if member can't
         *  be opened
         */
        public static ITextEditor openMember(IMember member) {
            ICompilationUnit cu = member.getCompilationUnit();
            return openMember(cu);
        }
        
        /**
         * Open a Java-Element in an <code>ITextEditor</code>. Don't focus it.
         * 
         * @param cu
         * the element to open
         * 
         * @return the editor showing <code>cu</code>, null if member can't
         *  be opened
         */
        public static ITextEditor openMember(ICompilationUnit cu) {
            ITextEditor editor;
            try {
                IEditorPart ed = JavaUI.openInEditor(cu, false, false);
                editor = ed.getAdapter(ITextEditor.class);
            } catch (PartInitException e) {
                /* the editor could not be initialized or no workbench page is
                 * active */
                return null;
            } catch (JavaModelException e) {
                /* cu does not exist or an exception occurs while accessing its
                 * underlying resource */
                return null;
            }
            
            return editor;
        }
    }
}