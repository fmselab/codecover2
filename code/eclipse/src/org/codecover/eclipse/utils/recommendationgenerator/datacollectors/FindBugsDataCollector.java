package org.codecover.eclipse.utils.recommendationgenerator.datacollectors;
//import java.io.IOException;
//import java.util.Date;
//import java.util.HashMap;
//import java.util.HashSet;
//import java.util.Map;
//import java.util.Set;
//
//import org.codecover.eclipse.utils.recommendationgenerator.DataCollector;
//import org.codecover.eclipse.utils.recommendationgenerator.ToResourceConverter;
//import org.eclipse.core.resources.IFolder;
//import org.eclipse.core.resources.IProject;
//import org.eclipse.core.resources.IResource;
//import org.eclipse.core.resources.IWorkspaceRoot;
//import org.eclipse.core.resources.ResourcesPlugin;
//import org.eclipse.core.runtime.IPath;
//import org.eclipse.jdt.core.IJavaProject;
//import org.eclipse.jdt.core.IPackageFragmentRoot;
//import org.eclipse.jdt.core.JavaCore;
//import org.eclipse.jdt.core.JavaModelException;
//import org.eclipse.jdt.internal.core.JavaProject;
//
//import edu.umd.cs.findbugs.BugCollectionBugReporter;
//import edu.umd.cs.findbugs.BugInstance;
//import edu.umd.cs.findbugs.BugReporter;
//import edu.umd.cs.findbugs.ClassScreener;
//import edu.umd.cs.findbugs.DetectorFactoryCollection;
//import edu.umd.cs.findbugs.FindBugs2;
//import edu.umd.cs.findbugs.Project;
//import edu.umd.cs.findbugs.config.UserPreferences;
//
//public class FindBugsDataCollector extends DataCollector {
//
//	public FindBugsDataCollector() {
//	}
//
//	@Override
//	public void invoke() throws JavaModelException {
//		Set<IProject> projects = new HashSet<IProject>();
//		// Alle verwendeten Projekte feststellen
//		for (IResource resource : getRelevantFiles()) {
//			if (resource != null) {
//				projects.add(resource.getProject());
//			}
//		}
//
//		// FindBugs auf jedes Projekt laufen lassen
//		for (IProject project : projects) {
//			if (project.isOpen() && JavaProject.hasJavaNature(project)) { 
//			  IJavaProject javaProject = JavaCore.create(project);
//				try {
//					IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
//					FindBugs2 fb = new FindBugs2();
//
//					Project fbProject = new Project();
//					
//					IPath outputLocation = javaProject.getOutputLocation();
//					for (IPackageFragmentRoot root : javaProject.getPackageFragmentRoots()) {
//						if (root.getKind() == IPackageFragmentRoot.K_SOURCE) {
//							IFolder folder = wsRoot.getFolder(root.getPath());
//							String sourceDir = folder.getLocation().toPortableString();
//							fbProject.addSourceDir(sourceDir);
//						}
//					}
//
//					IFolder folder = wsRoot.getFolder(outputLocation);
//					String classDir = folder.getLocation().toPortableString();
//					fbProject.setProjectName(project.getName());
//					fbProject.addFile(classDir);
//					
//
//					fb.setProject(fbProject);
//
//					// XMLBugReporter xmlBugReporter = new
//					// XMLBugReporter(project);
//					// xmlBugReporter.setPriorityThreshold(BugReporter.NORMAL);
//					BugCollectionBugReporter reporter = new BugCollectionBugReporter(fbProject);
//					reporter.setPriorityThreshold(BugReporter.NORMAL);
//					reporter.setErrorVerbosity(BugReporter.SILENT);
//					fb.setBugReporter(reporter);
//					fb.setClassScreener(new ClassScreener());
//					fb.setDetectorFactoryCollection(DetectorFactoryCollection.instance());
//					fb.setUserPreferences(UserPreferences.createDefaultUserPreferences());
//					// fb.setRankThreshold(20);
//					fb.finishSettings();
//					long a = new Date().getTime();
//					fb.execute();
//					System.out.println("FindBugs-Durchlauf: "+(new Date().getTime()-a)+" ms");
//					
//				/*	for (PackageStats packageStat : reporter.getProjectStats().getPackageStats()) {
//						if (packageStat.getTotalBugs() > 0) {
//							System.out.println(packageStat.getPackageName() + ": " + packageStat.getTotalBugs());
//						}
//					}
//					*/
//					
//					
//					Map<String, Integer> classErrorCount = new HashMap<String, Integer>(); 
//
//					for (BugInstance bug : reporter.getBugCollection().getCollection()) {
//						//	System.out.println(bug.getPrimaryClass().toString() + " - "
//					//			+ bug.getPrimarySourceLineAnnotation().toString());
//						String className = bug.getPrimaryClass().getClassName();
//						IResource resourceOfBug = ToResourceConverter.getResourceFromClass(className);
//						if (resourceOfBug != null) {
//							int startLine = bug.getPrimarySourceLineAnnotation().getStartLine();
//							int endLine = bug.getPrimarySourceLineAnnotation().getEndLine();
//							Double valuePerErrorLine = (Double) this.getErrorIndicator().getParameter("LineErrorRatingFactor").getValue();
//							// Die FindBugs-Bugschwere geht von 0-20. 
//							valuePerErrorLine *= (bug.getBugRank() / 10);
//							for (int line = startLine ; line <= endLine ; line++) {
//								// Wenn FindBugs mehrere Zeilen anmalt, verteilen wir die Fehlerpunkte auf diese Zeilen.
//								this.getErrorIndicator().addProbabilityInformation(resourceOfBug, line, valuePerErrorLine / ((endLine-startLine)==0?1:(endLine-startLine)));
//							}
//						} else {
//							System.out.println("Zu "+className+" konnte Eclipse keine Klasse finden.");
//						}
//						
//						if (classErrorCount.containsKey(className)) {
//							classErrorCount.put(className, classErrorCount.get(className)+1);
//						} else {
//							classErrorCount.put(className, 1);
//						}
//						
//					}
//					
//					for (String clazz : classErrorCount.keySet()) {
//						this.getErrorIndicator().addProbabilityInformation(
//								ToResourceConverter.getResourceFromClass(clazz), classErrorCount.get(clazz));
//					}
//
//				} catch (IOException e) {
//					// TODO Auto-generated catch block
//					e.printStackTrace();
//				} catch (InterruptedException e) {
//					// TODO Auto-generated catch block
//					e.printStackTrace();
//				}
//			}
//
//		}
//	}
//
//}
