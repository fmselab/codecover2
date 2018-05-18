package org.codecover.eclipse.utils.recommendationgenerator.datacollectors;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IResource;
import org.eclipse.team.core.TeamException;
import org.tigris.subversion.subclipse.core.ISVNLocalResource;
import org.tigris.subversion.subclipse.core.ISVNRemoteResource;
import org.tigris.subversion.subclipse.core.resources.SVNWorkspaceRoot;
import org.tigris.subversion.svnclientadapter.ISVNLogMessage;
import org.tigris.subversion.svnclientadapter.SVNRevision;
import org.tigris.subversion.svnclientadapter.SVNRevision.Number;

public class SVNInfoCache {

	private static Map<IResource, Long> firstRevisionCache = new HashMap<IResource, Long>();
	private static Map<IResource, ISVNRemoteResource> remoteRevisionCache = new HashMap<IResource, ISVNRemoteResource>();

	public static Long getFirstRevision(IResource resource) {
		if (firstRevisionCache.containsKey(resource)) {
			return firstRevisionCache.get(resource);
		}

		ISVNRemoteResource remoteResource = getRemoteResource(resource);
		
		try {
			ISVNLogMessage[] messages = remoteResource.getLogMessages(SVNRevision.BASE, SVNRevision.START, SVNRevision.COMMITTED,
					true, false, 100, true);
			if (messages != null && messages.length > 0) {
				messages[0].getDate();
				Number revision = messages[0].getRevision();
				System.out.println("Erste Revision von Datei " + resource.getName() + " ist " + revision.getNumber());
				firstRevisionCache.put(resource, revision.getNumber());
				return revision.getNumber();
			}
		} catch (TeamException e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	public static Date getFirstRevisionDate(IResource resource) {
		ISVNRemoteResource remoteResource = getRemoteResource(resource);
		if (remoteResource == null) {
			return null;
		}
		try {
			ISVNLogMessage[] messages = remoteResource.getLogMessages(SVNRevision.BASE, SVNRevision.START, SVNRevision.COMMITTED,
					true, false, 100, true);
			if (messages != null && messages.length > 0) {
				return messages[0].getDate();
			}
		} catch (TeamException e) {
			e.printStackTrace();
		}
		
		return null;
	}

	public static ISVNRemoteResource getRemoteResource(IResource resource) {
		if (remoteRevisionCache.containsKey(resource)) {
			return remoteRevisionCache.get(resource);
		}
		if (resource != null) {
			ISVNLocalResource svnResource = SVNWorkspaceRoot.getSVNResourceFor(resource);
			if (svnResource != null) {
				try {
					ISVNRemoteResource remoteResource = svnResource.getBaseResource();
					remoteRevisionCache.put(resource, remoteResource);
					return remoteResource;
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
		return null;
	}
	
	public static ISVNLocalResource getLocalResource(IResource resource) {
		ISVNLocalResource svnResource = SVNWorkspaceRoot.getSVNResourceFor(resource);
		return svnResource;
	}
}
