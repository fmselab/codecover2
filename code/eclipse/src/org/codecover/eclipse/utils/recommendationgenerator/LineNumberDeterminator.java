package org.codecover.eclipse.utils.recommendationgenerator;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;

public class LineNumberDeterminator {

	private static IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();

	private static Map<Position, Integer> offsetLineNumberMap = new HashMap<Position, Integer>();
	
	
	private static Map<IResource, List<Integer>> lineBreakPositionMap = new HashMap<IResource, List<Integer>>();
	
	public static int getLineNumber(IResource resource, int offset) {
		System.out.println("getLineNumbers "+resource.getName()+ " " +offset);
		Position p = new Position(resource, offset);
		if (offsetLineNumberMap.containsKey(p)) {
			return offsetLineNumberMap.get(p);
		}
		
		IFile iFile = wsRoot.getFile(resource.getFullPath());
		String portableString = iFile.getLocation().toPortableString();

		
		File file = new File(portableString);
		FileInputStream fis;
		try {
			fis = new FileInputStream(file);
			int n = 0;
			int line = 1;
			while ((n = fis.read()) != -1) {
				char c = (char) n;
				System.out.println(c);
				if (c == Character.LINE_SEPARATOR) {
					line++;
				}
			}
			fis.close();
			offsetLineNumberMap.put(p, line);
			return line;
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return -1;
	}
	
	public static List<Integer> getLineNumbers(IResource resource, int offsetStart, int offsetEnd) {
		long a = new Date().getTime();
		List<Integer> ret = new ArrayList<Integer>();
		
		List<Integer> lineBreakPositions;
		if (lineBreakPositionMap.containsKey(resource)) {
			lineBreakPositions = lineBreakPositionMap.get(resource);
		} else {
			lineBreakPositions = new ArrayList<Integer>();
			IFile iFile = wsRoot.getFile(resource.getFullPath());
			String portableString = iFile.getLocation().toPortableString();
			File file = new File(portableString);
			FileInputStream fis;
			try {
				fis = new FileInputStream(file);
				int n = 0;
				int count = 0;
				while ((n = fis.read()) != -1) {
					count++;
					if (n == 10) {
						lineBreakPositions.add(count);
					}
				}
				fis.close();
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			lineBreakPositionMap.put(resource, lineBreakPositions);
		}
		
		int beginline = 1;
		int endline = 1;
		for (Integer lineBreakPosition : lineBreakPositions) {
			if (lineBreakPosition < offsetStart) {
				beginline++;
			}
			if (lineBreakPosition < offsetEnd) {
				endline++;
			}
		}
		for (int i = beginline ; i <= endline ; i++) {
			ret.add(i);
		}
		
		return ret;
	}
	
	
	
	
	
	
	
	
	
	private static class Position {
		public Position(IResource resource, int offset) {
			super();
			this.resource = resource;
			this.offset = offset;
		}
		IResource resource;
		int offset;
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + offset;
			result = prime * result + ((resource == null) ? 0 : resource.hashCode());
			return result;
		}
		@Override
		public boolean equals(Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (!(obj instanceof Position)) {
				return false;
			}
			Position other = (Position) obj;
			if (offset != other.offset) {
				return false;
			}
			if (resource == null) {
				if (other.resource != null) {
					return false;
				}
			} else if (!resource.equals(other.resource)) {
				return false;
			}
			return true;
		}
	}
}
