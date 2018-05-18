package org.codecover.eclipse.views;

import org.codecover.eclipse.views.CoverageView.GroupByActionsManager;
import org.codecover.model.mast.HierarchyLevel;

/**
 * The types of hierarchical levels, e.g. packages or classes. This enum is
 * used to categorize the hierarchical code levels to be able to group the
 * code tree by specific types.
 * <p>
 * Bear in mind that classes, interfaces and enums are subsumed under
 * {@link #CLASS}.
 * <p>
 * A <code>Type</code> is greater than another one if a corresponding
 * <code>HierarchyLevel</code> would be a parent (or grandparent) of a
 * <code>HierarchyLevel</code> of the other type, i.e. packages are greater
 * than classes.
 *
 * @see GroupByActionsManager
 */
enum Type {
    /**
     * Constant for projects.
     */
    PROJECT     (CoverageView.DEFAULT_PACKAGE_NAME),
    /**
     * Constant for packages.
     */
    PACKAGE     (CoverageView.PACKAGE_NAME),
    /**
     * Constant for classes, interfaces and enums.
     */
    CLASS       (CoverageView.CLASS_NAME),
    /**
     * Constant for methods.
     */
    METHOD      (CoverageView.METHOD_NAME);

    private final String internalName;

    private Type(String internalName) {
        this.internalName = internalName;
    }

    /**
     * Returns the internal name of the corresponding type of a
     * <code>HierarchyLevel</code>
     * (see <code>HierarchyLevelType.getInternalName()</code>).
     * Java default packages are displayed as the project nodes, which are
     * always on top-level.
     * The name for
     * the {@link #CLASS} type is returned as &quot;class&quot; although it
     * subsumes classes, interfaces and enums.
     *
     * @return  the internal name of the <code>HierarchyLevel</code>
     */
    public String getName() {
        return this.internalName;
    }

    /**
     * Returns the <code>Type</code> which matches the internal name
     * of the given <code>HierarchyLevel</code>.
     *
     * @param hLev  the <code>HierarchyLevel</code>
     *
     * @return  the <code>Type</code> which matches the internal name of the
     *          given <code>HierarchyLevel</code>
     */
    public static Type typeOf(HierarchyLevel hLev) {
        return Type.typeOf(hLev.getType().getInternalName());
    }

    /**
     * Returns the <code>Type</code> which matches the given internal name
     * of a <code>HierarchyLevel</code>.
     *
     * @param name  the name
     *
     * @return  the <code>Type</code> which matches the given internal name
     *          of a <code>HierarchyLevel</code>
     */
    public static Type typeOf(String name) {
        if(name.equals(CoverageView.INTERFACE_NAME)
                || name.equals(CoverageView.ENUM_NAME)
                || name.equals(CoverageView.ANNOTATION_NAME)) {
            return Type.CLASS;
        }
        for(Type type : Type.values()) {
            if(type.getName().equals(name)) {
                return type;
            }
        }
        return null;
    }
}