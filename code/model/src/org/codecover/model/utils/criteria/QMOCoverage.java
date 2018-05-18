package org.codecover.model.utils.criteria;

/**
 * @author RS, 30.10.2009, the ?-operator
 *
 * @version 1.0 ($Id$)
 */
public class QMOCoverage extends AbstractCriterion {
    private static final QMOCoverage instance = new QMOCoverage();

    private final String INVOCATION_METHOD = "getInstance";

    /**
     * The prefix of QMO ID's:<br>
     * <code>{@value #ID_PREFIX}</code>
     */
    public static final String ID_PREFIX = "Q";
    
    /**
     * The suffix of the QMO first expression:<br>
     * <code>{@value #ID_SUFFIX_ZERO}</code>
     */
    public static final String ID_SUFFIX_ZERO = "-0";

    /**
     * The suffix of the QMO second expression:<br>
     * <code>{@value #ID_SUFFIX_ONE}</code>
     */
    public static final String ID_SUFFIX_ONE = "-1";

    
    /**
     * the String "<b>QMOCoverage</b>"
     */
    public static final String NAME = "QMOCoverage";

    /**
     * @return The single instance of QMOCoverage;
     */
    public static QMOCoverage getInstance() {
        return instance;
    }

    /**
     *
     */
    public QMOCoverage() {
        super(NAME, "org.codecover.model.utils.criteria.QMOCoverage", "org.codecover");
    }

}
