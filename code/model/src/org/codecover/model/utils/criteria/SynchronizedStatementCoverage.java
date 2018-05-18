package org.codecover.model.utils.criteria;

/**
 * @author 
 *
 * @version 1.0 ($Id$)
 */
public class SynchronizedStatementCoverage extends AbstractCriterion {
    private static final SynchronizedStatementCoverage instance = new SynchronizedStatementCoverage();

    private final String INVOCATION_METHOD = "getInstance";

    /**
     * The prefix of synchronized statements ID's:<br>
     * <code>{@value #ID_PREFIX}</code>
     */
    public static final String ID_PREFIX = "Y";
    
    /**
     * The suffix of the synchronized statement sub ID for zero:<br>
     * <code>{@value #ID_SUFFIX_ZERO}</code>
     */
    public static final String ID_SUFFIX_ZERO = "-0";

    /**
     * The suffix of the synchronized statement sub ID for one:<br>
     * <code>{@value #ID_SUFFIX_ONE}</code>
     */
    public static final String ID_SUFFIX_ONE = "-1";

    /**
     * The suffix of the synchronized statement sub ID for more than one:<br>
     * <code>{@value #ID_SUFFIX_ABOVE}</code>
     */
    public static final String ID_SUFFIX_ABOVE = "-2";
    
    /**
     * the String "<b>SynchronizedStatementCoverage</b>"
     */
    public static final String NAME = "SynchronizedStatementCoverage";

    /**
     * @return The single instance of SynchronizedStatementCoverage;
     */
    public static SynchronizedStatementCoverage getInstance() {
        return instance;
    }

    /**
     *
     */
    public SynchronizedStatementCoverage() {
        super(NAME, "org.codecover.model.utils.criteria.SynchronizedStatementCoverage", "org.codecover");
    }

}
