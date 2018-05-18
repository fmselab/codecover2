package org.codecover.instrumentation.c;

import org.codecover.model.extensions.AbstractExtension;
import org.codecover.model.extensions.AbstractPlugin;
import org.codecover.model.extensions.Extension;

public class InstrumenterPlugin extends AbstractPlugin {
    public InstrumenterPlugin() {
        super("C Instrumenter Plugin", "", new Extension<?>[]{
                new AbstractExtension<org.codecover.instrumentation.InstrumenterDescriptor>(
                        org.codecover.instrumentation.InstrumenterDescriptor.class,
                        "org.codecover.instrumentation.java15.InstrumenterDescriptor") {
                    @Override
                    public org.codecover.instrumentation.InstrumenterDescriptor getObject() {
                        return new org.codecover.instrumentation.c.InstrumenterDescriptor();
                    }
                }
        });
    }
}
