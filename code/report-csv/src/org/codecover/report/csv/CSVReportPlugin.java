/**
 * 
 */
package org.codecover.report.csv;

import org.codecover.model.extensions.AbstractExtension;
import org.codecover.model.extensions.AbstractPlugin;
import org.codecover.model.extensions.Extension;
import org.codecover.report.ReportGenerator;

/**
 * @author Michael
 *
 */
public class CSVReportPlugin extends AbstractPlugin {

        public CSVReportPlugin() {
                super("CSV Report",
                          "This plugin contains a csv report generator",
                          new Extension<?>[] {new AbstractExtension<ReportGenerator>(
                                            ReportGenerator.class,
                        "org.codecover.report.csv.CSVReportGenerator")
                          {
                              public ReportGenerator getObject() {
                                  return new CSVReportGenerator();
                              }
                          }
               });
        }

}
