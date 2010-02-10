/**
 * Class for processing phase zero, reading the file list. For more
 * information about the interface, see its documentation.
 */

import java.sql.ResultSet;
import java.sql.PreparedStatement;

public class FileListPhase implements Phase {
    
    /**
     * Reads a file list.
     *
     * @param in Input row to process
     * @param out Output row to produce. Please note this may contain
     *            old data. You should reset all fields.
     * @returns True if it has set statement ready for excecution.
     */
    public void process(ResultSet in, PreparedStatement out) {
	// TODO
    }

    /**
     * In case of an error in insertion. 
     *
     * @param in Input row which contains the row that was tried to process.
     * @param out Error row to produce. Please note that this may
     *        contain old data. You should reset all fields.
     */
    public void error(ResultSet in, Exception e, PreparedStatement err) {
	// TODO
    }

    public static void main() throws Exception {

	DatabaseTool tool =
	    new DatabaseTool("SELECT \"log_access.txt\"",
			     "INSERT phase_1_data (file) VALUES(?)",
			     "INSERT phase_0_error ()");
	
	tool.processTable(new FileListPhase());
    }
}
